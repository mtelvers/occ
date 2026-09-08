(* POSIX regular expressions (IEEE Std 1003.1-2017, XBD 9): basic regular
   expressions (9.3), extended ones (9.4), and the matching rule of 9.1.

   The rule is the part an approximate implementation gets wrong.  9.1 says
   a match is the *leftmost* one, and among matches starting there the
   *longest*; a Perl-style backtracker instead returns the first match a
   greedy left-to-right search stumbles on, which for `a|ab' against "ab"
   is "a" rather than "ab".  So the matcher here is a Thompson NFA
   simulated over the subject in lock step (Pike's virtual machine): the
   pattern compiles to a program, and a list of threads, each a program
   counter with its capture registers, advances one character at a time.
   Every thread reaching the match instruction offers an end position and
   the largest is kept, which is exactly "longest".  Threads that share a
   program counter behave identically from then on, so only the
   highest-priority one is kept, which bounds the work at each position by
   the length of the program: the whole search is linear in the subject.

   Capture positions for the subexpressions are those of the surviving
   thread, chosen by a greedy search order rather than by the standard's
   full subexpression rule; the overall match is always the standard's.
   Back-references (\1 to \9, 9.3.6) cannot be expressed in this machine
   and go to a backtracking matcher over the syntax tree instead, which
   for those patterns returns the first greedy match. *)

exception Error of string

let err m = raise (Error m)

(* ---------- character classes in the C locale (XBD 7.3.1) ---------- *)

let is_upper c = c >= 'A' && c <= 'Z'
let is_lower c = c >= 'a' && c <= 'z'
let is_digit c = c >= '0' && c <= '9'
let is_alpha c = is_upper c || is_lower c
let is_alnum c = is_alpha c || is_digit c
let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\011' || c = '\012' || c = '\r'
let is_blank c = c = ' ' || c = '\t'
let is_print c = c >= ' ' && c < '\127'
let is_graph c = c > ' ' && c < '\127'
let is_cntrl c = c < ' ' || c = '\127'
let is_punct c = is_graph c && not (is_alnum c)
let is_xdigit c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

(* the "word" characters of the GNU extensions \w, \< and \b *)
let is_word c = is_alnum c || c = '_'

let class_pred = function
  | "upper" -> is_upper | "lower" -> is_lower | "alpha" -> is_alpha
  | "digit" -> is_digit | "alnum" -> is_alnum | "space" -> is_space
  | "blank" -> is_blank | "print" -> is_print | "graph" -> is_graph
  | "cntrl" -> is_cntrl | "punct" -> is_punct | "xdigit" -> is_xdigit
  | name -> err (Printf.sprintf "unknown character class [:%s:]" name)

let set_of_pred p = Array.init 256 (fun i -> p (Char.chr i))

(* ---------- syntax ---------- *)

type node =
  | Empty
  | Lit of char
  | Any                                 (* . *)
  | Cls of bool array                   (* a bracket expression, 9.3.5 *)
  | Bol                                 (* ^ *)
  | Eol                                 (* $ *)
  | Word_start | Word_end               (* \< \> *)
  | Word_bound | Not_word_bound         (* \b \B *)
  | Cat of node * node
  | Alt of node * node
  | Rep of node * int * int option      (* {m,n}; None = unbounded *)
  | Group of int * node                 (* \(...\) or (...), numbered from 1 *)
  | Ref of int                          (* \1 to \9 *)

type parser_state = {
  src : string;
  mutable i : int;
  ere : bool;
  icase : bool;
  mutable ngroups : int;
}

let eof p = p.i >= String.length p.src
let peek p = if eof p then None else Some p.src.[p.i]
let peek2 p = if p.i + 1 >= String.length p.src then None else Some p.src.[p.i + 1]
let bump p = p.i <- p.i + 1

(* A bracket expression (9.3.5).  ']' first is an ordinary character, as is
   '-' first or last; [:class:] names a class, and in the C locale a
   collating element [.x.] or equivalence class [=x=] is one character. *)
let bracket p =
  let set = Array.make 256 false in
  let negated = peek p = Some '^' in
  if negated then bump p;
  let first = ref true in
  let add c = set.(Char.code c) <- true in
  let rec loop () =
    if eof p then err "unmatched [";
    let c = p.src.[p.i] in
    if c = ']' && not !first then bump p
    else begin
      first := false;
      if c = '[' && (match peek2 p with Some (':' | '.' | '=') -> true | _ -> false) then begin
        let kind = p.src.[p.i + 1] in
        p.i <- p.i + 2;
        let start = p.i in
        let stop = ref None in
        while !stop = None do
          if p.i + 1 >= String.length p.src then err "unmatched [ in bracket expression";
          if p.src.[p.i] = kind && p.src.[p.i + 1] = ']' then stop := Some p.i else bump p
        done;
        let name = String.sub p.src start (Option.get !stop - start) in
        p.i <- Option.get !stop + 2;
        (match kind with
         | ':' -> let pred = class_pred name in
             for k = 0 to 255 do if pred (Char.chr k) then set.(k) <- true done
         | _ -> if String.length name <> 1 then err "multi-character collating element"
             else add name.[0])
      end else begin
        bump p;
        (* a range, unless '-' is the last character before ']' *)
        if peek p = Some '-' && peek2 p <> Some ']' && peek2 p <> None then begin
          bump p;
          let hi = p.src.[p.i] in
          bump p;
          if Char.code hi < Char.code c then err "invalid range in bracket expression";
          for k = Char.code c to Char.code hi do set.(k) <- true done
        end else add c
      end;
      loop ()
    end in
  loop ();
  (* A case-insensitive match closes the set under case *before* it is
     negated: [^abc] must then exclude 'A' as well as 'a', where closing
     the negated set would have admitted both. *)
  if p.icase then
    for k = 0 to 255 do
      if set.(k) then begin
        let c = Char.chr k in
        set.(Char.code (Char.lowercase_ascii c)) <- true;
        set.(Char.code (Char.uppercase_ascii c)) <- true
      end
    done;
  if negated then Array.map not set else set

(* an interval {m,n} (9.3.6, 9.4.6); [close] is the text that ends it *)
let interval p close =
  let num () =
    let start = p.i in
    while (match peek p with Some c when is_digit c -> true | _ -> false) do bump p done;
    if p.i = start then None else Some (int_of_string (String.sub p.src start (p.i - start))) in
  let m = match num () with Some m -> m | None -> err "missing number in {}" in
  let hi =
    if peek p = Some ',' then (bump p; num ())      (* {m,} is unbounded *)
    else Some m in
  let n = String.length close in
  if p.i + n > String.length p.src || String.sub p.src p.i n <> close then err "unmatched {";
  p.i <- p.i + n;
  (match hi with Some h when h < m -> err "invalid interval" | _ -> ());
  (m, hi)

(* Is this '$' an anchor?  In a BRE only at the end of the whole pattern or
   of a subexpression or branch (9.3.8); in an ERE always (9.4.9). *)
let dollar_anchors p =
  p.ere || eof p
  || (p.i + 1 < String.length p.src && p.src.[p.i] = '\\'
      && (p.src.[p.i + 1] = ')' || p.src.[p.i + 1] = '|'))

let rec alternation p =
  let left = branch p in
  if (p.ere && peek p = Some '|')
  || (not p.ere && peek p = Some '\\' && peek2 p = Some '|') then begin
    bump p; if not p.ere then bump p;
    Alt (left, alternation p)
  end else left

(* a branch: a sequence of repeated atoms *)
and branch p =
  (* '^' is an anchor at the head of a branch in a BRE, anywhere in an ERE *)
  let anchor = if peek p = Some '^' then (bump p; true) else false in
  let rec seq acc first =
    let stop =
      eof p
      || (p.ere && (peek p = Some '|' || peek p = Some ')'))
      || (not p.ere && peek p = Some '\\'
          && (peek2 p = Some '|' || peek2 p = Some ')')) in
    if stop then acc
    else begin
      let a = atom p ~first in
      let a = postfix p a in
      seq (if acc = Empty then a else Cat (acc, a)) false
    end in
  let body = seq Empty true in
  if anchor then (if body = Empty then Bol else Cat (Bol, body)) else body

(* the repetition operators, which may be stacked: two stars in a row
   mean the same as one *)
and postfix p node =
  match peek p with
  | Some '*' -> bump p; postfix p (Rep (node, 0, None))
  | Some '+' when p.ere -> bump p; postfix p (Rep (node, 1, None))
  | Some '?' when p.ere -> bump p; postfix p (Rep (node, 0, Some 1))
  | Some '{' when p.ere ->
      bump p; let (m, n) = interval p "}" in postfix p (Rep (node, m, n))
  | Some '\\' when not p.ere ->
      (match peek2 p with
       | Some '+' -> p.i <- p.i + 2; postfix p (Rep (node, 1, None))
       | Some '?' -> p.i <- p.i + 2; postfix p (Rep (node, 0, Some 1))
       | Some '{' -> p.i <- p.i + 2;
           let (m, n) = interval p "\\}" in postfix p (Rep (node, m, n))
       | _ -> node)
  | _ -> node

and atom p ~first =
  match peek p with
  | None -> Empty
  | Some '.' -> bump p; Any
  | Some '[' -> bump p; Cls (bracket p)
  | Some '*' when not p.ere && first ->
      (* a '*' with nothing to repeat is an ordinary character in a BRE *)
      bump p; Lit '*'
  | Some '$' when dollar_anchors { p with i = p.i + 1 } ->
      (* look past the '$' to see whether it ends the branch *)
      bump p; Eol
  | Some '^' when p.ere -> bump p; Bol
  | Some '(' when p.ere ->
      bump p;
      p.ngroups <- p.ngroups + 1;
      let n = p.ngroups in
      let body = alternation p in
      if peek p <> Some ')' then err "unmatched (";
      bump p; Group (n, body)
  | Some ')' when p.ere -> err "unmatched )"
  | Some '\\' ->
      (match peek2 p with
       | None -> err "trailing backslash"
       | Some '(' when not p.ere ->
           p.i <- p.i + 2;
           p.ngroups <- p.ngroups + 1;
           let n = p.ngroups in
           let body = alternation p in
           if not (peek p = Some '\\' && peek2 p = Some ')') then err "unmatched \\(";
           p.i <- p.i + 2; Group (n, body)
       | Some c ->
           p.i <- p.i + 2;
           (match c with
            | '<' -> Word_start
            | '>' -> Word_end
            | 'b' -> Word_bound
            | 'B' -> Not_word_bound
            | 'w' -> Cls (set_of_pred is_word)
            | 'W' -> Cls (set_of_pred (fun c -> not (is_word c)))
            | 's' -> Cls (set_of_pred is_space)
            | 'S' -> Cls (set_of_pred (fun c -> not (is_space c)))
            | 'n' -> Lit '\n'
            | 't' -> Lit '\t'
            | 'r' -> Lit '\r'
            | c when is_digit c && c <> '0' -> Ref (Char.code c - Char.code '0')
            | c -> Lit c))
  | Some c -> bump p; Lit c

(* ---------- the program ---------- *)

type inst =
  | IChar of char
  | ICls of bool array
  | IAny
  | IBol | IEol
  | IWordS | IWordE | IWordB | INotWordB
  | ISave of int                        (* record the position in slot n *)
  | IJmp of int
  | ISplit of int * int                 (* try the first, then the second *)
  | IMatch

type asm = { mutable code : inst array; mutable len : int }

let emit a i =
  if a.len = Array.length a.code then begin
    let c = Array.make (2 * a.len + 16) IMatch in
    Array.blit a.code 0 c 0 a.len; a.code <- c
  end;
  a.code.(a.len) <- i;
  a.len <- a.len + 1;
  a.len - 1

let rec gen a node =
  match node with
  | Empty -> ()
  | Lit c -> ignore (emit a (IChar c))
  | Any -> ignore (emit a IAny)
  | Cls s -> ignore (emit a (ICls s))
  | Bol -> ignore (emit a IBol)
  | Eol -> ignore (emit a IEol)
  | Word_start -> ignore (emit a IWordS)
  | Word_end -> ignore (emit a IWordE)
  | Word_bound -> ignore (emit a IWordB)
  | Not_word_bound -> ignore (emit a INotWordB)
  | Ref _ ->
      (* a back-reference is not a finite-automaton construct; a pattern
         holding one is matched by run_bt instead, so this is unreachable *)
      err "back-reference in compiled program"
  | Cat (x, y) -> gen a x; gen a y
  | Group (n, x) ->
      ignore (emit a (ISave (2 * n))); gen a x; ignore (emit a (ISave (2 * n + 1)))
  | Alt (x, y) ->
      let sp = emit a (ISplit (0, 0)) in
      gen a x;
      let jp = emit a (IJmp 0) in
      let second = a.len in
      gen a y;
      a.code.(sp) <- ISplit (sp + 1, second);
      a.code.(jp) <- IJmp a.len
  | Rep (x, 0, None) ->
      (* greedy: the split prefers the body, so longer runs are found first *)
      let sp = emit a (ISplit (0, 0)) in
      gen a x;
      ignore (emit a (IJmp sp));
      a.code.(sp) <- ISplit (sp + 1, a.len)
  | Rep (x, 1, None) ->
      let body = a.len in
      gen a x;
      let sp = emit a (ISplit (0, 0)) in
      a.code.(sp) <- ISplit (body, a.len)
  | Rep (x, 0, Some 1) ->
      let sp = emit a (ISplit (0, 0)) in
      gen a x;
      a.code.(sp) <- ISplit (sp + 1, a.len)
  | Rep (_, m, Some n) when m > n -> err "invalid interval"
  | Rep (x, m, None) ->
      for _ = 1 to m - 1 do gen a x done;
      gen a (Rep (x, 1, None))
  | Rep (x, m, Some n) ->
      for _ = 1 to m do gen a x done;
      (* the optional copies nest, so x{2,4} is x x (x (x)?)? *)
      let rec opts k = if k = 0 then Empty else Rep (Cat (x, opts (k - 1)), 0, Some 1) in
      gen a (opts (n - m))

let rec has_ref = function
  | Ref _ -> true
  | Cat (x, y) | Alt (x, y) -> has_ref x || has_ref y
  | Rep (x, _, _) | Group (_, x) -> has_ref x
  | _ -> false

(* ---------- case folding ---------- *)

(* Only the literals need it: a bracket expression was already closed
   under case while it was parsed, before any negation. *)
let rec fold_case = function
  | Lit c when is_alpha c ->
      let s = Array.make 256 false in
      s.(Char.code (Char.lowercase_ascii c)) <- true;
      s.(Char.code (Char.uppercase_ascii c)) <- true;
      Cls s
  | Cat (x, y) -> Cat (fold_case x, fold_case y)
  | Alt (x, y) -> Alt (fold_case x, fold_case y)
  | Rep (x, m, n) -> Rep (fold_case x, m, n)
  | Group (n, x) -> Group (n, fold_case x)
  | n -> n

(* ---------- the compiled expression ---------- *)

type t = {
  prog : inst array;
  nslots : int;
  ngroups : int;
  ast : node;                  (* kept for the back-reference matcher *)
  refs : bool;
  icase : bool;
}

let compile ?(ere = false) ?(icase = false) src =
  let p = { src; i = 0; ere; icase; ngroups = 0 } in
  let ast = alternation p in
  if not (eof p) then err (Printf.sprintf "unexpected %C" src.[p.i]);
  let ast = if icase then fold_case ast else ast in
  let refs = has_ref ast in
  let a = { code = Array.make 32 IMatch; len = 0 } in
  (* a pattern with a back-reference is matched from the syntax tree, so
     there is no program to build for it *)
  if not refs then begin
    ignore (emit a (ISave 0));
    gen a ast;
    ignore (emit a (ISave 1));
    ignore (emit a IMatch)
  end;
  { prog = Array.sub a.code 0 a.len; nslots = 2 * (p.ngroups + 1);
    ngroups = p.ngroups; ast; refs; icase }

let ngroups t = t.ngroups

let word_bound s i =
  let n = String.length s in
  let before = i > 0 && is_word s.[i - 1] in
  let after = i < n && is_word s.[i] in
  before <> after

(* ---------- the virtual machine ---------- *)

(* One pass over [s] from [from].  Threads are kept in priority order, so
   that of two threads reaching one program counter the greedier survives;
   every thread that matches offers an end position and the leftmost,
   longest is kept. *)
let run_vm t s from =
  let n = String.length s in
  let prog = t.prog in
  let np = Array.length prog in
  let gen_of = Array.make np (-1) in
  let step = ref 0 in
  let out = ref [] in
  let best = ref None in
  let rec add i pc slots =
    if gen_of.(pc) <> !step then begin
      gen_of.(pc) <- !step;
      match prog.(pc) with
      | IJmp t -> add i t slots
      | ISplit (x, y) -> add i x slots; add i y slots
      | ISave k ->
          let s' = Array.copy slots in
          s'.(k) <- i;
          add i (pc + 1) s'
      | IBol -> if i = 0 then add i (pc + 1) slots
      | IEol -> if i = n then add i (pc + 1) slots
      | IWordS ->
          if i < n && is_word s.[i] && (i = 0 || not (is_word s.[i - 1]))
          then add i (pc + 1) slots
      | IWordE ->
          if i > 0 && is_word s.[i - 1] && (i = n || not (is_word s.[i]))
          then add i (pc + 1) slots
      | IWordB -> if word_bound s i then add i (pc + 1) slots
      | INotWordB -> if not (word_bound s i) then add i (pc + 1) slots
      | IChar _ | ICls _ | IAny | IMatch -> out := (pc, slots) :: !out
    end in
  let record slots =
    let start = slots.(0) and stop = slots.(1) in
    match !best with
    | Some (bs, be, _) when bs < start || (bs = start && be >= stop) -> ()
    | _ -> best := Some (start, stop, slots) in
  let carried = ref [] in
  let i = ref from in
  let stop = ref false in
  while not !stop do
    (* the thread list for position i: those carried over, then a fresh
       start here if nothing has matched yet (lowest priority, so an
       earlier start always wins: "leftmost") *)
    incr step; out := [];
    List.iter (fun (pc, slots) -> add !i pc slots) !carried;
    if !best = None then add !i 0 (Array.make t.nslots (-1));
    let clist = List.rev !out in
    incr step; out := [];
    List.iter (fun (pc, slots) ->
        match prog.(pc) with
        | IMatch -> record slots
        | IChar c -> if !i < n && s.[!i] = c then add (!i + 1) (pc + 1) slots
        | ICls set -> if !i < n && set.(Char.code s.[!i]) then add (!i + 1) (pc + 1) slots
        | IAny -> if !i < n then add (!i + 1) (pc + 1) slots
        | _ -> ()) clist;
    carried := List.rev !out;
    incr i;
    if !i > n then stop := true
    else if !carried = [] && !best <> None then stop := true
  done;
  match !best with
  | None -> None
  | Some (_, _, slots) -> Some slots

(* ---------- back-references ---------- *)

(* A backtracking matcher over the syntax tree, used only when the pattern
   has a back-reference (9.3.6), which no finite automaton can express.
   It returns the first match of a greedy left-to-right search, which is
   what the other implementations of these patterns also do. *)
let run_bt t s from =
  let n = String.length s in
  let slots = Array.make t.nslots (-1) in
  let fold c = if t.icase then Char.lowercase_ascii c else c in
  let rec go node i (k : int -> bool) =
    match node with
    | Empty -> k i
    | Lit c -> i < n && fold s.[i] = fold c && k (i + 1)
    | Any -> i < n && k (i + 1)
    | Cls set -> i < n && set.(Char.code s.[i]) && k (i + 1)
    | Bol -> i = 0 && k i
    | Eol -> i = n && k i
    | Word_start ->
        i < n && is_word s.[i] && (i = 0 || not (is_word s.[i - 1])) && k i
    | Word_end ->
        i > 0 && is_word s.[i - 1] && (i = n || not (is_word s.[i])) && k i
    | Word_bound -> word_bound s i && k i
    | Not_word_bound -> (not (word_bound s i)) && k i
    | Cat (x, y) -> go x i (fun j -> go y j k)
    | Alt (x, y) -> go x i k || go y i k
    | Group (g, x) ->
        let a = slots.(2 * g) and b = slots.(2 * g + 1) in
        slots.(2 * g) <- i;
        go x i (fun j -> slots.(2 * g + 1) <- j; k j)
        || (slots.(2 * g) <- a; slots.(2 * g + 1) <- b; false)
    | Ref g ->
        let a = slots.(2 * g) and b = slots.(2 * g + 1) in
        a >= 0 && b >= a &&
        (let len = b - a in
         i + len <= n
         && (let same = ref true in
             for d = 0 to len - 1 do
               if fold s.[i + d] <> fold s.[a + d] then same := false
             done;
             !same)
         && k (i + len))
    | Rep (x, m, max) ->
        (* greedy: as many copies as possible, then fewer *)
        let rec more count i =
          let can_stop = count >= m in
          let can_go = match max with None -> true | Some mx -> count < mx in
          (* an iteration that consumed nothing may not repeat, or the
             search would not terminate; it still counts towards m *)
          (can_go && go x i (fun j -> (j > i || count < m) && more (count + 1) j))
          || (can_stop && k i) in
        more 0 i in
  let rec attempt start =
    if start > n then None
    else begin
      Array.fill slots 0 t.nslots (-1);
      slots.(0) <- start;
      if go t.ast start (fun j -> slots.(1) <- j; true) then Some (Array.copy slots)
      else attempt (start + 1)
    end in
  attempt from

(* ---------- interface ---------- *)

(* Search [s] from index [from]; the result gives the whole match in
   element 0 and each subexpression in the corresponding element, with
   (-1, -1) for one that did not take part. *)
let search t s from =
  let slots = if t.refs then run_bt t s from else run_vm t s from in
  match slots with
  | None -> None
  | Some slots ->
      Some (Array.init (t.ngroups + 1) (fun g -> (slots.(2 * g), slots.(2 * g + 1))))

let matches t s = search t s 0 <> None

(* the whole subject, as an anchored match *)
let matches_all t s =
  match search t s 0 with
  | Some g when fst g.(0) = 0 && snd g.(0) = String.length s -> true
  | _ -> false
