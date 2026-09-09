(* sed, the stream editor (IEEE Std 1003.1-2017, XCU sed).

   sed reads one line at a time into the pattern space, runs the whole
   script over it, and prints the result unless -n was given; that cycle
   is the whole of its behaviour, and the commands that are hard to place
   are the ones that interfere with it -- `d' abandons the cycle, `D'
   restarts it without reading, `n' prints and reads, `N' appends without
   printing, `q' stops.  Those five are raised as exceptions out of the
   walk over the script.

   The script is compiled to a flat array rather than a tree, because
   `b' and `t' branch to a label and a block is only an address with a
   jump over it: as an array, a branch is an index.

   Beyond the standard, this accepts what the build's scripts use of GNU
   sed: -E for extended expressions, `\u' and friends in a replacement,
   and `a text' written on one line. *)

open Util

exception Bad of string

let bad fmt = Printf.ksprintf (fun s -> raise (Bad s)) fmt

(* ---------- addresses ---------- *)

type addr =
  | Line of int
  | Last
  | Regex of Posix.Regex.t
  | Last_regex                       (* an empty // means the one before *)
  | Step of int * int                (* first~step *)

type address =
  | Always
  | At of addr
  | Range of addr * addr

(* ---------- the replacement of an s command ---------- *)

type piece =
  | Text of string
  | Group of int                     (* \1 to \9, and & as 0 *)
  | Upper_one | Lower_one            (* \u \l *)
  | Upper_all | Lower_all | End_case (* \U \L \E *)

type subst = {
  sre : Posix.Regex.t;
  repl : piece list;
  global : bool;
  nth : int;
  sprint : bool;
  swrite : string option;
}

type op =
  | S of subst
  | P                                (* p *)
  | P_first                          (* P *)
  | D_all                            (* d *)
  | D_first                          (* D *)
  | N_read                           (* n *)
  | N_append                         (* N *)
  | A_append of string               (* a *)
  | I_insert of string               (* i *)
  | C_change of string               (* c *)
  | Y of string * string             (* y *)
  | Hold_set | Hold_add | Get | Get_add | Exchange   (* h H g G x *)
  | Q_print of int | Q_silent of int (* q Q *)
  | Equals                           (* = *)
  | List_line                        (* l *)
  | Branch of int                    (* b, resolved; -1 is the end *)
  | Branch_if of int                 (* t *)
  | Read_file of string              (* r *)
  | Write_file of string             (* w *)
  | Block of int                     (* {, with the index just past its } *)
  | Block_end                        (* } *)
  | Nop

type inst = {
  address : address;
  negate : bool;
  mutable active : bool;             (* inside a range *)
  mutable ends_at : int;             (* for a range given as a line number *)
  what : op;
}

(* ---------- parsing ---------- *)

type parser_state = {
  src : string;
  mutable i : int;
  ere : bool;
  mutable last_re : Posix.Regex.t option;
}

let eof p = p.i >= String.length p.src
let peek p = if eof p then '\000' else p.src.[p.i]

let rec skip_blank p =
  if not (eof p) && (peek p = ' ' || peek p = '\t') then (p.i <- p.i + 1; skip_blank p)

(* the text up to an unescaped [delim]; a backslash before the delimiter
   makes it literal, and everything else keeps its backslash *)
let until p delim =
  let b = Buffer.create 32 in
  let n = String.length p.src in
  let closed = ref false in
  while not !closed do
    if p.i >= n then bad "unterminated address or command"
    else if p.src.[p.i] = '\\' && p.i + 1 < n then begin
      if p.src.[p.i + 1] = delim then Buffer.add_char b delim
      else if p.src.[p.i + 1] = 'n' && delim <> 'n' then Buffer.add_char b '\n'
      else (Buffer.add_char b '\\'; Buffer.add_char b p.src.[p.i + 1]);
      p.i <- p.i + 2
    end
    else if p.src.[p.i] = delim then (closed := true; p.i <- p.i + 1)
    else (Buffer.add_char b p.src.[p.i]; p.i <- p.i + 1)
  done;
  Buffer.contents b

let compile_re p text icase =
  match text with
  | "" -> (match p.last_re with Some re -> re | None -> bad "no previous regular expression")
  | _ ->
      let re = try Posix.Regex.compile ~ere:p.ere ~icase text with
        | Posix.Regex.Error msg -> bad "%s" msg in
      p.last_re <- Some re;
      re

let address_at p =
  skip_blank p;
  if eof p then None
  else if Posix.Regex.is_digit (peek p) then begin
    let start = p.i in
    while not (eof p) && Posix.Regex.is_digit (peek p) do p.i <- p.i + 1 done;
    let first = int_of_string (String.sub p.src start (p.i - start)) in
    if not (eof p) && peek p = '~' then begin
      p.i <- p.i + 1;
      let s = p.i in
      while not (eof p) && Posix.Regex.is_digit (peek p) do p.i <- p.i + 1 done;
      Some (Step (first, int_of_string (String.sub p.src s (p.i - s))))
    end else Some (Line first)
  end
  else if peek p = '$' then (p.i <- p.i + 1; Some Last)
  else if peek p = '/' || peek p = '\\' then begin
    let delim = if peek p = '\\' then (p.i <- p.i + 1; let c = peek p in p.i <- p.i + 1; c)
      else (p.i <- p.i + 1; '/') in
    let text = until p delim in
    (* GNU allows I after an address to fold case *)
    let icase = not (eof p) && peek p = 'I' in
    if icase then p.i <- p.i + 1;
    if text = "" && not icase then Some Last_regex
    else Some (Regex (compile_re p text icase))
  end
  else None

let address p =
  match address_at p with
  | None -> Always
  | Some first ->
      skip_blank p;
      if not (eof p) && peek p = ',' then begin
        p.i <- p.i + 1;
        match address_at p with
        | Some second -> Range (first, second)
        | None -> bad "expected an address after ','"
      end else At first

(* the replacement text of an s command *)
let replacement text =
  let pieces = ref [] and b = Buffer.create 32 in
  let flush () = if Buffer.length b > 0 then (pieces := Text (Buffer.contents b) :: !pieces; Buffer.clear b) in
  let add p = flush (); pieces := p :: !pieces in
  let n = String.length text in
  let i = ref 0 in
  while !i < n do
    match text.[!i] with
    | '&' -> add (Group 0); incr i
    | '\\' when !i + 1 < n ->
        (match text.[!i + 1] with
         | c when c >= '0' && c <= '9' -> add (Group (Char.code c - 48))
         | 'n' -> Buffer.add_char b '\n'
         | 't' -> Buffer.add_char b '\t'
         | 'r' -> Buffer.add_char b '\r'
         | 'u' -> add Upper_one
         | 'l' -> add Lower_one
         | 'U' -> add Upper_all
         | 'L' -> add Lower_all
         | 'E' -> add End_case
         | '&' -> Buffer.add_char b '&'
         | '\\' -> Buffer.add_char b '\\'
         | c -> Buffer.add_char b c);
        i := !i + 2
    | c -> Buffer.add_char b c; incr i
  done;
  flush ();
  List.rev !pieces

(* the text of an a, i or c command: either the rest of the line, or a
   backslash and then the following lines *)
let text_argument p =
  skip_blank p;
  if not (eof p) && peek p = '\\' then begin
    p.i <- p.i + 1;
    if not (eof p) && peek p = '\n' then p.i <- p.i + 1
  end;
  let b = Buffer.create 32 in
  let n = String.length p.src in
  let finished = ref false in
  while not !finished do
    if p.i >= n then finished := true
    else if p.src.[p.i] = '\\' && p.i + 1 < n then
      (Buffer.add_char b p.src.[p.i + 1]; p.i <- p.i + 2)
    else if p.src.[p.i] = '\n' then (p.i <- p.i + 1; finished := true)
    else (Buffer.add_char b p.src.[p.i]; p.i <- p.i + 1)
  done;
  Buffer.contents b

let label_argument p =
  skip_blank p;
  let start = p.i in
  while not (eof p) && peek p <> '\n' && peek p <> ';' && peek p <> '}' do p.i <- p.i + 1 done;
  String.trim (String.sub p.src start (p.i - start))

let file_argument p =
  skip_blank p;
  let start = p.i in
  while not (eof p) && peek p <> '\n' do p.i <- p.i + 1 done;
  String.trim (String.sub p.src start (p.i - start))

let number_argument p default =
  skip_blank p;
  let start = p.i in
  while not (eof p) && Posix.Regex.is_digit (peek p) do p.i <- p.i + 1 done;
  if p.i = start then default else int_of_string (String.sub p.src start (p.i - start))

(* One command.  The result is the operation plus, for a label
   definition, the name, which the caller records. *)
let command p =
  let address = address p in
  skip_blank p;
  let negate = if not (eof p) && peek p = '!' then (p.i <- p.i + 1; skip_blank p; true) else false in
  if eof p then bad "missing command";
  let c = peek p in
  p.i <- p.i + 1;
  let what =
    match c with
    | 's' ->
        if eof p then bad "unterminated `s' command";
        let delim = peek p in
        p.i <- p.i + 1;
        let pattern = until p delim in
        let repl = until p delim in
        let global = ref false and nth = ref 0 and sprint = ref false
        and icase = ref false and swrite = ref None in
        let reading = ref true in
        while !reading && not (eof p) do
          match peek p with
          | 'g' -> global := true; p.i <- p.i + 1
          | 'p' -> sprint := true; p.i <- p.i + 1
          | 'i' | 'I' -> icase := true; p.i <- p.i + 1
          | 'm' | 'M' -> p.i <- p.i + 1
          | c when Posix.Regex.is_digit c -> nth := number_argument p 0
          | 'w' -> p.i <- p.i + 1; swrite := Some (file_argument p); reading := false
          | _ -> reading := false
        done;
        let sre = compile_re p pattern !icase in
        S { sre; repl = replacement repl; global = !global;
            nth = (if !nth = 0 then 1 else !nth); sprint = !sprint; swrite = !swrite }
    | 'y' ->
        let delim = peek p in
        p.i <- p.i + 1;
        let from = until p delim in
        let onto = until p delim in
        if String.length from <> String.length onto then bad "strings for `y' are of different lengths";
        Y (from, onto)
    | 'p' -> P
    | 'P' -> P_first
    | 'd' -> D_all
    | 'D' -> D_first
    | 'n' -> N_read
    | 'N' -> N_append
    | 'a' -> A_append (text_argument p)
    | 'i' -> I_insert (text_argument p)
    | 'c' -> C_change (text_argument p)
    | 'h' -> Hold_set
    | 'H' -> Hold_add
    | 'g' -> Get
    | 'G' -> Get_add
    | 'x' -> Exchange
    | 'q' -> Q_print (number_argument p 0)
    | 'Q' -> Q_silent (number_argument p 0)
    | '=' -> Equals
    | 'l' -> ignore (number_argument p 0); List_line
    | 'r' -> Read_file (file_argument p)
    | 'w' -> Write_file (file_argument p)
    | 'b' -> Branch (-2)                   (* resolved once the labels are known *)
    | 't' -> Branch_if (-2)
    | ':' -> Nop
    | '{' -> Block (-1)
    | '}' -> Block_end
    | '#' -> while not (eof p) && peek p <> '\n' do p.i <- p.i + 1 done; Nop
    | c -> bad "unknown command `%c'" c in
  (* a branch or a label carries a name, read here so that the caller can
     resolve it after the whole script is known *)
  let name = match c with
    | 'b' | 't' | ':' -> Some (label_argument p)
    | _ -> None in
  (address, negate, what, name)

let compile ~ere script =
  let p = { src = script; i = 0; ere; last_re = None } in
  let out = ref [] and labels = ref [] and refs = ref [] in
  let count = ref 0 in
  let add address negate what =
    out := { address; negate; active = false; ends_at = -1; what } :: !out;
    incr count in
  let rec go () =
    (* commands are separated by newlines or semicolons *)
    while not (eof p) && (peek p = '\n' || peek p = ';' || peek p = ' ' || peek p = '\t') do
      p.i <- p.i + 1
    done;
    if not (eof p) then begin
      if peek p = '#' then begin
        while not (eof p) && peek p <> '\n' do p.i <- p.i + 1 done
      end else begin
        let (address, negate, what, name) = command p in
        (match what, name with
         | Nop, Some label when label <> "" -> labels := (label, !count) :: !labels
         | Branch _, Some label -> refs := (!count, label, false) :: !refs
         | Branch_if _, Some label -> refs := (!count, label, true) :: !refs
         | _ -> ());
        add address negate what
      end;
      go ()
    end in
  go ();
  let insts = Array.of_list (List.rev !out) in
  (* a block's address, when it does not match, skips to just past its
     matching close *)
  let stack = ref [] in
  Array.iteri (fun k i ->
      match i.what with
      | Block _ -> stack := k :: !stack
      | Block_end ->
          (match !stack with
           | open_at :: rest ->
               insts.(open_at) <- { (insts.(open_at)) with what = Block (k + 1) };
               stack := rest
           | [] -> bad "unexpected `}'")
      | _ -> ()) insts;
  if !stack <> [] then bad "unmatched `{'";
  List.iter (fun (k, label, conditional) ->
      let target = if label = "" then -1 else
          match List.assoc_opt label !labels with
          | Some t -> t
          | None -> bad "can't find label for jump to `%s'" label in
      insts.(k) <- { (insts.(k)) with
                     what = if conditional then Branch_if target else Branch target })
    !refs;
  insts

(* ---------- running ---------- *)

exception Next_cycle of bool          (* true: print the pattern space first *)
exception Restart_cycle
exception Quit of int * bool          (* status, whether to print *)

type machine = {
  insts : inst array;
  mutable pattern : string;
  mutable hold : string;
  mutable lineno : int;
  mutable last_line : bool;
  mutable replaced : bool;            (* for t *)
  mutable appends : string list;      (* queued by a and r *)
  quiet : bool;
  out : Buffer.t;
  writers : (string, out_channel) Hashtbl.t;
  next_line : unit -> (string * bool) option;   (* text, had a newline *)
  mutable had_newline : bool;
}

let apply_case pieces groups pattern =
  let b = Buffer.create 64 in
  let mode = ref ' ' and once = ref ' ' in
  let put s =
    String.iter (fun c ->
        let c =
          if !once = 'u' then (once := ' '; Char.uppercase_ascii c)
          else if !once = 'l' then (once := ' '; Char.lowercase_ascii c)
          else if !mode = 'U' then Char.uppercase_ascii c
          else if !mode = 'L' then Char.lowercase_ascii c
          else c in
        Buffer.add_char b c) s in
  List.iter (fun piece ->
      match piece with
      | Text s -> put s
      | Group k ->
          if k < Array.length groups then
            (match groups.(k) with
             | (a, z) when a >= 0 -> put (String.sub pattern a (z - a))
             | _ -> ())
      | Upper_one -> once := 'u'
      | Lower_one -> once := 'l'
      | Upper_all -> mode := 'U'
      | Lower_all -> mode := 'L'
      | End_case -> mode := ' ') pieces;
  Buffer.contents b

let substitute m s =
  let text = m.pattern in
  let b = Buffer.create (String.length text) in
  let from = ref 0 and count = ref 0 and changed = ref false in
  let finished = ref false in
  while not !finished do
    match Posix.Regex.search s.sre text !from with
    | None -> finished := true
    | Some groups ->
        let (a, z) = groups.(0) in
        incr count;
        Buffer.add_string b (String.sub text !from (a - !from));
        if !count >= s.nth && (s.global || !count = s.nth) then begin
          Buffer.add_string b (apply_case s.repl groups text);
          changed := true
        end else
          Buffer.add_string b (String.sub text a (z - a));
        if z = a then begin
          (* an empty match: keep one character and go on, or the search
             would not advance *)
          if a < String.length text then Buffer.add_char b text.[a];
          from := a + 1;
          if !from > String.length text then finished := true
        end else from := z;
        if not s.global && !count >= s.nth then finished := true
  done;
  if !from <= String.length text then
    Buffer.add_string b (String.sub text !from (String.length text - !from));
  if !changed then begin
    m.pattern <- Buffer.contents b;
    m.replaced <- true;
    if s.sprint then (Buffer.add_string m.out m.pattern; Buffer.add_char m.out '\n');
    match s.swrite with
    | None -> ()
    | Some name ->
        let oc = match Hashtbl.find_opt m.writers name with
          | Some oc -> oc
          | None -> let oc = open_out_bin name in Hashtbl.replace m.writers name oc; oc in
        output_string oc (m.pattern ^ "\n")
  end

let matches_addr m = function
  | Line k -> m.lineno = k
  | Last -> m.last_line
  | Regex re -> Posix.Regex.search re m.pattern 0 <> None
  | Last_regex -> false
  | Step (first, step) ->
      if step <= 0 then m.lineno = first
      else m.lineno >= first && (m.lineno - first) mod step = 0

(* Does this instruction apply to the current line?  A range turns on at
   its first address and off at its second, and the standard has a range
   whose second address is a line number already passed match only the
   one line. *)
let selected m (i : inst) =
  let result =
    match i.address with
    | Always -> true
    | At a -> matches_addr m a
    | Range (a, z) ->
        if i.active then begin
          let ends =
            match z with
            | Line k -> m.lineno >= k
            | Last -> m.last_line
            | Regex re -> Posix.Regex.search re m.pattern 0 <> None
            | Last_regex -> false
            | Step (_, step) -> step > 0 && m.lineno mod step = 0 in
          if ends then i.active <- false;
          true
        end else if matches_addr m a then begin
          (* a range of one line when the end is a line number not later *)
          (match z with
           | Line k when k <= m.lineno -> ()
           | _ -> i.active <- true);
          true
        end else false in
  if i.negate then not result else result

let unambiguous s =
  let b = Buffer.create (String.length s + 8) in
  String.iter (fun c ->
      match c with
      | '\\' -> Buffer.add_string b "\\\\"
      | '\007' -> Buffer.add_string b "\\a"
      | '\b' -> Buffer.add_string b "\\b"
      | '\012' -> Buffer.add_string b "\\f"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | '\011' -> Buffer.add_string b "\\v"
      | c when Char.code c < 32 || Char.code c >= 127 ->
          Buffer.add_string b (Printf.sprintf "\\%03o" (Char.code c))
      | c -> Buffer.add_char b c) s;
  Buffer.add_char b '$';
  Buffer.contents b

let flush_appends m =
  List.iter (fun text -> Buffer.add_string m.out text) (List.rev m.appends);
  m.appends <- []

let print_pattern m =
  Buffer.add_string m.out m.pattern;
  Buffer.add_char m.out '\n'

let rec cycle m =
  m.replaced <- false;
  let k = ref 0 in
  let n = Array.length m.insts in
  (try
     while !k < n do
       let i = m.insts.(!k) in
       let take = selected m i in
       (match i.what with
        | Block target -> if not take then k := target - 1
        | Block_end | Nop -> ()
        | Branch target -> if take then (if target < 0 then k := n else k := target - 1)
        | Branch_if target ->
            if take && m.replaced then begin
              m.replaced <- false;
              if target < 0 then k := n else k := target - 1
            end
        | what when not take -> ignore what
        | S s -> substitute m s
        | P -> print_pattern m
        | P_first ->
            let upto = match String.index_opt m.pattern '\n' with
              | Some j -> String.sub m.pattern 0 j
              | None -> m.pattern in
            Buffer.add_string m.out upto; Buffer.add_char m.out '\n'
        | D_all -> raise (Next_cycle false)
        | D_first ->
            (match String.index_opt m.pattern '\n' with
             | Some j ->
                 m.pattern <- String.sub m.pattern (j + 1) (String.length m.pattern - j - 1);
                 raise Restart_cycle
             | None -> raise (Next_cycle false))
        | N_read ->
            if not m.quiet then print_pattern m;
            flush_appends m;
            (match m.next_line () with
             | Some (line, nl) -> m.pattern <- line; m.lineno <- m.lineno + 1; m.had_newline <- nl
             | None -> raise (Quit (0, false)))
        | N_append ->
            flush_appends m;
            (match m.next_line () with
             | Some (line, nl) ->
                 m.pattern <- m.pattern ^ "\n" ^ line;
                 m.lineno <- m.lineno + 1;
                 m.had_newline <- nl
             | None ->
                 (* GNU prints the pattern space and stops; the standard
                    would discard it *)
                 raise (Quit (0, true)))
        | A_append text -> m.appends <- (text ^ "\n") :: m.appends
        | I_insert text -> Buffer.add_string m.out (text ^ "\n")
        | C_change text ->
            (* for a range, the text is written when the range ends *)
            let ending = match i.address with Range _ -> not i.active | _ -> true in
            if ending then Buffer.add_string m.out (text ^ "\n");
            raise (Next_cycle false)
        | Y (from, onto) ->
            m.pattern <- String.map (fun c ->
                match String.index_opt from c with
                | Some j -> onto.[j]
                | None -> c) m.pattern
        | Hold_set -> m.hold <- m.pattern
        | Hold_add -> m.hold <- m.hold ^ "\n" ^ m.pattern
        | Get -> m.pattern <- m.hold
        | Get_add -> m.pattern <- m.pattern ^ "\n" ^ m.hold
        | Exchange -> let t = m.pattern in m.pattern <- m.hold; m.hold <- t
        | Q_print status -> raise (Quit (status, true))
        | Q_silent status -> raise (Quit (status, false))
        | Equals -> Buffer.add_string m.out (string_of_int m.lineno ^ "\n")
        | List_line -> Buffer.add_string m.out (unambiguous m.pattern ^ "\n")
        | Read_file name ->
            (match In_channel.with_open_bin name In_channel.input_all with
             | text -> m.appends <- text :: m.appends
             | exception _ -> ())
        | Write_file name ->
            let oc = match Hashtbl.find_opt m.writers name with
              | Some oc -> oc
              | None -> let oc = open_out_bin name in Hashtbl.replace m.writers name oc; oc in
            output_string oc (m.pattern ^ "\n"));
       incr k
     done;
     if not m.quiet then print_pattern m;
     flush_appends m
   with
   | Next_cycle print ->
       if print && not m.quiet then print_pattern m;
       flush_appends m
   | Restart_cycle ->
       flush_appends m;
       cycle m)

(* ---------- the utility ---------- *)

let main argv opts operands =
  let ere = Posix.Getopt.has opts "E" || Posix.Getopt.has opts "r"
            || Posix.Getopt.has opts "regexp-extended" in
  let quiet = Posix.Getopt.has opts "n" || Posix.Getopt.has opts "quiet"
              || Posix.Getopt.has opts "silent" in
  let in_place = Posix.Getopt.arg opts "i" in
  (* -u writes each line as it is produced rather than in blocks, so
     that a pipeline can be watched; without it the reference sed waits
     for a block, whatever it is reading *)
  if Posix.Getopt.has opts "u" || Posix.Getopt.has opts "unbuffered" then
    streaming := true;
  let scripts =
    Posix.Getopt.all opts "e" @ Posix.Getopt.all opts "expression"
    @ List.concat_map (fun f ->
        match In_channel.with_open_bin f In_channel.input_all with
        | text -> [ text ]
        | exception e -> warn "%s" (sys_message e); raise (Fail 2))
      (Posix.Getopt.all opts "f") in
  let script, files =
    if scripts <> [] then (String.concat "\n" scripts, operands)
    else match operands with
      | s :: rest -> (s, rest)
      | [] -> die 1 "usage: sed [-En] script [file...]" in
  ignore argv;
  let insts = match compile ~ere script with
    | i -> i
    | exception Bad msg -> die 1 "-e expression #1, char %d: %s" 0 msg in
  let writers = Hashtbl.create 4 in
  let status = ref 0 in
  (* each file is edited on its own when -i is given, and otherwise they
     are one stream *)
  let groups = match in_place with
    | Some _ -> List.map (fun f -> [ f ]) (inputs files)
    | None -> [ inputs files ] in
  List.iter (fun group ->
      Array.iter (fun i -> i.active <- false) insts;
      let out = Buffer.create 65536 in
      (* the lines of every file in the group, read one at a time, with a
         one-line lookahead so that `$' knows the last line *)
      let remaining = ref group in
      let channel = ref None in
      let pending = ref None in
      let rec raw_line () =
        match !channel with
        | None ->
            (match !remaining with
             | [] -> None
             | f :: rest ->
                 remaining := rest;
                 (match open_input f with
                  | ic -> channel := Some ic; raw_line ()
                  | exception e -> warn "%s" (sys_message e); status := 2; raw_line ()))
        | Some ic ->
            (match read_line_raw ic with
             | Some raw -> Some (chop raw)
             | None -> close_input ic; channel := None; raw_line ()) in
      let next_line () =
        match !pending with
        | Some line -> pending := raw_line (); Some line
        | None -> None in
      pending := raw_line ();
      let m = { insts; pattern = ""; hold = ""; lineno = 0; last_line = false;
                replaced = false; appends = []; quiet; out; writers;
                next_line; had_newline = true } in
      (try
         let rec loop () =
           match next_line () with
           | None -> ()
           | Some (line, nl) ->
               m.pattern <- line;
               m.lineno <- m.lineno + 1;
               m.had_newline <- nl;
               m.last_line <- (!pending = None);
               cycle m;
               loop () in
         loop ()
       with Quit (code, print) ->
         if print && not m.quiet then print_pattern m;
         flush_appends m;
         if code <> 0 then status := code);
      (* a last line that had no newline keeps none *)
      let text = Buffer.contents out in
      let text =
        if not m.had_newline && String.length text > 0 && text.[String.length text - 1] = '\n'
        then String.sub text 0 (String.length text - 1) else text in
      match in_place, group with
      | Some suffix, [ file ] when file <> "-" ->
          if suffix <> "" then
            (try Sys.rename file (file ^ suffix) with _ -> ());
          let oc = open_out_bin file in
          output_string oc text;
          close_out oc
      | _ -> emit text)
    groups;
  Hashtbl.iter (fun _ oc -> close_out oc) writers;
  flush_out ();
  !status
