(* awk: pattern scanning and processing (IEEE Std 1003.1-2017, XCU awk).

   awk reads each record of its input, splits it into fields, and runs
   every rule whose pattern matches; BEGIN and END rules run before the
   first record and after the last.  That much is small.  The language in
   the actions is not: it has its own expression grammar, arrays indexed
   by strings, user-defined functions, and a type system with one rule
   that has to be got right --

   a value that came from the input and looks like a number compares as a
   number, and one that came from the program as a string compares as a
   string.  So "10" from a field is less than "9" only when neither is
   numeric; XCU calls the first kind a numeric string, and it is why
   `$1 == 10' works on input without the program having to convert.

   The pieces below are the lexer, the parser to a syntax tree, and the
   evaluator, in that order. *)

open Util

exception Bad of string
exception Next_record
exception Next_file
exception Exit_program of int
exception Break_loop
exception Continue_loop
exception Return_value of Obj.t          (* the value is a [value]; see below *)

let bad fmt = Printf.ksprintf (fun s -> raise (Bad s)) fmt

(* ---------- values ---------- *)

type value =
  | Uninit                              (* never assigned: "" and 0 at once *)
  | Num of float
  | Str of string
  | Numeric_string of string * float    (* from the input, and looks numeric *)

(* Does this text read as a number?  XCU allows leading and trailing
   blanks and the forms of a C floating constant. *)
let numeric_text s =
  let t = String.trim s in
  if t = "" then None
  else
    match float_of_string_opt t with
    | Some v -> Some v
    | None ->
        (* accept a prefix such as "12abc" nowhere, but do accept the
           forms float_of_string rejects, like "+.5" *)
        None

let of_input s =
  match numeric_text s with
  | Some v -> Numeric_string (s, v)
  | None -> Str s

(* ---------- the lexer ---------- *)

type token =
  | Tnum of float
  | Tstr of string
  | Tere of string
  | Tname of string
  | Tfunc_name of string                (* a name written immediately before ( *)
  | Tbuiltin of string
  | Tkeyword of string
  | Top of string
  | Tnewline
  | Teof

let keywords = [
  "BEGIN"; "END"; "function"; "func"; "if"; "else"; "while"; "for"; "do";
  "break"; "continue"; "next"; "nextfile"; "exit"; "return"; "delete";
  "in"; "getline"; "print"; "printf";
]

let builtins = [
  "length"; "substr"; "index"; "split"; "sub"; "gsub"; "match"; "sprintf";
  "sin"; "cos"; "atan2"; "exp"; "log"; "sqrt"; "int"; "rand"; "srand";
  "tolower"; "toupper"; "system"; "close"; "fflush";
]

(* longest first, so that ">=" is not ">" and "=" *)
let operators = [
  "**="; "..."; "<<="; ">>=";
  "+="; "-="; "*="; "/="; "%="; "^="; "=="; "!="; "<="; ">="; "&&"; "||";
  "++"; "--"; "!~"; "**"; ">>";
  "{"; "}"; "("; ")"; "["; "]"; ";"; ","; "+"; "-"; "*"; "/"; "%"; "^";
  "<"; ">"; "="; "!"; "?"; ":"; "~"; "$"; "|"; "&";
]

type lexer = {
  src : string;
  mutable pos : int;
  mutable toks : (token * int) list;    (* reversed, with line numbers *)
  mutable line : int;
  mutable value_before : bool;          (* was the last token a value? *)
}

(* A '/' starts a regular expression unless a value has just been read,
   in which case it is division: awk cannot be lexed without knowing
   that much of the grammar. *)
let scan src =
  let lx = { src; pos = 0; toks = []; line = 1; value_before = false } in
  let n = String.length src in
  let emit t value = lx.toks <- (t, lx.line) :: lx.toks; lx.value_before <- value in
  while lx.pos < n do
    let c = src.[lx.pos] in
    if c = '\\' && lx.pos + 1 < n && src.[lx.pos + 1] = '\n' then
      (lx.pos <- lx.pos + 2; lx.line <- lx.line + 1)
    else if c = ' ' || c = '\t' || c = '\r' then lx.pos <- lx.pos + 1
    else if c = '#' then
      (while lx.pos < n && src.[lx.pos] <> '\n' do lx.pos <- lx.pos + 1 done)
    else if c = '\n' then (emit Tnewline false; lx.pos <- lx.pos + 1; lx.line <- lx.line + 1)
    else if c = '"' then begin
      (* a string, with the escapes of XCU awk *)
      let b = Buffer.create 32 in
      lx.pos <- lx.pos + 1;
      let closed = ref false in
      while not !closed do
        if lx.pos >= n then bad "newline in string"
        else match src.[lx.pos] with
          | '"' -> closed := true; lx.pos <- lx.pos + 1
          | '\\' when lx.pos + 1 < n ->
              (match src.[lx.pos + 1] with
               | 'n' -> Buffer.add_char b '\n' | 't' -> Buffer.add_char b '\t'
               | 'r' -> Buffer.add_char b '\r' | '\\' -> Buffer.add_char b '\\'
               | '"' -> Buffer.add_char b '"' | '/' -> Buffer.add_char b '/'
               | 'a' -> Buffer.add_char b '\007' | 'b' -> Buffer.add_char b '\b'
               | 'f' -> Buffer.add_char b '\012' | 'v' -> Buffer.add_char b '\011'
               | c when c >= '0' && c <= '7' ->
                   let k = ref (lx.pos + 1) and v = ref 0 and digits = ref 0 in
                   while !digits < 3 && !k < n && src.[!k] >= '0' && src.[!k] <= '7' do
                     v := !v * 8 + (Char.code src.[!k] - 48); incr k; incr digits
                   done;
                   Buffer.add_char b (Char.chr (!v land 255));
                   lx.pos <- !k - 2
               | c -> Buffer.add_char b '\\'; Buffer.add_char b c);
              lx.pos <- lx.pos + 2
          | c -> Buffer.add_char b c; lx.pos <- lx.pos + 1
      done;
      emit (Tstr (Buffer.contents b)) true
    end
    else if c = '/' && not lx.value_before then begin
      (* a regular expression; a backslash quotes the delimiter *)
      let b = Buffer.create 32 in
      lx.pos <- lx.pos + 1;
      let closed = ref false in
      while not !closed do
        if lx.pos >= n then bad "newline in regular expression"
        else match src.[lx.pos] with
          | '/' -> closed := true; lx.pos <- lx.pos + 1
          | '\\' when lx.pos + 1 < n && src.[lx.pos + 1] = '/' ->
              Buffer.add_char b '/'; lx.pos <- lx.pos + 2
          | '\\' when lx.pos + 1 < n ->
              Buffer.add_char b '\\'; Buffer.add_char b src.[lx.pos + 1];
              lx.pos <- lx.pos + 2
          | c -> Buffer.add_char b c; lx.pos <- lx.pos + 1
      done;
      emit (Tere (Buffer.contents b)) true
    end
    else if Posix.Regex.is_digit c || (c = '.' && lx.pos + 1 < n && Posix.Regex.is_digit src.[lx.pos + 1]) then begin
      let start = lx.pos in
      if c = '0' && lx.pos + 1 < n && (src.[lx.pos + 1] = 'x' || src.[lx.pos + 1] = 'X') then begin
        lx.pos <- lx.pos + 2;
        while lx.pos < n && Posix.Regex.is_xdigit src.[lx.pos] do lx.pos <- lx.pos + 1 done
      end else begin
        while lx.pos < n && Posix.Regex.is_digit src.[lx.pos] do lx.pos <- lx.pos + 1 done;
        if lx.pos < n && src.[lx.pos] = '.' then begin
          lx.pos <- lx.pos + 1;
          while lx.pos < n && Posix.Regex.is_digit src.[lx.pos] do lx.pos <- lx.pos + 1 done
        end;
        if lx.pos < n && (src.[lx.pos] = 'e' || src.[lx.pos] = 'E') then begin
          let save = lx.pos in
          lx.pos <- lx.pos + 1;
          if lx.pos < n && (src.[lx.pos] = '+' || src.[lx.pos] = '-') then lx.pos <- lx.pos + 1;
          if lx.pos < n && Posix.Regex.is_digit src.[lx.pos] then
            (while lx.pos < n && Posix.Regex.is_digit src.[lx.pos] do lx.pos <- lx.pos + 1 done)
          else lx.pos <- save
        end
      end;
      let text = String.sub src start (lx.pos - start) in
      emit (Tnum (match float_of_string_opt text with
          | Some v -> v
          | None -> (match int_of_string_opt text with Some v -> float_of_int v | None -> 0.0))) true
    end
    else if Posix.Regex.is_alpha c || c = '_' then begin
      let start = lx.pos in
      while lx.pos < n && (Posix.Regex.is_alnum src.[lx.pos] || src.[lx.pos] = '_') do
        lx.pos <- lx.pos + 1
      done;
      let word = String.sub src start (lx.pos - start) in
      if List.mem word keywords then emit (Tkeyword word) (word = "getline")
      else if List.mem word builtins then emit (Tbuiltin word) false
      else if lx.pos < n && src.[lx.pos] = '(' then emit (Tfunc_name word) false
      else emit (Tname word) true
    end
    else begin
      match List.find_opt (fun o ->
          let l = String.length o in
          lx.pos + l <= n && String.sub src lx.pos l = o) operators with
      | Some op ->
          lx.pos <- lx.pos + String.length op;
          emit (Top op) (op = ")" || op = "]" || op = "++" || op = "--")
      | None -> bad "unexpected character %C" c
    end
  done;
  lx.toks <- (Teof, lx.line) :: lx.toks;
  Array.of_list (List.rev lx.toks)

(* ---------- the syntax tree ---------- *)

type lvalue =
  | Lvar of string
  | Lfield of expr
  | Lindex of string * expr list

and expr =
  | Enum of float
  | Estr of string
  | Eregex of string                    (* /re/ on its own means $0 ~ /re/ *)
  | Elval of lvalue
  | Eassign of string * lvalue * expr   (* the operator, "" for plain = *)
  | Ebinary of string * expr * expr
  | Eunary of string * expr
  | Econcat of expr * expr
  | Ecompare of string * expr * expr
  | Ematch of bool * expr * expr
  | Ein of expr list * string
  | Eand of expr * expr
  | Eor of expr * expr
  | Enot of expr
  | Econd of expr * expr * expr
  | Ecall of string * expr list
  | Ebuiltin of string * expr list
  | Eincr of bool * bool * lvalue       (* prefix?, increment?, target *)
  | Egetline of getline
  | Egroup of expr list                 (* (a, b) before `in' *)

and getline = {
  gvar : lvalue option;
  gsource : gsource;
}

and gsource = Gmain | Gfile of expr | Gcommand of expr

type redirect = Rfile of expr | Rappend of expr | Rpipe of expr

type stmt =
  | Sprint of expr list * redirect option
  | Sprintf of expr list * redirect option
  | Sexpr of expr
  | Sif of expr * stmt * stmt option
  | Swhile of expr * stmt
  | Sdo of stmt * expr
  | Sfor of stmt option * expr option * stmt option * stmt
  | Sforin of string * string * stmt
  | Sblock of stmt list
  | Snext
  | Snextfile
  | Sexit of expr option
  | Sbreak
  | Scontinue
  | Sreturn of expr option
  | Sdelete of string * expr list
  | Snop

type pattern = Pbegin | Pend | Palways | Pexpr of expr | Prange of expr * expr

type item =
  | Rule of pattern * stmt option * bool ref   (* the flag holds a range's state *)
  | Function of string * string list * stmt

(* ---------- the parser ---------- *)

type parser_state = { toks : (token * int) array; mutable k : int }

let tok p = fst p.toks.(p.k)
let tline p = snd p.toks.(p.k)
let next p = if p.k < Array.length p.toks - 1 then p.k <- p.k + 1
let at_op p o = tok p = Top o
let at_kw p w = tok p = Tkeyword w

let expect_op p o =
  if at_op p o then next p else bad "line %d: expected `%s'" (tline p) o

let rec skip_newlines p =
  if tok p = Tnewline || at_op p ";" then (next p; skip_newlines p)

let rec skip_optional_newlines p =
  if tok p = Tnewline then (next p; skip_optional_newlines p)

(* A name used as an array must be a plain name (XCU awk); the grammar
   allows only that. *)
let array_name p =
  match tok p with
  | Tname v -> next p; v
  | _ -> bad "line %d: expected an array name" (tline p)

let rec expression ?(no_in = false) ?(no_gt = false) p = ternary ~no_in ~no_gt p

and ternary ~no_in ~no_gt p =
  let c = logical_or ~no_in ~no_gt p in
  if at_op p "?" then begin
    next p; skip_optional_newlines p;
    let a = ternary ~no_in ~no_gt p in
    skip_optional_newlines p;
    expect_op p ":";
    skip_optional_newlines p;
    let b = ternary ~no_in ~no_gt p in
    Econd (c, a, b)
  end else assign_tail ~no_in ~no_gt p c

(* Assignment is right-associative and its left side must be an lvalue;
   it is recognised after the fact, which keeps one expression parser. *)
and assign_tail ~no_in ~no_gt p left =
  let op = match tok p with
    | Top ("=" | "+=" | "-=" | "*=" | "/=" | "%=" | "^=" as o) -> Some o
    | Top "**=" -> Some "^="
    | _ -> None in
  match op, left with
  | Some o, Elval lv ->
      next p; skip_optional_newlines p;
      let right = ternary ~no_in ~no_gt p in
      Eassign ((if o = "=" then "" else String.sub o 0 1), lv, right)
  | _ -> left

and logical_or ~no_in ~no_gt p =
  let left = ref (logical_and ~no_in ~no_gt p) in
  while at_op p "||" do
    next p; skip_optional_newlines p;
    left := Eor (!left, logical_and ~no_in ~no_gt p)
  done;
  !left

and logical_and ~no_in ~no_gt p =
  let left = ref (in_expr ~no_in ~no_gt p) in
  while at_op p "&&" do
    next p; skip_optional_newlines p;
    left := Eand (!left, in_expr ~no_in ~no_gt p)
  done;
  !left

and in_expr ~no_in ~no_gt p =
  let left = ref (match_expr ~no_in ~no_gt p) in
  while (not no_in) && at_kw p "in" do
    next p;
    let name = array_name p in
    left := Ein ((match !left with Egroup l -> l | e -> [ e ]), name)
  done;
  !left

and match_expr ~no_in ~no_gt p =
  let left = ref (comparison ~no_in ~no_gt p) in
  let rec go () =
    if at_op p "~" then begin
      next p;
      left := Ematch (true, !left, comparison ~no_in ~no_gt p);
      go ()
    end else if at_op p "!~" then begin
      next p;
      left := Ematch (false, !left, comparison ~no_in ~no_gt p);
      go ()
    end in
  go ();
  !left

(* The comparisons do not associate, and inside a print statement a '>'
   is a redirection, not a comparison, which is what [no_gt] carries. *)
and comparison ~no_in ~no_gt p =
  let left = concatenation ~no_in ~no_gt p in
  match tok p with
  | Top ("<" | "<=" | "==" | "!=" | ">=" as o) ->
      next p; Ecompare (o, left, concatenation ~no_in ~no_gt p)
  | Top ">" when not no_gt ->
      next p; Ecompare (">", left, concatenation ~no_in ~no_gt p)
  | _ -> left

(* Concatenation has no operator: two values side by side are joined. *)
and concatenation ~no_in ~no_gt p =
  let left = ref (additive ~no_in ~no_gt p) in
  let starts_value () =
    match tok p with
    | Tnum _ | Tstr _ | Tere _ | Tname _ | Tfunc_name _ | Tbuiltin _ -> true
    | Top ("$" | "(" | "!" | "++" | "--") -> true
    | Top "-" | Top "+" -> false        (* those belong to the additive level *)
    | Tkeyword _ -> false
    | _ -> false in
  while starts_value () do
    left := Econcat (!left, additive ~no_in ~no_gt p)
  done;
  !left

and additive ~no_in ~no_gt p =
  let left = ref (multiplicative ~no_in ~no_gt p) in
  let rec go () =
    if at_op p "+" then (next p; left := Ebinary ("+", !left, multiplicative ~no_in ~no_gt p); go ())
    else if at_op p "-" then (next p; left := Ebinary ("-", !left, multiplicative ~no_in ~no_gt p); go ()) in
  go ();
  !left

and multiplicative ~no_in ~no_gt p =
  let left = ref (unary ~no_in ~no_gt p) in
  let rec go () =
    match tok p with
    | Top "*" -> next p; left := Ebinary ("*", !left, unary ~no_in ~no_gt p); go ()
    | Top "/" -> next p; left := Ebinary ("/", !left, unary ~no_in ~no_gt p); go ()
    | Top "%" -> next p; left := Ebinary ("%", !left, unary ~no_in ~no_gt p); go ()
    | _ -> () in
  go ();
  !left

and unary ~no_in ~no_gt p =
  match tok p with
  | Top "!" -> next p; Enot (unary ~no_in ~no_gt p)
  | Top "-" -> next p; Eunary ("-", unary ~no_in ~no_gt p)
  | Top "+" -> next p; Eunary ("+", unary ~no_in ~no_gt p)
  | _ -> power ~no_in ~no_gt p

(* '^' is right-associative and binds tighter than unary minus on its
   right, so -2^2 is -4 and 2^-1 is a half *)
and power ~no_in ~no_gt p =
  let base = postfix ~no_in ~no_gt p in
  if at_op p "^" || at_op p "**" then begin
    next p;
    Ebinary ("^", base, unary ~no_in ~no_gt p)
  end else base

and postfix ~no_in ~no_gt p =
  let e = primary ~no_in ~no_gt p in
  match tok p, e with
  | Top "++", Elval lv -> next p; Eincr (false, true, lv)
  | Top "--", Elval lv -> next p; Eincr (false, false, lv)
  | _ -> e

and primary ~no_in ~no_gt p =
  match tok p with
  | Tnum v -> next p; Enum v
  | Tstr s -> next p; Estr s
  | Tere s -> next p; Eregex s
  | Top "++" -> next p; (match primary ~no_in ~no_gt p with
      | Elval lv -> Eincr (true, true, lv)
      | e -> Ebinary ("+", Enum 0.0, e))
  | Top "--" -> next p; (match primary ~no_in ~no_gt p with
      | Elval lv -> Eincr (true, false, lv)
      | e -> Ebinary ("-", Enum 0.0, e))
  | Top "$" -> next p; Elval (Lfield (primary ~no_in ~no_gt p))
  | Top "(" ->
      next p;
      let first = expression p in
      if at_op p "," then begin
        (* a parenthesised list, which only `in' and print accept *)
        let items = ref [ first ] in
        while at_op p "," do next p; skip_optional_newlines p; items := expression p :: !items done;
        expect_op p ")";
        Egroup (List.rev !items)
      end else (expect_op p ")"; first)
  | Tkeyword "getline" ->
      next p;
      let gvar = match tok p with
        | Tname _ | Top "$" ->
            (match primary ~no_in ~no_gt:true p with
             | Elval lv -> Some lv
             | _ -> None)
        | _ -> None in
      if at_op p "<" then begin
        next p;
        let file = concatenation ~no_in ~no_gt:true p in
        Egetline { gvar; gsource = Gfile file }
      end else Egetline { gvar; gsource = Gmain }
  | Tfunc_name name ->
      next p;
      expect_op p "(";
      let args = arguments p in
      Ecall (name, args)
  | Tbuiltin name ->
      next p;
      if at_op p "(" then (next p; Ebuiltin (name, arguments p))
      else if name = "length" then Ebuiltin ("length", [])
      else bad "line %d: %s needs arguments" (tline p) name
  | Tname name ->
      next p;
      if at_op p "[" then begin
        next p;
        let subs = ref [ expression p ] in
        while at_op p "," do next p; subs := expression p :: !subs done;
        expect_op p "]";
        Elval (Lindex (name, List.rev !subs))
      end else Elval (Lvar name)
  | t -> ignore t; bad "line %d: unexpected token" (tline p)

and arguments p =
  skip_optional_newlines p;
  if at_op p ")" then (next p; [])
  else begin
    let args = ref [ expression p ] in
    while at_op p "," do next p; skip_optional_newlines p; args := expression p :: !args done;
    expect_op p ")";
    List.rev !args
  end

(* A pipeline into getline: `cmd | getline' is written with the command
   on the left, so it is picked up after a complete expression. *)
and pipe_getline p left =
  if at_op p "|" && (match fst p.toks.(p.k + 1) with Tkeyword "getline" -> true | _ -> false)
  then begin
    next p; next p;
    let gvar = match tok p with
      | Tname _ | Top "$" ->
          (match primary ~no_in:false ~no_gt:true p with
           | Elval lv -> Some lv
           | _ -> None)
      | _ -> None in
    pipe_getline p (Egetline { gvar; gsource = Gcommand left })
  end else left

and full_expression ?(no_gt = false) p =
  let e = expression ~no_gt p in
  pipe_getline p e

(* ---------- statements ---------- *)

and simple_statement p =
  match tok p with
  | Tkeyword "print" ->
      next p;
      let args, redirect = print_arguments p in
      Sprint (args, redirect)
  | Tkeyword "printf" ->
      next p;
      let args, redirect = print_arguments p in
      Sprintf (args, redirect)
  | Tkeyword "delete" ->
      next p;
      let name = array_name p in
      if at_op p "[" then begin
        next p;
        let subs = ref [ expression p ] in
        while at_op p "," do next p; subs := expression p :: !subs done;
        expect_op p "]";
        Sdelete (name, List.rev !subs)
      end else Sdelete (name, [])
  | Tkeyword "next" -> next p; Snext
  | Tkeyword "nextfile" -> next p; Snextfile
  | Tkeyword "break" -> next p; Sbreak
  | Tkeyword "continue" -> next p; Scontinue
  | Tkeyword "exit" ->
      next p;
      if ends_statement p then Sexit None else Sexit (Some (full_expression p))
  | Tkeyword "return" ->
      next p;
      if ends_statement p then Sreturn None else Sreturn (Some (full_expression p))
  | _ -> Sexpr (full_expression p)

and ends_statement p =
  match tok p with
  | Tnewline | Teof -> true
  | Top (";" | "}") -> true
  | Tkeyword "else" -> true
  | _ -> false

(* print and printf take an expression list, and inside it a '>' is a
   redirection; a parenthesised list is also allowed. *)
and print_arguments p =
  let args =
    if ends_statement p || at_op p ">" || at_op p ">>" || at_op p "|" then []
    else begin
      let first = expression ~no_gt:true p in
      match first with
      | Egroup items when not (at_op p ",") -> items
      | _ ->
          let items = ref [ first ] in
          while at_op p "," do
            next p; skip_optional_newlines p;
            items := expression ~no_gt:true p :: !items
          done;
          List.rev !items
    end in
  let redirect =
    if at_op p ">" then (next p; Some (Rfile (concatenation ~no_in:false ~no_gt:true p)))
    else if at_op p ">>" then (next p; Some (Rappend (concatenation ~no_in:false ~no_gt:true p)))
    else if at_op p "|" then (next p; Some (Rpipe (concatenation ~no_in:false ~no_gt:true p)))
    else None in
  (args, redirect)

and statement p =
  skip_optional_newlines p;
  match tok p with
  | Top "{" ->
      next p;
      let body = statement_list p in
      expect_op p "}";
      Sblock body
  | Top ";" -> next p; Snop
  | Tkeyword "if" ->
      next p;
      expect_op p "(";
      let c = full_expression p in
      expect_op p ")";
      let then_ = statement p in
      let saved = p.k in
      skip_newlines p;
      if at_kw p "else" then begin
        next p;
        Sif (c, then_, Some (statement p))
      end else (p.k <- saved; Sif (c, then_, None))
  | Tkeyword "while" ->
      next p;
      expect_op p "(";
      let c = full_expression p in
      expect_op p ")";
      if ends_statement p && tok p <> Top "{" then (terminator p; Swhile (c, Snop))
      else Swhile (c, statement p)
  | Tkeyword "do" ->
      next p;
      let body = statement p in
      skip_newlines p;
      if not (at_kw p "while") then bad "line %d: expected `while'" (tline p);
      next p;
      expect_op p "(";
      let c = full_expression p in
      expect_op p ")";
      Sdo (body, c)
  | Tkeyword "for" ->
      next p;
      expect_op p "(";
      (* `for (x in a)' or the three-part form *)
      let is_for_in =
        match tok p, fst p.toks.(p.k + 1), fst p.toks.(p.k + 2) with
        | Tname _, Tkeyword "in", Tname _ -> true
        | _ -> false in
      if is_for_in then begin
        let v = match tok p with Tname v -> next p; v | _ -> bad "for" in
        next p;
        let a = array_name p in
        expect_op p ")";
        Sforin (v, a, statement p)
      end else begin
        let init = if at_op p ";" then None else Some (simple_statement p) in
        expect_op p ";";
        skip_optional_newlines p;
        let cond = if at_op p ";" then None else Some (full_expression p) in
        expect_op p ";";
        skip_optional_newlines p;
        let step = if at_op p ")" then None else Some (simple_statement p) in
        expect_op p ")";
        Sfor (init, cond, step, statement p)
      end
  | _ ->
      let s = simple_statement p in
      terminator p;
      s

and terminator p =
  match tok p with
  | Top ";" -> next p; skip_optional_newlines p
  | Tnewline -> next p
  | _ -> ()

and statement_list p =
  let out = ref [] in
  let rec go () =
    skip_newlines p;
    match tok p with
    | Top "}" | Teof -> ()
    | _ -> out := statement p :: !out; go () in
  go ();
  List.rev !out

(* ---------- the program ---------- *)

let program text =
  let toks = scan text in
  let p = { toks; k = 0 } in
  let items = ref [] in
  let rec go () =
    skip_newlines p;
    match tok p with
    | Teof -> ()
    | Tkeyword ("function" | "func") ->
        next p;
        let name = match tok p with
          | Tname v | Tfunc_name v -> next p; v
          | _ -> bad "line %d: expected a function name" (tline p) in
        expect_op p "(";
        let params = ref [] in
        skip_optional_newlines p;
        if not (at_op p ")") then begin
          let rec more () =
            (match tok p with
             | Tname v -> next p; params := v :: !params
             | _ -> bad "line %d: expected a parameter name" (tline p));
            if at_op p "," then (next p; skip_optional_newlines p; more ()) in
          more ()
        end;
        expect_op p ")";
        skip_optional_newlines p;
        let body = statement p in
        items := Function (name, List.rev !params, body) :: !items;
        go ()
    | Tkeyword "BEGIN" ->
        next p; skip_optional_newlines p;
        items := Rule (Pbegin, Some (statement p), ref false) :: !items;
        go ()
    | Tkeyword "END" ->
        next p; skip_optional_newlines p;
        items := Rule (Pend, Some (statement p), ref false) :: !items;
        go ()
    | Top "{" ->
        items := Rule (Palways, Some (statement p), ref false) :: !items;
        go ()
    | _ ->
        let first = full_expression p in
        let pattern =
          if at_op p "," then begin
            next p; skip_optional_newlines p;
            Prange (first, full_expression p)
          end else Pexpr first in
        skip_optional_newlines p;
        let action = if at_op p "{" then Some (statement p) else None in
        terminator p;
        items := Rule (pattern, action, ref false) :: !items;
        go () in
  go ();
  List.rev !items

(* ---------- the evaluator ---------- *)

type cell =
  | Scalar of value
  | Table of (string, value) Hashtbl.t

type stream =
  | Out of out_channel
  | Pipe_out of out_channel
  | In of in_channel
  | Pipe_in of in_channel

type machine = {
  globals : (string, cell ref) Hashtbl.t;
  mutable locals : (string * cell ref) list;
  funcs : (string, string list * stmt) Hashtbl.t;
  mutable fields : string array;         (* $1 onwards *)
  mutable record : string;               (* $0 *)
  mutable split_done : bool;
  streams : (string, stream) Hashtbl.t;
  mutable files : string list;           (* the input still to read *)
  mutable current : in_channel option;
  mutable exit_status : int;
  out : Buffer.t;
  regex_cache : (string * bool, Posix.Regex.t) Hashtbl.t;
}

let to_number = function
  | Uninit -> 0.0
  | Num v -> v
  | Numeric_string (_, v) -> v
  | Str s ->
      (* a string used as a number takes its longest numeric prefix *)
      let t = String.trim s in
      let n = String.length t in
      let k = ref 0 in
      if !k < n && (t.[!k] = '-' || t.[!k] = '+') then incr k;
      let digits = ref 0 in
      while !k < n && Posix.Regex.is_digit t.[!k] do incr k; incr digits done;
      if !k < n && t.[!k] = '.' then begin
        incr k;
        while !k < n && Posix.Regex.is_digit t.[!k] do incr k; incr digits done
      end;
      if !digits > 0 && !k < n && (t.[!k] = 'e' || t.[!k] = 'E') then begin
        let save = !k in
        incr k;
        if !k < n && (t.[!k] = '-' || t.[!k] = '+') then incr k;
        if !k < n && Posix.Regex.is_digit t.[!k] then
          (while !k < n && Posix.Regex.is_digit t.[!k] do incr k done)
        else k := save
      end;
      if !digits = 0 then 0.0
      else (match float_of_string_opt (String.sub t 0 !k) with Some v -> v | None -> 0.0)

let number_to_string ~fmt v =
  if Float.is_integer v && Float.abs v < 1e16 then Printf.sprintf "%.0f" v
  else
    let (text, errors) = Posix.Fmt.printf fmt [ string_of_float v ] in
    if errors = [] then text else Printf.sprintf "%.6g" v

let is_numeric = function
  | Num _ | Numeric_string _ | Uninit -> true
  | Str _ -> false

(* ---------- the machine ---------- *)

let find_cell m name =
  match List.assoc_opt name m.locals with
  | Some c -> c
  | None ->
      match Hashtbl.find_opt m.globals name with
      | Some c -> c
      | None ->
          let c = ref (Scalar Uninit) in
          Hashtbl.replace m.globals name c;
          c

let table_of m name =
  let c = find_cell m name in
  match !c with
  | Table t -> t
  | Scalar Uninit -> let t = Hashtbl.create 16 in c := Table t; t
  | Scalar _ -> bad "%s: cannot be used as an array" name

let get_var m name =
  match !(find_cell m name) with
  | Scalar v -> v
  | Table _ -> bad "%s: is an array" name

let convfmt m = match get_var m "CONVFMT" with Uninit -> "%.6g" | v ->
  (match v with Str s | Numeric_string (s, _) -> s | Num f -> string_of_float f | Uninit -> "%.6g")

let to_text m v =
  match v with
  | Uninit -> ""
  | Str s -> s
  | Numeric_string (s, _) -> s
  | Num f -> number_to_string ~fmt:(convfmt m) f

let set_var m name v = (find_cell m name) := Scalar v

(* ---------- fields ---------- *)

let regex m text =
  let key = (text, false) in
  match Hashtbl.find_opt m.regex_cache key with
  | Some re -> re
  | None ->
      let re = try Posix.Regex.compile ~ere:true text with
        | Posix.Regex.Error msg -> bad "%s: %s" text msg in
      Hashtbl.replace m.regex_cache key re;
      re

let split_record m =
  let fs = to_text m (get_var m "FS") in
  let line = m.record in
  let parts =
    if fs = " " then
      (* the default: runs of blanks, with none at the ends *)
      List.filter (fun s -> s <> "")
        (String.split_on_char ' '
           (String.map (fun c -> if c = '\t' || c = '\n' then ' ' else c) line))
    else if String.length fs = 1 && fs <> "\\" then
      (if line = "" then [] else String.split_on_char fs.[0] line)
    else if line = "" then []
    else begin
      (* a regular expression separates the fields *)
      let re = regex m fs in
      let out = ref [] and from = ref 0 in
      let finished = ref false in
      while not !finished do
        match Posix.Regex.search re line !from with
        | Some groups when snd groups.(0) > fst groups.(0) ->
            let (a, z) = groups.(0) in
            out := String.sub line !from (a - !from) :: !out;
            from := z
        | _ -> finished := true
      done;
      out := String.sub line !from (String.length line - !from) :: !out;
      List.rev !out
    end in
  m.fields <- Array.of_list parts;
  m.split_done <- true;
  set_var m "NF" (Num (float_of_int (Array.length m.fields)))

let ensure_split m = if not m.split_done then split_record m

let rebuild_record m =
  let ofs = to_text m (get_var m "OFS") in
  let ofs = if ofs = "" && get_var m "OFS" = Uninit then " " else ofs in
  m.record <- String.concat ofs (Array.to_list m.fields)

let get_field m k =
  if k = 0 then of_input m.record
  else begin
    ensure_split m;
    if k <= Array.length m.fields then of_input m.fields.(k - 1) else Uninit
  end

let set_field m k text =
  if k = 0 then begin
    m.record <- text;
    m.split_done <- false;
    ensure_split m
  end else begin
    ensure_split m;
    if k > Array.length m.fields then begin
      let bigger = Array.make k "" in
      Array.blit m.fields 0 bigger 0 (Array.length m.fields);
      m.fields <- bigger;
      set_var m "NF" (Num (float_of_int k))
    end;
    m.fields.(k - 1) <- text;
    rebuild_record m
  end

let set_nf m n =
  ensure_split m;
  let n = max 0 n in
  let bigger = Array.make n "" in
  Array.blit m.fields 0 bigger 0 (min n (Array.length m.fields));
  m.fields <- bigger;
  set_var m "NF" (Num (float_of_int n));
  rebuild_record m

(* ---------- output ---------- *)

let output m redirect text =
  match redirect with
  | None -> Buffer.add_string m.out text
  | Some (name, kind) ->
      let key = (match kind with `File -> ">" | `Append -> ">>" | `Pipe -> "|") ^ name in
      let stream =
        match Hashtbl.find_opt m.streams key with
        | Some s -> s
        | None ->
            let s = match kind with
              | `File -> Out (if name = "/dev/stderr" then stderr
                              else if name = "/dev/stdout" then stdout
                              else open_out_bin name)
              | `Append -> Out (open_out_gen [ Open_wronly; Open_creat; Open_append; Open_binary ] 0o666 name)
              | `Pipe ->
                  Buffer.add_string m.out "";
                  print_string (Buffer.contents m.out);
                  Buffer.clear m.out;
                  flush stdout;
                  Pipe_out (Unix.open_process_out name) in
            Hashtbl.replace m.streams key s;
            s in
      (match stream with
       | Out oc | Pipe_out oc -> output_string oc text
       | _ -> ())

(* ---------- evaluating ---------- *)

let subscript m subs values =
  ignore m;
  ignore subs;
  String.concat "\028" values          (* SUBSEP's default *)

let rec eval m e : value =
  match e with
  | Enum v -> Num v
  | Estr s -> Str s
  | Eregex text ->
      Num (if Posix.Regex.search (regex m text) m.record 0 <> None then 1.0 else 0.0)
  | Elval lv -> read m lv
  | Eassign (op, lv, rhs) ->
      let v = eval m rhs in
      let v =
        if op = "" then v
        else begin
          let old = to_number (read m lv) in
          let x = to_number v in
          Num (match op with
              | "+" -> old +. x
              | "-" -> old -. x
              | "*" -> old *. x
              | "/" -> if x = 0.0 then bad "division by zero" else old /. x
              | "%" -> if x = 0.0 then bad "division by zero in %%" else Float.rem old x
              | "^" -> Float.pow old x
              | _ -> x)
        end in
      write m lv v;
      v
  | Ebinary (op, a, b) ->
      let x = to_number (eval m a) and y = to_number (eval m b) in
      Num (match op with
          | "+" -> x +. y
          | "-" -> x -. y
          | "*" -> x *. y
          | "/" -> if y = 0.0 then bad "division by zero" else x /. y
          | "%" -> if y = 0.0 then bad "division by zero in %%" else Float.rem x y
          | "^" -> Float.pow x y
          | _ -> bad "unknown operator %s" op)
  | Eunary ("-", a) -> Num (-. (to_number (eval m a)))
  | Eunary (_, a) -> Num (to_number (eval m a))
  | Econcat (a, b) ->
      let x = to_text m (eval m a) in
      let y = to_text m (eval m b) in
      Str (x ^ y)
  | Ecompare (op, a, b) ->
      let x = eval m a and y = eval m b in
      let c =
        if is_numeric x && is_numeric y then compare (to_number x) (to_number y)
        else compare (to_text m x) (to_text m y) in
      Num (if (match op with
          | "<" -> c < 0 | "<=" -> c <= 0 | ">" -> c > 0 | ">=" -> c >= 0
          | "==" -> c = 0 | _ -> c <> 0) then 1.0 else 0.0)
  | Ematch (positive, a, b) ->
      let subject = to_text m (eval m a) in
      let pattern = match b with Eregex t -> t | e -> to_text m (eval m e) in
      let hit = Posix.Regex.search (regex m pattern) subject 0 <> None in
      Num (if hit = positive then 1.0 else 0.0)
  | Ein (subs, name) ->
      let key = subscript m subs (List.map (fun s -> to_text m (eval m s)) subs) in
      Num (if Hashtbl.mem (table_of m name) key then 1.0 else 0.0)
  | Eand (a, b) ->
      Num (if truth m (eval m a) && truth m (eval m b) then 1.0 else 0.0)
  | Eor (a, b) ->
      Num (if truth m (eval m a) || truth m (eval m b) then 1.0 else 0.0)
  | Enot a -> Num (if truth m (eval m a) then 0.0 else 1.0)
  | Econd (c, a, b) -> if truth m (eval m c) then eval m a else eval m b
  | Eincr (prefix, up, lv) ->
      let old = to_number (read m lv) in
      let now = if up then old +. 1.0 else old -. 1.0 in
      write m lv (Num now);
      Num (if prefix then now else old)
  | Egroup [ e ] -> eval m e
  | Egroup l -> Str (String.concat (to_text m (get_var m "SUBSEP")) (List.map (fun e -> to_text m (eval m e)) l))
  | Ecall (name, args) -> call m name args
  | Ebuiltin (name, args) -> builtin m name args
  | Egetline g -> getline m g

and truth m v =
  match v with
  | Uninit -> false
  | Num f -> f <> 0.0
  | Numeric_string (_, f) -> f <> 0.0
  | Str s -> ignore m; s <> ""

and read m lv =
  match lv with
  | Lvar "NF" -> ensure_split m; get_var m "NF"
  | Lvar name -> get_var m name
  | Lfield e -> get_field m (int_of_float (to_number (eval m e)))
  | Lindex (name, subs) ->
      let t = table_of m name in
      let key = subscript m subs (List.map (fun s -> to_text m (eval m s)) subs) in
      (match Hashtbl.find_opt t key with
       | Some v -> v
       | None -> Hashtbl.replace t key Uninit; Uninit)

and write m lv v =
  match lv with
  | Lvar "NF" -> set_nf m (int_of_float (to_number v))
  | Lvar name ->
      set_var m name v;
      if name = "FS" then () else if name = "RS" then ()
  | Lfield e -> set_field m (int_of_float (to_number (eval m e))) (to_text m v)
  | Lindex (name, subs) ->
      let t = table_of m name in
      let key = subscript m subs (List.map (fun s -> to_text m (eval m s)) subs) in
      Hashtbl.replace t key v

(* ---------- built-in functions ---------- *)

and builtin m name args =
  let text k = to_text m (eval m (List.nth args k)) in
  let number k = to_number (eval m (List.nth args k)) in
  let count = List.length args in
  match name with
  | "length" ->
      if count = 0 then Num (float_of_int (String.length m.record))
      else
        (match List.hd args with
         | Elval (Lvar v) when (match !(find_cell m v) with Table _ -> true | _ -> false) ->
             Num (float_of_int (Hashtbl.length (table_of m v)))
         | _ -> Num (float_of_int (String.length (text 0))))
  | "substr" ->
      let s = text 0 in
      let n = String.length s in
      (* Positions count from one.  A start before the string is taken as
         one without shortening the result, and a length that runs past
         the end is clipped; both follow the awk the build is tested
         with rather than the strict reading of the standard, which
         would shorten the result instead. *)
      let start = max 1 (int_of_float (Float.trunc (number 1))) in
      let last =
        if count >= 3 then
          let len = int_of_float (Float.trunc (number 2)) in
          min (n + 1) (start + max 0 len)
        else n + 1 in
      if last <= start || start > n then Str ""
      else Str (String.sub s (start - 1) (last - start))
  | "index" ->
      let s = text 0 and t = text 1 in
      let n = String.length s and k = String.length t in
      let rec go i = if i + k > n then 0 else if String.sub s i k = t then i + 1 else go (i + 1) in
      Num (float_of_int (go 0))
  | "split" ->
      let s = text 0 in
      let name = match List.nth args 1 with
        | Elval (Lvar v) -> v
        | _ -> bad "split: the second argument must be an array" in
      let t = table_of m name in
      Hashtbl.reset t;
      let saved_fs = get_var m "FS" in
      let fs = if count >= 3 then
          (match List.nth args 2 with
           | Eregex text -> Str text
           | e -> eval m e)
        else saved_fs in
      set_var m "FS" fs;
      let saved_record = m.record and saved_fields = m.fields and saved_split = m.split_done in
      m.record <- s;
      m.split_done <- false;
      split_record m;
      let parts = m.fields in
      m.record <- saved_record; m.fields <- saved_fields; m.split_done <- saved_split;
      set_var m "FS" saved_fs;
      Array.iteri (fun i part -> Hashtbl.replace t (string_of_int (i + 1)) (of_input part)) parts;
      Num (float_of_int (Array.length parts))
  | "sub" | "gsub" ->
      let pattern = match List.hd args with
        | Eregex t -> t
        | e -> to_text m (eval m e) in
      let replacement = text 1 in
      let target = if count >= 3 then
          (match List.nth args 2 with
           | Elval lv -> lv
           | _ -> bad "%s: the third argument must be assignable" name)
        else Lfield (Enum 0.0) in
      let subject = to_text m (read m target) in
      let re = regex m pattern in
      let global = name = "gsub" in
      let b = Buffer.create (String.length subject) in
      let from = ref 0 and count_done = ref 0 in
      let finished = ref false in
      while not !finished do
        match Posix.Regex.search re subject !from with
        | None -> finished := true
        | Some groups ->
            let (a, z) = groups.(0) in
            Buffer.add_string b (String.sub subject !from (a - !from));
            (* in the replacement, & is the text matched and \& a literal
               ampersand *)
            let k = ref 0 in
            let r = replacement in
            while !k < String.length r do
              if r.[!k] = '\\' && !k + 1 < String.length r && r.[!k + 1] = '&'
              then (Buffer.add_char b '&'; k := !k + 2)
              else if r.[!k] = '\\' && !k + 1 < String.length r && r.[!k + 1] = '\\'
              then (Buffer.add_char b '\\'; k := !k + 2)
              else if r.[!k] = '&' then (Buffer.add_string b (String.sub subject a (z - a)); incr k)
              else (Buffer.add_char b r.[!k]; incr k)
            done;
            incr count_done;
            if z = a then begin
              if a < String.length subject then Buffer.add_char b subject.[a];
              from := a + 1;
              if !from > String.length subject then finished := true
            end else from := z;
            if not global then finished := true
      done;
      if !from <= String.length subject then
        Buffer.add_string b (String.sub subject !from (String.length subject - !from));
      if !count_done > 0 then write m target (Str (Buffer.contents b));
      Num (float_of_int !count_done)
  | "match" ->
      let subject = text 0 in
      let pattern = match List.nth args 1 with Eregex t -> t | e -> to_text m (eval m e) in
      (match Posix.Regex.search (regex m pattern) subject 0 with
       | Some groups ->
           let (a, z) = groups.(0) in
           set_var m "RSTART" (Num (float_of_int (a + 1)));
           set_var m "RLENGTH" (Num (float_of_int (z - a)));
           Num (float_of_int (a + 1))
       | None ->
           set_var m "RSTART" (Num 0.0);
           set_var m "RLENGTH" (Num (-1.0));
           Num 0.0)
  | "sprintf" ->
      let format = text 0 in
      let rest = List.filteri (fun i _ -> i > 0) args in
      let (out, errors) = Posix.Fmt.printf format (List.map (fun e -> to_text m (eval m e)) rest) in
      List.iter (fun msg -> warn "%s" msg) errors;
      Str out
  | "sin" -> Num (sin (number 0))
  | "cos" -> Num (cos (number 0))
  | "atan2" -> Num (atan2 (number 0) (number 1))
  | "exp" -> Num (exp (number 0))
  | "log" -> Num (log (number 0))
  | "sqrt" -> Num (sqrt (number 0))
  | "int" -> Num (Float.of_int (int_of_float (Float.trunc (number 0))))
  | "rand" -> Num (Random.float 1.0)
  | "srand" ->
      let previous = match get_var m "__srand_seed" with Uninit -> 0.0 | v -> to_number v in
      let seed = if count >= 1 then number 0 else Unix.time () in
      set_var m "__srand_seed" (Num seed);
      Random.init (int_of_float seed);
      Num previous
  | "tolower" -> Str (String.lowercase_ascii (text 0))
  | "toupper" -> Str (String.uppercase_ascii (text 0))
  | "system" ->
      print_string (Buffer.contents m.out);
      Buffer.clear m.out;
      flush stdout;
      Num (float_of_int (Sys.command (text 0)))
  | "close" ->
      let key = text 0 in
      let closed = ref (-1) in
      List.iter (fun prefix ->
          match Hashtbl.find_opt m.streams (prefix ^ key) with
          | Some s ->
              (match s with
               | Out oc -> if oc != stdout && oc != stderr then close_out oc; closed := 0
               | Pipe_out oc -> closed := (match Unix.close_process_out oc with
                   | Unix.WEXITED c -> c | _ -> 1)
               | In ic -> close_in ic; closed := 0
               | Pipe_in ic -> closed := (match Unix.close_process_in ic with
                   | Unix.WEXITED c -> c | _ -> 1));
              Hashtbl.remove m.streams (prefix ^ key)
          | None -> ()) [ ">"; ">>"; "|"; "<"; "cmd|" ];
      Num (float_of_int !closed)
  | "fflush" -> flush stdout; Num 0.0
  | _ -> bad "%s: unknown function" name

(* ---------- user functions ---------- *)

and call m name args =
  match Hashtbl.find_opt m.funcs name with
  | None -> bad "%s: called but not defined" name
  | Some (params, body) ->
      (* An argument that is an array is passed by reference and a scalar
         by value; a parameter with no argument is a local variable
         (XCU awk). *)
      let bindings = List.mapi (fun i param ->
          match List.nth_opt args i with
          | None -> (param, ref (Scalar Uninit))
          | Some (Elval (Lvar v)) when (match !(find_cell m v) with Table _ -> true | _ -> false) ->
              (param, find_cell m v)
          | Some e -> (param, ref (Scalar (eval m e)))) params in
      let saved = m.locals in
      m.locals <- bindings;
      let result =
        match run m body with
        | () -> Uninit
        | exception Return_value v -> (Obj.obj v : value) in
      m.locals <- saved;
      result

(* ---------- getline ---------- *)

and getline m g =
  let store text =
    match g.gvar with
    | Some lv -> write m lv (of_input text)
    | None ->
        m.record <- text;
        m.split_done <- false;
        ensure_split m in
  let bump_nr () =
    set_var m "NR" (Num (to_number (get_var m "NR") +. 1.0)) in
  match g.gsource with
  | Gmain ->
      (match next_record m with
       | Some text ->
           store text;
           bump_nr ();
           set_var m "FNR" (Num (to_number (get_var m "FNR") +. 1.0));
           Num 1.0
       | None -> Num 0.0)
  | Gfile e ->
      let name = to_text m (eval m e) in
      let key = "<" ^ name in
      let ic =
        match Hashtbl.find_opt m.streams key with
        | Some (In ic) -> Some ic
        | _ ->
            (match (if name = "-" || name = "/dev/stdin" then Some stdin else
                      match open_in_bin name with ic -> Some ic | exception _ -> None) with
             | Some ic -> Hashtbl.replace m.streams key (In ic); Some ic
             | None -> None) in
      (match ic with
       | None -> Num (-1.0)
       | Some ic ->
           (match read_line_raw ic with
            | Some raw -> store (fst (chop raw)); if g.gvar = None then bump_nr (); Num 1.0
            | None -> Num 0.0))
  | Gcommand e ->
      let command = to_text m (eval m e) in
      let key = "cmd|" ^ command in
      let ic =
        match Hashtbl.find_opt m.streams key with
        | Some (Pipe_in ic) -> Some ic
        | _ ->
            print_string (Buffer.contents m.out);
            Buffer.clear m.out;
            flush stdout;
            (match Unix.open_process_in command with
             | ic -> Hashtbl.replace m.streams key (Pipe_in ic); Some ic
             | exception _ -> None) in
      (match ic with
       | None -> Num (-1.0)
       | Some ic ->
           (match read_line_raw ic with
            | Some raw -> store (fst (chop raw)); bump_nr (); Num 1.0
            | None -> Num 0.0))

(* the next record of the main input, opening files as they are needed *)
and next_record m =
  match m.current with
  | Some ic ->
      (match read_line_raw ic with
       | Some raw -> Some (fst (chop raw))
       | None -> if ic != stdin then close_in ic; m.current <- None; next_record m)
  | None ->
      match m.files with
      | [] -> None
      | file :: rest ->
          m.files <- rest;
          (* an operand of the form var=value is an assignment, not a file *)
          (match String.index_opt file '=' with
           | Some k when k > 0 && Posix.Regex.is_alpha file.[0] ->
               set_var m (String.sub file 0 k)
                 (of_input (String.sub file (k + 1) (String.length file - k - 1)));
               next_record m
           | _ ->
               set_var m "FILENAME" (Str file);
               set_var m "FNR" (Num 0.0);
               (match (if file = "-" then Some stdin else
                         match open_in_bin file with ic -> Some ic | exception e ->
                           warn "%s" (sys_message e); m.exit_status <- 2; None) with
                | Some ic -> m.current <- Some ic; next_record m
                | None -> next_record m))

(* ---------- statements ---------- *)

and run m s =
  match s with
  | Snop -> ()
  | Sblock l -> List.iter (run m) l
  | Sexpr e -> ignore (eval m e)
  | Sprint (args, redirect) ->
      let ofs = match get_var m "OFS" with Uninit -> " " | v -> to_text m v in
      let ors = match get_var m "ORS" with Uninit -> "\n" | v -> to_text m v in
      let text =
        if args = [] then m.record
        else String.concat ofs (List.map (fun e -> to_text m (eval m e)) args) in
      output m (redirect_of m redirect) (text ^ ors)
  | Sprintf (args, redirect) ->
      (match args with
       | [] -> bad "printf: no format"
       | format :: rest ->
           let f = to_text m (eval m format) in
           let (text, errors) = Posix.Fmt.printf f (List.map (fun e -> to_text m (eval m e)) rest) in
           List.iter (fun msg -> warn "%s" msg) errors;
           output m (redirect_of m redirect) text)
  | Sif (c, a, b) ->
      if truth m (eval m c) then run m a
      else (match b with Some s -> run m s | None -> ())
  | Swhile (c, body) ->
      (try
         while truth m (eval m c) do
           try run m body with Continue_loop -> ()
         done
       with Break_loop -> ())
  | Sdo (body, c) ->
      (try
         let again = ref true in
         while !again do
           (try run m body with Continue_loop -> ());
           again := truth m (eval m c)
         done
       with Break_loop -> ())
  | Sfor (init, cond, step, body) ->
      (match init with Some s -> run m s | None -> ());
      (try
         while (match cond with Some c -> truth m (eval m c) | None -> true) do
           (try run m body with Continue_loop -> ());
           match step with Some s -> run m s | None -> ()
         done
       with Break_loop -> ())
  | Sforin (v, array, body) ->
      let t = table_of m array in
      (* the standard leaves the order unspecified; a settled one makes a
         build that uses awk reproducible *)
      let keys = List.sort compare (Hashtbl.fold (fun k _ acc -> k :: acc) t []) in
      (try
         List.iter (fun k ->
             set_var m v (of_input k);
             try run m body with Continue_loop -> ()) keys
       with Break_loop -> ())
  | Snext -> raise Next_record
  | Snextfile -> raise Next_file
  | Sbreak -> raise Break_loop
  | Scontinue -> raise Continue_loop
  | Sexit e ->
      let code = match e with Some e -> int_of_float (to_number (eval m e)) | None -> m.exit_status in
      raise (Exit_program code)
  | Sreturn e ->
      let v = match e with Some e -> eval m e | None -> Uninit in
      raise (Return_value (Obj.repr v))
  | Sdelete (name, subs) ->
      let t = table_of m name in
      if subs = [] then Hashtbl.reset t
      else Hashtbl.remove t (subscript m subs (List.map (fun s -> to_text m (eval m s)) subs))

and redirect_of m = function
  | None -> None
  | Some (Rfile e) -> Some (to_text m (eval m e), `File)
  | Some (Rappend e) -> Some (to_text m (eval m e), `Append)
  | Some (Rpipe e) -> Some (to_text m (eval m e), `Pipe)

(* ---------- the utility ---------- *)

let create program_items operands =
  let m = {
    globals = Hashtbl.create 64;
    locals = [];
    funcs = Hashtbl.create 8;
    fields = [||];
    record = "";
    split_done = true;
    streams = Hashtbl.create 8;
    files = operands;
    current = None;
    exit_status = 0;
    out = Buffer.create 65536;
    regex_cache = Hashtbl.create 16;
  } in
  set_var m "FS" (Str " ");
  set_var m "OFS" (Str " ");
  set_var m "ORS" (Str "\n");
  set_var m "RS" (Str "\n");
  set_var m "NR" (Num 0.0);
  set_var m "NF" (Num 0.0);
  set_var m "FNR" (Num 0.0);
  set_var m "SUBSEP" (Str "\028");
  set_var m "CONVFMT" (Str "%.6g");
  set_var m "OFMT" (Str "%.6g");
  set_var m "RSTART" (Num 0.0);
  set_var m "RLENGTH" (Num (-1.0));
  (* ENVIRON holds the environment, which summarize.awk reads *)
  let environ = Hashtbl.create 64 in
  Array.iter (fun kv ->
      match String.index_opt kv '=' with
      | Some k ->
          Hashtbl.replace environ (String.sub kv 0 k)
            (of_input (String.sub kv (k + 1) (String.length kv - k - 1)))
      | None -> ()) (Unix.environment ());
  Hashtbl.replace m.globals "ENVIRON" (ref (Table environ));
  List.iter (fun item ->
      match item with
      | Function (name, params, body) -> Hashtbl.replace m.funcs name (params, body)
      | Rule _ -> ()) program_items;
  m

let main _argv opts operands =
  let assignments = Posix.Getopt.all opts "v" in
  let program_text =
    match Posix.Getopt.all opts "f" with
    | [] ->
        (match operands with
         | text :: _ -> text
         | [] -> die 2 "usage: awk [-F sepstring] [-v assignment]... program [argument...]")
    | files ->
        String.concat "\n" (List.map (fun f ->
            match In_channel.with_open_bin f In_channel.input_all with
            | text -> text
            | exception e -> warn "%s" (sys_message e); raise (Fail 2)) files) in
  let inputs =
    match Posix.Getopt.all opts "f" with
    | [] -> (match operands with _ :: rest -> rest | [] -> [])
    | _ -> operands in
  let items = match program program_text with
    | i -> i
    | exception Bad msg -> warn "%s" msg; raise (Fail 2) in
  let m = create items (if inputs = [] then [ "-" ] else inputs) in
  (match Posix.Getopt.arg opts "F" with
   | Some fs ->
       (* -F takes an escape sequence, and a lone 't' means a tab *)
       let fs = if fs = "t" then "\t" else Posix.Fmt.printf_escapes fs in
       set_var m "FS" (Str fs)
   | None -> ());
  List.iter (fun a ->
      match String.index_opt a '=' with
      | Some k ->
          set_var m (String.sub a 0 k)
            (of_input (Posix.Fmt.printf_escapes (String.sub a (k + 1) (String.length a - k - 1))))
      | None -> warn "%s: not an assignment" a) assignments;
  let status = ref 0 in
  let finish () =
    print_string (Buffer.contents m.out);
    Buffer.clear m.out;
    flush stdout;
    Hashtbl.iter (fun _ s ->
        match s with
        | Out oc -> if oc != stdout && oc != stderr then (try close_out oc with _ -> ())
        | Pipe_out oc -> (try ignore (Unix.close_process_out oc) with _ -> ())
        | In ic -> (try close_in ic with _ -> ())
        | Pipe_in ic -> (try ignore (Unix.close_process_in ic) with _ -> ())) m.streams in
  let begins = List.filter_map (function Rule (Pbegin, a, _) -> a | _ -> None) items in
  let ends = List.filter_map (function Rule (Pend, a, _) -> a | _ -> None) items in
  let rules = List.filter (function
      | Rule (Pbegin, _, _) | Rule (Pend, _, _) | Function _ -> false
      | Rule _ -> true) items in
  let run_ends () =
    List.iter (fun a ->
        match run m a with
        | () -> ()
        | exception Exit_program c -> status := c) ends in
  (try
     List.iter (fun a -> run m a) begins;
     (* the input is only read if a rule or an END action needs it *)
     if rules <> [] || ends <> [] then begin
       let more = ref true in
       while !more do
         match next_record m with
         | None -> more := false
         | Some text ->
             m.record <- text;
             m.split_done <- false;
             set_var m "NR" (Num (to_number (get_var m "NR") +. 1.0));
             set_var m "FNR" (Num (to_number (get_var m "FNR") +. 1.0));
             (try
                List.iter (fun item ->
                    match item with
                    | Rule (pattern, action, active) ->
                        let take =
                          match pattern with
                          | Palways -> true
                          | Pexpr e -> truth m (eval m e)
                          | Prange (a, z) ->
                              if !active then begin
                                if truth m (eval m z) then active := false;
                                true
                              end else if truth m (eval m a) then begin
                                if not (truth m (eval m z)) then active := true;
                                true
                              end else false
                          | Pbegin | Pend -> false in
                        if take then
                          (match action with
                           | Some a -> run m a
                           | None -> run m (Sprint ([], None)))
                    | Function _ -> ()) rules
              with
              | Next_record -> ()
              | Next_file ->
                  (match m.current with
                   | Some ic -> if ic != stdin then close_in ic; m.current <- None
                   | None -> ()))
       done
     end;
     run_ends ()
   with
   | Exit_program c ->
       status := c;
       (* an exit in a BEGIN or a rule still runs the END actions *)
       (try run_ends () with Exit_program c -> status := c)
   | Bad msg -> warn "%s" msg; status := 2);
  finish ();
  if !status <> 0 then !status else m.exit_status
