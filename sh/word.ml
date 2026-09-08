(* Taking a raw word apart (XCU 2.2 quoting, 2.6 word expansions).

   The lexer left each word as it was written; this pass turns it into the
   list of parts the expander walks.  The three quoting forms differ in
   what stays special inside them (2.2.1 to 2.2.3): after a backslash,
   nothing; inside single quotes, nothing at all; inside double quotes,
   only '$', '`' and a backslash before one of those, a '"' or a newline.

   A command substitution holds a whole program, so this module needs the
   parser, which needs this module: [program] is the parser's entry point,
   filled in by Parse. *)

open Ast

let program : (string -> Ast.program) ref =
  ref (fun _ -> failwith "Word.program: not set")

let is_name_start c = Posix.Regex.is_alpha c || c = '_'
let is_name_char c = Posix.Regex.is_alnum c || c = '_'

(* the special parameters of 2.5.2, which are one character long *)
let is_special c = String.contains "@*#?-$!0" c

exception Error of string

let err fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt

(* ---------- ${...} ---------- *)

(* The body of a ${...}, without the braces: a parameter, possibly with
   one of the operators of 2.6.2. *)
let rec braced body =
  let n = String.length body in
  if n = 0 then err "bad substitution"
  else if body.[0] = '#' && n > 1 then
    (* ${#x} is a length, but ${#} and ${#?} name the parameter '#' *)
    (match braced (String.sub body 1 (n - 1)) with
     | { pname; pop = Get } -> { pname; pop = Length }
     | _ -> err "bad substitution")
  else begin
    (* the name: a digit string, one special character, or a name *)
    let stop =
      if Posix.Regex.is_digit body.[0] then begin
        let k = ref 0 in
        while !k < n && Posix.Regex.is_digit body.[!k] do incr k done; !k
      end else if is_special body.[0] then 1
      else begin
        let k = ref 0 in
        while !k < n && is_name_char body.[!k] do incr k done; !k
      end in
    if stop = 0 then err "bad substitution";
    let pname = String.sub body 0 stop in
    let rest = String.sub body stop (n - stop) in
    if rest = "" then { pname; pop = Get }
    else begin
      let colon = rest.[0] = ':' in
      let rest = if colon then String.sub rest 1 (String.length rest - 1) else rest in
      if rest = "" then err "bad substitution";
      let arg k = parse (String.sub rest k (String.length rest - k)) in
      let doubled c = String.length rest > 1 && rest.[1] = c in
      match rest.[0] with
      | '-' -> { pname; pop = Default (colon, arg 1) }
      | '=' -> { pname; pop = Assign (colon, arg 1) }
      | '?' -> { pname; pop = Fail (colon, arg 1) }
      | '+' -> { pname; pop = Alt (colon, arg 1) }
      | '#' when not colon -> { pname; pop = Prefix (doubled '#', arg (if doubled '#' then 2 else 1)) }
      | '%' when not colon -> { pname; pop = Suffix (doubled '%', arg (if doubled '%' then 2 else 1)) }
      | c -> err "bad substitution: unknown operator %C" c
    end
  end

(* ---------- $... ---------- *)

(* The expansion starting at [i] (a '$' or a '`'): its part and the index
   just past it.  The bounds were already found by the lexer's skip
   functions, which this reuses so that the two passes agree. *)
and expansion src i n =
  if src.[i] = '`' then begin
    let stop = Lex.skip_expansion src i n 0 in
    let body = String.sub src (i + 1) (stop - i - 2) in
    (* inside backquotes a backslash quotes only \, $ and ` (2.6.3) *)
    let b = Buffer.create (String.length body) in
    let k = ref 0 in
    while !k < String.length body do
      if body.[!k] = '\\' && !k + 1 < String.length body
         && (body.[!k + 1] = '\\' || body.[!k + 1] = '$' || body.[!k + 1] = '`')
      then (Buffer.add_char b body.[!k + 1]; k := !k + 2)
      else (Buffer.add_char b body.[!k]; incr k)
    done;
    (Subst (!program (Buffer.contents b)), stop)
  end
  else if i + 1 >= n then (Str "$", i + 1)
  else
    match src.[i + 1] with
    | '(' when i + 2 < n && src.[i + 2] = '(' ->
        let stop = Lex.skip_expansion src i n 0 in
        (Arith (parse (String.sub src (i + 3) (stop - i - 5))), stop)
    | '(' ->
        let stop = Lex.skip_expansion src i n 0 in
        (Subst (!program (String.sub src (i + 2) (stop - i - 3))), stop)
    | '{' ->
        let stop = Lex.skip_expansion src i n 0 in
        (Param (braced (String.sub src (i + 2) (stop - i - 3))), stop)
    | c when is_name_start c ->
        let k = ref (i + 1) in
        while !k < n && is_name_char src.[!k] do incr k done;
        (Param { pname = String.sub src (i + 1) (!k - i - 1); pop = Get }, !k)
    | c when Posix.Regex.is_digit c ->
        (* only one digit without braces (2.5.1) *)
        (Param { pname = String.make 1 c; pop = Get }, i + 2)
    | c when is_special c -> (Param { pname = String.make 1 c; pop = Get }, i + 2)
    | _ -> (Str "$", i + 1)

(* ---------- a word ---------- *)

and parse src =
  let n = String.length src in
  let parts = ref [] and buf = Buffer.create 32 in
  let flush () =
    if Buffer.length buf > 0 then (parts := Str (Buffer.contents buf) :: !parts; Buffer.clear buf) in
  let add p = flush (); parts := p :: !parts in
  let i = ref 0 in
  while !i < n do
    match src.[!i] with
    | '\'' ->
        let stop = Lex.skip_single src (!i + 1) n 0 in
        add (Single (String.sub src (!i + 1) (stop - !i - 2)));
        i := stop
    | '"' ->
        let stop = Lex.skip_double src (!i + 1) n 0 in
        add (Double (dquoted (String.sub src (!i + 1) (stop - !i - 2))));
        i := stop
    | '\\' when !i + 1 < n -> add (Esc src.[!i + 1]); i := !i + 2
    | '\\' -> Buffer.add_char buf '\\'; incr i
    | '$' | '`' ->
        let (p, stop) = expansion src !i n in
        add p; i := stop
    | c -> Buffer.add_char buf c; incr i
  done;
  flush ();
  List.rev !parts

(* the inside of a double-quoted string (2.2.3) *)
and dquoted src =
  let n = String.length src in
  let parts = ref [] and buf = Buffer.create 32 in
  let flush () =
    if Buffer.length buf > 0 then (parts := Str (Buffer.contents buf) :: !parts; Buffer.clear buf) in
  let add p = flush (); parts := p :: !parts in
  let i = ref 0 in
  while !i < n do
    match src.[!i] with
    | '\\' when !i + 1 < n
             && (match src.[!i + 1] with '$' | '`' | '"' | '\\' | '\n' -> true | _ -> false) ->
        (* only these are quoted by a backslash inside double quotes; a
           backslash before anything else stands for itself *)
        if src.[!i + 1] = '\n' then i := !i + 2      (* a continuation *)
        else (add (Esc src.[!i + 1]); i := !i + 2)
    | '$' | '`' ->
        let (p, stop) = expansion src !i n in
        add p; i := stop
    | c -> Buffer.add_char buf c; incr i
  done;
  flush ();
  List.rev !parts

(* The body of a here-document (2.7.4).  It behaves as if double-quoted
   -- one field, expansions performed -- but a quote character in it is
   ordinary text, and a backslash quotes only the three characters that
   are still special. *)
and heredoc src =
  let n = String.length src in
  let parts = ref [] and buf = Buffer.create 256 in
  let flush () =
    if Buffer.length buf > 0 then (parts := Str (Buffer.contents buf) :: !parts; Buffer.clear buf) in
  let add p = flush (); parts := p :: !parts in
  let i = ref 0 in
  while !i < n do
    match src.[!i] with
    | '\\' when !i + 1 < n
             && (match src.[!i + 1] with '$' | '`' | '\\' | '\n' -> true | _ -> false) ->
        if src.[!i + 1] = '\n' then i := !i + 2
        else (add (Esc src.[!i + 1]); i := !i + 2)
    | '$' | '`' ->
        let (p, stop) = expansion src !i n in
        add p; i := stop
    | c -> Buffer.add_char buf c; incr i
  done;
  flush ();
  List.rev !parts

(* ---------- assignments ---------- *)

(* Is this raw word an assignment (2.9.1)?  It is when everything before
   the first '=' is a name, and the '=' is not quoted, which the scan
   below sees because a quote would have ended the name. *)
let assignment raw =
  let n = String.length raw in
  if n = 0 || not (is_name_start raw.[0]) then None
  else begin
    let k = ref 0 in
    while !k < n && is_name_char raw.[!k] do incr k done;
    if !k < n && raw.[!k] = '=' then
      Some (String.sub raw 0 !k, parse (String.sub raw (!k + 1) (n - !k - 1)))
    else None
  end

(* the text of a word that is entirely literal, for the places where the
   grammar wants a name: a function's name, a for loop's variable *)
let rec literal parts =
  let b = Buffer.create 16 in
  let ok = List.for_all (fun p ->
      match p with
      | Str s | Single s -> Buffer.add_string b s; true
      | Esc c -> Buffer.add_char b c; true
      | Double ps -> (match literal ps with Some s -> Buffer.add_string b s; true | None -> false)
      | _ -> false) parts in
  if ok then Some (Buffer.contents b) else None
