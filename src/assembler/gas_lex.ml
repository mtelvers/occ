(* Lexical analysis of GNU assembler source (GNU as manual, "Syntax").

   Statements end at a newline or a semicolon.  Comments are "#" to end of
   line and C-style "/* */".  Symbol names may contain letters, digits,
   "_", "." and "$"; a leading "%" marks a register, a leading "$" an
   immediate.  Local numeric labels "1:" and their references "1f"/"1b"
   are lexed as their own token so the parser can rename them. *)

type token =
  | IDENT of string        (* symbols, mnemonics and directives (".text") *)
  | INT of int64
  | STRING of string       (* with escapes decoded *)
  | REG of string          (* %rax -> "rax" *)
  | LOCALREF of int * bool (* 1f is (1, true), 1b is (1, false) *)
  | DOLLAR | COMMA | LPAREN | RPAREN | STAR | PLUS | MINUS | SLASH | PERCENT
  | AMP | BAR | CARET | TILDE | SHL | SHR | COLON | AT | EQUAL | BANG
  | NEWLINE
  | EOF

type t = {
  text : string;
  mutable pos : int;
  mutable line : int;
  file : string;
}

let make file text = { text; pos = 0; line = 1; file }

(* a second token of lookahead, for telling "(%rax)" from "(1+2)(%rax)" *)
let save lx = (lx.pos, lx.line)
let restore lx (pos, line) = lx.pos <- pos; lx.line <- line

let loc lx = { Loc.file = lx.file; line = lx.line; col = 0 }
let error lx fmt = Diag.error (loc lx) fmt

let peek_char lx k = if lx.pos + k < String.length lx.text then lx.text.[lx.pos + k] else '\000'

let is_ident_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '.' || c = '$'
let is_ident_char c = is_ident_start c || (c >= '0' && c <= '9')
let is_digit c = c >= '0' && c <= '9'
let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

let rec skip_space lx =
  match peek_char lx 0 with
  | ' ' | '\t' | '\r' -> lx.pos <- lx.pos + 1; skip_space lx
  | '#' -> while peek_char lx 0 <> '\n' && peek_char lx 0 <> '\000' do lx.pos <- lx.pos + 1 done
  | '/' when peek_char lx 1 = '*' ->
      lx.pos <- lx.pos + 2;
      while not (peek_char lx 0 = '*' && peek_char lx 1 = '/') do
        if peek_char lx 0 = '\000' then error lx "unterminated comment";
        if peek_char lx 0 = '\n' then lx.line <- lx.line + 1;
        lx.pos <- lx.pos + 1
      done;
      lx.pos <- lx.pos + 2; skip_space lx
  | _ -> ()

let take_while lx p =
  let start = lx.pos in
  while p (peek_char lx 0) do lx.pos <- lx.pos + 1 done;
  String.sub lx.text start (lx.pos - start)

(* Integers: decimal, 0x hex, 0b binary, or octal with a leading 0.
   Int64.of_string accepts the full unsigned range for hex, which the
   producers use for 64-bit bit patterns. *)
let number lx =
  let s =
    if peek_char lx 0 = '0' && (peek_char lx 1 = 'x' || peek_char lx 1 = 'X') then begin
      lx.pos <- lx.pos + 2; "0x" ^ take_while lx is_hex
    end else if peek_char lx 0 = '0' && (peek_char lx 1 = 'b' || peek_char lx 1 = 'B')
              && (peek_char lx 2 = '0' || peek_char lx 2 = '1') then begin
      lx.pos <- lx.pos + 2; "0b" ^ take_while lx (fun c -> c = '0' || c = '1')
    end else begin
      let digits = take_while lx is_digit in
      if String.length digits > 1 && digits.[0] = '0' then "0o" ^ String.sub digits 1 (String.length digits - 1)
      else "0u" ^ digits   (* unsigned: 9223372036854775808 wraps to min_int, as in gas *)
    end in
  match Int64.of_string_opt s with
  | Some v -> v
  | None -> error lx "bad number %s" s

(* String literals with C escapes (GNU as manual, "Strings"). *)
let string_literal lx =
  lx.pos <- lx.pos + 1;
  let b = Buffer.create 32 in
  let rec go () =
    match peek_char lx 0 with
    | '"' -> lx.pos <- lx.pos + 1
    | '\000' -> error lx "unterminated string"
    | '\\' ->
        lx.pos <- lx.pos + 1;
        let c = peek_char lx 0 in
        lx.pos <- lx.pos + 1;
        (match c with
         | 'n' -> Buffer.add_char b '\n' | 't' -> Buffer.add_char b '\t'
         | 'r' -> Buffer.add_char b '\r' | 'b' -> Buffer.add_char b '\b'
         | 'f' -> Buffer.add_char b '\012' | '"' -> Buffer.add_char b '"'
         | '\\' -> Buffer.add_char b '\\'
         | 'x' ->
             let h = take_while lx is_hex in
             Buffer.add_char b (Char.chr (int_of_string ("0x" ^ h) land 0xff))
         | '0' .. '7' ->
             let v = ref (Char.code c - 48) and n = ref 1 in
             while !n < 3 && peek_char lx 0 >= '0' && peek_char lx 0 <= '7' do
               v := !v * 8 + Char.code (peek_char lx 0) - 48; lx.pos <- lx.pos + 1; incr n
             done;
             Buffer.add_char b (Char.chr (!v land 0xff))
         | c -> Buffer.add_char b c);
        go ()
    | c -> Buffer.add_char b c; lx.pos <- lx.pos + 1; go () in
  go ();
  Buffer.contents b

let next lx =
  skip_space lx;
  let c = peek_char lx 0 in
  let one tok = lx.pos <- lx.pos + 1; tok in
  match c with
  | '\000' -> EOF
  | '\n' -> lx.pos <- lx.pos + 1; lx.line <- lx.line + 1; NEWLINE
  | ';' -> one NEWLINE
  | '"' -> STRING (string_literal lx)
  | '%' -> lx.pos <- lx.pos + 1;
      if is_ident_start (peek_char lx 0) then REG (take_while lx is_ident_char) else PERCENT
  | '$' -> one DOLLAR
  | ',' -> one COMMA | '(' -> one LPAREN | ')' -> one RPAREN | '*' -> one STAR
  | '+' -> one PLUS | '-' -> one MINUS | '/' -> one SLASH | '&' -> one AMP
  | '|' -> one BAR | '^' -> one CARET | '~' -> one TILDE | ':' -> one COLON
  | '@' -> one AT | '=' -> one EQUAL | '!' -> one BANG
  | '<' when peek_char lx 1 = '<' -> lx.pos <- lx.pos + 2; SHL
  | '>' when peek_char lx 1 = '>' -> lx.pos <- lx.pos + 2; SHR
  | c when is_digit c ->
      (* "1f" and "1b" are local label references, unless part of a hex number *)
      let start = lx.pos in
      let digits = take_while lx is_digit in
      let d = peek_char lx 0 in
      if (d = 'f' || d = 'b') && not (is_ident_char (peek_char lx 1)) && not (digits = "0" && d = 'b') then begin
        lx.pos <- lx.pos + 1;
        LOCALREF (int_of_string digits, d = 'f')
      end else begin
        lx.pos <- start;
        INT (number lx)
      end
  | c when is_ident_start c -> IDENT (take_while lx is_ident_char)
  | c -> error lx "unexpected character %C" c
