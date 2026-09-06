open Token

(* ---- Cursor ------------------------------------------------------------ *)

type cursor = {
  src : string;
  mutable pos : int;
  mutable line : int;
  mutable col : int;
  mutable file : string;
}

let eof = '\000' (* a NUL byte may not appear in a source file, 5.2.1 *)

let peek c k = if c.pos + k < String.length c.src then c.src.[c.pos + k] else eof
let loc c = { Loc.file = c.file; line = c.line; col = c.col }

let advance c =
  if peek c 0 = '\n' then (c.line <- c.line + 1; c.col <- 1) else c.col <- c.col + 1;
  c.pos <- c.pos + 1

let advance_n c n = for _ = 1 to n do advance c done

(* 6.4.2.1 *)
let is_digit ch = '0' <= ch && ch <= '9'
let is_nondigit ch = ch = '_' || ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z')
let is_hex ch = is_digit ch || ('a' <= ch && ch <= 'f') || ('A' <= ch && ch <= 'F')
let is_octal ch = '0' <= ch && ch <= '7'
let hex_value ch =
  if is_digit ch then Char.code ch - 48
  else Char.code (Char.lowercase_ascii ch) - Char.code 'a' + 10

let utf8_encode b cp =
  let add x = Buffer.add_char b (Char.chr x) in
  if cp < 0x80 then add cp
  else if cp < 0x800 then (add (0xC0 lor (cp lsr 6)); add (0x80 lor (cp land 0x3F)))
  else if cp < 0x10000 then
    (add (0xE0 lor (cp lsr 12)); add (0x80 lor ((cp lsr 6) land 0x3F)); add (0x80 lor (cp land 0x3F)))
  else
    (add (0xF0 lor (cp lsr 18)); add (0x80 lor ((cp lsr 12) land 0x3F));
     add (0x80 lor ((cp lsr 6) land 0x3F)); add (0x80 lor (cp land 0x3F)))

(* Decode one UTF-8 sequence at the cursor, returning its code point. *)
let utf8_decode c =
  let b0 = Char.code (peek c 0) in
  let n, init =
    if b0 < 0x80 then 1, b0
    else if b0 land 0xE0 = 0xC0 then 2, b0 land 0x1F
    else if b0 land 0xF0 = 0xE0 then 3, b0 land 0x0F
    else if b0 land 0xF8 = 0xF0 then 4, b0 land 0x07
    else Diag.error (loc c) "invalid UTF-8 byte 0x%02x" b0 in
  let cp = ref init in
  for k = 1 to n - 1 do cp := (!cp lsl 6) lor (Char.code (peek c k) land 0x3F) done;
  advance_n c n;
  !cp

(* ---- Whitespace, comments, directives ------------------------------------ *)

(* Skip a comment (6.4.9); the cursor is on its first character. *)
let skip_comment c =
  let start = loc c in
  if peek c 1 = '*' then begin
    advance_n c 2;
    while not (peek c 0 = '*' && peek c 1 = '/') do
      if peek c 0 = eof then Diag.error start "unterminated comment";
      advance c
    done;
    advance_n c 2
  end else
    while peek c 0 <> '\n' && peek c 0 <> eof do advance c done

let skip_to_eol c = while peek c 0 <> '\n' && peek c 0 <> eof do advance c done

(* A leftover directive line.  A line marker [# 12 "file.c" flags] (as
   [gcc -E] writes them, 6.10.4 [#line]) resets the position; anything
   else, such as [#pragma], is skipped. *)
let directive c =
  advance c; (* '#' *)
  while peek c 0 = ' ' || peek c 0 = '\t' do advance c done;
  if peek c 0 = 'l' && String.sub c.src c.pos 4 = "line" then advance_n c 4;
  while peek c 0 = ' ' || peek c 0 = '\t' do advance c done;
  if is_digit (peek c 0) then begin
    let n = ref 0 in
    while is_digit (peek c 0) do n := !n * 10 + Char.code (peek c 0) - 48; advance c done;
    while peek c 0 = ' ' || peek c 0 = '\t' do advance c done;
    let file =
      if peek c 0 = '"' then begin
        advance c;
        let b = Buffer.create 32 in
        while peek c 0 <> '"' && peek c 0 <> '\n' do
          if peek c 0 = '\\' then advance c;
          Buffer.add_char b (peek c 0); advance c
        done;
        Buffer.contents b
      end else c.file in
    skip_to_eol c;
    if peek c 0 = '\n' then advance c;
    c.line <- !n;
    c.file <- file
  end else
    skip_to_eol c

(* ---- Escape sequences (6.4.4.4) ----------------------------------------- *)

(* The cursor is just after the backslash.  Returns the value, and whether
   it was a numeric escape (whose value is a byte or code unit, not a
   character to be encoded). *)
let escape c =
  let start = loc c in
  let ch = peek c 0 in
  advance c;
  match ch with
  | '\'' -> 0x27, false | '"' -> 0x22, false | '?' -> 0x3F, false | '\\' -> 0x5C, false
  | 'a' -> 7, false | 'b' -> 8, false | 'f' -> 12, false | 'n' -> 10, false
  | 'r' -> 13, false | 't' -> 9, false | 'v' -> 11, false
  | '0' .. '7' ->
      let v = ref (Char.code ch - 48) and n = ref 1 in
      while !n < 3 && is_octal (peek c 0) do
        v := (!v * 8) + Char.code (peek c 0) - 48; advance c; incr n
      done;
      !v, true
  | 'x' ->
      if not (is_hex (peek c 0)) then Diag.error start "\\x used with no following hex digits";
      let v = ref 0 in
      while is_hex (peek c 0) do v := (!v * 16) + hex_value (peek c 0); advance c done;
      !v, true
  | 'u' | 'U' ->
      (* 6.4.3 universal character names *)
      let n = if ch = 'u' then 4 else 8 in
      let v = ref 0 in
      for _ = 1 to n do
        if not (is_hex (peek c 0)) then Diag.error start "incomplete universal character name";
        v := (!v * 16) + hex_value (peek c 0); advance c
      done;
      if !v < 0xA0 && !v <> 0x24 && !v <> 0x40 && !v <> 0x60 || (0xD800 <= !v && !v <= 0xDFFF) then
        Diag.error start "universal character name \\%c%0*X is not a valid character" ch n !v;
      !v, false
  | _ -> Diag.error start "unknown escape sequence \\%c" ch

(* ---- Identifiers ---------------------------------------------------------- *)

let keyword_table =
  let t = Hashtbl.create 64 in
  List.iter (fun (s, k) -> Hashtbl.replace t s k) Token.keywords;
  t

let identifier c =
  let b = Buffer.create 16 in
  let continue = ref true in
  while !continue do
    let ch = peek c 0 in
    if is_nondigit ch || is_digit ch then (Buffer.add_char b ch; advance c)
    else if ch = '\\' && (peek c 1 = 'u' || peek c 1 = 'U') then begin
      advance c;
      let cp, _ = escape c in
      utf8_encode b cp
    end
    else if Char.code ch >= 0x80 then utf8_encode b (utf8_decode c) (* 6.4.2.1p3, D.1 *)
    else continue := false
  done;
  let s = Buffer.contents b in
  match Hashtbl.find_opt keyword_table s with
  | Some k -> Keyword k
  | None -> Ident s

(* ---- Numbers (6.4.4.1, 6.4.4.2) ------------------------------------------ *)

(* A pp-number (6.4.8) is scanned greedily and then classified: this is
   what makes "0x1e+1" one ill-formed token rather than an addition. *)
let number c =
  let start = loc c in
  let b = Buffer.create 16 in
  let continue = ref true in
  while !continue do
    let ch = peek c 0 in
    if is_digit ch || is_nondigit ch || ch = '.' then (Buffer.add_char b ch; advance c)
    else continue := false;
    if (ch = 'e' || ch = 'E' || ch = 'p' || ch = 'P') && (peek c 0 = '+' || peek c 0 = '-') then
      (Buffer.add_char b (peek c 0); advance c)
  done;
  let text = Buffer.contents b in
  let n = String.length text in
  let hex = n > 1 && text.[0] = '0' && (text.[1] = 'x' || text.[1] = 'X') in
  let has ch = String.contains text ch in
  let floating =
    has '.' || (if hex then has 'p' || has 'P' else has 'e' || has 'E') in
  if floating then begin
    let body, suffix =
      match text.[n - 1] with
      | 'f' | 'F' -> String.sub text 0 (n - 1), F_f
      | 'l' | 'L' -> String.sub text 0 (n - 1), F_l
      | _ -> text, F_none in
    (* Validate the shape by letting OCaml parse it: it accepts exactly the
       decimal and hexadecimal forms of 6.4.4.2 (a lone "." aside). *)
    let ok = body <> "." && (match float_of_string_opt body with Some _ -> true | None -> false)
             && (not hex || has 'p' || has 'P') in
    if not ok then Diag.error start "invalid floating constant %S" text;
    Float_const { text = body; suffix }
  end else begin
    (* Split the suffix: up to one u/U and up to two l/L (same case). *)
    let i = ref n in
    while !i > 0 && (match text.[!i - 1] with 'u' | 'U' | 'l' | 'L' -> true | _ -> false) do decr i done;
    let digits = String.sub text 0 !i and suffix = String.sub text !i (n - !i) in
    let unsigned = String.contains suffix 'u' || String.contains suffix 'U' in
    let longs = String.length suffix - (if unsigned then 1 else 0) in
    let valid_suffix =
      match suffix with
      | "" | "u" | "U" | "l" | "L" | "ll" | "LL"
      | "ul" | "uL" | "Ul" | "UL" | "lu" | "lU" | "Lu" | "LU"
      | "ull" | "uLL" | "Ull" | "ULL" | "llu" | "llU" | "LLu" | "LLU" -> true
      | _ -> false in
    if not valid_suffix then Diag.error start "invalid suffix %S on integer constant" suffix;
    let radix, value =
      if hex then 16, String.sub digits 2 (String.length digits - 2)
      else if String.length digits > 1 && digits.[0] = '0' then 8, digits
      else 10, digits in
    if value = "" then Diag.error start "invalid integer constant %S" text;
    String.iter (fun ch ->
        let ok = match radix with 16 -> is_hex ch | 8 -> is_octal ch | _ -> is_digit ch in
        if not ok then Diag.error start "invalid digit '%c' in integer constant %S" ch text) value;
    Int_const { value; radix; suffix = { unsigned; longs } }
  end

(* ---- Character constants and string literals (6.4.4.4, 6.4.5) ------------ *)

(* If the cursor is on an encoding prefix followed by a quote, consume the
   prefix and return the encoding; otherwise consume nothing. *)
let encoding_prefix c =
  match peek c 0, peek c 1, peek c 2 with
  | 'u', '8', '"' -> advance_n c 2; Some Utf8
  | 'u', ('"' | '\''), _ -> advance c; Some Char16
  | 'U', ('"' | '\''), _ -> advance c; Some Char32
  | 'L', ('"' | '\''), _ -> advance c; Some Wide
  | _ -> None

(* Read the body of a literal up to [close], producing code units: for
   plain and UTF-8 literals these are bytes, for wide ones code points.
   Numeric escapes are code units as written; everything else is a
   character, decoded from the UTF-8 source. *)
let literal_body c close enc =
  let start = loc c in
  advance c; (* opening quote *)
  let units = ref [] in
  let push u = units := u :: !units in
  while peek c 0 <> close do
    (match peek c 0 with
     | '\n' | '\000' -> Diag.error start "missing terminating %c character" close
     | '\\' ->
         advance c;
         let v, numeric = escape c in
         if numeric then begin
           let limit = match enc with Plain | Utf8 -> 0xFF | Char16 -> 0xFFFF | Wide | Char32 -> 0x7FFFFFFF in
           if v > limit then Diag.error start "escape sequence out of range";
           push v
         end else begin
           match enc with
           | Plain | Utf8 -> let b = Buffer.create 4 in utf8_encode b v; String.iter (fun ch -> push (Char.code ch)) (Buffer.contents b)
           | Wide | Char16 | Char32 -> push v
         end
     | ch when Char.code ch >= 0x80 ->
         (match enc with
          | Plain | Utf8 -> push (Char.code ch); advance c
          | Wide | Char16 | Char32 -> push (utf8_decode c))
     | ch -> push (Char.code ch); advance c)
  done;
  advance c; (* closing quote *)
  List.rev !units

let char_constant c enc =
  let start = loc c in
  let chars = literal_body c '\'' enc in
  if chars = [] then Diag.error start "empty character constant";
  Char_const { chars; enc }

let string_literal c enc =
  let units = literal_body c '"' enc in
  let b = Buffer.create 32 in
  (match enc with
   | Plain | Utf8 -> List.iter (fun u -> Buffer.add_char b (Char.chr u)) units
   | Wide | Char16 | Char32 -> List.iter (utf8_encode b) units);
  String_lit { bytes = Buffer.contents b; enc }

(* ---- Punctuators (6.4.6) -------------------------------------------------- *)

let punctuator c =
  let start = loc c in
  let take n p = advance_n c n; Punct p in
  match peek c 0, peek c 1, peek c 2, peek c 3 with
  | '%', ':', '%', ':' -> take 4 HashHash
  | '.', '.', '.', _ -> take 3 Ellipsis
  | '<', '<', '=', _ -> take 3 LShiftEq
  | '>', '>', '=', _ -> take 3 RShiftEq
  | '-', '>', _, _ -> take 2 Arrow
  | '+', '+', _, _ -> take 2 PlusPlus
  | '-', '-', _, _ -> take 2 MinusMinus
  | '<', '<', _, _ -> take 2 LShift
  | '>', '>', _, _ -> take 2 RShift
  | '<', '=', _, _ -> take 2 Le
  | '>', '=', _, _ -> take 2 Ge
  | '=', '=', _, _ -> take 2 EqEq
  | '!', '=', _, _ -> take 2 BangEq
  | '&', '&', _, _ -> take 2 AmpAmp
  | '|', '|', _, _ -> take 2 BarBar
  | '*', '=', _, _ -> take 2 StarEq
  | '/', '=', _, _ -> take 2 SlashEq
  | '%', '=', _, _ -> take 2 PercentEq
  | '+', '=', _, _ -> take 2 PlusEq
  | '-', '=', _, _ -> take 2 MinusEq
  | '&', '=', _, _ -> take 2 AmpEq
  | '^', '=', _, _ -> take 2 CaretEq
  | '|', '=', _, _ -> take 2 BarEq
  | '#', '#', _, _ -> take 2 HashHash
  (* digraphs, 6.4.6p3 *)
  | '<', ':', _, _ -> take 2 LBracket
  | ':', '>', _, _ -> take 2 RBracket
  | '<', '%', _, _ -> take 2 LBrace
  | '%', '>', _, _ -> take 2 RBrace
  | '%', ':', _, _ -> take 2 Hash
  | '[', _, _, _ -> take 1 LBracket | ']', _, _, _ -> take 1 RBracket
  | '(', _, _, _ -> take 1 LParen   | ')', _, _, _ -> take 1 RParen
  | '{', _, _, _ -> take 1 LBrace   | '}', _, _, _ -> take 1 RBrace
  | '.', _, _, _ -> take 1 Dot      | '&', _, _, _ -> take 1 Amp
  | '*', _, _, _ -> take 1 Star     | '+', _, _, _ -> take 1 Plus
  | '-', _, _, _ -> take 1 Minus    | '~', _, _, _ -> take 1 Tilde
  | '!', _, _, _ -> take 1 Bang     | '/', _, _, _ -> take 1 Slash
  | '%', _, _, _ -> take 1 Percent  | '<', _, _, _ -> take 1 Lt
  | '>', _, _, _ -> take 1 Gt       | '^', _, _, _ -> take 1 Caret
  | '|', _, _, _ -> take 1 Bar      | '?', _, _, _ -> take 1 Question
  | ':', _, _, _ -> take 1 Colon    | ';', _, _, _ -> take 1 Semi
  | '=', _, _, _ -> take 1 Eq       | ',', _, _, _ -> take 1 Comma
  | '#', _, _, _ -> take 1 Hash
  | ch, _, _, _ -> Diag.error start "stray '%c' in program" ch

(* ---- Phase 6: string literal concatenation (6.4.5p5) ---------------------- *)

let rec concatenate = function
  | ({ tok = String_lit a; loc } as t1) :: { tok = String_lit b; loc = loc2 } :: rest ->
      let enc =
        match a.enc, b.enc with
        | e, Plain | Plain, e -> e
        | e1, e2 when e1 = e2 -> e1
        | _ -> Diag.error loc2 "concatenation of string literals with different encoding prefixes" in
      ignore t1;
      concatenate ({ tok = String_lit { bytes = a.bytes ^ b.bytes; enc }; loc } :: rest)
  | t :: rest -> t :: concatenate rest
  | [] -> []

(* ---- Main loop --------------------------------------------------------------- *)

let tokenize ~file src =
  let c = { src; pos = 0; line = 1; col = 1; file } in
  let toks = ref [] in
  let line_start = ref true in
  let finished = ref false in
  while not !finished do
    let ch = peek c 0 in
    match ch with
    | ' ' | '\t' | '\r' | '\012' | '\011' -> advance c
    | '\n' -> advance c; line_start := true
    | '/' when peek c 1 = '*' || peek c 1 = '/' -> skip_comment c
    | '#' when !line_start -> directive c
    | _ ->
        let loc = loc c in
        let tok =
          match ch with
          | '\000' -> finished := true; Eof
          | '0' .. '9' -> number c
          | '.' when is_digit (peek c 1) -> number c
          | '"' -> string_literal c Plain
          | '\'' -> char_constant c Plain
          | 'u' | 'U' | 'L' ->
              (match encoding_prefix c with
               | Some enc -> if peek c 0 = '"' then string_literal c enc else char_constant c enc
               | None -> identifier c)
          | _ when is_nondigit ch || ch = '\\' || Char.code ch >= 0x80 -> identifier c
          | _ -> punctuator c in
        line_start := false;
        toks := { tok; loc } :: !toks
  done;
  concatenate (List.rev !toks)
