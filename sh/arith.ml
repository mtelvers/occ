(* Arithmetic expansion (IEEE Std 1003.1-2017, XCU 2.6.4).

   2.6.4 says the expression is evaluated as if it were a C integer
   constant expression, except that a name in it is read as a variable
   and the assignment operators may be used.  So this is the expression
   grammar of C11 6.5 over signed integers, with the same precedence
   levels, evaluated as it is parsed.  A name whose value is empty or
   unset counts as zero. *)

exception Error of string

let err fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt

type state = {
  src : string;
  mutable i : int;
  get : string -> string;             (* read a variable *)
  set : string -> string -> unit;     (* write one back *)
}

let rec skip st =
  if st.i < String.length st.src && (st.src.[st.i] = ' ' || st.src.[st.i] = '\t' || st.src.[st.i] = '\n')
  then (st.i <- st.i + 1; skip st)

let eof st = st.i >= String.length st.src
let peek st = if eof st then '\000' else st.src.[st.i]

(* does the operator [op] start here? *)
let at st op =
  let n = String.length op in
  st.i + n <= String.length st.src && String.sub st.src st.i n = op

let take st op = if at st op then (st.i <- st.i + String.length op; true) else false

(* A value is either a number or the name of a variable, so that the
   assignment operators have something to assign to. *)
type value = Num of int | Var of string * int

let num st = function
  | Num n -> n
  | Var (name, _) ->
      let text = String.trim (st.get name) in
      if text = "" then 0
      else (match int_of_string_opt text with
          | Some n -> n
          | None -> err "%s: expression expected" text)

let name_char c = Posix.Regex.is_alnum c || c = '_'

(* An integer constant (C11 6.4.4.1): decimal, or octal with a leading 0,
   or hexadecimal with 0x. *)
let constant st =
  let start = st.i in
  if take st "0x" || take st "0X" then begin
    while not (eof st) && Posix.Regex.is_xdigit (peek st) do st.i <- st.i + 1 done;
    if st.i = start + 2 then err "bad hexadecimal constant";
    int_of_string (String.sub st.src start (st.i - start))
  end else begin
    while not (eof st) && Posix.Regex.is_digit (peek st) do st.i <- st.i + 1 done;
    let text = String.sub st.src start (st.i - start) in
    if String.length text > 1 && text.[0] = '0' then int_of_string ("0o" ^ text)
    else int_of_string text
  end

let rec expression st = comma st

(* 6.5.17 *)
and comma st =
  let v = assignment st in
  skip st;
  if at st "," then (st.i <- st.i + 1; comma st) else v

(* 6.5.16, right-associative, and the only place a name is written to *)
and assignment st =
  let save = st.i in
  let left = conditional st in
  skip st;
  let compound =
    [ "*=", ( * ); "/=", (/); "%=", (mod); "+=", (+); "-=", (-);
      "<<=", (lsl); ">>=", (asr); "&=", (land); "^=", (lxor); "|=", (lor) ] in
  let hit = List.find_opt (fun (op, _) -> at st op) compound in
  match hit, left with
  | Some (op, f), Var (name, _) ->
      st.i <- st.i + String.length op;
      let right = num st (assignment st) in
      let v = f (num st left) right in
      st.set name (string_of_int v);
      Num v
  | _ ->
      if at st "=" && not (at st "==") then begin
        match left with
        | Var (name, _) ->
            st.i <- st.i + 1;
            let v = num st (assignment st) in
            st.set name (string_of_int v);
            Num v
        | Num _ -> st.i <- save; err "cannot assign to a constant"
      end else left

(* 6.5.15 *)
and conditional st =
  let c = logical_or st in
  skip st;
  if at st "?" then begin
    st.i <- st.i + 1;
    let a = expression st in
    skip st;
    if not (take st ":") then err "expected `:' in ?: expression";
    let b = conditional st in
    if num st c <> 0 then a else b
  end else c

and logical_or st =
  let rec go left =
    skip st;
    if at st "||" then begin
      st.i <- st.i + 2;
      let right = logical_and st in
      go (Num (if num st left <> 0 || num st right <> 0 then 1 else 0))
    end else left in
  go (logical_and st)

and logical_and st =
  let rec go left =
    skip st;
    if at st "&&" then begin
      st.i <- st.i + 2;
      let right = bit_or st in
      go (Num (if num st left <> 0 && num st right <> 0 then 1 else 0))
    end else left in
  go (bit_or st)

(* the bitwise levels, 6.5.10 to 6.5.12 *)
and bit_or st =
  let rec go left =
    skip st;
    if at st "|" && not (at st "||") && not (at st "|=") then begin
      st.i <- st.i + 1;
      let right = bit_xor st in
      go (Num (num st left lor num st right))
    end else left in
  go (bit_xor st)

and bit_xor st =
  let rec go left =
    skip st;
    if at st "^" && not (at st "^=") then begin
      st.i <- st.i + 1;
      let right = bit_and st in
      go (Num (num st left lxor num st right))
    end else left in
  go (bit_and st)

and bit_and st =
  let rec go left =
    skip st;
    if at st "&" && not (at st "&&") && not (at st "&=") then begin
      st.i <- st.i + 1;
      let right = equality st in
      go (Num (num st left land num st right))
    end else left in
  go (equality st)

and equality st =
  let rec go left =
    skip st;
    if at st "==" then begin
      st.i <- st.i + 2;
      let right = relational st in
      go (Num (if num st left = num st right then 1 else 0))
    end else if at st "!=" then begin
      st.i <- st.i + 2;
      let right = relational st in
      go (Num (if num st left <> num st right then 1 else 0))
    end else left in
  go (relational st)

and relational st =
  let rec go left =
    skip st;
    let cmp op f =
      st.i <- st.i + String.length op;
      let right = shift st in
      go (Num (if f (num st left) (num st right) then 1 else 0)) in
    if at st "<=" then cmp "<=" (<=)
    else if at st ">=" then cmp ">=" (>=)
    else if at st "<<" || at st ">>" then left
    else if at st "<" then cmp "<" (<)
    else if at st ">" then cmp ">" (>)
    else left in
  go (shift st)

and shift st =
  let rec go left =
    skip st;
    if at st "<<" && not (at st "<<=") then begin
      st.i <- st.i + 2;
      let right = additive st in
      go (Num (num st left lsl num st right))
    end else if at st ">>" && not (at st ">>=") then begin
      st.i <- st.i + 2;
      let right = additive st in
      go (Num (num st left asr num st right))
    end else left in
  go (additive st)

and additive st =
  let rec go left =
    skip st;
    if at st "+" && not (at st "+=") then begin
      st.i <- st.i + 1;
      let right = multiplicative st in
      go (Num (num st left + num st right))
    end else if at st "-" && not (at st "-=") then begin
      st.i <- st.i + 1;
      let right = multiplicative st in
      go (Num (num st left - num st right))
    end else left in
  go (multiplicative st)

and multiplicative st =
  let rec go left =
    skip st;
    let apply op f =
      st.i <- st.i + String.length op;
      let right = num st (unary st) in
      if (op = "/" || op = "%") && right = 0 then err "division by 0";
      go (Num (f (num st left) right)) in
    if at st "*" && not (at st "*=") then apply "*" ( * )
    else if at st "/" && not (at st "/=") then apply "/" (/)
    else if at st "%" && not (at st "%=") then apply "%" (mod)
    else left in
  go (unary st)

(* 6.5.3 *)
and unary st =
  skip st;
  if take st "!" then Num (if num st (unary st) = 0 then 1 else 0)
  else if take st "~" then Num (lnot (num st (unary st)))
  else if at st "-" && not (at st "--") then (st.i <- st.i + 1; Num (- (num st (unary st))))
  else if at st "+" && not (at st "++") then (st.i <- st.i + 1; unary st)
  else primary st

and primary st =
  skip st;
  if eof st then err "unexpected end of expression"
  else if peek st = '(' then begin
    st.i <- st.i + 1;
    let v = expression st in
    skip st;
    if not (take st ")") then err "expected `)'";
    Num (num st v)
  end
  else if Posix.Regex.is_digit (peek st) then Num (constant st)
  else if Posix.Regex.is_alpha (peek st) || peek st = '_' then begin
    let start = st.i in
    while not (eof st) && name_char (peek st) do st.i <- st.i + 1 done;
    let name = String.sub st.src start (st.i - start) in
    Var (name, 0)
  end
  else err "unexpected %C in expression" (peek st)

let eval ~get ~set text =
  let st = { src = text; i = 0; get; set } in
  let v = expression st in
  skip st;
  if not (eof st) then err "unexpected %C in expression" (peek st);
  num st v
