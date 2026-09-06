type config = {
  include_dirs : string list;
  system_dirs : string list;
  defines : (string * string option) list;
  undefines : string list;
  includes : string list;
  line_markers : bool;
}

let predefined_extras = [
  "-D__REDIRECT(name,proto,alias)=name proto __asm__(#alias)";
  "-D__REDIRECT_NTH(name,proto,alias)=name proto __asm__(#alias)";
  "-D__REDIRECT_NTHNL(name,proto,alias)=name proto __asm__(#alias)";
]

(* ---- Preprocessing tokens (6.4) -------------------------------------------- *)

type kind = Ident | Number | Char | String | Punct | Other | Newline | Eof

type ptok = {
  kind : kind;
  text : string; (* the spelling, kept for # and ## *)
  ws : bool; (* preceded by whitespace *)
  bol : bool; (* first token on its line *)
  loc : Loc.t;
  hide : string list; (* macros this token must not be expanded by (6.10.3.4p2) *)
}

let error loc fmt = Diag.error loc fmt

(* ---- Phases 1-2: characters, with trigraphs and line splicing -------------- *)

type cursor = {
  src : string;
  mutable pos : int;
  mutable line : int;
  mutable col : int;
  mutable file : string; (* as #line may change it *)
  mutable presumed_line : int; (* line number as #line has it *)
}

let eof = '\000'

(* The character at [pos + k], looking through trigraphs and backslash-newline. *)
let rec raw_peek c pos k : char * int =
  (* returns the character and the position just after it *)
  if pos >= String.length c.src then eof, pos
  else
    let ch = c.src.[pos] in
    let ch, next =
      if ch = '?' && pos + 2 < String.length c.src && c.src.[pos + 1] = '?' then
        (match c.src.[pos + 2] with
         | '=' -> '#', pos + 3 | '/' -> '\\', pos + 3 | '\'' -> '^', pos + 3 | '(' -> '[', pos + 3
         | ')' -> ']', pos + 3 | '!' -> '|', pos + 3 | '<' -> '{', pos + 3 | '>' -> '}', pos + 3
         | '-' -> '~', pos + 3 | _ -> ch, pos + 1)
      else ch, pos + 1 in
    if ch = '\\' && next < String.length c.src && (c.src.[next] = '\n' || (c.src.[next] = '\r' && next + 1 < String.length c.src && c.src.[next + 1] = '\n')) then begin
      (* line splice: skip it and read on *)
      let after = if c.src.[next] = '\n' then next + 1 else next + 2 in
      let ch', next' = raw_peek c after 0 in
      if k = 0 then ch', next' else raw_peek c next' (k - 1)
    end
    else if k = 0 then ch, next
    else raw_peek c next (k - 1)

let peek c k = fst (raw_peek c c.pos k)

let advance c =
  let ch, next = raw_peek c c.pos 0 in
  (* count newlines swallowed by splices *)
  for i = c.pos to next - 1 do if c.src.[i] = '\n' then (c.line <- c.line + 1; c.presumed_line <- c.presumed_line + 1; c.col <- 0) done;
  if ch = '\n' && next > 0 && c.src.[next - 1] <> '\n' then () ;
  c.col <- c.col + 1;
  c.pos <- next

let loc c = { Loc.file = c.file; line = c.presumed_line; col = c.col + 1 }

(* ---- Phase 3: tokens ---------------------------------------------------------- *)

let is_digit ch = '0' <= ch && ch <= '9'
let is_nondigit ch = ch = '_' || ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch = '$'
let is_ident ch = is_nondigit ch || is_digit ch || Char.code ch >= 0x80

(* Longest-match punctuators, digraphs spelled as themselves. *)
let punctuators = [
  "%:%:"; "..."; "<<="; ">>=";
  "->"; "++"; "--"; "<<"; ">>"; "<="; ">="; "=="; "!="; "&&"; "||"; "*="; "/="; "%="; "+="; "-=";
  "&="; "^="; "|="; "##"; "<:"; ":>"; "<%"; "%>"; "%:";
  "["; "]"; "("; ")"; "{"; "}"; "."; "&"; "*"; "+"; "-"; "~"; "!"; "/"; "%"; "<"; ">"; "^"; "|";
  "?"; ":"; ";"; "="; ","; "#" ]

(* Read one preprocessing token.  [in_include] makes <...> a header name. *)
let rec next_token c ~in_include ~at_bol : ptok =
  let ws = ref false in
  let bol = ref at_bol in
  let rec skip_ws () =
    match peek c 0 with
    | ' ' | '\t' | '\r' | '\012' | '\011' -> advance c; ws := true; skip_ws ()
    | '/' when peek c 1 = '*' ->
        let start = loc c in
        advance c; advance c;
        while not (peek c 0 = '*' && peek c 1 = '/') do
          if peek c 0 = eof then error start "unterminated comment";
          advance c
        done;
        advance c; advance c; ws := true; skip_ws ()
    | '/' when peek c 1 = '/' ->
        while peek c 0 <> '\n' && peek c 0 <> eof do advance c done; ws := true; skip_ws ()
    | _ -> () in
  skip_ws ();
  let start = loc c in
  let mk kind text = { kind; text; ws = !ws; bol = !bol; loc = start; hide = [] } in
  let b = Buffer.create 16 in
  let take () = Buffer.add_char b (peek c 0); advance c in
  match peek c 0 with
  | '\000' -> mk Eof ""
  | '\n' -> advance c; mk Newline "\n"
  | '<' when in_include ->
      take ();
      while peek c 0 <> '>' && peek c 0 <> '\n' && peek c 0 <> eof do take () done;
      if peek c 0 = '>' then take () else error start "missing terminating > character";
      mk Other (Buffer.contents b)
  | ch when is_digit ch || (ch = '.' && is_digit (peek c 1)) ->
      (* pp-number, 6.4.8 *)
      let continue = ref true in
      while !continue do
        let ch = peek c 0 in
        if is_digit ch || is_nondigit ch || ch = '.' then take ()
        else continue := false;
        if (ch = 'e' || ch = 'E' || ch = 'p' || ch = 'P') && (peek c 0 = '+' || peek c 0 = '-') then take ()
      done;
      mk Number (Buffer.contents b)
  | ('"' | '\'') as q -> string_like c b q start; mk (if q = '"' then String else Char) (Buffer.contents b)
  | ('L' | 'u' | 'U') as p when (peek c 1 = '"' || peek c 1 = '\'') || (p = 'u' && peek c 1 = '8' && peek c 2 = '"') ->
      take (); if peek c 0 = '8' then take ();
      let q = peek c 0 in
      string_like c b q start;
      mk (if q = '"' then String else Char) (Buffer.contents b)
  | ch when is_nondigit ch || Char.code ch >= 0x80 || (ch = '\\' && (peek c 1 = 'u' || peek c 1 = 'U')) ->
      let continue = ref true in
      while !continue do
        let ch = peek c 0 in
        if is_ident ch then take ()
        else if ch = '\\' && (peek c 1 = 'u' || peek c 1 = 'U') then (take (); take ())
        else continue := false
      done;
      mk Ident (Buffer.contents b)
  | _ ->
      let rec try_punct = function
        | [] -> None
        | p :: rest ->
            let n = String.length p in
            let rec matches i = i >= n || (peek c i = p.[i] && matches (i + 1)) in
            if matches 0 then Some p else try_punct rest in
      (match try_punct punctuators with
       | Some p ->
           for _ = 1 to String.length p do advance c done;
           (* digraphs mean their primary spelling (6.4.6p3) *)
           let p = match p with "<:" -> "[" | ":>" -> "]" | "<%" -> "{" | "%>" -> "}" | "%:" -> "#" | "%:%:" -> "##" | p -> p in
           mk Punct p
       | None -> take (); mk Other (Buffer.contents b))

and string_like c b q start =
  Buffer.add_char b (peek c 0); advance c;
  while peek c 0 <> q do
    (match peek c 0 with
     | '\n' | '\000' -> error start "missing terminating %c character" q
     | '\\' -> Buffer.add_char b '\\'; advance c
     | _ -> ());
    Buffer.add_char b (peek c 0); advance c
  done;
  Buffer.add_char b q; advance c

(* Tokenize a whole string, for macro bodies given on the command line and
   for re-lexing pasted tokens. *)
let tokens_of_string ?(file = "<command line>") s : ptok list =
  let c = { src = s; pos = 0; line = 1; col = 0; file; presumed_line = 1 } in
  let rec go acc =
    let t = next_token c ~in_include:false ~at_bol:false in
    match t.kind with Eof -> List.rev acc | Newline -> go acc | _ -> go (t :: acc) in
  go []

(* ---- Macros --------------------------------------------------------------------- *)

type macro =
  | Object of ptok list
  | Function of { params : string list; variadic : bool; body : ptok list }
  | Builtin (* __FILE__, __LINE__, ... expanded specially *)

type state = {
  cfg : config;
  macros : (string, macro) Hashtbl.t;
  once : (string, unit) Hashtbl.t; (* files marked #pragma once *)
  out : Buffer.t;
  mutable out_file : string;
  mutable out_line : int;
  mutable at_line_start : bool;
  mutable counter : int; (* __COUNTER__ *)
  mutable depth : int; (* include nesting *)
  main_file : string;
  mutable included : string list; (* user headers read, most recent first *)
  mutable last_text : string; (* the previous token on the output line *)
}

let same_tokens a b =
  List.length a = List.length b
  && List.for_all2 (fun x y -> x.text = y.text && x.kind = y.kind && (x.ws = y.ws)) a b

(* 6.10.3p2: a redefinition must be identical, spelling and whitespace. *)
let define st loc name (m : macro) =
  (match Hashtbl.find_opt st.macros name, m with
   | Some (Object a), Object b when same_tokens a b -> ()
   | Some (Function f), Function g when f.params = g.params && f.variadic = g.variadic && same_tokens f.body g.body -> ()
   | Some Builtin, _ -> error loc "redefining builtin macro \"%s\"" name
   | Some _, _ -> Diag.warning loc "\"%s\" redefined" name
   | None, _ -> ());
  Hashtbl.replace st.macros name m

(* Parse "NAME body" or "NAME(params) body" from the tokens of a #define line. *)
let parse_define st loc (toks : ptok list) =
  match toks with
  | { kind = Ident; text = name; _ } :: rest ->
      (match rest with
       | { kind = Punct; text = "("; ws = false; _ } :: rest ->
           let rec params acc = function
             | { kind = Punct; text = ")"; _ } :: body -> List.rev acc, false, body
             | { kind = Punct; text = "..."; _ } :: { kind = Punct; text = ")"; _ } :: body -> List.rev ("__VA_ARGS__" :: acc), true, body
             | { kind = Ident; text = p; _ } :: { kind = Punct; text = "..."; _ } :: { kind = Punct; text = ")"; _ } :: body ->
                 (* GNU named variadic parameter *)
                 List.rev (p :: acc), true, body
             | { kind = Ident; text = p; _ } :: { kind = Punct; text = ","; _ } :: rest when acc <> [] || true -> params (p :: acc) rest
             | { kind = Ident; text = p; _ } :: ({ kind = Punct; text = ")"; _ } :: _ as rest) -> params (p :: acc) rest
             | t :: _ -> error t.loc "expected parameter name or ')' in macro parameter list"
             | [] -> error loc "missing ')' in macro parameter list" in
           let params, variadic, body = params [] rest in
           (match body with
            | { kind = Punct; text = "##"; _ } :: _ -> error loc "'##' cannot appear at either end of a macro expansion"
            | _ -> ());
           (match List.rev body with
            | { kind = Punct; text = "##"; _ } :: _ -> error loc "'##' cannot appear at either end of a macro expansion"
            | _ -> ());
           define st loc name (Function { params; variadic; body })
       | body -> define st loc name (Object body))
  | t :: _ -> error t.loc "macro names must be identifiers"
  | [] -> error loc "no macro name given in #define directive"

(* ---- Token streams and expansion (6.10.3) ------------------------------------ *)

(* A stream yields tokens from pushed-back lists first, then from [source]. *)
type stream = { mutable pending : ptok list; source : unit -> ptok }

let next_tok s =
  match s.pending with
  | t :: rest -> s.pending <- rest; t
  | [] -> s.source ()

let push_back s toks = s.pending <- toks @ s.pending

let stream_of_list toks =
  let eof_tok = { kind = Eof; text = ""; ws = false; bol = false; loc = Loc.none; hide = [] } in
  { pending = toks; source = (fun () -> eof_tok) }

let is_punct t p = t.kind = Punct && t.text = p

(* The spelling of a token sequence, for # (6.10.3.2p2): tokens separated by
   one space where the source had whitespace, with double quotes and
   backslashes escaped inside string and character literals. *)
let stringize (toks : ptok list) : string =
  let b = Buffer.create 64 in
  Buffer.add_char b '"';
  List.iteri (fun i t ->
      if i > 0 && t.ws then Buffer.add_char b ' ';
      match t.kind with
      | String | Char -> String.iter (fun ch -> if ch = '"' || ch = '\\' then Buffer.add_char b '\\'; Buffer.add_char b ch) t.text
      | _ -> Buffer.add_string b t.text) toks;
  Buffer.add_char b '"';
  Buffer.contents b

(* ## (6.10.3.3): the two spellings joined must form one token. *)
let paste loc (a : ptok) (b : ptok) : ptok =
  let text = a.text ^ b.text in
  match tokens_of_string text with
  | [ t ] -> { t with ws = a.ws; bol = a.bol; loc = a.loc; hide = a.hide }
  | _ -> error loc "pasting \"%s\" and \"%s\" does not give a valid preprocessing token" a.text b.text

let builtin_expansion st (t : ptok) : ptok list =
  let mk kind text = [ { t with kind; text; hide = [ t.text ] } ] in
  match t.text with
  | "__FILE__" -> mk String (Printf.sprintf "%S" t.loc.file)
  | "__LINE__" -> mk Number (string_of_int t.loc.line)
  | "__COUNTER__" -> let n = st.counter in st.counter <- n + 1; mk Number (string_of_int n)
  | "__INCLUDE_LEVEL__" -> mk Number (string_of_int st.depth)
  | "__BASE_FILE__" -> mk String (Printf.sprintf "%S" st.main_file)
  | "__DATE__" -> mk String "\"Jan  1 1970\"" (* reproducible builds *)
  | "__TIME__" -> mk String "\"00:00:00\""
  | _ -> [ { t with hide = t.text :: t.hide } ] (* __has_builtin outside #if: left as is *)

(* Expand tokens from [s] until Eof, returning the expanded list.  [one]
   stops after the first token has been fully dealt with, for #if. *)
let rec expand_all st (s : stream) : ptok list =
  let out = ref [] in
  let rec loop () =
    let t = next_tok s in
    match t.kind with
    | Eof -> ()
    | Newline -> out := { t with kind = Newline } :: !out; loop ()
    | Ident when not (List.mem t.text t.hide) && Hashtbl.mem st.macros t.text ->
        (match Hashtbl.find st.macros t.text with
         | Builtin -> push_back s (builtin_expansion st t); loop ()
         | Object body ->
             let hs = t.text :: t.hide in
             push_back s (subst st t body [] [] hs);
             loop ()
         | Function { params; variadic; body } ->
             (* a function-like macro name not followed by ( is not an invocation *)
             let rec skip_nl acc =
               let n = next_tok s in
               if n.kind = Newline then skip_nl (n :: acc) else n, acc in
             let n, skipped = skip_nl [] in
             if is_punct n "(" then begin
               let args, rparen = collect_args st s t (List.length params) variadic in
               (* the result's hide set is (T.hs ∩ rparen.hs) ∪ {T} *)
               let hs = List.sort_uniq compare (t.text :: List.filter (fun h -> List.mem h rparen.hide) t.hide) in
               let result = subst st t body params args hs in
               (* the result takes the whitespace of the macro name *)
               let result = match result with r :: rest -> { r with ws = t.ws || r.ws; bol = t.bol } :: rest | [] -> [] in
               push_back s result;
               loop ()
             end else begin
               push_back s (List.rev (n :: skipped));
               out := t :: !out; loop ()
             end)
    | _ -> out := t :: !out; loop () in
  loop ();
  List.rev !out

(* Collect the actual arguments of a function-like macro; the ( has been
   read.  Returns the raw token lists and the closing parenthesis. *)
and collect_args st (s : stream) (name : ptok) nparams variadic : ptok list list * ptok =
  let args = ref [] and cur = ref [] and depth = ref 0 and nl = ref false in
  let finish_arg () = args := List.rev !cur :: !args; cur := [] in
  let add t = cur := { t with ws = t.ws || !nl } :: !cur; nl := false in
  let rec loop () =
    let t = next_tok s in
    match t.kind with
    | Eof -> error name.loc "unterminated argument list invoking macro \"%s\"" name.text
    | Newline -> nl := true; loop () (* newlines in arguments are whitespace *)
    | Punct when t.text = "(" -> incr depth; add t; loop ()
    | Punct when t.text = ")" ->
        if !depth = 0 then (finish_arg (); t) else (decr depth; add t; loop ())
    | Punct when t.text = "," && !depth = 0 && not (variadic && List.length !args = nparams - 1) ->
        finish_arg (); loop ()
    | _ -> add t; loop () in
  let rparen = loop () in
  let args = List.rev !args in
  (* f() is a call with one empty argument, which is also zero arguments *)
  let args = if nparams = 0 && args = [ [] ] then [] else args in
  let args =
    if variadic && List.length args = nparams - 1 then args @ [ [] ] (* missing variadic part *)
    else args in
  if List.length args <> nparams then
    error name.loc "macro \"%s\" requires %d arguments, but %d given" name.text nparams (List.length args);
  ignore st;
  args, rparen

(* Substitution (6.10.3.1-3): [is] is the body, [fp] the parameters, [ap]
   the raw actuals, [hs] the hide set for the result. *)
and subst st (name : ptok) (is : ptok list) (fp : string list) (ap : ptok list list) (hs : string list) : ptok list =
  let actual p = match List.assoc_opt p (List.combine fp ap) with Some a -> Some a | None -> None in
  (* 6.10.3.1p1: each argument is completely macro-replaced once, and that
     expansion is used wherever the parameter occurs unstringized and unpasted *)
  let expanded = Hashtbl.create 8 in
  let expand_actual p a =
    match Hashtbl.find_opt expanded p with
    | Some e -> e
    | None -> let e = expand_all st (stream_of_list a) in Hashtbl.replace expanded p e; e in
  let rec go is os =
    match is with
    | [] -> List.rev os
    | { kind = Punct; text = "#"; _ } :: ({ kind = Ident; text = p; _ } as pt) :: rest when actual p <> None ->
        let a = Option.get (actual p) in
        go rest ({ pt with kind = String; text = stringize a; hide = [] } :: os)
    | ({ kind = Punct; text = "##"; loc; _ }) :: ({ kind = Ident; text = p; _ }) :: rest when actual p <> None ->
        let a = Option.get (actual p) in
        (match a, os with
         | [], ({ kind = Punct; text = ","; _ } :: os') when p = "__VA_ARGS__" ->
             go rest os' (* GNU ", ## __VA_ARGS__" with no arguments: the comma goes too *)
         | [], _ -> go rest os (* placemarker: nothing to paste *)
         | _, ({ kind = Punct; text = ","; _ } :: _) when p = "__VA_ARGS__" ->
             (* GNU ", ## __VA_ARGS__" with arguments present: the comma stays, nothing is pasted *)
             go rest (List.rev_append a os)
         | first :: more, prev :: os' -> go rest (List.rev_append more (paste loc prev first :: os'))
         | first :: more, [] -> go rest (List.rev_append more [ first ]))
    | ({ kind = Punct; text = "##"; loc; _ }) :: t :: rest ->
        (match os with
         | prev :: os' -> go rest (paste loc prev t :: os')
         | [] -> go rest [ t ])
    | ({ kind = Ident; text = p; _ }) :: ({ kind = Punct; text = "##"; _ } :: _ as rest) when actual p <> None ->
        let a = Option.get (actual p) in
        (match a with
         | [] ->
             (* an empty argument before ##: also GNU's ", ## __VA_ARGS__" drops the comma *)
             (match rest, os with
              | { kind = Punct; text = "##"; _ } :: { kind = Ident; text = v; _ } :: rest', ({ kind = Punct; text = ","; _ } :: os')
                when v = p && p = "__VA_ARGS__" -> ignore rest'; go (List.tl rest) os'
              | { kind = Punct; text = "##"; _ } :: rest', _ -> go rest' os (* skip the ## as well *)
              | _ -> go rest os)
         | _ -> go rest (List.rev_append a os))
    | ({ kind = Ident; text = p; _ } as pt) :: rest when actual p <> None ->
        let a = expand_actual p (Option.get (actual p)) in
        let a = match a with first :: more -> { first with ws = pt.ws } :: more | [] -> [] in
        go rest (List.rev_append a os)
    | t :: rest -> go rest (t :: os) in
  let result = go is [] in
  (* expansion results are located at the invocation, for diagnostics and
     line markers alike *)
  List.map (fun t -> { t with hide = List.sort_uniq compare (hs @ t.hide); loc = name.loc }) result

(* ---- #if expressions (6.10.1) ---------------------------------------------- *)

(* Values are intmax_t or uintmax_t (6.10.1p4). *)
type value = { v : int64; unsigned : bool }

(* The builtins __has_builtin admits to: those doc/extensions.md lists. *)
let supported_builtins = [
  "__builtin_expect"; "__builtin_trap"; "__builtin_unreachable"; "__builtin_prefetch";
  "__builtin_return_address"; "__builtin_add_overflow"; "__builtin_sub_overflow"; "__builtin_mul_overflow";
  "__builtin_va_start"; "__builtin_va_arg"; "__builtin_va_end"; "__builtin_va_copy"; "__builtin_offsetof";
  "__builtin_setjmp"; "__builtin_longjmp" ]

let rec eval_if st loc (toks : ptok list) : bool =
  (* 1. 'defined' and __has_include before expansion *)
  let rec pre = function
    | { kind = Ident; text = "defined"; _ } as d :: rest ->
        let name, rest = match rest with
          | { kind = Punct; text = "("; _ } :: { kind = Ident; text = n; _ } :: { kind = Punct; text = ")"; _ } :: rest -> n, rest
          | { kind = Ident; text = n; _ } :: rest -> n, rest
          | _ -> error d.loc "operator \"defined\" requires an identifier" in
        { d with kind = Number; text = (if Hashtbl.mem st.macros name then "1" else "0"); hide = [] } :: pre rest
    | { kind = Ident; text = ("__has_include" | "__has_include_next"); _ } as d :: { kind = Punct; text = "("; _ } :: rest ->
        let rec upto_rparen acc = function
          | { kind = Punct; text = ")"; _ } :: rest -> List.rev acc, rest
          | t :: rest -> upto_rparen (t :: acc) rest
          | [] -> error d.loc "missing ')' after __has_include" in
        let inner, rest = upto_rparen [] rest in
        let found = match header_of_tokens st d.loc inner with
          | Some (name, angled) -> find_include st d.loc ~angled ~from:(Filename.dirname d.loc.file) name <> None
          | None -> false in
        { d with kind = Number; text = (if found then "1" else "0"); hide = [] } :: pre rest
    | { kind = Ident; text = ("__has_builtin" | "__has_attribute" | "__has_feature" | "__has_extension" | "__has_c_attribute") as q; _ } as d
      :: { kind = Punct; text = "("; _ } :: rest ->
        let rec upto_rparen acc depth = function
          | { kind = Punct; text = ")"; _ } :: rest when depth = 0 -> List.rev acc, rest
          | ({ kind = Punct; text = ")"; _ } as t) :: rest -> upto_rparen (t :: acc) (depth - 1) rest
          | ({ kind = Punct; text = "("; _ } as t) :: rest -> upto_rparen (t :: acc) (depth + 1) rest
          | t :: rest -> upto_rparen (t :: acc) depth rest
          | [] -> error d.loc "missing ')' after %s" q in
        let inner, rest = upto_rparen [] 0 rest in
        let yes = match q, inner with
          | "__has_builtin", [ { kind = Ident; text; _ } ] -> List.mem text supported_builtins
          | "__has_attribute", _ -> true (* every attribute is parsed (and most ignored), see doc/extensions.md *)
          | _ -> false in
        { d with kind = Number; text = (if yes then "1" else "0"); hide = [] } :: pre rest
    | t :: rest -> t :: pre rest
    | [] -> [] in
  let toks = expand_all st (stream_of_list (pre toks)) in
  (* __has_attribute and friends often arrive through a macro, e.g. glibc's
     __glibc_has_attribute, so look again after expansion *)
  let toks = pre toks in
  let toks = List.filter (fun t -> t.kind <> Newline) toks in
  (* 2. remaining identifiers are 0 (6.10.1p4) *)
  let toks = List.map (fun t -> if t.kind = Ident then { t with kind = Number; text = "0" } else t) toks in
  (* 3. evaluate *)
  let toks = ref toks in
  let peek () = match !toks with t :: _ -> Some t | [] -> None in
  let adv () = match !toks with _ :: rest -> toks := rest | [] -> () in
  let expect p = match peek () with Some t when is_punct t p -> adv () | _ -> error loc "expected '%s' in preprocessor expression" p in
  let int_of v = { v; unsigned = false } in
  let truth x = x.v <> 0L in
  let arith f g a b = (* signed/unsigned per 6.3.1.8 *)
    let u = a.unsigned || b.unsigned in
    { v = (if u then g a.v b.v else f a.v b.v); unsigned = u } in
  let cmp fs fu a b =
    let u = a.unsigned || b.unsigned in
    int_of (if (if u then fu a.v b.v else fs a.v b.v) then 1L else 0L) in
  let rec primary () : value =
    match peek () with
    | Some ({ kind = Number; text; _ }) -> adv (); number loc text
    | Some ({ kind = Char; text; _ }) -> adv (); char_const loc text
    | Some t when is_punct t "(" -> adv (); let v = cond () in expect ")"; v
    | Some t when is_punct t "-" -> adv (); let v = primary_unary () in { v with v = Int64.neg v.v }
    | Some t when is_punct t "+" -> adv (); primary_unary ()
    | Some t when is_punct t "~" -> adv (); let v = primary_unary () in { v with v = Int64.lognot v.v }
    | Some t when is_punct t "!" -> adv (); let v = primary_unary () in int_of (if truth v then 0L else 1L)
    | Some t -> error t.loc "token \"%s\" is not valid in preprocessor expressions" t.text
    | None -> error loc "#if with no expression"
  and primary_unary () = primary ()
  and binary prec : value =
    let table = [
      10, [ "*"; "/"; "%" ]; 9, [ "+"; "-" ]; 8, [ "<<"; ">>" ]; 7, [ "<"; ">"; "<="; ">=" ];
      6, [ "=="; "!=" ]; 5, [ "&" ]; 4, [ "^" ]; 3, [ "|" ]; 2, [ "&&" ]; 1, [ "||" ] ] in
    let rec loop lhs =
      match peek () with
      | Some ({ kind = Punct; text; _ } as t) ->
          (match List.find_opt (fun (p, ops) -> p >= prec && List.mem text ops) table with
           | Some (p, _) ->
               adv ();
               let rhs = binary (p + 1) in
               let r = match text with
                 | "*" -> arith Int64.mul Int64.mul lhs rhs
                 | "/" -> if rhs.v = 0L then error t.loc "division by zero in #if" else arith Int64.div Int64.unsigned_div lhs rhs
                 | "%" -> if rhs.v = 0L then error t.loc "division by zero in #if" else arith Int64.rem Int64.unsigned_rem lhs rhs
                 | "+" -> arith Int64.add Int64.add lhs rhs
                 | "-" -> arith Int64.sub Int64.sub lhs rhs
                 | "<<" -> { lhs with v = Int64.shift_left lhs.v (Int64.to_int rhs.v) }
                 | ">>" -> { lhs with v = (if lhs.unsigned then Int64.shift_right_logical else Int64.shift_right) lhs.v (Int64.to_int rhs.v) }
                 | "<" -> cmp (fun a b -> compare a b < 0) (fun a b -> Int64.unsigned_compare a b < 0) lhs rhs
                 | ">" -> cmp (fun a b -> compare a b > 0) (fun a b -> Int64.unsigned_compare a b > 0) lhs rhs
                 | "<=" -> cmp (fun a b -> compare a b <= 0) (fun a b -> Int64.unsigned_compare a b <= 0) lhs rhs
                 | ">=" -> cmp (fun a b -> compare a b >= 0) (fun a b -> Int64.unsigned_compare a b >= 0) lhs rhs
                 | "==" -> int_of (if lhs.v = rhs.v then 1L else 0L)
                 | "!=" -> int_of (if lhs.v <> rhs.v then 1L else 0L)
                 | "&" -> arith Int64.logand Int64.logand lhs rhs
                 | "^" -> arith Int64.logxor Int64.logxor lhs rhs
                 | "|" -> arith Int64.logor Int64.logor lhs rhs
                 | "&&" -> int_of (if truth lhs && truth rhs then 1L else 0L)
                 | "||" -> int_of (if truth lhs || truth rhs then 1L else 0L)
                 | _ -> assert false in
               loop r
           | None -> lhs)
      | _ -> lhs in
    loop (primary ())
  and cond () : value =
    let c = binary 1 in
    match peek () with
    | Some t when is_punct t "?" ->
        adv ();
        let a = cond () in
        expect ":";
        let b = cond () in
        if truth c then { a with unsigned = a.unsigned || b.unsigned } else { b with unsigned = a.unsigned || b.unsigned }
    | _ -> c in
  let v = cond () in
  (match peek () with Some t -> error t.loc "missing binary operator before token \"%s\"" t.text | None -> ());
  truth v

and number loc text : value =
  let n = String.length text in
  let i = ref n in
  while !i > 0 && (match text.[!i - 1] with 'u' | 'U' | 'l' | 'L' -> true | _ -> false) do decr i done;
  let digits = String.sub text 0 !i and suffix = String.sub text !i (n - !i) in
  let unsigned = String.contains suffix 'u' || String.contains suffix 'U' in
  let radix, digits =
    if String.length digits > 1 && digits.[0] = '0' && (digits.[1] = 'x' || digits.[1] = 'X') then 16, String.sub digits 2 (String.length digits - 2)
    else if String.length digits > 1 && digits.[0] = '0' then 8, digits
    else 10, digits in
  if digits = "" || String.exists (fun ch -> not (Lexer.hex_value ch < radix && (ch <> '.'))) digits then
    error loc "invalid integer constant \"%s\" in preprocessor expression" text;
  let v = ref 0L in
  String.iter (fun ch -> v := Int64.add (Int64.mul !v (Int64.of_int radix)) (Int64.of_int (Lexer.hex_value ch))) digits;
  (* a decimal constant too large for intmax_t is unsigned, as in 6.4.4.1 *)
  { v = !v; unsigned = unsigned || (radix <> 10 && Int64.compare !v 0L < 0) }

and char_const loc text : value =
  match Lexer.tokenize ~file:loc.Loc.file text with
  | [ { Token.tok = Token.Char_const { chars; enc }; _ }; _ ] ->
      let v = match chars with
        | [ c ] when enc = Token.Plain -> Int64.of_int (if c > 127 then c - 256 else c)
        | cs -> List.fold_left (fun acc c -> Int64.logor (Int64.shift_left acc 8) (Int64.of_int c)) 0L cs in
      { v; unsigned = false }
  | _ -> error loc "invalid character constant in preprocessor expression"

(* ---- #include ------------------------------------------------------------------- *)

(* The header name from the tokens after #include, after macro expansion
   if they are not a string or <...> already (6.10.2p4). *)
and header_of_tokens st loc (toks : ptok list) : (string * bool) option =
  let toks = List.filter (fun t -> t.kind <> Newline) toks in
  let from_tokens = function
    | [ { kind = String; text; _ } ] when text.[0] = '"' -> Some (String.sub text 1 (String.length text - 2), false)
    | [ { kind = Other; text; _ } ] when text.[0] = '<' -> Some (String.sub text 1 (String.length text - 2), true)
    | ({ kind = Punct; text = "<"; _ } :: rest) ->
        (* < tokens > assembled from spellings, 6.10.2p4 *)
        let rec upto acc = function
          | [ { kind = Punct; text = ">"; _ } ] -> Some (String.concat "" (List.rev acc), true)
          | t :: rest -> upto (((if t.ws && acc <> [] then " " else "") ^ t.text) :: acc) rest
          | [] -> None in
        upto [] rest
    | _ -> None in
  match from_tokens toks with
  | Some h -> Some h
  | None ->
      let expanded = List.filter (fun t -> t.kind <> Newline) (expand_all st (stream_of_list toks)) in
      (match from_tokens expanded with
       | Some h -> Some h
       | None -> error loc "#include expects \"FILENAME\" or <FILENAME>")

and find_include st _loc ~angled ~from name : string option =
  let candidates =
    (if Filename.is_relative name then [] else [ name ])
    @ (if angled then [] else [ Filename.concat from name ])
    @ List.map (fun d -> Filename.concat d name) (st.cfg.include_dirs @ st.cfg.system_dirs) in
  List.find_opt (fun f -> Sys.file_exists f && not (Sys.is_directory f)) candidates

(* ---- Output ------------------------------------------------------------------------ *)

let emit_marker st (loc : Loc.t) =
  if not st.at_line_start then Buffer.add_char st.out '\n';
  if st.cfg.line_markers then Buffer.add_string st.out (Printf.sprintf "# %d \"%s\"\n" loc.line loc.file);
  st.out_file <- loc.file; st.out_line <- loc.line; st.at_line_start <- true

(* Two adjacent tokens need a separating space if the source had one, or
   if their spellings would otherwise lex as a single token (as when macro
   expansion puts "+" next to "+"). *)
let would_paste (a : string) (b : string) =
  a <> "" && b <> "" &&
  (match tokens_of_string (a ^ b) with [ _ ] -> true | _ -> false)

let emit st (t : ptok) =
  if t.loc.file <> st.out_file || t.loc.line < st.out_line || t.loc.line > st.out_line + 8 then emit_marker st t.loc
  else while st.out_line < t.loc.line do Buffer.add_char st.out '\n'; st.out_line <- st.out_line + 1; st.at_line_start <- true done;
  if not st.at_line_start && (t.ws || would_paste st.last_text t.text) then Buffer.add_char st.out ' ';
  Buffer.add_string st.out t.text;
  st.last_text <- t.text;
  st.at_line_start <- false

(* ---- Directives and the main loop (6.10) -------------------------------------- *)

(* One #if group: [taken] is whether the current group is being emitted,
   [any_taken] whether some earlier group of this #if was, so that later
   #elif and #else groups are skipped (6.10.1p6). *)
type cond = { mutable taken : bool; mutable any_taken : bool; mutable seen_else : bool; was_skipping : bool }

let rec process_file st ~(file : string) ~(text : string) =
  let c = { src = text; pos = 0; line = 1; col = 0; file; presumed_line = 1 } in
  st.depth <- st.depth + 1;
  if st.depth > 200 then error (loc c) "#include nested too deeply";
  let conds : cond list ref = ref [] in
  let skipping () = match !conds with { taken = false; _ } :: _ -> true | _ -> false in
  let at_bol = ref true in
  let in_include = ref false in
  let source () =
    let t = next_token c ~in_include:!in_include ~at_bol:!at_bol in
    at_bol := (t.kind = Newline);
    t in
  let s = { pending = []; source } in
  (* read the rest of a directive line, raw *)
  let rest_of_line () =
    let rec go acc =
      let t = next_tok s in
      match t.kind with Newline | Eof -> List.rev acc | _ -> go (t :: acc) in
    go [] in
  let directive (hash : ptok) =
    let d = next_tok s in
    match d.kind with
    | Newline | Eof -> () (* the null directive *)
    | Number ->
        (* a line marker: # 12 "file" *)
        let rest = rest_of_line () in
        c.presumed_line <- int_of_string d.text - 1;
        (match rest with { kind = String; text; _ } :: _ -> c.file <- String.sub text 1 (String.length text - 2) | _ -> ())
    | Ident ->
        (match d.text with
         | "if" | "ifdef" | "ifndef" ->
             if skipping () then (ignore (rest_of_line ()); conds := { taken = false; any_taken = true; seen_else = false; was_skipping = true } :: !conds)
             else begin
               let toks = rest_of_line () in
               let v = match d.text with
                 | "if" -> eval_if st d.loc toks
                 | _ ->
                     (match toks with
                      | [ { kind = Ident; text; _ } ] -> Hashtbl.mem st.macros text = (d.text = "ifdef")
                      | _ -> error d.loc "#%s expects a single identifier" d.text) in
               conds := { taken = v; any_taken = v; seen_else = false; was_skipping = false } :: !conds
             end
         | "elif" ->
             (match !conds with
              | [] -> error d.loc "#elif without #if"
              | cd :: _ ->
                  if cd.seen_else then error d.loc "#elif after #else";
                  let toks = rest_of_line () in
                  if cd.was_skipping then ()
                  else if cd.any_taken then cd.taken <- false
                  else begin
                    cd.taken <- eval_if st d.loc toks;
                    cd.any_taken <- cd.taken
                  end)
         | "else" ->
             ignore (rest_of_line ());
             (match !conds with
              | [] -> error d.loc "#else without #if"
              | cd :: _ ->
                  if cd.seen_else then error d.loc "#else after #else";
                  cd.seen_else <- true;
                  if not cd.was_skipping then (cd.taken <- not cd.any_taken; cd.any_taken <- true))
         | "endif" ->
             ignore (rest_of_line ());
             (match !conds with [] -> error d.loc "#endif without #if" | _ :: rest -> conds := rest)
         | _ when skipping () -> ignore (rest_of_line ())
         | "define" -> parse_define st d.loc (rest_of_line ())
         | "undef" ->
             (match rest_of_line () with
              | [ { kind = Ident; text; _ } ] -> Hashtbl.remove st.macros text
              | _ -> error d.loc "#undef expects a single identifier")
         | "include" ->
             in_include := true;
             let toks = rest_of_line () in
             in_include := false;
             (match header_of_tokens st d.loc toks with
              | None -> assert false
              | Some (name, angled) ->
                  (match find_include st d.loc ~angled ~from:(Filename.dirname file) name with
                   | None -> error d.loc "%s: No such file or directory" name
                   | Some path -> include_file st path))
         | "line" ->
             let toks = expand_all st (stream_of_list (rest_of_line ())) in
             (match toks with
              | { kind = Number; text; _ } :: rest ->
                  c.presumed_line <- int_of_string text - 1;
                  (match rest with { kind = String; text; _ } :: _ -> c.file <- String.sub text 1 (String.length text - 2) | _ -> ())
              | _ -> error d.loc "#line expects a line number")
         | "error" -> error d.loc "#error %s" (String.concat " " (List.map (fun t -> t.text) (rest_of_line ())))
         | "warning" -> Diag.warning d.loc "#warning %s" (String.concat " " (List.map (fun t -> t.text) (rest_of_line ())))
         | "pragma" ->
             (match rest_of_line () with
              | [ { kind = Ident; text = "once"; _ } ] -> Hashtbl.replace st.once file ()
              | _ -> () (* other pragmas are not meaningful to this compiler *))
         | "ident" | "sccs" | "assert" | "unassert" -> ignore (rest_of_line ())
         | _ -> error d.loc "invalid preprocessing directive #%s" d.text)
    | _ -> if skipping () then ignore (rest_of_line ()) else error hash.loc "invalid preprocessing directive" in
  (* the main loop: directives at the start of a line, expansion elsewhere *)
  let rec loop () =
    let t = next_tok s in
    match t.kind with
    | Eof -> ()
    | Punct when t.text = "#" && t.bol -> directive t; loop ()
    | Newline -> loop ()
    | _ when skipping () -> loop ()
    | Ident when t.text = "_Pragma" ->
        (* _Pragma ( string-literal ) is a #pragma, which we ignore *)
        let rec upto_rparen () = let u = next_tok s in if not (is_punct u ")") && u.kind <> Eof then upto_rparen () in
        upto_rparen (); loop ()
    | _ ->
        push_back s [ t ];
        let toks = expand_one st s in
        List.iter (fun t -> if t.kind <> Newline then emit st t) toks;
        loop () in
  loop ();
  if !conds <> [] then error (loc c) "unterminated #if";
  st.depth <- st.depth - 1

(* Expand whatever begins at the head of the stream, one token's worth. *)
and expand_one st (s : stream) : ptok list =
  (* run the expander on a stream that yields one source token then pretends
     to end, except that function-like invocations may read further *)
  let taken = ref false in
  let sub = { pending = []; source = (fun () ->
      if !taken then { kind = Eof; text = ""; ws = false; bol = false; loc = Loc.none; hide = [] }
      else (taken := true; next_tok s)) } in
  let first = next_tok sub in
  match first.kind with
  | Ident when Hashtbl.mem st.macros first.text && not (List.mem first.text first.hide) ->
      (* let the full expander see the invocation, reading arguments from [s] *)
      let sub = { pending = [ first ]; source = (fun () -> next_tok s) } in
      let result = expand_first st sub in
      s.pending <- sub.pending @ s.pending;
      result
  | _ -> [ first ]

(* Like [expand_all] but stops once the first non-macro token is produced,
   leaving the rest pending on the stream. *)
and expand_first st (s : stream) : ptok list =
  let t = next_tok s in
  match t.kind with
  | Eof -> []
  | Ident when not (List.mem t.text t.hide) && Hashtbl.mem st.macros t.text ->
      (match Hashtbl.find st.macros t.text with
       | Builtin -> push_back s (builtin_expansion st t); expand_first st s
       | Object body ->
           push_back s (subst st t body [] [] (t.text :: t.hide));
           expand_first st s
       | Function { params; variadic; body } ->
           let rec skip_nl acc =
             let n = next_tok s in
             if n.kind = Newline then skip_nl (n :: acc) else n, acc in
           let n, skipped = skip_nl [] in
           if is_punct n "(" then begin
             let args, rparen = collect_args st s t (List.length params) variadic in
             let hs = List.sort_uniq compare (t.text :: List.filter (fun h -> List.mem h rparen.hide) t.hide) in
             let result = subst st t body params args hs in
             let result = match result with r :: rest -> { r with ws = t.ws || r.ws; bol = t.bol } :: rest | [] -> [] in
             push_back s result;
             expand_first st s
           end else begin
             push_back s (List.rev (n :: skipped));
             [ t ]
           end)
  | _ -> [ t ]

and include_file st path =
  if Hashtbl.mem st.once path then ()
  else begin
    let text = In_channel.with_open_bin path In_channel.input_all in
    if not (List.exists (fun d -> String.length path > String.length d && String.sub path 0 (String.length d) = d) st.cfg.system_dirs)
       && not (List.mem path st.included) then st.included <- path :: st.included;
    process_file st ~file:path ~text
  end

(* ---- Predefined macros ------------------------------------------------------------ *)

let predefined = [
  "__STDC__ 1"; "__STDC_VERSION__ 201112L"; "__STDC_HOSTED__ 1";
  "__STDC_UTF_16__ 1"; "__STDC_UTF_32__ 1";
  "__x86_64__ 1"; "__x86_64 1"; "__amd64__ 1"; "__amd64 1";
  "__linux__ 1"; "__linux 1"; "__gnu_linux__ 1"; "__unix__ 1"; "__unix 1"; "__ELF__ 1";
  "__LP64__ 1"; "_LP64 1"; "__CHAR_BIT__ 8";
  "__SIZEOF_SHORT__ 2"; "__SIZEOF_INT__ 4"; "__SIZEOF_LONG__ 8"; "__SIZEOF_LONG_LONG__ 8";
  "__SIZEOF_POINTER__ 8"; "__SIZEOF_FLOAT__ 4"; "__SIZEOF_DOUBLE__ 8"; "__SIZEOF_LONG_DOUBLE__ 16";
  "__SIZEOF_SIZE_T__ 8"; "__SIZEOF_WCHAR_T__ 4"; "__SIZEOF_WINT_T__ 4"; "__SIZEOF_PTRDIFF_T__ 8";
  "__SIZE_TYPE__ long unsigned int"; "__PTRDIFF_TYPE__ long int"; "__WCHAR_TYPE__ int"; "__WINT_TYPE__ unsigned int";
  "__INTMAX_TYPE__ long int"; "__UINTMAX_TYPE__ long unsigned int"; "__CHAR16_TYPE__ short unsigned int";
  "__CHAR32_TYPE__ unsigned int"; "__INTPTR_TYPE__ long int"; "__UINTPTR_TYPE__ long unsigned int";
  "__SCHAR_MAX__ 0x7f"; "__SHRT_MAX__ 0x7fff"; "__INT_MAX__ 0x7fffffff"; "__LONG_MAX__ 0x7fffffffffffffffL";
  "__LONG_LONG_MAX__ 0x7fffffffffffffffLL"; "__WCHAR_MAX__ 0x7fffffff"; "__WCHAR_MIN__ (-__WCHAR_MAX__ - 1)";
  "__INTMAX_MAX__ 0x7fffffffffffffffL"; "__UINTMAX_MAX__ 0xffffffffffffffffUL"; "__SIZE_MAX__ 0xffffffffffffffffUL";
  "__PTRDIFF_MAX__ 0x7fffffffffffffffL"; "__INTPTR_MAX__ 0x7fffffffffffffffL"; "__UINTPTR_MAX__ 0xffffffffffffffffUL";
  "__ORDER_LITTLE_ENDIAN__ 1234"; "__ORDER_BIG_ENDIAN__ 4321"; "__ORDER_PDP_ENDIAN__ 3412";
  "__BYTE_ORDER__ __ORDER_LITTLE_ENDIAN__"; "__BIGGEST_ALIGNMENT__ 16"; "__USER_LABEL_PREFIX__ ";
  "__OCC__ 1"; "__occ__ 1"; "unix 1"; "linux 1";
]

let builtin_names = [ "__FILE__"; "__LINE__"; "__COUNTER__"; "__INCLUDE_LEVEL__"; "__BASE_FILE__"; "__DATE__"; "__TIME__";
                      (* these are defined so that "#ifdef __has_builtin" is true; #if handles them *)
                      "__has_builtin"; "__has_attribute"; "__has_feature"; "__has_extension"; "__has_c_attribute"; "__has_include" ]

let run cfg file =
  let st = { cfg; macros = Hashtbl.create 512; once = Hashtbl.create 16; out = Buffer.create 65536;
             out_file = ""; out_line = 0; at_line_start = true; counter = 0; depth = 0; main_file = file; included = []; last_text = "" } in
  List.iter (fun n -> Hashtbl.replace st.macros n Builtin) builtin_names;
  let define_line line = parse_define st { Loc.file = "<built-in>"; line = 1; col = 1 } (tokens_of_string line) in
  List.iter define_line predefined;
  List.iter (fun (name, value) ->
      (* -D NAME=VALUE, with NAME possibly "F(a,b)"; -D NAME means 1 *)
      define_line (name ^ " " ^ Option.value value ~default:"1")) cfg.defines;
  List.iter (Hashtbl.remove st.macros) cfg.undefines;
  (* glibc's feature test macros live in stdc-predef.h, which gcc includes
     implicitly; so do we when it is found on the system path *)
  (match find_include st Loc.none ~angled:true ~from:"" "stdc-predef.h" with
   | Some p -> include_file st p
   | None -> ());
  List.iter (fun f ->
      match find_include st Loc.none ~angled:false ~from:(Sys.getcwd ()) f with
      | Some p -> include_file st p
      | None -> error Loc.none "%s: No such file or directory" f) cfg.includes;
  let text = In_channel.with_open_bin file In_channel.input_all in
  process_file st ~file ~text;
  if not st.at_line_start then Buffer.add_char st.out '\n';
  Buffer.contents st.out, List.rev st.included
