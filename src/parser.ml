open Token
open Syntax

(* ---- Token stream ---------------------------------------------------------- *)

type state = {
  toks : loc_token array;
  mutable pos : int;
  (* Scopes, innermost first.  Each maps an identifier to whether it
     currently names a type (6.7.8).  An ordinary declaration of the same
     name in an inner scope hides a typedef name (6.2.1p4). *)
  mutable scopes : (string, bool) Hashtbl.t list;
}

let peek st = st.toks.(st.pos).tok
let peek2 st = if st.pos + 1 < Array.length st.toks then st.toks.(st.pos + 1).tok else Eof
let loc st = st.toks.(st.pos).loc
let advance st = if st.pos < Array.length st.toks - 1 then st.pos <- st.pos + 1

let describe = function
  | Keyword k -> "'" ^ Token.keyword_to_string k ^ "'"
  | Ident s -> "identifier '" ^ s ^ "'"
  | Int_const _ | Float_const _ | Char_const _ -> "constant"
  | String_lit _ -> "string literal"
  | Punct p -> "'" ^ Token.punct_to_string p ^ "'"
  | Eof -> "end of file"

let error st fmt = Diag.error (loc st) fmt

let expect st tok =
  if peek st = tok then advance st
  else error st "expected %s before %s" (describe tok) (describe (peek st))

let accept st tok = if peek st = tok then (advance st; true) else false
let punct st p = accept st (Punct p)
let expect_punct st p = expect st (Punct p)
let keyword st k = accept st (Keyword k)

let expect_ident st =
  match peek st with
  | Ident s -> advance st; s
  | t -> error st "expected identifier before %s" (describe t)

(* ---- Scopes ------------------------------------------------------------------ *)

let push_scope st = st.scopes <- Hashtbl.create 16 :: st.scopes
let pop_scope st = st.scopes <- List.tl st.scopes

let declare st name ~is_type =
  match st.scopes with
  | s :: _ -> Hashtbl.replace s name is_type
  | [] -> assert false

let is_typedef_name st name =
  let rec look = function
    | [] -> false
    | s :: rest -> (match Hashtbl.find_opt s name with Some b -> b | None -> look rest) in
  look st.scopes

(* Does the token start a declaration specifier (6.7)?  This is the test
   behind every declaration-or-expression ambiguity in the grammar. *)
let starts_type_specifier st = function
  | Keyword (Void | Char | Short | Int | Long | Float | Double | Signed | Unsigned
            | Bool | Complex | Struct | Union | Enum | Atomic | Typeof) -> true
  | Ident s -> is_typedef_name st s
  | _ -> false

let starts_declaration_specifier st = function
  | Keyword (Typedef | Extern | Static | Thread_local | Auto | Register
            | Const | Restrict | Volatile | Inline | Noreturn | Alignas | Attribute) -> true
  | t -> starts_type_specifier st t

(* ---- Attributes and asm labels (extensions, doc/extensions.md) ------------ *)

(* __attribute__((name(args), ...)); arguments are kept as expressions. *)
let rec attributes st =
  if keyword st Attribute then begin
    expect_punct st LParen; expect_punct st LParen;
    let rec list acc =
      match peek st with
      | Punct RParen -> List.rev acc
      | Ident _ | Keyword _ ->
          (* attribute names may be keywords: __attribute__((const)) *)
          let aname = match peek st with Ident s -> s | Keyword k -> Token.keyword_to_string k | _ -> assert false in
          advance st;
          let aargs =
            if punct st LParen then begin
              let args = if peek st = Punct RParen then [] else argument_list st in
              expect_punct st RParen; args
            end else [] in
          let acc = { aname; aargs } :: acc in
          if punct st Comma then list acc else List.rev acc
      | Punct Comma -> advance st; list acc
      | t -> error st "unexpected %s in attribute list" (describe t) in
    let attrs = list [] in
    expect_punct st RParen; expect_punct st RParen;
    attrs @ attributes st
  end else []

(* ---- Expressions (6.5) ------------------------------------------------------ *)

and primary st : expr =
  let l = loc st in
  let node e = { e; eloc = l } in
  match peek st with
  | Ident "__builtin_va_arg" ->
      advance st; expect_punct st LParen;
      let ap = assignment st in
      expect_punct st Comma;
      let t = type_name st in
      expect_punct st RParen;
      node (Va_arg (ap, t))
  | Ident "__builtin_offsetof" ->
      advance st; expect_punct st LParen;
      let t = type_name st in
      expect_punct st Comma;
      let rec designators acc =
        match peek st with
        | Ident f -> advance st; designators (Field f :: acc)
        | Punct Dot -> advance st; designators acc
        | Punct LBracket -> advance st; let e = expression st in expect_punct st RBracket; designators (Subscript e :: acc)
        | _ -> List.rev acc in
      let d = designators [] in
      expect_punct st RParen;
      node (Offsetof (t, d))
  | Ident s -> advance st; node (Ident s)
  | (Int_const _ | Float_const _ | Char_const _) as c -> advance st; node (Const c)
  | String_lit { bytes; enc } -> advance st; node (String (bytes, enc))
  | Punct LParen ->
      advance st;
      let e = expression st in
      expect_punct st RParen;
      e
  | Keyword Generic ->
      (* 6.5.1.1 *)
      advance st; expect_punct st LParen;
      let ctrl = assignment st in
      let rec assocs acc =
        if punct st Comma then begin
          let t = if keyword st Default then None else Some (type_name st) in
          expect_punct st Colon;
          let e = assignment st in
          assocs ((t, e) :: acc)
        end else List.rev acc in
      let a = assocs [] in
      expect_punct st RParen;
      node (Generic (ctrl, a))
  | t -> error st "expected expression before %s" (describe t)

and argument_list st =
  let rec go acc =
    let e = assignment st in
    if punct st Comma then go (e :: acc) else List.rev (e :: acc) in
  go []

and postfix st (e : expr) : expr =
  let l = loc st in
  let node d = { e = d; eloc = l } in
  match peek st with
  | Punct LBracket ->
      advance st;
      let i = expression st in
      expect_punct st RBracket;
      postfix st (node (Index (e, i)))
  | Punct LParen ->
      advance st;
      let args = if peek st = Punct RParen then [] else argument_list st in
      expect_punct st RParen;
      postfix st (node (Call (e, args)))
  | Punct Dot -> advance st; let f = expect_ident st in postfix st (node (Member (e, f)))
  | Punct Arrow -> advance st; let f = expect_ident st in postfix st (node (Arrow (e, f)))
  | Punct PlusPlus -> advance st; postfix st (node (Unop (Postinc, e)))
  | Punct MinusMinus -> advance st; postfix st (node (Unop (Postdec, e)))
  | _ -> e

(* A '(' begins a cast or compound literal when a type name follows. *)
and paren_starts_type st =
  peek st = Punct LParen && starts_type_specifier st (peek2 st)
  || peek st = Punct LParen
     && (match peek2 st with Keyword (Const | Volatile | Restrict | Attribute) -> true | _ -> false)

and unary st : expr =
  let l = loc st in
  let node d = { e = d; eloc = l } in
  match peek st with
  | Punct PlusPlus -> advance st; node (Unop (Preinc, unary st))
  | Punct MinusMinus -> advance st; node (Unop (Predec, unary st))
  | Punct Amp -> advance st; node (Unop (Addr, cast st))
  | Punct Star -> advance st; node (Unop (Deref, cast st))
  | Punct Plus -> advance st; cast st
  | Punct Minus -> advance st; node (Unop (Neg, cast st))
  | Punct Tilde -> advance st; node (Unop (Not, cast st))
  | Punct Bang -> advance st; node (Unop (Lnot, cast st))
  | Keyword Sizeof ->
      advance st;
      if paren_starts_type st then begin
        advance st;
        let t = type_name st in
        expect_punct st RParen;
        (* sizeof (T){...} is sizeof a compound literal, not of T *)
        if peek st = Punct LBrace then node (Sizeof_expr (postfix st (node (Compound_literal (t, initialiser st)))))
        else node (Sizeof_type t)
      end else node (Sizeof_expr (unary st))
  | Keyword Alignof ->
      advance st; expect_punct st LParen;
      let t = type_name st in
      expect_punct st RParen;
      node (Alignof t)
  | _ -> postfix st (primary st)

and cast st : expr =
  let l = loc st in
  if paren_starts_type st then begin
    advance st;
    let t = type_name st in
    expect_punct st RParen;
    if peek st = Punct LBrace then postfix st { e = Compound_literal (t, initialiser st); eloc = l } (* 6.5.2.5 *)
    else { e = Cast (t, cast st); eloc = l }
  end else unary st

(* 6.5.5 to 6.5.14 as one precedence-climbing loop.  Higher binds tighter. *)
and binop_of_token = function
  | Punct Star -> Some (Mul, 10) | Punct Slash -> Some (Div, 10) | Punct Percent -> Some (Mod, 10)
  | Punct Plus -> Some (Add, 9) | Punct Minus -> Some (Sub, 9)
  | Punct LShift -> Some (Shl, 8) | Punct RShift -> Some (Shr, 8)
  | Punct Lt -> Some (Lt, 7) | Punct Gt -> Some (Gt, 7) | Punct Le -> Some (Le, 7) | Punct Ge -> Some (Ge, 7)
  | Punct EqEq -> Some (Eq, 6) | Punct BangEq -> Some (Ne, 6)
  | Punct Amp -> Some (Band, 5) | Punct Caret -> Some (Bxor, 4) | Punct Bar -> Some (Bor, 3)
  | Punct AmpAmp -> Some (Land, 2) | Punct BarBar -> Some (Lor, 1)
  | _ -> None

and binary st min_prec : expr =
  let rec loop lhs =
    match binop_of_token (peek st) with
    | Some (op, prec) when prec >= min_prec ->
        let l = loc st in
        advance st;
        let rhs = binary st (prec + 1) in
        loop { e = Binop (op, lhs, rhs); eloc = l }
    | _ -> lhs in
  loop (cast st)

and conditional st : expr =
  let c = binary st 1 in
  if peek st = Punct Question then begin
    let l = loc st in
    advance st;
    let a = expression st in
    expect_punct st Colon;
    let b = conditional st in
    { e = Cond (c, a, b); eloc = l }
  end else c

and assignment_op = function
  | Punct Eq -> Some None | Punct StarEq -> Some (Some Mul) | Punct SlashEq -> Some (Some Div)
  | Punct PercentEq -> Some (Some Mod) | Punct PlusEq -> Some (Some Add) | Punct MinusEq -> Some (Some Sub)
  | Punct LShiftEq -> Some (Some Shl) | Punct RShiftEq -> Some (Some Shr) | Punct AmpEq -> Some (Some Band)
  | Punct CaretEq -> Some (Some Bxor) | Punct BarEq -> Some (Some Bor)
  | _ -> None

(* 6.5.16: the left operand is a unary-expression, but parsing it as a
   conditional-expression and letting elaboration reject non-lvalues gives
   better error messages, and is what every C compiler does. *)
and assignment st : expr =
  let lhs = conditional st in
  match assignment_op (peek st) with
  | Some op ->
      let l = loc st in
      advance st;
      let rhs = assignment st in
      { e = Assign (op, lhs, rhs); eloc = l }
  | None -> lhs

and expression st : expr =
  let e = assignment st in
  let rec loop lhs =
    if peek st = Punct Comma then begin
      let l = loc st in
      advance st;
      loop { e = Binop (Comma, lhs, assignment st); eloc = l }
    end else lhs in
  loop e

and constant_expression st = conditional st

(* ---- Declarations (6.7) ----------------------------------------------------- *)

and empty_specifiers = { storage = None; thread_local = false; type_specs = []; quals = []; funcs = []; align = None; attrs = [] }

(* declaration-specifiers and specifier-qualifier-list share this; the
   latter simply never sees storage classes.  A typedef name counts as a
   type specifier only while no other type specifier has been seen, which
   is what lets "typedef int T; { int T; }" redeclare T (6.7.8p2). *)
and specifiers st : specifiers =
  let single sp = advance st; Some sp in
  let rec loop sp =
    let sp' =
      match peek st with
      | Keyword (Typedef | Extern | Static | Auto | Register) when sp.storage <> None ->
          error st "multiple storage classes in declaration specifiers"
      | Keyword Typedef -> single { sp with storage = Some Typedef }
      | Keyword Extern -> single { sp with storage = Some Extern }
      | Keyword Static -> single { sp with storage = Some Static }
      | Keyword Thread_local -> single { sp with thread_local = true }
      | Keyword Auto -> single { sp with storage = Some Auto }
      | Keyword Register -> single { sp with storage = Some Register }
      | Keyword Const -> single { sp with quals = Q_const :: sp.quals }
      | Keyword Restrict -> single { sp with quals = Q_restrict :: sp.quals }
      | Keyword Volatile -> single { sp with quals = Q_volatile :: sp.quals }
      | Keyword Inline -> single { sp with funcs = Fs_inline :: sp.funcs }
      | Keyword Noreturn -> single { sp with funcs = Fs_noreturn :: sp.funcs }
      | Keyword Void -> single { sp with type_specs = Ts_void :: sp.type_specs }
      | Keyword Char -> single { sp with type_specs = Ts_char :: sp.type_specs }
      | Keyword Short -> single { sp with type_specs = Ts_short :: sp.type_specs }
      | Keyword Int -> single { sp with type_specs = Ts_int :: sp.type_specs }
      | Keyword Long -> single { sp with type_specs = Ts_long :: sp.type_specs }
      | Keyword Float -> single { sp with type_specs = Ts_float :: sp.type_specs }
      | Keyword Double -> single { sp with type_specs = Ts_double :: sp.type_specs }
      | Keyword Signed -> single { sp with type_specs = Ts_signed :: sp.type_specs }
      | Keyword Unsigned -> single { sp with type_specs = Ts_unsigned :: sp.type_specs }
      | Keyword Bool -> single { sp with type_specs = Ts_bool :: sp.type_specs }
      | Keyword Complex -> single { sp with type_specs = Ts_complex :: sp.type_specs }
      | Keyword Atomic when peek2 st = Punct LParen ->
          (* 6.7.2.4: _Atomic ( type-name ) is a type specifier *)
          advance st; advance st;
          let t = type_name st in
          expect_punct st RParen;
          Some { sp with type_specs = Ts_atomic t :: sp.type_specs }
      | Keyword Atomic -> single { sp with quals = Q_atomic :: sp.quals }
      | Keyword Alignas ->
          advance st; expect_punct st LParen;
          let a = if paren_starts_type_inner st then Align_type (type_name st) else Align_expr (constant_expression st) in
          expect_punct st RParen;
          Some { sp with align = Some a }
      | Keyword Attribute ->
          let attrs = attributes st in
          Some { sp with attrs = sp.attrs @ attrs }
      | Keyword Struct -> advance st; let s = struct_specifier st in Some { sp with type_specs = Ts_struct s :: sp.type_specs }
      | Keyword Union -> advance st; let s = struct_specifier st in Some { sp with type_specs = Ts_union s :: sp.type_specs }
      | Keyword Enum -> advance st; let s = enum_specifier st in Some { sp with type_specs = Ts_enum s :: sp.type_specs }
      | Keyword Typeof ->
          advance st; expect_punct st LParen;
          let e = expression st in
          expect_punct st RParen;
          Some { sp with type_specs = Ts_typeof e :: sp.type_specs }
      | Ident s when sp.type_specs = [] && is_typedef_name st s ->
          single { sp with type_specs = [ Ts_typedef_name s ] }
      | _ -> None in
    match sp' with
    | Some sp -> loop sp
    | None -> sp in
  let sp = loop empty_specifiers in
  { sp with type_specs = List.rev sp.type_specs; quals = List.rev sp.quals }

(* Inside "_Alignas(" the token itself, not a '(' , must start a type. *)
and paren_starts_type_inner st = starts_type_specifier st (peek st)
  || (match peek st with Keyword (Const | Volatile | Restrict) -> true | _ -> false)

(* 6.7.2.1; the 'struct' or 'union' keyword has been consumed. *)
and struct_specifier st : struct_spec =
  let _attrs = attributes st in
  let tag = match peek st with Ident s -> advance st; Some s | _ -> None in
  if punct st LBrace then begin
    let rec members acc =
      match peek st with
      | Punct RBrace -> advance st; List.rev acc
      | Keyword Static_assert -> let _ = static_assert st in members acc
      | Punct Semi -> advance st; members acc (* stray ';' is a common extension *)
      | _ ->
          let mspecs = specifiers st in
          if mspecs.type_specs = [] && mspecs.quals = [] then
            error st "expected specifier-qualifier-list before %s" (describe (peek st));
          let rec decls acc =
            (* anonymous struct/union member (6.7.2.1p13) or a bit-field ": w" *)
            let d = if peek st = Punct Colon || peek st = Punct Semi then None else Some (declarator st ~abstract:false) in
            let w = if punct st Colon then Some (constant_expression st) else None in
            let acc = (d, w) :: acc in
            if punct st Comma then decls acc else List.rev acc in
          let mdecls = decls [] in
          expect_punct st Semi;
          members ({ mspecs; mdecls } :: acc) in
    let m = members [] in
    let _ = attributes st in
    { tag; members = Some m }
  end else begin
    if tag = None then error st "expected identifier or '{' before %s" (describe (peek st));
    { tag; members = None }
  end

(* 6.7.2.2; the 'enum' keyword has been consumed. *)
and enum_specifier st : enum_spec =
  let _ = attributes st in
  let etag = match peek st with Ident s -> advance st; Some s | _ -> None in
  if punct st LBrace then begin
    let rec enumerators acc =
      match peek st with
      | Punct RBrace -> advance st; List.rev acc
      | Ident name ->
          advance st;
          let _ = attributes st in
          declare st name ~is_type:false;
          let v = if punct st Eq then Some (constant_expression st) else None in
          let acc = (name, v) :: acc in
          if punct st Comma then enumerators acc
          else (expect_punct st RBrace; List.rev acc)
      | t -> error st "expected identifier before %s" (describe t) in
    let e = enumerators [] in
    let _ = attributes st in
    { etag; enumerators = Some e }
  end else begin
    if etag = None then error st "expected identifier or '{' before %s" (describe (peek st));
    { etag; enumerators = None }
  end

and type_qualifiers st =
  let rec loop acc =
    match peek st with
    | Keyword Const -> advance st; loop (Q_const :: acc)
    | Keyword Restrict -> advance st; loop (Q_restrict :: acc)
    | Keyword Volatile -> advance st; loop (Q_volatile :: acc)
    | Keyword Atomic -> advance st; loop (Q_atomic :: acc)
    | Keyword Attribute -> let _ = attributes st in loop acc
    | _ -> List.rev acc in
  loop []

(* 6.7.6 declarator and 6.7.7 abstract-declarator, in one function: with
   [abstract] the identifier may be omitted.  The result nests inside-out:
   "*x[3]" is [D_pointer ([], D_array (D_ident x, ...))], read as "pointer
   to (array of (x))" and inverted by [Elab] into "x is array of pointer". *)
and declarator st ~abstract : declarator =
  let l = loc st in
  if punct st Star then begin
    let q = type_qualifiers st in
    let inner = declarator st ~abstract in
    { d = D_pointer (q, inner); dloc = l }
  end else
    let base =
      match peek st with
      | Ident s -> advance st; { d = D_ident s; dloc = l }
      | Punct LParen when starts_nested_declarator st ->
          advance st;
          let inner = declarator st ~abstract in
          expect_punct st RParen;
          inner
      | _ when abstract -> { d = D_abstract; dloc = l }
      | t -> error st "expected identifier or '(' before %s" (describe t) in
    declarator_suffixes st base

(* After '(' in a declarator: a nested declarator, or a parameter list?
   6.7.6.3p11: an identifier that could be either a typedef name or a
   parameter name is a typedef name, hence a parameter list. *)
and starts_nested_declarator st =
  match peek2 st with
  | Punct (Star | LParen | LBracket) -> true
  | Ident s -> not (is_typedef_name st s)
  | Keyword Attribute -> true
  | _ -> false

and declarator_suffixes st (d : declarator) : declarator =
  let l = loc st in
  match peek st with
  | Punct LBracket ->
      advance st;
      let static1 = keyword st Static in
      let q = type_qualifiers st in
      let static2 = keyword st Static in
      let size =
        if peek st = Punct RBracket then None
        else if peek st = Punct Star && peek2 st = Punct RBracket then (advance st; None) (* [*], 6.7.6.2 *)
        else Some (assignment st) in
      expect_punct st RBracket;
      declarator_suffixes st { d = D_array (d, q, size, static1 || static2); dloc = l }
  | Punct LParen ->
      advance st;
      let d' =
        match peek st with
        | Punct RParen -> { d = D_func (d, [], false); dloc = l } (* unspecified parameters *)
        | Ident s when not (is_typedef_name st s) ->
            (* K&R identifier list *)
            let rec idents acc =
              let name = expect_ident st in
              if punct st Comma then idents (name :: acc) else List.rev (name :: acc) in
            { d = D_ident_list (d, idents []); dloc = l }
        | _ ->
            push_scope st; (* function prototype scope, 6.2.1p4 *)
            let params, variadic = parameter_list st in
            pop_scope st;
            { d = D_func (d, params, variadic); dloc = l } in
      expect_punct st RParen;
      declarator_suffixes st d'
  | Keyword Attribute ->
      let a = attributes st in
      declarator_suffixes st { d = D_attr (d, a); dloc = l }
  | Keyword Asm ->
      advance st; expect_punct st LParen;
      let label = match peek st with String_lit { bytes; _ } -> advance st; bytes | t -> error st "expected string literal before %s" (describe t) in
      expect_punct st RParen;
      declarator_suffixes st { d = D_asm_label (d, label); dloc = l }
  | _ -> d

(* 6.7.6.3 parameter-type-list, up to but excluding the closing ')'. *)
and parameter_list st : param_decl list * bool =
  let rec loop acc =
    if punct st Ellipsis then List.rev acc, true
    else begin
      let pspecs = specifiers st in
      if pspecs.type_specs = [] && pspecs.quals = [] && pspecs.storage = None then
        error st "expected declaration specifiers before %s" (describe (peek st));
      let pdecl = declarator st ~abstract:true in
      (match declarator_name pdecl with Some n -> declare st n ~is_type:false | None -> ());
      let acc = { pspecs; pdecl } :: acc in
      if punct st Comma then loop acc else List.rev acc, false
    end in
  loop []

and declarator_name (d : declarator) =
  match d.d with
  | D_ident s -> Some s
  | D_abstract -> None
  | D_pointer (_, d) | D_array (d, _, _, _) | D_func (d, _, _) | D_ident_list (d, _)
  | D_attr (d, _) | D_asm_label (d, _) -> declarator_name d

(* 6.7.7 *)
and type_name st : type_name =
  let tspecs = specifiers st in
  let tdecl = declarator st ~abstract:true in
  { tspecs; tdecl }

(* 6.7.9 *)
and initialiser st : initialiser =
  let l = loc st in
  if punct st LBrace then begin
    let rec items acc =
      if peek st = Punct RBrace then List.rev acc
      else begin
        let rec designation acc =
          match peek st with
          | Punct LBracket -> advance st; let e = constant_expression st in expect_punct st RBracket; designation (Subscript e :: acc)
          | Punct Dot -> advance st; let f = expect_ident st in designation (Field f :: acc)
          | _ -> List.rev acc in
        let des = designation [] in
        if des <> [] then expect_punct st Eq;
        let init = initialiser st in
        let acc = (des, init) :: acc in
        if punct st Comma then items acc else List.rev acc
      end in
    let l' = items [] in
    expect_punct st RBrace;
    { i = Init_list l'; iloc = l }
  end else { i = Init_expr (assignment st); iloc = l }

(* 6.7.10 *)
and static_assert st : declaration =
  expect st (Keyword Static_assert);
  expect_punct st LParen;
  let e = constant_expression st in
  expect_punct st Comma;
  let msg = match peek st with String_lit _ as s -> advance st; s | t -> error st "expected string literal before %s" (describe t) in
  expect_punct st RParen;
  expect_punct st Semi;
  Static_assert (e, msg)

(* A declaration (6.7) or, at file scope, a function definition (6.9.1).
   The two share everything up to the first declarator. *)
and declaration_or_function st ~allow_function : external_decl =
  if peek st = Keyword Static_assert then Ext_decl (static_assert st)
  else begin
    let l = loc st in
    let specs = specifiers st in
    if specs.type_specs = [] && specs.quals = [] && specs.storage = None && specs.funcs = [] then
      error st "expected declaration specifiers before %s" (describe (peek st));
    if punct st Semi then Ext_decl (Decl (specs, [])) (* e.g. "struct s { ... };" *)
    else begin
      let first = declarator st ~abstract:false in
      let register d =
        match declarator_name d with
        | Some n -> declare st n ~is_type:(specs.storage = Some Typedef)
        | None -> () in
      let is_function_definition =
        allow_function && specs.storage <> Some Typedef
        && (match peek st with
            | Punct LBrace -> true
            | _ -> (* K&R: declarations of the parameters follow *)
                (match first.d with D_ident_list _ | D_func _ -> starts_declaration_specifier st (peek st) | _ -> false)) in
      if is_function_definition then begin
        register first;
        push_scope st;
        List.iter (fun n -> declare st n ~is_type:false) (parameter_names first);
        let rec kr acc =
          if peek st = Punct LBrace then List.rev acc
          else match declaration_or_function st ~allow_function:false with
            | Ext_decl d -> kr (d :: acc)
            | Ext_func _ -> assert false in
        let kr_decls = kr [] in
        let body = compound_statement st in
        pop_scope st;
        Ext_func { fspecs = specs; fdecl = first; kr_decls; body; floc = l }
      end else begin
        let rec rest acc d =
          register d;
          let init = if punct st Eq then Some (initialiser st) else None in
          let acc = { decl = d; init } :: acc in
          if punct st Comma then rest acc (declarator st ~abstract:false)
          else (expect_punct st Semi; List.rev acc) in
        Ext_decl (Decl (specs, rest [] first))
      end
    end
  end

(* Parameter names declared by a function declarator, for the body's scope. *)
and parameter_names (d : declarator) : string list =
  match d.d with
  | D_func (_, params, _) -> List.filter_map (fun p -> declarator_name p.pdecl) params
  | D_ident_list (_, names) -> names
  | D_pointer (_, d) | D_array (d, _, _, _) | D_attr (d, _) | D_asm_label (d, _) -> parameter_names d
  | D_ident _ | D_abstract -> []

(* ---- Statements (6.8) --------------------------------------------------------- *)

and compound_statement st : stmt =
  let l = loc st in
  expect_punct st LBrace;
  push_scope st;
  let rec items acc =
    if peek st = Punct RBrace then List.rev acc
    else
      let item =
        if starts_declaration st then Item_decl (declaration st) else Item_stmt (statement st) in
      items (item :: acc) in
  let body = items [] in
  pop_scope st;
  expect_punct st RBrace;
  { s = Block body; sloc = l }

(* 6.8.2: a block item is a declaration if it starts like one.  An
   identifier followed by ':' is a label even if it names a type. *)
and starts_declaration st =
  match peek st with
  | Ident _ when peek2 st = Punct Colon -> false
  | Keyword Static_assert -> true
  | t -> starts_declaration_specifier st t

and declaration st : declaration =
  match declaration_or_function st ~allow_function:false with
  | Ext_decl d -> d
  | Ext_func _ -> assert false

and statement st : stmt =
  let l = loc st in
  let node s = { s; sloc = l } in
  match peek st with
  | Ident name when peek2 st = Punct Colon ->
      advance st; advance st;
      let _ = attributes st in
      node (Label (name, statement st))
  | Keyword Case ->
      advance st;
      let e = constant_expression st in
      expect_punct st Colon;
      node (Case (e, statement st))
  | Keyword Default -> advance st; expect_punct st Colon; node (Default (statement st))
  | Punct LBrace -> compound_statement st
  | Punct Semi -> advance st; node (Expr None)
  | Keyword If ->
      advance st; expect_punct st LParen;
      let c = expression st in
      expect_punct st RParen;
      let a = statement st in
      let b = if keyword st Else then Some (statement st) else None in
      node (If (c, a, b))
  | Keyword Switch ->
      advance st; expect_punct st LParen;
      let c = expression st in
      expect_punct st RParen;
      node (Switch (c, statement st))
  | Keyword While ->
      advance st; expect_punct st LParen;
      let c = expression st in
      expect_punct st RParen;
      node (While (c, statement st))
  | Keyword Do ->
      advance st;
      let body = statement st in
      expect st (Keyword While); expect_punct st LParen;
      let c = expression st in
      expect_punct st RParen; expect_punct st Semi;
      node (Do (body, c))
  | Keyword For ->
      advance st; expect_punct st LParen;
      push_scope st; (* 6.8.5.3: a declaration in the for clause has loop scope *)
      let init =
        if punct st Semi then For_none
        else if starts_declaration st then For_decl (declaration st)
        else (let e = expression st in expect_punct st Semi; For_expr e) in
      let cond = if peek st = Punct Semi then None else Some (expression st) in
      expect_punct st Semi;
      let step = if peek st = Punct RParen then None else Some (expression st) in
      expect_punct st RParen;
      let body = statement st in
      pop_scope st;
      node (For (init, cond, step, body))
  | Keyword Goto -> advance st; let name = expect_ident st in expect_punct st Semi; node (Goto name)
  | Keyword Continue -> advance st; expect_punct st Semi; node Continue
  | Keyword Break -> advance st; expect_punct st Semi; node Break
  | Keyword Return ->
      advance st;
      let e = if peek st = Punct Semi then None else Some (expression st) in
      expect_punct st Semi;
      node (Return e)
  | Keyword Asm ->
      advance st;
      let _ = type_qualifiers st in
      expect_punct st LParen;
      let text = match peek st with String_lit { bytes; _ } -> advance st; bytes | t -> error st "expected string literal before %s" (describe t) in
      (* operands are not supported; see doc/extensions.md *)
      expect_punct st RParen; expect_punct st Semi;
      node (Asm text)
  | _ ->
      let e = expression st in
      expect_punct st Semi;
      node (Expr (Some e))

(* ---- Translation unit (6.9) ----------------------------------------------- *)

let parse toks =
  let st = { toks = Array.of_list toks; pos = 0; scopes = [ Hashtbl.create 256 ] } in
  (* The implementation's built-in type names, visible in every file. *)
  declare st "__builtin_va_list" ~is_type:true;
  let rec loop acc =
    match peek st with
    | Eof -> List.rev acc
    | Punct Semi -> advance st; loop acc (* empty declaration: accepted, as by every compiler *)
    | _ -> loop (declaration_or_function st ~allow_function:true :: acc) in
  loop []
