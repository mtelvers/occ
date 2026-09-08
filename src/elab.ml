module S = Syntax
module T = Typed
module C = Ctype

type ctx = {
  env : Env.t;
  globals : (int, T.global) Hashtbl.t; (* by symbol id *)
  mutable global_order : T.symbol list; (* definition order, reversed *)
  mutable funcs : T.func list; (* reversed *)
  (* per function *)
  mutable ret_type : C.t;
  labels : (string, unit) Hashtbl.t;
  mutable gotos : (string * Loc.t) list;
  mutable switch_types : C.t list; (* innermost first *)
  mutable case_values : (int64 list ref) list;
  mutable loops : int;
  mutable func_name : string;
  mutable vla_sizes : (int * T.expr) list;   (* variable length array id -> its size, as an expression *)
  mutable vla_pending : T.stmt list;          (* declarations of hidden size variables, for the declaration in progress *)
  mutable in_type_name : bool;                (* elaborating a type name: sizes stay expressions *)
  mutable vla_count : int;
}

let error = Diag.error

(* What a declarator yields: the declared name (absent for abstract
   declarators), the type, and two extension annotations. *)
type decl_result = { name : string option; ty : C.t; asm_label : string option; kr_params : string list option; attrs : S.attribute list }

let mk ?(lvalue = false) loc ty e : T.expr = { T.e; ty; lvalue; loc }
let int_const loc ty v = mk loc ty (T.Int (Const_eval.normalise ty v))

(* ---- Conversions (6.3) -------------------------------------------------------- *)

(* 6.3.2.1p2-4: an lvalue that is not an array is converted to the value
   stored in it (left to context); an array becomes a pointer to its first
   element; a function designator becomes a pointer to the function.
   Qualifiers are dropped from the result. *)
let rvalue (e : T.expr) : T.expr =
  match e.ty.u with
  | C.Array (elem, _) | C.Vla (elem, _) -> mk e.loc (C.pointer elem) (T.Convert e)
  | C.Func _ -> mk e.loc (C.pointer e.ty) (T.Convert e)
  | _ -> { e with ty = C.strip e.ty; lvalue = false }

(* Retag rather than wrap when the conversion is the identity. *)
let convert (e : T.expr) (ty : C.t) : T.expr =
  if C.compatible (C.strip e.ty) (C.strip ty) then { e with ty = C.strip ty; lvalue = false }
  else
    match e.e with
    | T.Int v when C.is_integer ty -> int_const e.loc (C.strip ty) v (* fold constants *)
    | T.Int v when C.is_floating ty -> mk e.loc (C.strip ty) (T.Float (Const_eval.to_float e.ty v))
    | T.Float f when C.is_floating ty -> mk e.loc (C.strip ty) (T.Float f)
    | _ -> mk e.loc (C.strip ty) (T.Convert e)

let underlying ctx (t : C.t) : C.t =
  match t.u with
  | C.Enum tag -> (match (Env.tag_info ctx.env tag).underlying with Some u -> u | None -> C.int)
  | _ -> t

(* 6.3.1.1p2 integer promotions *)
let promote ctx (e : T.expr) : T.expr =
  let e = rvalue e in
  match (underlying ctx e.ty).u with
  | C.Integer k when C.rank k < C.rank C.Int -> convert e C.int
  | C.Integer _ -> { e with ty = underlying ctx e.ty }
  | _ -> e

(* 6.3.1.8 usual arithmetic conversions *)
let usual_arithmetic ctx (a : T.expr) (b : T.expr) : T.expr * T.expr * C.t =
  let float_rank = function C.Floating C.LongDouble -> 3 | C.Floating C.Double -> 2 | C.Floating C.Float -> 1 | _ -> 0 in
  if C.is_floating a.ty || C.is_floating b.ty then begin
    let ty = if float_rank a.ty.u >= float_rank b.ty.u then C.strip a.ty else C.strip b.ty in
    convert (rvalue a) ty, convert (rvalue b) ty, ty
  end else begin
    let a = promote ctx a and b = promote ctx b in
    match a.ty.u, b.ty.u with
    | C.Integer x, C.Integer y ->
        let ty =
          if x = y then a.ty
          else if C.is_signed x = C.is_signed y then (if C.rank x > C.rank y then a.ty else b.ty)
          else
            let s, u = if C.is_signed x then x, y else y, x in
            if C.rank u >= C.rank s then C.unqualified (C.Integer u)
            else if Target.size_of_ikind s > Target.size_of_ikind u then C.unqualified (C.Integer s)
            else C.unqualified (C.Integer (C.to_unsigned s)) in
        convert a ty, convert b ty, ty
    | _ -> error a.loc "invalid operands to binary operator ('%a' and '%a')" C.pp a.ty C.pp b.ty
  end

(* 6.3.2.3p3: an integer constant expression with value 0, or such an
   expression cast to void *. *)
let is_null_pointer_constant ctx (e : T.expr) =
  let rec go (e : T.expr) =
    match e.e with
    | T.Int 0L when C.is_integer e.ty -> true
    | T.Convert x when (match e.ty.u with C.Pointer p -> p.u = C.Void && not (C.is_qualified p) | _ -> false) -> go x
    | _ -> C.is_integer e.ty && Const_eval.eval ctx.env e = Some (Const_eval.Int 0L) in
  go e

let pointee (t : C.t) = match t.u with C.Pointer p -> p | _ -> assert false

(* 6.5.16.1p1: the constraints on simple assignment, shared by argument
   passing, return and initialisation (6.5.2.2p7, 6.8.6.4p3, 6.7.9p11). *)
let assign_convert ctx ~what (e : T.expr) (ty : C.t) : T.expr =
  let e = rvalue e in
  let ty = C.strip ty in
  let bad () =
    error e.loc "incompatible types in %s: '%a' from '%a'" what C.pp ty C.pp e.ty in
  match ty.u, e.ty.u with
  | (C.Integer C.Bool), C.Pointer _ -> convert e ty
  | (C.Integer _ | C.Floating _ | C.Enum _), (C.Integer _ | C.Floating _ | C.Enum _) -> convert e ty
  | (C.Struct _ | C.Union _), _ when C.compatible ty e.ty -> e
  | C.Pointer p, C.Pointer q ->
      (* an array type is qualified through its element type (6.7.3p9) *)
      let rec quals (t : C.t) = match t.u with C.Array (e, _) | C.Vla (e, _) -> quals e | _ -> t.q in
      let pq = quals p and qq = quals q in
      let qual_ok = (pq.const || not qq.const) && (pq.volatile || not qq.volatile) in
      (* Extension (doc/extensions.md): void * converts to and from pointers
         to functions, as POSIX dlsym requires and every Unix compiler allows. *)
      if C.compatible (C.strip p) (C.strip q) || p.u = C.Void || q.u = C.Void then begin
        if not qual_ok then error e.loc "%s discards qualifiers from pointer target type ('%a' to '%a')" what C.pp e.ty C.pp ty;
        convert e ty
      end else if is_null_pointer_constant ctx e then convert e ty
      else bad ()
  | C.Pointer _, (C.Integer _ | C.Enum _) when is_null_pointer_constant ctx e -> convert e ty
  | C.Pointer _, (C.Integer _ | C.Enum _) ->
      Diag.warning e.loc "%s makes pointer from integer without a cast" what; convert e ty
  | (C.Integer _ | C.Enum _), C.Pointer _ ->
      Diag.warning e.loc "%s makes integer from pointer without a cast" what; convert e ty
  | _ -> bad ()

let scalar_condition ctx (e : T.expr) =
  let e = rvalue e in
  if not (C.is_scalar (underlying ctx e.ty)) then
    error e.loc "used %a where a scalar is required" C.pp e.ty;
  e

(* ---- Types from specifiers and declarators (6.7) ----------------------------- *)

let quals_of (qs : S.qualifier list) : C.qual =
  List.fold_left (fun (q : C.qual) -> function
      | S.Q_const -> { q with const = true } | S.Q_volatile -> { q with volatile = true }
      | S.Q_restrict -> { q with restrict = true } | S.Q_atomic -> { q with atomic = true })
    C.no_qual qs

(* 6.7.3p9: qualifying an array type qualifies its element type. *)
let rec qualify (t : C.t) (q : C.qual) : C.t =
  match t.u with
  | C.Array (e, n) -> { t with u = C.Array (qualify e q, n) }
  | C.Vla (e, id) -> { t with u = C.Vla (qualify e q, id) }
  | _ -> { t with q = C.merge_qual t.q q }

(* 6.7.5: _Alignas(type) or _Alignas(constant); zero means unspecified. *)
let rec alignment ctx loc (sp : S.specifiers) : int option =
  match sp.align with
  | None -> None
  | Some (S.Align_type tn) -> Some (Env.align_of ctx.env loc (type_name ctx tn))
  | Some (S.Align_expr e) ->
      let n = Int64.to_int (Const_eval.int_const ctx.env (expr ctx e)) in
      if n = 0 then None
      else if n < 0 || n land (n - 1) <> 0 then error loc "requested alignment is not a positive power of 2"
      else Some n

and base_type ctx loc (sp : S.specifiers) : C.t =
  let count x = List.length (List.filter (( = ) x) sp.type_specs) in
  let v = count S.Ts_void and c = count S.Ts_char and s = count S.Ts_short and i = count S.Ts_int
  and l = count S.Ts_long and f = count S.Ts_float and d = count S.Ts_double and b = count S.Ts_bool
  and sg = count S.Ts_signed and us = count S.Ts_unsigned and cx = count S.Ts_complex in
  let others = List.filter (function
      | S.Ts_struct _ | S.Ts_union _ | S.Ts_enum _ | S.Ts_typedef_name _ | S.Ts_atomic _ | S.Ts_typeof _ -> true
      | _ -> false) sp.type_specs in
  let basic = v + c + s + i + l + f + d + b + sg + us + cx in
  let t =
    match others with
    | [ S.Ts_struct sp ] when basic = 0 -> tagged_type ctx loc `Struct sp
    | [ S.Ts_union sp ] when basic = 0 -> tagged_type ctx loc `Union sp
    | [ S.Ts_enum es ] when basic = 0 -> enum_type ctx loc es
    | [ S.Ts_typedef_name n ] when basic = 0 ->
        (match Env.lookup ctx.env n with Some (Env.Typedef t) -> t | _ -> error loc "'%s' does not name a type" n)
    | [ S.Ts_atomic tn ] when basic = 0 -> qualify (type_name ctx tn) { C.no_qual with atomic = true }
    | [ S.Ts_typeof e ] when basic = 0 -> (expr ctx e).ty
    | [] ->
        if sg > 1 || us > 1 || (sg > 0 && us > 0) || l > 2 || cx > 0 then error loc "invalid type specifier combination";
        let ik k = C.unqualified (C.Integer k) in
        (match v, c, s, i, l, f, d, b with
         | 1, 0, 0, 0, 0, 0, 0, 0 when sg + us = 0 -> C.void
         | 0, 1, 0, 0, 0, 0, 0, 0 -> ik (if us > 0 then C.UChar else if sg > 0 then C.SChar else C.Char)
         | 0, 0, 1, (0 | 1), 0, 0, 0, 0 -> ik (if us > 0 then C.UShort else C.Short)
         | 0, 0, 0, (0 | 1), 0, 0, 0, 0 when i + sg + us > 0 -> ik (if us > 0 then C.UInt else C.Int)
         | 0, 0, 0, (0 | 1), 1, 0, 0, 0 -> ik (if us > 0 then C.ULong else C.Long)
         | 0, 0, 0, (0 | 1), 2, 0, 0, 0 -> ik (if us > 0 then C.ULLong else C.LLong)
         | 0, 0, 0, 0, 0, 1, 0, 0 when sg + us = 0 -> C.unqualified (C.Floating C.Float)
         | 0, 0, 0, 0, 0, 0, 1, 0 when sg + us = 0 -> C.unqualified (C.Floating C.Double)
         | 0, 0, 0, 0, 1, 0, 1, 0 when sg + us = 0 -> C.unqualified (C.Floating C.LongDouble)
         | 0, 0, 0, 0, 0, 0, 0, 1 when sg + us = 0 -> C.bool
         | 0, 0, 0, 0, 0, 0, 0, 0 -> error loc "declaration specifiers do not name a type"
         | _ -> error loc "invalid type specifier combination")
    | _ -> error loc "two or more data types in declaration specifiers" in
  qualify t (quals_of sp.quals)

(* 6.7.2.1.  A tag with a member list defines the type in the current
   scope; a tag alone refers to a visible one or declares an incomplete
   type (6.7.2.3). *)
and tagged_type ctx loc kind (sp : S.struct_spec) : C.t =
  let mk_type (info : Env.tag_info) = C.unqualified (if kind = `Struct then C.Struct info.tag else C.Union info.tag) in
  let info =
    match sp.tag, sp.members with
    | Some name, None ->
        (match Env.lookup_tag ctx.env name with
         | Some info -> if info.kind <> kind then error loc "'%s' defined as wrong kind of tag" name; info
         | None -> let info = Env.new_tag ctx.env kind (Some name) in Env.declare_tag ctx.env name info; info)
    | Some name, Some _ ->
        (match Env.lookup_tag_here ctx.env name with
         | Some info when info.kind = kind && info.layout = None -> info
         | Some _ -> error loc "redefinition of '%s'" name
         | None -> let info = Env.new_tag ctx.env kind (Some name) in Env.declare_tag ctx.env name info; info)
    | None, _ -> Env.new_tag ctx.env kind None in
  (match sp.members with
   | None -> ()
   | Some members ->
       let mems = List.concat_map (fun (m : S.member) ->
           let base = base_type ctx loc m.mspecs in
           List.map (fun (d, width) ->
               let name, ty = match d with
                 | Some d -> let r = apply_declarator ctx base d in r.name, r.ty
                 | None -> None, base in
               if C.is_function ty then error loc "field declared as a function";
               let width = Option.map (fun w ->
                   let w = Int64.to_int (Const_eval.int_const ctx.env (expr ctx w)) in
                   if not (C.is_integer ty) then error loc "bit-field has non-integer type '%a'" C.pp ty;
                   if w < 0 || w > 8 * Env.size_of ctx.env loc ty then error loc "width of bit-field exceeds its type";
                   if w = 0 && name <> None then error loc "named bit-field has zero width";
                   w) width in
               name, ty, width, alignment ctx loc m.mspecs) m.mdecls) members in
       (* every member but the last must be complete (6.7.2.1p3) *)
       let n = List.length mems in
       List.iteri (fun k (_, ty, _, _) ->
           if not (Env.is_complete ctx.env ty) && not (k = n - 1 && C.is_array ty && kind = `Struct && n > 1) then
             error loc "field has incomplete type '%a'" C.pp ty) mems;
       info.layout <- Some (Env.layout_struct ctx.env ~is_union:(kind = `Union) mems loc));
  mk_type info

(* 6.7.2.2.  Enumerators have type int; the enumeration itself gets the
   implementation's underlying type once its members are known. *)
and enum_type ctx loc (es : S.enum_spec) : C.t =
  let info =
    match es.etag, es.enumerators with
    | Some name, None ->
        (match Env.lookup_tag ctx.env name with
         | Some info -> if info.kind <> `Enum then error loc "'%s' defined as wrong kind of tag" name; info
         | None -> let info = Env.new_tag ctx.env `Enum (Some name) in Env.declare_tag ctx.env name info; info)
    | Some name, Some _ ->
        (match Env.lookup_tag_here ctx.env name with
         | Some info when info.kind = `Enum && info.underlying = None -> info
         | Some _ -> error loc "redefinition of 'enum %s'" name
         | None -> let info = Env.new_tag ctx.env `Enum (Some name) in Env.declare_tag ctx.env name info; info)
    | None, _ -> Env.new_tag ctx.env `Enum None in
  let ty = C.unqualified (C.Enum info.tag) in
  (match es.enumerators with
   | None -> ()
   | Some items ->
       let next = ref 0L and negative = ref false in
       List.iter (fun (name, v) ->
           let v = match v with
             | Some e -> Const_eval.int_const ctx.env (expr ctx e)
             | None -> !next in
           if v < 0L then negative := true;
           if Env.lookup_here ctx.env name <> None then error loc "redeclaration of enumerator '%s'" name;
           Env.declare ctx.env name (Env.Enum_const (v, C.int));
           next := Int64.add v 1L) items;
       info.underlying <- Some (Target.enum_underlying ~has_negative:!negative));
  ty

(* 6.7.6: the type described by [base] and [d], read inside-out, plus the
   declared name.  See [Syntax.declarator]. *)
and apply_declarator ctx (base : C.t) (d : S.declarator) : decl_result =
  match d.d with
  | S.D_ident name -> { name = Some name; ty = base; asm_label = None; kr_params = None; attrs = [] }
  | S.D_abstract -> { name = None; ty = base; asm_label = None; kr_params = None; attrs = [] }
  | S.D_pointer (qs, inner) -> apply_declarator ctx (qualify (C.pointer base) (quals_of qs)) inner
  | S.D_array (inner, _qs, size, _static) ->
      if C.is_function base then error d.dloc "declaration of array of functions";
      if not (Env.is_complete ctx.env base) then error d.dloc "array type has incomplete element type '%a'" C.pp base;
      let ty =
        match size with
        | None -> C.array base None
        | Some e ->
            let se = expr ctx e in
            if not (C.is_integer se.ty) then error d.dloc "size of array has non-integer type";
            (match Const_eval.eval ctx.env se with
             | Some (Const_eval.Int v) ->
                 if v < 0L then error d.dloc "size of array is negative";
                 C.array base (Some (Int64.to_int v))
             | _ -> vla_type ctx d.dloc base se) in
      apply_declarator ctx ty inner
  | S.D_func (inner, params, variadic) ->
      if C.is_function base then error d.dloc "function returning a function";
      if C.is_array base then error d.dloc "function returning an array";
      Env.push ctx.env; (* prototype scope, for tags declared in the list *)
      let params =
        match params with
        | [ { S.pspecs; pdecl = { d = S.D_abstract; _ } } ] when (base_type ctx d.dloc pspecs).u = C.Void -> Some []
        | [] -> None   (* "()" says nothing about the parameters (6.7.6.3p14) *)
        | ps -> Some (List.map (parameter ctx) ps) in
      Env.pop ctx.env;
      apply_declarator ctx (C.unqualified (C.Func { ret = base; params; variadic })) inner
  | S.D_ident_list (inner, names) ->
      let r = apply_declarator ctx (C.unqualified (C.Func { ret = base; params = None; variadic = false })) inner in
      { r with kr_params = Some names }
  | S.D_attr (inner, attrs) ->
      let r = apply_declarator ctx base inner in
      { r with attrs = r.attrs @ attrs }
  | S.D_asm_label (inner, label) ->
      let r = apply_declarator ctx base inner in
      { r with asm_label = Some label }

and parameter ctx (p : S.param_decl) : C.param =
  let base = base_type ctx p.pdecl.dloc p.pspecs in
  if p.pspecs.storage <> None && p.pspecs.storage <> Some S.Register then
    error p.pdecl.dloc "storage class specified for parameter";
  let r = apply_declarator ctx base p.pdecl in
  (* 6.7.6.3p7-8: arrays and functions adjust to pointers *)
  let ty = match r.ty.u with
    | C.Array (e, _) | C.Vla (e, _) -> { (C.pointer e) with q = r.ty.q }
    | C.Func _ -> C.pointer r.ty
    | _ -> r.ty in
  (match r.name with Some n -> Env.declare ctx.env n (Env.Var (Env.fresh_symbol ctx.env n ty T.Local)) | None -> ());
  { C.pname = r.name; ptype = ty }

and type_name ctx (tn : S.type_name) : C.t =
  let base = base_type ctx tn.tdecl.dloc tn.tspecs in
  let saved = ctx.in_type_name in
  ctx.in_type_name <- true;
  let r = apply_declarator ctx base tn.tdecl in
  ctx.in_type_name <- saved;
  if r.name <> None then error tn.tdecl.dloc "type name declares an identifier";
  r.ty

(* A variable length array type (6.7.6.2p4).  In a declaration the size is
   evaluated once, into a hidden variable declared just before; in a type
   name (sizeof, a cast) the expression itself is kept and evaluated where
   the type name is. *)
and vla_type ctx loc elem (size : T.expr) : C.t =
  if ctx.func_name = "" then error loc "variably modified type at file scope";
  let size = convert (rvalue size) C.size_t in
  ctx.vla_count <- ctx.vla_count + 1;
  let id = ctx.vla_count in
  let size_expr =
    if ctx.in_type_name then size
    else begin
      let sym = Env.fresh_symbol ctx.env (Printf.sprintf "__vla_size%d" id) C.size_t T.Local in
      ctx.vla_pending <- ctx.vla_pending @ [ T.Decl (sym, Some (T.Init_scalar size)) ];
      mk ~lvalue:true loc C.size_t (T.Var sym)
    end in
  ctx.vla_sizes <- (id, size_expr) :: ctx.vla_sizes;
  C.vla elem id

(* sizeof a type with a run-time size: the product of the sizes *)
and vla_sizeof ctx loc (ty : C.t) : T.expr =
  match ty.u with
  | C.Vla (e, id) -> mk loc C.size_t (T.Binop (Syntax.Mul, rvalue (List.assoc id ctx.vla_sizes), vla_sizeof ctx loc e))
  | C.Array (e, Some k) when C.has_vla e ->
      mk loc C.size_t (T.Binop (Syntax.Mul, int_const loc C.size_t (Int64.of_int k), vla_sizeof ctx loc e))
  | _ -> int_const loc C.size_t (Int64.of_int (Env.size_of ctx.env loc ty))

(* ---- Constants (6.4.4) ------------------------------------------------------- *)

(* 6.4.4.1p5: the type is the first in the list for the suffix and radix
   in which the value fits. *)
and integer_constant loc (value : string) radix (suffix : Token.int_suffix) : T.expr =
  let v = ref 0L and overflow = ref false in
  String.iter (fun ch ->
      let d = Int64.of_int (Lexer.hex_value ch) in
      let r = Int64.of_int radix in
      if Int64.unsigned_compare !v (Int64.unsigned_div (Int64.sub 0L 1L) r) > 0 then overflow := true;
      v := Int64.add (Int64.mul !v r) d;
      if Int64.unsigned_compare !v d < 0 then overflow := true) value;
  if !overflow then error loc "integer constant is too large for its type";
  let v = !v in
  let fits k =
    match k with
    | C.Int -> Int64.compare v 0L >= 0 && Int64.compare v 0x7FFF_FFFFL <= 0
    | C.UInt -> Int64.unsigned_compare v 0xFFFF_FFFFL <= 0
    | C.Long | C.LLong -> Int64.compare v 0L >= 0
    | C.ULong | C.ULLong -> true
    | _ -> false in
  let candidates =
    match suffix.unsigned, suffix.longs, radix = 10 with
    | false, 0, true -> [ C.Int; C.Long; C.LLong ]
    | false, 0, false -> [ C.Int; C.UInt; C.Long; C.ULong; C.LLong; C.ULLong ]
    | true, 0, _ -> [ C.UInt; C.ULong; C.ULLong ]
    | false, 1, true -> [ C.Long; C.LLong ]
    | false, 1, false -> [ C.Long; C.ULong; C.LLong; C.ULLong ]
    | true, 1, _ -> [ C.ULong; C.ULLong ]
    | false, _, true -> [ C.LLong ]
    | false, _, false -> [ C.LLong; C.ULLong ]
    | true, _, _ -> [ C.ULLong ] in
  match List.find_opt fits candidates with
  | Some k -> int_const loc (C.unqualified (C.Integer k)) v
  | None ->
      (* 6.4.4.1p6: a decimal constant too large for long long may have an
         extended type; we follow gcc and give it unsigned long long. *)
      Diag.warning loc "integer constant is so large that it is unsigned";
      int_const loc (C.unqualified (C.Integer C.ULLong)) v

and character_constant loc (chars : int list) (enc : Token.encoding) : T.expr =
  match enc with
  | Token.Plain | Token.Utf8 ->
      (* 6.4.4.4p10: a multi-character constant has type int and an
         implementation-defined value; like gcc, bytes are packed big-endian. *)
      if List.length chars > 4 then Diag.warning loc "multi-character character constant is too long";
      let v = match chars with
        | [ c ] -> Int64.of_int (if c > 127 then c - 256 else c) (* plain char is signed *)
        | _ -> List.fold_left (fun acc c -> Int64.logor (Int64.shift_left acc 8) (Int64.of_int c)) 0L chars in
      int_const loc C.int v
  | Token.Wide -> int_const loc C.wchar (Int64.of_int (List.hd chars))
  | Token.Char16 -> int_const loc C.char16 (Int64.of_int (List.hd chars))
  | Token.Char32 -> int_const loc C.char32 (Int64.of_int (List.hd chars))

and string_literal loc (bytes : string) (enc : Token.encoding) : T.expr =
  let elem, len =
    match enc with
    | Token.Plain | Token.Utf8 -> C.char, String.length bytes
    | Token.Wide | Token.Char16 | Token.Char32 ->
        let n = ref 0 in
        String.iter (fun ch -> if Char.code ch land 0xC0 <> 0x80 then incr n) bytes;
        (match enc with Token.Wide -> C.wchar | Token.Char16 -> C.char16 | _ -> C.char32), !n in
  mk ~lvalue:true loc (C.array elem (Some (len + 1))) (T.String bytes)

(* ---- Expressions (6.5) --------------------------------------------------------- *)

and expr ctx (e : S.expr) : T.expr =
  let loc = e.eloc in
  match e.e with
  | S.Ident name ->
      (match Env.lookup ctx.env name with
       | Some (Env.Var sym) -> mk ~lvalue:(not (C.is_function sym.ty)) loc sym.ty (T.Var sym)
       | Some (Env.Enum_const (v, ty)) -> int_const loc ty v
       | Some (Env.Typedef _) -> error loc "unexpected type name '%s'" name
       | None when name = "__func__" && ctx.func_name <> "" ->
           (* 6.4.2.2: as if "static const char __func__[] = "name";" *)
           string_literal loc ctx.func_name Token.Plain
       | None -> error loc "'%s' undeclared" name)
  | S.Const (Token.Int_const { value; radix; suffix }) -> integer_constant loc value radix suffix
  | S.Const (Token.Float_const { text; suffix }) ->
      let f = float_of_string text in
      (match suffix with
       | Token.F_none -> mk loc C.double (T.Float f)
       | Token.F_f -> mk loc (C.unqualified (C.Floating C.Float)) (T.Float (Int32.float_of_bits (Int32.bits_of_float f)))
       | Token.F_l -> mk loc (C.unqualified (C.Floating C.LongDouble)) (T.Float f))
  | S.Const (Token.Char_const { chars; enc }) -> character_constant loc chars enc
  | S.Const _ -> assert false
  | S.String (bytes, enc) -> string_literal loc bytes enc
  | S.Unop (op, x) -> unary ctx loc op x
  | S.Binop (op, a, b) -> binary ctx loc op (expr ctx a) (expr ctx b)
  | S.Assign (None, l, r) ->
      let l = expr ctx l in
      modifiable_lvalue ctx l;
      let r = assign_convert ctx ~what:"assignment" (expr ctx r) l.ty in
      mk loc (C.strip l.ty) (T.Assign (l, r))
  | S.Assign (Some op, l, r) ->
      (* 6.5.16.2: E1 op= E2 is E1 = E1 op E2 with E1 evaluated once *)
      let l = expr ctx l in
      modifiable_lvalue ctx l;
      let r = expr ctx r in
      let computed = binary ctx loc op l r in
      let comp_ty = match op with S.Shl | S.Shr -> (promote ctx l).ty | _ -> computed.ty in
      let r = match computed.e with
        | T.Binop (_, _, r') when C.is_pointer l.ty -> r'
        | T.Binop (_, _, r') -> r'
        | _ -> assert false in
      ignore (assign_convert ctx ~what:"assignment" computed l.ty);
      mk loc (C.strip l.ty) (T.Compound_assign (op, l, r, comp_ty))
  | S.Cond (c, a, b) ->
      let c = scalar_condition ctx (expr ctx c) in
      let a = rvalue (expr ctx a) and b = rvalue (expr ctx b) in
      (* 6.5.15p3-6 *)
      let a, b, ty =
        if C.is_arithmetic a.ty && C.is_arithmetic b.ty then usual_arithmetic ctx a b
        else if C.is_record a.ty && C.compatible a.ty b.ty then a, b, a.ty
        else if a.ty.u = C.Void && b.ty.u = C.Void then a, b, C.void
        else if C.is_pointer a.ty && is_null_pointer_constant ctx b then a, convert b a.ty, a.ty
        else if C.is_pointer b.ty && is_null_pointer_constant ctx a then convert a b.ty, b, b.ty
        else if C.is_pointer a.ty && C.is_pointer b.ty then begin
          let p = pointee a.ty and q = pointee b.ty in
          let quals = C.merge_qual p.q q.q in
          let target =
            if C.compatible (C.strip p) (C.strip q) then { (C.composite p q) with q = quals }
            else if p.u = C.Void || q.u = C.Void then { C.void with q = quals }
            else error loc "pointer type mismatch in conditional expression ('%a' and '%a')" C.pp a.ty C.pp b.ty in
          let ty = C.pointer target in
          convert a ty, convert b ty, ty
        end
        else error loc "type mismatch in conditional expression ('%a' and '%a')" C.pp a.ty C.pp b.ty in
      mk loc ty (T.Cond (c, a, b))
  | S.Call ({ e = S.Ident name; _ }, args) when Env.lookup ctx.env name = None && String.length name > 2 && String.sub name 0 2 = "__" ->
      builtin ctx loc name args
  | S.Call (f, args) ->
      let f = rvalue (expr ctx f) in
      let fty = match f.ty.u with
        | C.Pointer { u = C.Func ft; _ } -> ft
        | _ -> error loc "called object is not a function or function pointer ('%a')" C.pp f.ty in
      let args = List.map (expr ctx) args in
      let args =
        match fty.params with
        | None -> List.map (default_promote ctx) args
        | Some params ->
            let np = List.length params and na = List.length args in
            if na < np || (na > np && not fty.variadic) then
              error loc "%s arguments to function (expected %d, got %d)" (if na < np then "too few" else "too many") np na;
            List.mapi (fun i a ->
                if i < np then assign_convert ctx ~what:"argument passing" a (List.nth params i).ptype
                else default_promote ctx a) args in
      if not (C.is_void fty.ret) && not (Env.is_complete ctx.env fty.ret) then
        error loc "function returns incomplete type '%a'" C.pp fty.ret;
      mk loc (C.strip fty.ret) (T.Call (f, args))
  | S.Index (a, i) ->
      (* 6.5.2.1p2: identical to *((a)+(i)) *)
      let a = expr ctx a and i = expr ctx i in
      let sum = binary ctx loc S.Add a i in
      if not (C.is_pointer sum.ty) then error loc "subscripted value is neither array nor pointer";
      deref ctx loc sum
  | S.Member (x, field) -> member ctx loc (expr ctx x) field
  | S.Arrow (x, field) ->
      let x = rvalue (expr ctx x) in
      if not (C.is_pointer x.ty) then error loc "invalid type argument of '->' ('%a')" C.pp x.ty;
      member ctx loc (deref ctx loc x) field
  | S.Cast (tn, x) ->
      let ty = type_name ctx tn in
      let x = rvalue (expr ctx x) in
      if ty.u = C.Void then mk loc C.void (T.Convert x)
      else begin
        if not (C.is_scalar (underlying ctx ty)) then error loc "conversion to non-scalar type '%a' requested" C.pp ty;
        if not (C.is_scalar (underlying ctx x.ty)) then error loc "cast of non-scalar type '%a'" C.pp x.ty;
        if C.is_pointer ty && C.is_floating x.ty || C.is_floating ty && C.is_pointer x.ty then
          error loc "cast between pointer and floating type";
        convert x ty
      end
  | S.Sizeof_expr x ->
      let x = expr ctx x in
      if C.is_function x.ty then error loc "invalid application of 'sizeof' to a function type";
      if C.has_vla x.ty then vla_sizeof ctx loc x.ty
      else int_const loc C.size_t (Int64.of_int (Env.size_of ctx.env loc x.ty))
  | S.Sizeof_type tn ->
      let ty = type_name ctx tn in
      if C.is_function ty then error loc "invalid application of 'sizeof' to a function type";
      if C.has_vla ty then vla_sizeof ctx loc ty
      else int_const loc C.size_t (Int64.of_int (Env.size_of ctx.env loc ty))
  | S.Alignof tn -> int_const loc C.size_t (Int64.of_int (Env.align_of ctx.env loc (type_name ctx tn)))
  | S.Compound_literal (tn, init) ->
      let ty = type_name ctx tn in
      let init, ty = initialiser ctx ty init in
      let storage = if Env.at_file_scope ctx.env then T.Static { linkage = T.No_linkage; tls = false } else T.Local in
      let sym = Env.fresh_symbol ctx.env "compound_literal" ty storage in
      if Env.at_file_scope ctx.env then define_global ctx sym (Some init);
      mk ~lvalue:true loc ty (T.Compound_literal (sym, init))
  | S.Generic (ctrl, assocs) ->
      (* 6.5.1.1: the controlling expression undergoes lvalue conversion (C17 clarification) *)
      let cty = (rvalue (expr ctx ctrl)).ty in
      let matches = List.filter (fun (t, _) -> match t with Some t -> C.compatible (type_name ctx t) cty | None -> false) assocs in
      (match matches, List.find_opt (fun (t, _) -> t = None) assocs with
       | [ (_, e) ], _ | [], Some (_, e) -> expr ctx e
       | [], None -> error loc "'_Generic' selector of type '%a' is not compatible with any association" C.pp cty
       | _ -> error loc "'_Generic' selector matches more than one association")
  | S.Va_arg (ap, tn) ->
      let ap = rvalue (expr ctx ap) in
      let ty = type_name ctx tn in
      mk loc ty (T.Va_arg (ap, ty))
  | S.Offsetof (tn, designators) ->
      let ty = type_name ctx tn in
      let rec walk ty off = function
        | [] -> off
        | S.Field f :: rest ->
            (match Env.find_field ctx.env ty f with
             | Some path ->
                 let last = List.nth path (List.length path - 1) in
                 walk last.Env.ftype (off + List.fold_left (fun a (fl : Env.field) -> a + fl.offset) 0 path) rest
             | None -> error loc "no member named '%s' in '%a'" f C.pp ty)
        | S.Subscript i :: rest ->
            (match ty.u with
             | C.Array (e, _) ->
                 let i = Int64.to_int (Const_eval.int_const ctx.env (expr ctx i)) in
                 walk e (off + i * Env.size_of ctx.env loc e) rest
             | _ -> error loc "subscripted value in offsetof is not an array") in
      int_const loc C.size_t (Int64.of_int (walk ty 0 designators))

(* 6.5.2.2p6 default argument promotions *)
and default_promote ctx (a : T.expr) : T.expr =
  let a = rvalue a in
  match a.ty.u with
  | C.Floating C.Float -> convert a C.double
  | C.Integer _ | C.Enum _ -> promote ctx a
  | _ -> a

and deref _ctx loc (p : T.expr) : T.expr =
  let p = rvalue p in
  match p.ty.u with
  | C.Pointer ({ u = C.Func _; _ } as f) -> mk loc f (T.Deref p)
  | C.Pointer t ->
      if t.u = C.Void then error loc "dereferencing 'void *' pointer";
      mk ~lvalue:true loc t (T.Deref p)
  | _ -> error loc "invalid type argument of unary '*' ('%a')" C.pp p.ty

and member ctx loc (x : T.expr) field : T.expr =
  if not (C.is_record x.ty) then error loc "request for member '%s' in something not a structure or union" field;
  if not (Env.is_complete ctx.env x.ty) then error loc "invalid use of incomplete type '%a'" C.pp x.ty;
  match Env.find_field ctx.env x.ty field with
  | None -> error loc "'%a' has no member named '%s'" C.pp x.ty field
  | Some path ->
      (* qualifiers of the aggregate apply to the member (6.5.2.3p3) *)
      List.fold_left (fun (base : T.expr) (f : Env.field) ->
          mk ~lvalue:base.lvalue loc (qualify f.ftype base.ty.q) (T.Member (base, f))) x path

and modifiable_lvalue ctx (l : T.expr) =
  if not l.lvalue then error l.loc "lvalue required as left operand of assignment";
  if C.is_array l.ty then error l.loc "assignment to expression with array type";
  if not (Env.is_complete ctx.env l.ty) then error l.loc "assignment to incomplete type '%a'" C.pp l.ty;
  if l.ty.q.const then error l.loc "assignment of read-only location";
  (* a struct with a const member is not modifiable either (6.3.2.1p1) *)
  let rec has_const (t : C.t) =
    match t.u with
    | C.Struct tag | C.Union tag ->
        (match (Env.tag_info ctx.env tag).layout with
         | Some l -> List.exists (fun (f : Env.field) -> f.ftype.q.const || has_const f.ftype) l.fields
         | None -> false)
    | C.Array (e, _) -> has_const e
    | _ -> false in
  if has_const l.ty then error l.loc "assignment of read-only member"

and unary ctx loc (op : S.unop) (x : S.expr) : T.expr =
  let x = expr ctx x in
  match op with
  | S.Addr ->
      (match x.e with
       | T.Member (_, f) when f.bits <> None -> error loc "cannot take address of bit-field"
       | _ -> ());
      if not x.lvalue && not (C.is_function x.ty) then error loc "lvalue required as unary '&' operand";
      (* &*p and &a[i] are not lvalue uses (6.5.3.2p3) *)
      (match x.e with
       | T.Deref p -> { p with loc; ty = C.pointer x.ty }
       | _ -> mk loc (C.pointer x.ty) (T.Addr x))
  | S.Deref -> deref ctx loc x
  | S.Neg | S.Not ->
      let x = promote ctx x in
      if op = S.Not && not (C.is_integer x.ty) then error loc "wrong type argument to bit-complement";
      if not (C.is_arithmetic x.ty) then error loc "wrong type argument to unary minus";
      (match x.e with
       | T.Int v -> int_const loc x.ty (if op = S.Neg then Int64.neg v else Int64.lognot v)
       | T.Float f when op = S.Neg -> mk loc x.ty (T.Float (-. f))
       | _ -> mk loc x.ty (T.Unop (op, x)))
  | S.Lnot ->
      let x = scalar_condition ctx x in
      mk loc C.int (T.Unop (S.Lnot, x))
  | S.Preinc | S.Predec | S.Postinc | S.Postdec ->
      modifiable_lvalue ctx x;
      if not (C.is_scalar (underlying ctx x.ty)) then error loc "wrong type argument to increment";
      if C.is_pointer x.ty && not (Env.is_complete ctx.env (pointee x.ty)) then
        error loc "arithmetic on pointer to incomplete type";
      mk loc (C.strip x.ty) (T.Unop (op, x))

and binary ctx loc (op : S.binop) (a : T.expr) (b : T.expr) : T.expr =
  let a = rvalue a and b = rvalue b in
  let ua = underlying ctx a.ty and ub = underlying ctx b.ty in
  let arith what pred =
    if not (pred ua && pred ub) then
      error loc "invalid operands to binary %s ('%a' and '%a')" what C.pp a.ty C.pp b.ty in
  let fold ty =
    let e = mk loc ty (T.Binop (op, a, b)) in
    match Const_eval.eval ctx.env e with
    | Some (Const_eval.Int v) when C.is_integer ty -> int_const loc ty v
    | _ -> e in
  let complete_pointee (p : T.expr) =
    if not (Env.is_complete ctx.env (pointee p.ty)) then
      error loc "arithmetic on pointer to incomplete type '%a'" C.pp p.ty in
  match op with
  | S.Mul | S.Div -> arith "*" C.is_arithmetic; let a, b, ty = usual_arithmetic ctx a b in
      let e = mk loc ty (T.Binop (op, a, b)) in
      (match Const_eval.eval ctx.env e with Some (Const_eval.Int v) -> int_const loc ty v | _ -> e)
  | S.Mod | S.Band | S.Bor | S.Bxor ->
      arith (match op with S.Mod -> "%" | S.Band -> "&" | S.Bor -> "|" | _ -> "^") C.is_integer;
      let a, b, ty = usual_arithmetic ctx a b in
      let e = mk loc ty (T.Binop (op, a, b)) in
      (match Const_eval.eval ctx.env e with Some (Const_eval.Int v) -> int_const loc ty v | _ -> e)
  | S.Shl | S.Shr ->
      arith "<<" C.is_integer;
      let a = promote ctx a and b = promote ctx b in
      let e = mk loc a.ty (T.Binop (op, a, b)) in
      (match Const_eval.eval ctx.env e with Some (Const_eval.Int v) -> int_const loc a.ty v | _ -> e)
  | S.Add ->
      if C.is_arithmetic ua && C.is_arithmetic ub then begin
        let a, b, ty = usual_arithmetic ctx a b in
        let e = mk loc ty (T.Binop (op, a, b)) in
        (match Const_eval.eval ctx.env e with Some (Const_eval.Int v) -> int_const loc ty v | _ -> e)
      end else begin
        (* pointer + integer, in either order; the pointer comes first in Typed *)
        let p, i = if C.is_pointer a.ty then a, b else b, a in
        if not (C.is_pointer p.ty && C.is_integer (underlying ctx i.ty)) then
          error loc "invalid operands to binary + ('%a' and '%a')" C.pp a.ty C.pp b.ty;
        complete_pointee p;
        mk loc p.ty (T.Binop (S.Add, p, convert (promote ctx i) C.long))
      end
  | S.Sub ->
      if C.is_arithmetic ua && C.is_arithmetic ub then begin
        let a, b, ty = usual_arithmetic ctx a b in
        let e = mk loc ty (T.Binop (op, a, b)) in
        (match Const_eval.eval ctx.env e with Some (Const_eval.Int v) -> int_const loc ty v | _ -> e)
      end else if C.is_pointer a.ty && C.is_integer ub then begin
        complete_pointee a;
        mk loc a.ty (T.Binop (S.Sub, a, convert (promote ctx b) C.long))
      end else if C.is_pointer a.ty && C.is_pointer b.ty then begin
        if not (C.compatible (C.strip (pointee a.ty)) (C.strip (pointee b.ty))) then
          error loc "invalid operands to binary - ('%a' and '%a')" C.pp a.ty C.pp b.ty;
        complete_pointee a;
        mk loc C.ptrdiff_t (T.Binop (S.Sub, a, b))
      end else error loc "invalid operands to binary - ('%a' and '%a')" C.pp a.ty C.pp b.ty
  | S.Lt | S.Gt | S.Le | S.Ge | S.Eq | S.Ne ->
      if C.is_arithmetic ua && C.is_arithmetic ub then begin
        let a, b, _ = usual_arithmetic ctx a b in
        let e = mk loc C.int (T.Binop (op, a, b)) in
        (match Const_eval.eval ctx.env e with Some (Const_eval.Int v) -> int_const loc C.int v | _ -> e)
      end else begin
        (* 6.5.8p2, 6.5.9p2: pointers to compatible types, or void *, or a null pointer constant *)
        let a, b =
          if C.is_pointer a.ty && is_null_pointer_constant ctx b then a, convert b a.ty
          else if C.is_pointer b.ty && is_null_pointer_constant ctx a then convert a b.ty, b
          else if C.is_pointer a.ty && C.is_pointer b.ty then begin
            let p = pointee a.ty and q = pointee b.ty in
            if not (C.compatible (C.strip p) (C.strip q) || p.u = C.Void || q.u = C.Void) then
              Diag.warning loc "comparison of distinct pointer types ('%a' and '%a')" C.pp a.ty C.pp b.ty;
            a, convert b a.ty
          end else if C.is_pointer a.ty && C.is_integer ub || C.is_pointer b.ty && C.is_integer ua then begin
            Diag.warning loc "comparison between pointer and integer";
            if C.is_pointer a.ty then a, convert b a.ty else convert a b.ty, b
          end else error loc "invalid operands to comparison ('%a' and '%a')" C.pp a.ty C.pp b.ty in
        mk loc C.int (T.Binop (op, a, b))
      end
  | S.Land | S.Lor ->
      let a = scalar_condition ctx a and b = scalar_condition ctx b in
      ignore fold; mk loc C.int (T.Binop (op, a, b))
  | S.Comma -> mk loc b.ty (T.Binop (S.Comma, a, b))

(* Builtins (doc/extensions.md).  These are recognised by name because
   their argument lists are not ordinary: some take types, some are type
   generic, and the atomic ones take memory orders that must be constants. *)
and builtin ctx loc name (args : S.expr list) : T.expr =
  let args_e = lazy (List.map (fun a -> rvalue (expr ctx a)) args) in
  let nargs n = if List.length args <> n then error loc "'%s' expects %d arguments" name n in
  let order (e : T.expr) =
    match Const_eval.eval ctx.env e with
    | Some (Const_eval.Int 0L) -> Ir.Relaxed | Some (Const_eval.Int 1L) -> Ir.Consume
    | Some (Const_eval.Int 2L) -> Ir.Acquire | Some (Const_eval.Int 3L) -> Ir.Release
    | Some (Const_eval.Int 4L) -> Ir.Acq_rel | Some (Const_eval.Int 5L) -> Ir.Seq_cst
    | _ -> error e.loc "memory order must be a constant" in
  let atomic_pointee (p : T.expr) =
    match p.ty.u with
    | C.Pointer t when C.is_scalar (underlying ctx t) -> C.strip t
    | _ -> error p.loc "'%s' expects a pointer to an atomic scalar (got '%a')" name C.pp p.ty in
  match name with
  | "__builtin_expect" -> nargs 2; convert (List.hd (Lazy.force args_e)) C.long
  | "__builtin_trap" | "__builtin_unreachable" -> nargs 0; mk loc C.void (T.Builtin (name, []))
  | "__builtin_prefetch" -> mk loc C.void (T.Builtin (name, Lazy.force args_e))
  | "__builtin_return_address" -> nargs 1; mk loc (C.pointer C.void) (T.Builtin (name, Lazy.force args_e))
  | "__builtin_add_overflow" | "__builtin_sub_overflow" | "__builtin_mul_overflow" ->
      nargs 3;
      let a = Lazy.force args_e in
      (match (List.nth a 2).ty.u with
       | C.Pointer t when C.is_integer t -> ()
       | _ -> error loc "third argument of '%s' must be a pointer to an integer" name);
      mk loc C.bool (T.Builtin (name, a))
  | "__builtin_va_start" -> nargs 2; mk loc C.void (T.Builtin (name, Lazy.force args_e))
  | "__builtin_va_end" -> nargs 1; mk loc C.void (T.Builtin (name, Lazy.force args_e))
  | "__builtin_va_copy" -> nargs 2; mk loc C.void (T.Builtin (name, Lazy.force args_e))
  | "__builtin_setjmp" | "__builtin_longjmp" -> mk loc (if name = "__builtin_setjmp" then C.int else C.void) (T.Builtin (name, Lazy.force args_e))
  | "__occ_atomic_load" ->
      nargs 2; let a = Lazy.force args_e in
      let ty = atomic_pointee (List.nth a 0) in
      mk loc ty (T.Atomic_op (T.Load, [ order (List.nth a 1) ], [ List.nth a 0 ]))
  | "__occ_atomic_store" ->
      nargs 3; let a = Lazy.force args_e in
      let ty = atomic_pointee (List.nth a 0) in
      let v = assign_convert ctx ~what:"atomic store" (List.nth a 1) ty in
      mk loc C.void (T.Atomic_op (T.Store, [ order (List.nth a 2) ], [ List.nth a 0; v ]))
  | "__occ_atomic_exchange" ->
      nargs 3; let a = Lazy.force args_e in
      let ty = atomic_pointee (List.nth a 0) in
      let v = assign_convert ctx ~what:"atomic exchange" (List.nth a 1) ty in
      mk loc ty (T.Atomic_op (T.Exchange, [ order (List.nth a 2) ], [ List.nth a 0; v ]))
  | "__occ_atomic_compare_exchange_strong" | "__occ_atomic_compare_exchange_weak" ->
      nargs 5; let a = Lazy.force args_e in
      let ty = atomic_pointee (List.nth a 0) in
      (match (List.nth a 1).ty.u with
       | C.Pointer t when C.compatible (C.strip t) ty -> ()
       | _ -> error loc "'%s': expected pointer to '%a' as second argument" name C.pp ty);
      let v = assign_convert ctx ~what:"atomic compare-exchange" (List.nth a 2) ty in
      mk loc C.bool (T.Atomic_op (T.Compare_exchange (name = "__occ_atomic_compare_exchange_strong"),
                                  [ order (List.nth a 3); order (List.nth a 4) ], [ List.nth a 0; List.nth a 1; v ]))
  | "__occ_atomic_fetch_add" | "__occ_atomic_fetch_sub" | "__occ_atomic_fetch_or"
  | "__occ_atomic_fetch_xor" | "__occ_atomic_fetch_and" ->
      nargs 3; let a = Lazy.force args_e in
      let ty = atomic_pointee (List.nth a 0) in
      let op = match name with
        | "__occ_atomic_fetch_add" -> S.Add | "__occ_atomic_fetch_sub" -> S.Sub
        | "__occ_atomic_fetch_or" -> S.Bor | "__occ_atomic_fetch_xor" -> S.Bxor | _ -> S.Band in
      let v = List.nth a 1 in
      let v =
        if C.is_pointer ty then convert (promote ctx v) C.long (* scaled in Lower, 7.17.7.5p3 *)
        else if C.is_integer (underlying ctx ty) then convert v ty
        else error loc "'%s' requires an integer or pointer type" name in
      mk loc ty (T.Atomic_op (T.Fetch op, [ order (List.nth a 2) ], [ List.nth a 0; v ]))
  | "__occ_atomic_thread_fence" -> nargs 1; mk loc C.void (T.Atomic_op (T.Fence, [ order (List.hd (Lazy.force args_e)) ], []))
  | "__occ_atomic_signal_fence" -> nargs 1; mk loc C.void (T.Atomic_op (T.Signal_fence, [ order (List.hd (Lazy.force args_e)) ], []))
  | _ -> error loc "'%s' undeclared (unsupported builtin; see doc/extensions.md)" name

(* ---- Initializers (6.7.9) --------------------------------------------------------- *)

(* [initialiser ctx ty init] elaborates an initializer for an object of
   type [ty], returning the flattened initializer and the type, which is
   completed when [ty] is an array of unknown size (6.7.9p22). *)
and initialiser ctx (ty : C.t) (init : S.initialiser) : T.init * C.t =
  match init.i, ty.u with
  | S.Init_expr e, C.Array (elem, n) when is_string_for ctx elem e ->
      (* 6.7.9p14-15 *)
      let s = match (expr ctx e).e with T.String s -> s | _ -> assert false in
      let len = string_length elem s in
      let n = match n with
        | None -> len + 1
        | Some n -> if len > n then error init.iloc "initializer-string for array is too long"; n in
      T.Init_string s, C.array elem (Some n)
  | S.Init_list [ ([], ({ i = S.Init_expr e; _ } as inner)) ], C.Array (elem, _) when is_string_for ctx elem e ->
      initialiser ctx ty inner
  | S.Init_expr e, (C.Struct _ | C.Union _) ->
      let e = rvalue (expr ctx e) in
      if not (C.compatible e.ty (C.strip ty)) then
        error init.iloc "incompatible types in initialization: '%a' from '%a'" C.pp ty C.pp e.ty;
      T.Init_scalar e, ty
  | S.Init_expr _, C.Array _ -> error init.iloc "invalid initializer for an array"
  | S.Init_expr e, _ ->
      T.Init_scalar (assign_convert ctx ~what:"initialization" (expr ctx e) ty), ty
  | S.Init_list [ ([], ({ i = S.Init_expr _; _ } as inner)) ], _ when C.is_scalar (underlying ctx ty) ->
      initialiser ctx ty inner (* 6.7.9p11: braces around a scalar *)
  | S.Init_list [], _ when C.is_scalar (underlying ctx ty) ->
      error init.iloc "empty scalar initializer"
  | S.Init_list _, _ when C.is_scalar (underlying ctx ty) ->
      error init.iloc "too many elements in scalar initializer"
  | S.Init_list items, _ ->
      let items = ref items in
      let leaves, ty = fill ctx ty 0 items ~braced:true init.iloc in
      if !items <> [] then error init.iloc "excess elements in initializer";
      T.Init_agg leaves, ty

and is_string_for ctx (elem : C.t) (e : S.expr) =
  match e.e with
  | S.String (_, enc) ->
      let lit_elem = match enc with
        | Token.Plain | Token.Utf8 -> C.char | Token.Wide -> C.wchar
        | Token.Char16 -> C.char16 | Token.Char32 -> C.char32 in
      let elem = underlying ctx (C.strip elem) in
      (match lit_elem.u, elem.u with
       | C.Integer C.Char, C.Integer (C.Char | C.SChar | C.UChar) -> true
       | _ -> C.compatible lit_elem elem)
  | _ -> false

and string_length (elem : C.t) s =
  if (match elem.u with C.Integer (C.Char | C.SChar | C.UChar) -> true | _ -> false) then String.length s
  else (let n = ref 0 in String.iter (fun ch -> if Char.code ch land 0xC0 <> 0x80 then incr n) s; !n)

(* Members of an aggregate as (name, offset, type, bits) in order; for an
   array of unknown size the list is unbounded and produced on demand. *)
and members ctx loc (ty : C.t) : (string option * int * C.t * (int * int) option) Seq.t * bool (* is_union *) =
  match ty.u with
  | C.Array (elem, n) ->
      let sz = Env.size_of ctx.env loc elem in
      let rec from i () =
        if (match n with Some n -> i >= n | None -> false) then Seq.Nil
        else Seq.Cons ((None, i * sz, elem, None), from (i + 1)) in
      from 0, false
  | C.Struct tag | C.Union tag ->
      let info = Env.tag_info ctx.env tag in
      let l = match info.layout with Some l -> l | None -> error loc "initialization of incomplete type '%a'" C.pp ty in
      List.to_seq (List.map (fun (f : Env.field) -> f.fname, f.offset, f.ftype, f.bits) l.fields), info.kind = `Union
  | _ -> assert false

(* Consume items for the object of type [ty] at [base] offset.  With
   [braced], this object owns the list and excess items are an error;
   otherwise (brace elision, 6.7.9p20) it takes what it needs and stops.
   Returns the leaves and the type, completed if it was an array of
   unknown size. *)
and fill ctx (ty : C.t) base (items : (S.designator list * S.initialiser) list ref) ~braced loc : T.init_item list * C.t =
  let mems, is_union = members ctx loc ty in
  let leaves = ref [] in
  let max_index = ref (-1) in
  let unknown_array = match ty.u with C.Array (_, None) -> true | _ -> false in
  let elem_size = match ty.u with C.Array (e, _) -> Env.size_of ctx.env loc e | _ -> 0 in
  (* the current member as a position in the sequence *)
  let rec seek seq i = if i = 0 then seq else match seq () with Seq.Nil -> Seq.empty | Seq.Cons (_, rest) -> seek rest (i - 1) in
  let rec loop seq index =
    match !items with
    | [] -> ()
    | (designators, init) :: rest ->
        (* a designator addresses a member of this object, or of an
           enclosing one when we are here by brace elision *)
        let target, seq, index =
          match designators, ty.u with
          | S.Field f :: _, (C.Struct _ | C.Union _) ->
              let rec find seq i = match seq () with
                | Seq.Nil -> None
                | Seq.Cons ((name, _, _, _), _) when name = Some f -> Some (seq, i)
                | Seq.Cons (_, rest) -> find rest (i + 1) in
              (match find (fst (members ctx loc ty)) 0 with
               | Some (seq, i) -> `Member, seq, i
               | None ->
                   (* an anonymous member may hold it; otherwise it belongs to an outer object *)
                   let anon = Env.find_field ctx.env ty f in
                   (match anon with
                    | Some (first :: _ :: _) ->
                        let rec find seq i = match seq () with
                          | Seq.Nil -> assert false
                          | Seq.Cons ((_, off, _, _), _) when off = first.offset -> seq, i
                          | Seq.Cons (_, rest) -> find rest (i + 1) in
                        let seq, i = find (fst (members ctx loc ty)) 0 in
                        `Anon, seq, i
                    | _ -> if braced then error init.iloc "no member named '%s' in '%a'" f C.pp ty else `Outer, seq, index))
          | S.Subscript e :: _, C.Array (_, n) ->
              let i = Int64.to_int (Const_eval.int_const ctx.env (expr ctx e)) in
              if i < 0 || (match n with Some n -> i >= n | None -> false) then error init.iloc "array index in initializer exceeds array bounds";
              `Member, seek (fst (members ctx loc ty)) i, i
          | (S.Field _ | S.Subscript _) :: _, _ ->
              if braced then error init.iloc "designator does not match the type '%a'" C.pp ty else `Outer, seq, index
          | [], _ -> `Member, seq, index in
        if target = `Outer then ()
        else
          match seq () with
          | Seq.Nil -> if braced then error init.iloc "excess elements in initializer" else ()
          | Seq.Cons ((_, off, mty, bits), seq_rest) ->
              if index > !max_index then max_index := index;
              (* the item for this member: strip one designator level *)
              let sub_item = match designators, target with
                | _ :: tail, `Member -> tail, init
                | _, `Anon -> designators, init
                | _ -> [], init in
              items := sub_item :: rest;
              let member_leaves =
                match sub_item, mty.u with
                | ([], { S.i = S.Init_list _; _ }), (C.Array _ | C.Struct _ | C.Union _) ->
                    items := rest;
                    let init, _ = initialiser ctx mty (snd sub_item) in
                    (match init with T.Init_agg l -> List.map (fun (it : T.init_item) -> { it with off = it.off + base + off }) l
                     | _ -> [ { T.off = base + off; ity = mty; bits; init } ])
                | ([], { S.i = S.Init_expr e; _ }), (C.Array (elem, _)) when is_string_for ctx elem e ->
                    items := rest;
                    let init, _ = initialiser ctx mty (snd sub_item) in
                    [ { T.off = base + off; ity = mty; bits; init } ]
                | ([], { S.i = S.Init_expr e; _ }), (C.Struct _ | C.Union _) when (C.compatible (rvalue (expr ctx e)).ty (C.strip mty)) ->
                    items := rest;
                    [ { T.off = base + off; ity = mty; bits; init = T.Init_scalar (rvalue (expr ctx e)) } ]
                | _, (C.Array _ | C.Struct _ | C.Union _) ->
                    (* brace elision, or a nested designator: descend sharing the list *)
                    let l, _ = fill ctx mty (base + off) items ~braced:false loc in l
                | ([], init), _ ->
                    items := rest;
                    let i, _ = initialiser ctx mty init in
                    [ { T.off = base + off; ity = mty; bits; init = i } ]
                | (_ :: _, _), _ -> error init.iloc "designator in initializer for scalar type '%a'" C.pp mty in
              leaves := List.rev_append member_leaves !leaves;
              if is_union && designators = [] then () (* only the first member without a designator *)
              else loop seq_rest (index + 1) in
  loop mems 0;
  let ty = if unknown_array then C.array (match ty.u with C.Array (e, _) -> e | _ -> assert false) (Some (!max_index + 1)) else ty in
  ignore elem_size;
  List.rev !leaves, ty

(* ---- Declarations (6.7) ------------------------------------------------------------ *)

and define_global ctx (sym : T.symbol) (init : T.init option) =
  if not (Hashtbl.mem ctx.globals sym.id) then ctx.global_order <- sym :: ctx.global_order;
  Hashtbl.replace ctx.globals sym.id { T.gsym = sym; ginit = init; defined = true }

and declare_global ctx (sym : T.symbol) =
  if not (Hashtbl.mem ctx.globals sym.id) then begin
    ctx.global_order <- sym :: ctx.global_order;
    Hashtbl.replace ctx.globals sym.id { T.gsym = sym; ginit = None; defined = false }
  end

(* One declaration, returning the statements it contributes to a block
   (declarations of locals with automatic storage). *)
(* GNU linkage attributes (extension, doc/extensions.md): the ones the C
   library needs turned from parsed syntax into facts on the symbol. *)
and apply_attributes ctx (sym : T.symbol) (attrs : S.attribute list) =
  let prio a = match a with [] -> Some 65535 | [ e ] -> Some (Int64.to_int (Const_eval.int_const ctx.env (expr ctx e))) | _ -> None in
  List.iter (fun (a : S.attribute) ->
      match a.aname with
      | "weak" | "__weak__" -> sym.link.weak <- true
      | "alias" | "__alias__" ->
          (match a.aargs with [ { S.e = S.String (s, _); _ } ] -> sym.link.alias <- Some s | _ -> ())
      | "visibility" | "__visibility__" ->
          (match a.aargs with
           | [ { S.e = S.String (v, _); _ } ] ->
               sym.link.visibility <- (match v with "hidden" -> T.Hidden | "protected" -> T.Protected | "internal" -> T.Internal | _ -> T.Default)
           | _ -> ())
      | "constructor" | "__constructor__" -> sym.link.constructor <- prio a.aargs
      | "destructor" | "__destructor__" -> sym.link.destructor <- prio a.aargs
      | "aligned" | "__aligned__" ->
          (match a.aargs with [ e ] -> let n = Int64.to_int (Const_eval.int_const ctx.env (expr ctx e)) in if Option.value sym.align ~default:0 < n then sym.align <- Some n | _ -> ())
      | _ -> ()  (* the many attributes that do not affect code are ignored *)) attrs

(* the hidden size variables of any variable length arrays come first *)
and declaration ctx (d : S.declaration) : T.stmt list =
  let stmts = declaration_body ctx d in
  let pending = ctx.vla_pending in
  ctx.vla_pending <- [];
  pending @ stmts

and declaration_body ctx (d : S.declaration) : T.stmt list =
  match d with
  | S.Static_assert (e, msg) ->
      let v = Const_eval.int_const ctx.env (expr ctx e) in
      if v = 0L then begin
        let msg = match msg with Token.String_lit { bytes; _ } -> bytes | _ -> "" in
        error e.eloc "static assertion failed: %s" msg
      end;
      []
  | S.Decl (specs, decls) ->
      let loc = match decls with d :: _ -> d.decl.dloc | [] -> Loc.none in
      let base = base_type ctx loc specs in
      if decls = [] then begin
        (* 6.7p2: a declaration must declare something, unless it is a tag *)
        (match specs.type_specs with
         | [ (S.Ts_struct _ | S.Ts_union _ | S.Ts_enum _) ] -> ()
         | _ -> Diag.warning loc "useless type name in empty declaration");
        []
      end else
        List.concat_map (fun (idecl : S.init_declarator) -> init_declarator ctx specs base idecl) decls

and init_declarator ctx (specs : S.specifiers) (base : C.t) (idecl : S.init_declarator) : T.stmt list =
  let loc = idecl.decl.dloc in
  let r = apply_declarator ctx base idecl.decl in
  let name = match r.name with Some n -> n | None -> error loc "declaration does not declare anything" in
  let file_scope = Env.at_file_scope ctx.env in
  let ty = r.ty in
  let annotate (sym : T.symbol) =
    (match r.asm_label with Some l -> sym.asm_name <- Some l | None -> ());
    (match alignment ctx loc specs with
     | Some n -> if Option.value sym.align ~default:0 < n then sym.align <- Some n
     | None -> ());
    apply_attributes ctx sym (specs.S.attrs @ r.attrs);
    sym in
  match specs.storage with
  | Some S.Typedef ->
      if idecl.init <> None then error loc "typedef '%s' is initialized" name;
      (match Env.lookup_here ctx.env name with
       | Some (Env.Typedef t) when C.compatible t ty -> () (* 6.7p3: typedef redefinition of the same type is allowed *)
       | Some _ -> error loc "redefinition of '%s'" name
       | None -> Env.declare ctx.env name (Env.Typedef ty));
      []
  | storage ->
      if C.is_function ty then begin
        if idecl.init <> None then error loc "function '%s' is initialized like a variable" name;
        let linkage = match storage with
          | Some S.Static -> T.Internal
          | Some (S.Auto | S.Register) -> error loc "invalid storage class for function '%s'" name
          | _ ->
              (* 6.7.4p7: 'inline' alone is an inline definition, not an
                 external one; we give it internal linkage (see Elab notes) *)
              if List.mem S.Fs_inline specs.funcs && storage <> Some S.Extern && not file_scope then T.External
              else T.External in
        let sym = annotate (redeclare ctx loc name ty (T.Static { linkage; tls = false })) in
        if sym.link.alias <> None then define_global ctx sym None else declare_global ctx sym;
        []
      end else begin
        if ty.u = C.Void then error loc "variable '%s' declared void" name;
        let tls = specs.thread_local in
        let is_static_storage = file_scope || storage = Some S.Static || storage = Some S.Extern || tls in
        if is_static_storage then begin
          let linkage =
            if storage = Some S.Static && file_scope then T.Internal
            else if storage = Some S.Extern || file_scope then T.External
            else T.No_linkage in
          let sym = annotate (redeclare ctx loc name ty (T.Static { linkage; tls })) in
          (match idecl.init with
           | Some init ->
               if storage = Some S.Extern && not file_scope then error loc "'%s' has both 'extern' and initializer" name;
               let init, ty = initialiser ctx sym.ty init in
               sym.ty <- ty;
               check_constant_init ctx init;
               if Hashtbl.mem ctx.globals sym.id && (Hashtbl.find ctx.globals sym.id).ginit <> None then
                 error loc "redefinition of '%s'" name;
               define_global ctx sym (Some init)
           | None ->
               if storage = Some S.Extern then declare_global ctx sym
               else begin
                 (* tentative definition (6.9.2p2), or a static local *)
                 if not (Hashtbl.mem ctx.globals sym.id && (Hashtbl.find ctx.globals sym.id).defined) then
                   define_global ctx sym None
               end);
          if not (Env.is_complete ctx.env sym.ty) && storage <> Some S.Extern && not (file_scope && C.is_array sym.ty) then
            error loc "storage size of '%s' isn't known" name;
          []
        end else begin
          (* automatic storage *)
          if Env.lookup_here ctx.env name <> None then error loc "redeclaration of '%s'" name;
          let sym = annotate (Env.fresh_symbol ctx.env name ty T.Local) in
          Env.declare ctx.env name (Env.Var sym);
          if idecl.init <> None && C.has_vla sym.ty then error loc "variable-sized object '%s' may not be initialized" name;
          let init = Option.map (fun init ->
              let init, ty = initialiser ctx sym.ty init in
              sym.ty <- ty; init) idecl.init in
          if not (Env.is_complete ctx.env sym.ty) then error loc "storage size of '%s' isn't known" name;
          [ T.Decl (sym, init) ]
        end
      end

(* 6.7p4, 6.2.2p2: a redeclaration must be compatible and names the same
   entity; the type becomes the composite type. *)
and redeclare ctx loc name ty storage : T.symbol =
  let prior = if Env.at_file_scope ctx.env then Env.lookup_here ctx.env name else Env.lookup ctx.env name in
  match prior, storage with
  | Some (Env.Var sym), T.Static { linkage; _ } when (match sym.storage with T.Static _ -> true | T.Local -> false) ->
      if not (C.compatible (C.strip sym.ty) (C.strip ty)) then
        error loc "conflicting types for '%s': '%a' after '%a'" name C.pp ty C.pp sym.ty;
      (match sym.storage, linkage with
       | T.Static { linkage = T.Internal; _ }, T.External | T.Static { linkage = T.External; _ }, T.Internal when Env.at_file_scope ctx.env ->
           (* 6.2.2p7 is undefined; be strict except for extern after static (6.2.2p4) *)
           if linkage = T.Internal then error loc "static declaration of '%s' follows non-static declaration" name
       | _ -> ());
      sym.ty <- C.composite sym.ty ty;
      if not (Env.at_file_scope ctx.env) then Env.declare ctx.env name (Env.Var sym);
      sym
  | Some _, _ when Env.lookup_here ctx.env name <> None -> error loc "redeclaration of '%s'" name
  | _ ->
      let sym = Env.fresh_symbol ctx.env name ty storage in
      Env.declare ctx.env name (Env.Var sym);
      sym

(* 6.7.9p4: initializers for objects of static storage duration must be
   constant expressions or string literals. *)
and check_constant_init ctx (init : T.init) =
  match init with
  | T.Init_scalar e ->
      if Const_eval.eval ctx.env e = None then error e.loc "initializer element is not constant"
  | T.Init_agg items -> List.iter (fun (it : T.init_item) -> check_constant_init ctx it.init) items
  | T.Init_string _ -> ()

(* ---- Statements (6.8) ---------------------------------------------------------------- *)

and statement ctx (s : S.stmt) : T.stmt =
  let loc = s.sloc in
  match s.s with
  | S.Label (name, body) ->
      if Hashtbl.mem ctx.labels name then error loc "duplicate label '%s'" name;
      Hashtbl.replace ctx.labels name ();
      T.Label (name, statement ctx body)
  | S.Case (e, body) ->
      (match ctx.switch_types with
       | [] -> error loc "case label not within a switch statement"
       | ty :: _ ->
           let v = Const_eval.normalise ty (Const_eval.int_const ctx.env (expr ctx e)) in
           let seen = List.hd ctx.case_values in
           if List.mem v !seen then error loc "duplicate case value";
           seen := v :: !seen;
           T.Case (v, statement ctx body))
  | S.Default body ->
      if ctx.switch_types = [] then error loc "'default' label not within a switch statement";
      T.Default (statement ctx body)
  | S.Block items ->
      Env.push ctx.env;
      let body = block_items ctx items in
      Env.pop ctx.env;
      T.Block body
  | S.Expr None -> T.Block []
  | S.Expr (Some e) -> T.Expr (rvalue (expr ctx e))
  | S.If (c, a, b) ->
      T.If (scalar_condition ctx (expr ctx c), statement ctx a, Option.map (statement ctx) b)
  | S.Switch (c, body) ->
      let c = promote ctx (expr ctx c) in
      if not (C.is_integer c.ty) then error loc "switch quantity not an integer";
      ctx.switch_types <- c.ty :: ctx.switch_types;
      ctx.case_values <- ref [] :: ctx.case_values;
      ctx.loops <- ctx.loops + 1;
      let body = statement ctx body in
      ctx.loops <- ctx.loops - 1;
      ctx.switch_types <- List.tl ctx.switch_types;
      ctx.case_values <- List.tl ctx.case_values;
      T.Switch (c, body)
  | S.While (c, body) ->
      let c = scalar_condition ctx (expr ctx c) in
      T.While (c, loop_body ctx body)
  | S.Do (body, c) ->
      let body = loop_body ctx body in
      T.Do (body, scalar_condition ctx (expr ctx c))
  | S.For (init, c, step, body) ->
      Env.push ctx.env;
      let init = match init with
        | S.For_none -> None
        | S.For_expr e -> Some (T.Expr (rvalue (expr ctx e)))
        | S.For_decl d ->
            (match d with
             | S.Decl ({ storage = Some (S.Static | S.Extern | S.Typedef); _ }, _) | S.Decl ({ thread_local = true; _ }, _) ->
                 error loc "declaration of non-automatic variable in 'for' loop"
             | _ -> ());
            Some (T.Block (declaration ctx d)) in
      let c = Option.map (fun c -> scalar_condition ctx (expr ctx c)) c in
      let step = Option.map (fun e -> rvalue (expr ctx e)) step in
      let body = loop_body ctx body in
      Env.pop ctx.env;
      T.For (init, c, step, body)
  | S.Goto name -> ctx.gotos <- (name, loc) :: ctx.gotos; T.Goto name
  | S.Continue ->
      if ctx.loops = 0 || ctx.loops = List.length ctx.switch_types && not (in_loop ctx) then
        error loc "continue statement not within a loop";
      T.Continue
  | S.Break -> if ctx.loops = 0 then error loc "break statement not within loop or switch"; T.Break
  | S.Return None ->
      if not (C.is_void ctx.ret_type) then error loc "'return' with no value, in function returning non-void";
      T.Return None
  | S.Return (Some e) ->
      let e = expr ctx e in
      if C.is_void ctx.ret_type then begin
        if not (C.is_void (rvalue e).ty) then error loc "'return' with a value, in function returning void";
        T.Return None
      end else T.Return (Some (assign_convert ctx ~what:"return" e ctx.ret_type))
  | S.Asm a ->
      (* outputs are modifiable lvalues; inputs are values (a memory
         constraint keeps its lvalue, since its address is what the
         assembler sees) *)
      let outputs = List.map (fun (o : S.asm_operand) ->
          let e = expr ctx o.aexpr in
          modifiable_lvalue ctx e;
          (o.oname, o.constr, e)) a.outputs in
      let inputs = List.map (fun (o : S.asm_operand) ->
          let e = expr ctx o.aexpr in
          let e = if String.contains o.constr 'm' then e else rvalue e in
          (o.oname, o.constr, e)) a.inputs in
      T.Asm { T.template = a.template; outputs; inputs; clobbers = a.clobbers }

and in_loop ctx = ctx.loops > List.length ctx.switch_types

and loop_body ctx body =
  ctx.loops <- ctx.loops + 1;
  (* continue must know it is in a loop, not just a switch; we track
     loops and switches together and separate them in [in_loop] *)
  let saved = ctx.switch_types in
  ctx.switch_types <- [];
  let saved_cases = ctx.case_values in
  ctx.case_values <- [];
  let b = statement ctx body in
  ctx.switch_types <- saved;
  ctx.case_values <- saved_cases;
  ctx.loops <- ctx.loops - 1;
  b

and block_items ctx (items : S.block_item list) : T.stmt list =
  List.concat_map (function
      | S.Item_decl d -> declaration ctx d
      | S.Item_stmt s -> [ statement ctx s ]) items

(* ---- Functions (6.9.1) --------------------------------------------------------------- *)

and function_definition ctx (f : S.func_def) =
  let loc = f.floc in
  let base = base_type ctx loc f.fspecs in
  Env.push ctx.env; (* the parameters' scope, which is the body's block *)
  let r = apply_declarator ctx base f.fdecl in
  let name = match r.name with Some n -> n | None -> error loc "function definition without a name" in
  let fty = match r.ty.u with C.Func ft -> ft | _ -> error loc "'%s' is declared as a non-function but defined as one" name in
  (* K&R definitions: parameter types come from the declarations after the declarator *)
  let fty, params =
    match r.kr_params with
    | Some names ->
        List.iter (fun d -> ignore (declaration ctx d)) f.kr_decls;
        let params = List.map (fun n ->
            match Env.lookup_here ctx.env n with
            | Some (Env.Var sym) -> sym
            | _ -> let sym = Env.fresh_symbol ctx.env n C.int T.Local in Env.declare ctx.env n (Env.Var sym); sym) names in
        { fty with params = None }, params
    | None ->
        (* the prototype scope is gone; declare the parameters in the body's scope *)
        let params = List.map (fun (p : C.param) ->
            match p.pname with
            | Some n ->
                let sym = Env.fresh_symbol ctx.env n p.ptype T.Local in
                Env.declare ctx.env n (Env.Var sym); sym
            | None -> error loc "parameter name omitted in definition of '%s'" name) (Option.value fty.params ~default:[]) in
        fty, params in
  List.iter (fun (p : T.symbol) ->
      if not (Env.is_complete ctx.env p.ty) then error loc "parameter '%s' has incomplete type '%a'" p.name C.pp p.ty) params;
  if not (C.is_void fty.ret) && not (Env.is_complete ctx.env fty.ret) then
    error loc "return type of '%s' is incomplete ('%a')" name C.pp fty.ret;
  let ty = C.unqualified (C.Func fty) in
  let is_inline = List.mem S.Fs_inline f.fspecs.funcs in
  let linkage = match f.fspecs.storage with
    | Some S.Static -> T.Internal
    | Some (S.Auto | S.Register | S.Typedef) -> error loc "invalid storage class for function '%s'" name
    | None when is_inline -> T.Internal (* an inline definition is not an external one, 6.7.4p7 *)
    | _ -> T.External in
  (* the function's name belongs to the enclosing scope, so that it is
     visible for recursion but does not clash with a parameter *)
  let sym = Env.in_enclosing_scope ctx.env (fun () ->
      redeclare ctx loc name ty (T.Static { linkage; tls = false })) in
  (match Hashtbl.find_opt ctx.globals sym.id with
   | Some { T.defined = true; _ } when List.exists (fun (fn : T.func) -> fn.fsym.id = sym.id) ctx.funcs -> error loc "redefinition of '%s'" name
   | _ -> ());
  Hashtbl.replace ctx.globals sym.id { T.gsym = sym; ginit = None; defined = true };
  if not (List.exists (fun s -> s.T.id = sym.id) ctx.global_order) then ctx.global_order <- sym :: ctx.global_order;
  ctx.ret_type <- fty.ret;
  ctx.func_name <- name;
  Hashtbl.reset ctx.labels;
  ctx.gotos <- [];
  let body = match f.body.s with S.Block items -> T.Block (block_items ctx items) | _ -> assert false in
  List.iter (fun (l, loc) -> if not (Hashtbl.mem ctx.labels l) then error loc "label '%s' used but not defined" l) ctx.gotos;
  Env.pop ctx.env;
  ctx.funcs <- { T.fsym = sym; params; body; inline = is_inline && linkage = T.Internal; loc } :: ctx.funcs


(* ---- Translation unit (6.9) ---------------------------------------------------------- *)

let translation_unit (tu : S.translation_unit) : Env.t * T.translation_unit =
  let ctx = {
    env = Env.create (); globals = Hashtbl.create 64; global_order = []; funcs = [];
    ret_type = C.void; labels = Hashtbl.create 16; gotos = []; switch_types = []; case_values = [];
    loops = 0; func_name = ""; vla_sizes = []; vla_pending = []; in_type_name = false; vla_count = 0 } in
  Env.declare ctx.env "__builtin_va_list"
    (Env.Typedef (C.array (C.unqualified (C.Struct (Env.new_tag ctx.env `Struct (Some "__va_list_tag")).tag)) (Some 1)));
  (* the va_list tag: struct { unsigned gp_offset, fp_offset; void *overflow_arg_area, *reg_save_area; } (ABI 3.5.7) *)
  (match Env.lookup ctx.env "__builtin_va_list" with
   | Some (Env.Typedef { u = C.Array ({ u = C.Struct tag; _ }, _); _ }) ->
       let info = Env.tag_info ctx.env tag in
       info.layout <- Some (Env.layout_struct ctx.env ~is_union:false
                              [ Some "gp_offset", C.uint, None, None; Some "fp_offset", C.uint, None, None;
                                Some "overflow_arg_area", C.pointer C.void, None, None; Some "reg_save_area", C.pointer C.void, None, None ] Loc.none)
   | _ -> assert false);
  let asm_blocks = ref [] in
  List.iter (function
      | S.Ext_decl d -> ignore (declaration ctx d)
      | S.Ext_func f -> function_definition ctx f
      | S.Ext_asm text -> asm_blocks := text :: !asm_blocks) tu;
  let globals = List.rev_map (fun (s : T.symbol) -> Hashtbl.find ctx.globals s.id) ctx.global_order in
  (* functions are in [funcs]; drop their placeholder globals *)
  (* function definitions live in [funcs]; drop their placeholder globals,
     but keep function aliases, which have no body of their own *)
  let globals = List.filter (fun (g : T.global) -> not (C.is_function g.gsym.ty && g.defined && g.gsym.link.alias = None)) globals in
  ctx.env, { T.funcs = List.rev ctx.funcs; globals; asm_blocks = List.rev !asm_blocks; vla_sizes = ctx.vla_sizes }
