(* A readable tree rendering of [Syntax] for --dump=ast.  It shows the
   structure the parser built, not C syntax, so declarator nesting and the
   specifier bag are visible as parsed. *)

open Syntax

let p ppf fmt = Format.fprintf ppf fmt

let unop = function
  | Neg -> "-" | Not -> "~" | Lnot -> "!" | Addr -> "&" | Deref -> "*"
  | Preinc -> "++pre" | Predec -> "--pre" | Postinc -> "post++" | Postdec -> "post--"

let binop = function
  | Mul -> "*" | Div -> "/" | Mod -> "%" | Add -> "+" | Sub -> "-" | Shl -> "<<" | Shr -> ">>"
  | Lt -> "<" | Gt -> ">" | Le -> "<=" | Ge -> ">=" | Eq -> "==" | Ne -> "!="
  | Band -> "&" | Bxor -> "^" | Bor -> "|" | Land -> "&&" | Lor -> "||" | Comma -> ","

let storage = function
  | Typedef -> "typedef" | Extern -> "extern" | Static -> "static" | Auto -> "auto" | Register -> "register"

let qualifier = function Q_const -> "const" | Q_restrict -> "restrict" | Q_volatile -> "volatile" | Q_atomic -> "_Atomic"

let rec type_spec ppf = function
  | Ts_void -> p ppf "void" | Ts_char -> p ppf "char" | Ts_short -> p ppf "short" | Ts_int -> p ppf "int"
  | Ts_long -> p ppf "long" | Ts_float -> p ppf "float" | Ts_double -> p ppf "double"
  | Ts_signed -> p ppf "signed" | Ts_unsigned -> p ppf "unsigned" | Ts_bool -> p ppf "_Bool" | Ts_complex -> p ppf "_Complex"
  | Ts_atomic t -> p ppf "_Atomic(%a)" type_name t
  | Ts_struct s -> p ppf "struct %s%s" (Option.value s.tag ~default:"<anon>") (if s.members = None then "" else "{...}")
  | Ts_union s -> p ppf "union %s%s" (Option.value s.tag ~default:"<anon>") (if s.members = None then "" else "{...}")
  | Ts_enum e -> p ppf "enum %s%s" (Option.value e.etag ~default:"<anon>") (if e.enumerators = None then "" else "{...}")
  | Ts_typedef_name n -> p ppf "typedef-name %s" n
  | Ts_typeof e -> p ppf "typeof(%a)" expr e

and specifiers ppf (sp : specifiers) =
  let words =
    Option.to_list (Option.map storage sp.storage)
    @ (if sp.thread_local then [ "_Thread_local" ] else [])
    @ List.map qualifier sp.quals
    @ List.map (function Fs_inline -> "inline" | Fs_noreturn -> "_Noreturn") sp.funcs in
  p ppf "[%s%s%a]" (String.concat " " words) (if words = [] then "" else " ")
    (Format.pp_print_list ~pp_sep:(fun ppf () -> p ppf " ") type_spec) sp.type_specs

and declarator ppf (d : declarator) =
  match d.d with
  | D_ident s -> p ppf "%s" s
  | D_abstract -> p ppf "<abstract>"
  | D_pointer (qs, inner) -> p ppf "(pointer%s %a)" (String.concat "" (List.map (fun q -> " " ^ qualifier q) qs)) declarator inner
  | D_array (inner, _, size, _) -> p ppf "(array %a%a)" declarator inner (fun ppf -> function None -> () | Some e -> p ppf " %a" expr e) size
  | D_func (inner, params, variadic) ->
      p ppf "(function %a (%a%s))" declarator inner
        (Format.pp_print_list ~pp_sep:(fun ppf () -> p ppf ", ") (fun ppf pr -> p ppf "%a %a" specifiers pr.pspecs declarator pr.pdecl)) params
        (if variadic then ", ..." else "")
  | D_ident_list (inner, names) -> p ppf "(function %a (%s) K&R)" declarator inner (String.concat ", " names)
  | D_attr (inner, attrs) -> p ppf "(attributes [%s] %a)" (String.concat ", " (List.map (fun a -> a.aname) attrs)) declarator inner
  | D_asm_label (inner, l) -> p ppf "(asm-label %S %a)" l declarator inner

and type_name ppf (t : type_name) = p ppf "%a %a" specifiers t.tspecs declarator t.tdecl

and expr ppf (e : expr) =
  match e.e with
  | Ident s -> p ppf "%s" s
  | Const t -> Token.pp ppf t
  | String (s, _) -> p ppf "%S" s
  | Unop (op, x) -> p ppf "(%s %a)" (unop op) expr x
  | Binop (op, a, b) -> p ppf "(%s %a %a)" (binop op) expr a expr b
  | Assign (None, l, r) -> p ppf "(= %a %a)" expr l expr r
  | Assign (Some op, l, r) -> p ppf "(%s= %a %a)" (binop op) expr l expr r
  | Cond (c, a, b) -> p ppf "(?: %a %a %a)" expr c expr a expr b
  | Call (f, args) -> p ppf "(call %a%a)" expr f (fun ppf -> List.iter (fun a -> p ppf " %a" expr a)) args
  | Index (a, i) -> p ppf "(index %a %a)" expr a expr i
  | Member (x, f) -> p ppf "(. %a %s)" expr x f
  | Arrow (x, f) -> p ppf "(-> %a %s)" expr x f
  | Cast (t, x) -> p ppf "(cast %a %a)" type_name t expr x
  | Sizeof_expr x -> p ppf "(sizeof %a)" expr x
  | Sizeof_type t -> p ppf "(sizeof-type %a)" type_name t
  | Alignof t -> p ppf "(_Alignof %a)" type_name t
  | Compound_literal (t, i) -> p ppf "(compound-literal %a %a)" type_name t initialiser i
  | Generic (c, assocs) ->
      p ppf "(_Generic %a%a)" expr c
        (fun ppf -> List.iter (fun (t, e) -> match t with
             | Some t -> p ppf " [%a: %a]" type_name t expr e
             | None -> p ppf " [default: %a]" expr e)) assocs
  | Va_arg (ap, t) -> p ppf "(va_arg %a %a)" expr ap type_name t
  | Offsetof (t, _) -> p ppf "(offsetof %a ...)" type_name t

and initialiser ppf (i : initialiser) =
  match i.i with
  | Init_expr e -> expr ppf e
  | Init_list items ->
      p ppf "{%a}" (Format.pp_print_list ~pp_sep:(fun ppf () -> p ppf ", ")
                      (fun ppf (des, init) ->
                         List.iter (function Field f -> p ppf ".%s" f | Subscript e -> p ppf "[%a]" expr e) des;
                         if des <> [] then p ppf " = ";
                         initialiser ppf init)) items

let rec stmt indent ppf (s : stmt) =
  let ind = String.make indent ' ' in
  let sub = stmt (indent + 2) in
  match s.s with
  | Label (l, body) -> p ppf "%s%s:@.%a" ind l sub body
  | Case (e, body) -> p ppf "%scase %a:@.%a" ind expr e sub body
  | Default body -> p ppf "%sdefault:@.%a" ind sub body
  | Block items -> p ppf "%sblock@.%a" ind (fun ppf -> List.iter (block_item (indent + 2) ppf)) items
  | Expr None -> p ppf "%s;@." ind
  | Expr (Some e) -> p ppf "%s%a@." ind expr e
  | If (c, a, b) ->
      p ppf "%sif %a@.%a" ind expr c sub a;
      (match b with Some b -> p ppf "%selse@.%a" ind sub b | None -> ())
  | Switch (c, body) -> p ppf "%sswitch %a@.%a" ind expr c sub body
  | While (c, body) -> p ppf "%swhile %a@.%a" ind expr c sub body
  | Do (body, c) -> p ppf "%sdo@.%a%swhile %a@." ind sub body ind expr c
  | For (init, c, step, body) ->
      p ppf "%sfor (%a; %a; %a)@.%a" ind
        (fun ppf -> function For_none -> () | For_expr e -> expr ppf e | For_decl d -> declaration 0 ppf d) init
        (fun ppf -> function None -> () | Some e -> expr ppf e) c
        (fun ppf -> function None -> () | Some e -> expr ppf e) step
        sub body
  | Goto l -> p ppf "%sgoto %s@." ind l
  | Continue -> p ppf "%scontinue@." ind
  | Break -> p ppf "%sbreak@." ind
  | Return None -> p ppf "%sreturn@." ind
  | Return (Some e) -> p ppf "%sreturn %a@." ind expr e
  | Asm text -> p ppf "%sasm %S@." ind text

and block_item indent ppf = function
  | Item_decl d -> declaration indent ppf d
  | Item_stmt s -> stmt indent ppf s

and declaration indent ppf (d : declaration) =
  let ind = String.make indent ' ' in
  match d with
  | Static_assert (e, _) -> p ppf "%s_Static_assert %a@." ind expr e
  | Decl (sp, decls) ->
      p ppf "%sdeclare %a" ind specifiers sp;
      List.iter (fun (idecl : init_declarator) ->
          p ppf " %a" declarator idecl.decl;
          (match idecl.init with Some i -> p ppf " = %a" initialiser i | None -> ())) decls;
      p ppf "@."

let translation_unit ppf (tu : translation_unit) =
  List.iter (function
      | Ext_decl d -> declaration 0 ppf d
      | Ext_func f ->
          p ppf "function %a %a@." specifiers f.fspecs declarator f.fdecl;
          List.iter (declaration 2 ppf) f.kr_decls;
          stmt 2 ppf f.body) tu
