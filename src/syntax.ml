(* The abstract syntax produced by the parser: C11 6.5 to 6.9 as written,
   before any typing.  Declarations keep the specifier/declarator split of
   6.7 because that is the shape the grammar has; [Elab] turns them into
   named, typed entities. *)

type loc = Loc.t

type unop = Neg | Not | Lnot | Addr | Deref | Preinc | Predec | Postinc | Postdec

type binop =
  | Mul | Div | Mod | Add | Sub | Shl | Shr
  | Lt | Gt | Le | Ge | Eq | Ne
  | Band | Bxor | Bor | Land | Lor
  | Comma

type expr = { e : expr_desc; eloc : loc }

and expr_desc =
  | Ident of string
  | Const of Token.t (* Int_const, Float_const, Char_const *)
  | String of string * Token.encoding (* already concatenated, phase 6 *)
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Assign of binop option * expr * expr (* [Some op] for compound assignment *)
  | Cond of expr * expr * expr
  | Call of expr * expr list
  | Index of expr * expr
  | Member of expr * string
  | Arrow of expr * string
  | Cast of type_name * expr
  | Sizeof_expr of expr
  | Sizeof_type of type_name
  | Alignof of type_name
  | Compound_literal of type_name * initialiser
  | Generic of expr * (type_name option * expr) list (* 6.5.1.1 *)
  (* extensions: builtins whose arguments include a type, so they cannot
     be ordinary calls.  Other __builtin_* names are plain [Call]s. *)
  | Va_arg of expr * type_name
  | Offsetof of type_name * designator list

and initialiser = { i : init_desc; iloc : loc }

and init_desc =
  | Init_expr of expr
  | Init_list of (designator list * initialiser) list (* 6.7.9 *)

and designator = Field of string | Subscript of expr

(* 6.7.1 to 6.7.5, collected as a bag the way the grammar allows. *)
and specifiers = {
  storage : storage option;
  thread_local : bool; (* 6.7.1p2: may accompany static or extern *)
  type_specs : type_spec list;
  quals : qualifier list;
  funcs : func_spec list;
  align : alignment option;
  attrs : attribute list;
}

and storage = Typedef | Extern | Static | Auto | Register

and type_spec =
  | Ts_void | Ts_char | Ts_short | Ts_int | Ts_long | Ts_float | Ts_double
  | Ts_signed | Ts_unsigned | Ts_bool | Ts_complex
  | Ts_atomic of type_name
  | Ts_struct of struct_spec
  | Ts_union of struct_spec
  | Ts_enum of enum_spec
  | Ts_typedef_name of string
  | Ts_typeof of expr

and qualifier = Q_const | Q_restrict | Q_volatile | Q_atomic

and func_spec = Fs_inline | Fs_noreturn

and alignment = Align_type of type_name | Align_expr of expr

and struct_spec = { tag : string option; members : member list option }

and member = {
  mspecs : specifiers;
  mdecls : (declarator option * expr option) list; (* bit-field width *)
}

and enum_spec = { etag : string option; enumerators : (string * expr option) list option }

(* 6.7.6.  A declarator is read inside-out: [Pointer (Array (Ident x))]
   is "*x[]".  Keeping the grammar's shape here and inverting it in
   [Elab] is the textbook approach and matches how 6.7.6.2 is worded. *)
and declarator = { d : declarator_desc; dloc : loc }

and declarator_desc =
  | D_ident of string
  | D_abstract
  | D_pointer of qualifier list * declarator
  | D_array of declarator * qualifier list * expr option * bool (* static *)
  | D_func of declarator * param_decl list * bool (* variadic *)
  | D_ident_list of declarator * string list (* K&R identifier list *)
  | D_attr of declarator * attribute list
  | D_asm_label of declarator * string

and param_decl = { pspecs : specifiers; pdecl : declarator }

and type_name = { tspecs : specifiers; tdecl : declarator }

and attribute = { aname : string; aargs : expr list }

and init_declarator = { decl : declarator; init : initialiser option }

and declaration =
  | Decl of specifiers * init_declarator list
  | Static_assert of expr * Token.t (* string literal *)

and stmt = { s : stmt_desc; sloc : loc }

and stmt_desc =
  | Label of string * stmt
  | Case of expr * stmt
  | Default of stmt
  | Block of block_item list
  | Expr of expr option
  | If of expr * stmt * stmt option
  | Switch of expr * stmt
  | While of expr * stmt
  | Do of stmt * expr
  | For of for_init * expr option * expr option * stmt
  | Goto of string
  | Continue
  | Break
  | Return of expr option
  (* extension: GNU inline assembly, basic or with operands *)
  | Asm of asm

and asm = {
  template : string;
  outputs : asm_operand list;
  inputs : asm_operand list;
  clobbers : string list;
  volatile : bool;
}

and asm_operand = { oname : string option; constr : string; aexpr : expr }

and for_init = For_none | For_expr of expr | For_decl of declaration

and block_item = Item_decl of declaration | Item_stmt of stmt

and func_def = {
  fspecs : specifiers;
  fdecl : declarator;
  kr_decls : declaration list; (* K&R parameter declarations *)
  body : stmt;
  floc : loc;
}

and external_decl = Ext_decl of declaration | Ext_func of func_def | Ext_asm of string (* file-scope asm *)

type translation_unit = external_decl list
