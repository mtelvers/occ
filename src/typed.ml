(* The typed abstract syntax produced by elaboration.

   Every expression carries its type; every conversion of 6.3 that the
   standard calls implicit is an explicit [Convert] node, including the
   array-to-pointer and function-to-pointer conversions of 6.3.2.1.  The
   one thing left to context is lvalue conversion (6.3.2.1p2): an
   expression marked [lvalue] denotes an object, and whether the object's
   address or its value is wanted is decided by the node using it.  Names
   are resolved to unique symbols; scopes are gone. *)

type symbol = {
  id : int;
  name : string;
  mutable ty : Ctype.t;
  storage : storage;
  mutable align : int option; (* _Alignas (6.7.5), or __attribute__((aligned)) *)
  mutable asm_name : string option; (* __asm__("label") on the declaration *)
}

and storage =
  | Local (* automatic storage duration, 6.2.4p5 *)
  | Static of { linkage : linkage; tls : bool } (* static or thread storage *)

and linkage = External | Internal | No_linkage (* 6.2.2 *)

type expr = { e : expr_desc; ty : Ctype.t; lvalue : bool; loc : Loc.t }

and expr_desc =
  | Var of symbol
  | Int of int64 (* normalised to [ty], two's complement in 64 bits *)
  | Float of float
  | String of string (* bytes; wide strings are UTF-8 to be re-encoded per element *)
  | Convert of expr (* 6.3: to [ty] *)
  | Unop of Syntax.unop * expr
  | Binop of Syntax.binop * expr * expr (* on pointers: scaled by the pointee size in Lower *)
  | Assign of expr * expr
  | Compound_assign of Syntax.binop * expr * expr * Ctype.t (* computation type *)
  | Cond of expr * expr * expr
  | Call of expr * expr list (* callee is a pointer to function *)
  | Deref of expr
  | Addr of expr
  | Member of expr * Ctype.field (* [expr] is an lvalue of struct/union type *)
  | Compound_literal of symbol * init (* a fresh object initialised in place *)
  | Va_arg of expr * Ctype.t
  | Builtin of string * expr list (* doc/extensions.md *)
  | Atomic_op of atomic_op * Ir.memory_order list * expr list

(* 7.17.7, as produced by the <stdatomic.h> in include/. *)
and atomic_op = Load | Store | Exchange | Compare_exchange of bool | Fetch of Syntax.binop | Fence | Signal_fence

and init =
  | Init_scalar of expr
  | Init_agg of init_item list (* leaves at absolute byte offsets; the rest is zero *)
  | Init_string of string (* string literal bytes for an array of char/wchar_t *)

(* [bits] as in [Env.field]: a bit-field leaf.  Items are in source order,
   so on overlap (a union initialised twice) the later one wins. *)
and init_item = { off : int; ity : Ctype.t; bits : (int * int) option; init : init }

type stmt =
  | Expr of expr
  | Decl of symbol * init option (* a local with automatic storage *)
  | Block of stmt list
  | If of expr * stmt * stmt option
  | Switch of expr * stmt
  | Case of int64 * stmt
  | Default of stmt
  | While of expr * stmt
  | Do of stmt * expr
  | For of stmt option * expr option * expr option * stmt
  | Label of string * stmt
  | Goto of string
  | Continue
  | Break
  | Return of expr option
  | Asm of string

type func = { fsym : symbol; params : symbol list; body : stmt; inline : bool; loc : Loc.t }
(** [inline]: an inline definition (6.7.4p7); it is emitted only if
    something in the translation unit refers to it, as gcc does for
    [static inline], so that unused header functions do not drag in
    their callees at link time. *)

type global = { gsym : symbol; ginit : init option; defined : bool }
(** [defined = false] is a declaration only (extern, or an undefined
    function); [ginit = None] with [defined] is a tentative definition
    zero-initialised (6.9.2p2). *)

type translation_unit = { funcs : func list; globals : global list }
