(** Scopes and tags (6.2.1, 6.2.3), and the layout of aggregates.

    Ordinary identifiers (objects, functions, typedef names, enumeration
    constants) live in one name space; struct, union and enum tags in
    another.  Both are block scoped.  Labels have their own name space per
    function and are handled in [Elab]. *)

type entry =
  | Var of Typed.symbol
  | Typedef of Ctype.t
  | Enum_const of int64 * Ctype.t

type field = Ctype.field = { fname : string option; ftype : Ctype.t; offset : int; bits : (int * int) option }

type layout = { fields : field list; size : int; align : int }

type tag_info = {
  tag : Ctype.tag;
  kind : [ `Struct | `Union | `Enum ];
  mutable layout : layout option; (** [None] while incomplete *)
  mutable underlying : Ctype.t option; (** enums only *)
}

type t

val create : unit -> t
val push : t -> unit
val pop : t -> unit
val at_file_scope : t -> bool

val in_enclosing_scope : t -> (unit -> 'a) -> 'a
(** Run [f] with the innermost scope temporarily removed: a function's
    name is declared in the scope enclosing its parameters (6.2.1p4). *)

val declare : t -> string -> entry -> unit
val lookup : t -> string -> entry option
val lookup_here : t -> string -> entry option
(** Only the innermost scope, for redeclaration checks (6.7p3). *)

val fresh_symbol : t -> string -> Ctype.t -> Typed.storage -> Typed.symbol

val declare_tag : t -> string -> tag_info -> unit
val lookup_tag : t -> string -> tag_info option
val lookup_tag_here : t -> string -> tag_info option
val new_tag : t -> [ `Struct | `Union | `Enum ] -> string option -> tag_info
val tag_info : t -> Ctype.tag -> tag_info

val size_align : t -> Ctype.t -> (int * int) option
(** Size and alignment of a complete object type, [None] if incomplete. *)

val size_of : t -> Loc.t -> Ctype.t -> int
(** As [size_align], raising a diagnostic on an incomplete type (6.5.3.4p1). *)

val align_of : t -> Loc.t -> Ctype.t -> int
val is_complete : t -> Ctype.t -> bool

val layout_struct : t -> is_union:bool -> (string option * Ctype.t * int option * int option) list -> Loc.t -> layout
(** Lay out members (name, type, bit-field width, _Alignas) per the System V rules. *)

val find_field : t -> Ctype.t -> string -> (field list) option
(** Path of fields to a named member, descending through anonymous
    structs and unions (6.7.2.1p13). *)
