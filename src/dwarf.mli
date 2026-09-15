(** DWARF 4 debugging information: the part that is not machine
    knowledge.  The assembler builds .debug_line from .file and .loc and
    .eh_frame from the .cfi directives; this writes the tree of entries,
    which is the same on every machine. *)

type ty =
  | Void
  | Base of string * int * int  (** name, DW_ATE encoding, byte size *)
  | Pointer  (** to void: pointee types are not described *)
  | Struct of string
  | Union of string

type location =
  | At_cfa_offset of int  (** DW_OP_fbreg *)
  | In_register of int  (** DW_OP_regN, by the machine's DWARF numbering *)

type param = { pname : string; ptype : ty; ploc : location }

type func = { dfile : int; dline : int; dparams : param list; dret : ty }

type subprogram = { sname : string; sglobal : bool; sinfo : func }

val of_ctype : Ctype.t -> ty
(** What DWARF is told about a C type: scalars exactly, aggregates by name. *)

val escape : string -> string
(** A string as the assembler wants to see it. *)

val info : Format.formatter -> subprogram list -> source:string -> unit
(** Print .debug_info and .debug_abbrev for these functions, and the
    label the line table is referred to by. *)
