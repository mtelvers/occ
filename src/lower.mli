(** [Typed] to [Ir]: statements become labels and branches, lvalues become
    addresses, short-circuit operators become control flow, aggregates
    become stack slots and memcpy, static initializers become data. *)

val program : source:string -> Env.t -> Typed.translation_unit -> Ir.program
(** [source] names the translation unit for the debug information. *)
