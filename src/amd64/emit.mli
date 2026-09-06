(** Print an [Asm.program] in GNU as syntax.  The only module that knows
    the assembler's spelling. *)

val program : Format.formatter -> Asm.program -> unit
