(** Print an [Asm.program] in GNU as syntax.  The only module that knows
    the assembler's spelling. *)

val program : Format.formatter -> Asm.program -> unit

val reg : Asm.width -> Asm.reg -> string
(** The spelling of a register at a width, e.g. [reg L RAX] is ["%eax"];
    inline assembly operands are substituted with it. *)
