(** Print an RV64 [Asm.program] in GNU as syntax.  The only module that
    knows the assembler's spelling. *)

val program : Format.formatter -> Asm.program -> unit

val reg : Asm.reg -> string
(** The ABI's name for a register, ["a0"] and so on. *)

val reg_of_name : string -> Asm.reg option
(** The register of that name, by the ABI's naming (["a0"], ["s2"]) or
    the hardware's (["x10"], ["f3"]); [None] if there is none.  Inline
    assembly names registers this way. *)
