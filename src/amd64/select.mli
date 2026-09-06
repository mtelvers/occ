(** Instruction selection: [Ir] to [Asm].

    Chapter-one code generation: every IR virtual register lives in an
    8-byte frame slot and each instruction loads its operands into fixed
    scratch registers (rax, rcx, rdx and xmm0, xmm1), computes, and stores
    the result back.  The calling convention (registers, stack arguments,
    aggregates by eightbyte class, varargs register save area) is applied
    here.  [pic] selects position-independent addressing for shared objects. *)

val program : pic:bool -> debug:bool -> Ir.program -> Asm.program
(** [debug] adds line-table directives and the per-function records that
    [Emit] turns into DWARF.  Unwind information (CFI) is always emitted. *)
