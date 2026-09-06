(** Register allocation by linear scan (Poletto and Sarkar, 1999).

    Every IR register has a live interval: from its definition to its
    last use, as positions in the instruction list.  This is exact for
    the IR [Lower] produces, where a register is defined before any use in
    program order and loop-carried values live in C variables, never in
    registers.  Intervals are visited in order of their start; each takes
    a free physical register if one exists, otherwise the interval that
    ends last among those competing for one is spilled to a frame slot.
    Spilled intervals that do not overlap share a slot.

    The callee-saved general registers ([rbx], [r12] to [r15]) can hold
    any interval: values in them survive calls.  A caller-saved register
    ([rsi], [rdi], [r8], [r9]) can hold an interval only if no instruction
    inside it clobbers that register, which [clobbers] decides from a short
    table (calls clobber them all, block copies use [rdi] and [rsi], and
    so on).  Every floating-point register is caller-saved on x86-64, so
    floating-point values are always spilled.  The registers are fewer
    than a production allocator would use, and that is the point: the
    whole algorithm is a page, and the code generator only has to ask
    where a value lives. *)

type location =
  | Register of Asm.reg
  | Spill of int (** index of an 8-byte frame slot; slots are shared over time *)

type assignment = {
  where : (int, location) Hashtbl.t; (** by IR register *)
  spill_slots : int; (** number of slots needed *)
  used : Asm.reg list; (** callee-saved registers to preserve in the prologue *)
}

val allocate : Ir.func -> assignment

val regs_of_instr : Ir.instr -> int list * int list
(** The registers an instruction defines and uses. *)
