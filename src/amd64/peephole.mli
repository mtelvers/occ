(** Peephole cleanup of the selected code.

    Selection keeps every value in its frame slot or register and loads
    operands into fixed scratch registers, so a value computed by one
    instruction and consumed by the next is stored to its slot and read
    straight back.  This pass removes that reload, and the store too when
    no other instruction reads the slot.  It looks only at adjacent
    instructions, never across a label, so nothing about control flow is
    assumed. *)

val func : Asm.func -> Asm.func
