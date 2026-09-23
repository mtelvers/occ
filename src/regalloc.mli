(** Linear-scan register allocation over the IR, which is the same job on
    both machines: they supply their registers and what their selected
    code writes, and everything else is read off the IR. *)

type 'r location = Register of 'r | Spill of int

type 'r assignment = { where : (int, 'r location) Hashtbl.t; spill_slots : int; used : 'r list }

val regs_of_instr : Ir.instr -> int list * int list
(** the virtual registers an instruction defines, and those it uses *)

val allocate :
  callee_saved:'r list ->
  caller_saved:'r list ->
  clobbers:(Ir.instr -> 'r list) ->
  name:('r -> string) ->
  Ir.func -> 'r assignment
(** [allocate ~callee_saved ~caller_saved ~clobbers ~name f] places each
    of [f]'s virtual registers in one of those physical registers or in a
    spill slot.  [clobbers] says which registers an instruction's
    selected code may write for itself, so that a value living across it
    is not put there; [name] is for the dump [OCC_DUMP_ALLOC] asks for. *)
