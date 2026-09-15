(* Instruction selection for RV64: [Ir] to [Asm].

   Not yet written.  The driver reaches here when it is asked for RISC-V
   code, and says so plainly rather than producing something wrong. *)

let program ~pic ~debug (_ : Ir.program) : Asm.program =
  ignore pic; ignore debug;
  failwith "the RISC-V back end is not written yet"
