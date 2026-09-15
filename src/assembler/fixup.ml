(* What an instruction or a data item leaves for the assembler to fill in.

   Anything that refers to a symbol is encoded as zero bits plus a fixup
   saying where the value goes and how it is to be measured.  After
   layout each one is either patched with a value the assembler knows or
   turned into a relocation for the linker.

   This is the assembler's own notion rather than a machine's, which is
   why it is here and not beside either encoder -- the line table and the
   frame tables leave fixups too. *)

open Gas

(* Which part of an instruction the value goes into.  x86-64 patches
   whole bytes, so [size] says everything; RISC-V cuts an immediate
   across an instruction's fields, and which field it is also decides
   which relocation names the symbol. *)
type field =
  | Whole                 (* [size] bytes as they lie: x86-64, and all data *)
  | Rv_hi20               (* lui or auipc: the top twenty bits *)
  | Rv_lo12_i             (* addi or a load: the low twelve, in the I-type field *)
  | Rv_lo12_s             (* a store: the same twelve bits, split in two *)
  | Rv_branch             (* B-type: a signed thirteen-bit displacement *)
  | Rv_jal                (* J-type: a signed twenty-one-bit displacement *)
  | Rv_call               (* auipc and jalr together: the pair takes one relocation *)

type t = {
  at : int;              (* offset of the field within the instruction *)
  size : int;            (* 1, 2, 4 or 8 bytes, for a whole-byte field *)
  target : expr;         (* symbol plus offset; its @modifier picks the relocation *)
  pcrel : bool;          (* relative to [pcbase] (rip-relative, a call) *)
  pcbase : int;          (* offset the value is relative to: the end of the instruction *)
  signed : bool;         (* a 32-bit absolute value is sign-extended (R_X86_64_32S) *)
  relaxable : bool;      (* a GOTPCREL load the linker may relax to a direct reference *)
  branch : bool;         (* the target of a call or jump: relocates as PLT32 *)
  field : field;
}

let make ?(pcrel = false) ?(pcbase = 0) ?(signed = false) ?(relaxable = false)
    ?(branch = false) ?(field = Whole) ~at ~size target =
  { at; size; target; pcrel; pcbase; signed; relaxable; branch; field }
