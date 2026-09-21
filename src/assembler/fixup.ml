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
  | Rv_none               (* a relocation that names an instruction and changes no bits *)

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

(* One encoding of a branch the assembler may have to lengthen: its
   bytes, the fixups in them, and how many bits of signed displacement it
   can reach.  A branch to a nearby label is written in the short form
   and, when layout shows the target is out of reach, in the long one --
   x86-64's pair is a one-byte displacement and a four-byte one, RISC-V's
   a conditional branch and an inverted branch around a jump. *)
type form = { fbytes : string; ffixups : t list; fbits : int }

let fits_signed bits v =
  bits >= 64 || (let half = Int64.shift_left 1L (bits - 1) in
                 Int64.compare v (Int64.neg half) >= 0 && Int64.compare v half < 0)

(* What an encoder makes of one instruction: bytes with fixups in them,
   or, for a branch, the two forms to choose between after layout. *)
type result =
  | Fixed of string * t list
  | Relaxable of { short : form; long : form }

(* An instruction neither encoder can make sense of. *)
exception Bad of string

let bad fmt = Printf.ksprintf (fun s -> raise (Bad s)) fmt
