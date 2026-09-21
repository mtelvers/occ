(* An abstract syntax for RV64 assembly in the spelling GNU as accepts.

   As on the other machine, the code generator builds this rather than
   printing strings, so that [Emit] is the only place that knows how the
   assembler spells things.  What differs from x86-64 is the shape of the
   machine rather than the shape of this file: every instruction is a
   fixed 32 bits, the only addressing mode is a register plus a signed
   twelve-bit offset, and arithmetic is three-address.

   The register names are the ABI's (RISC-V calling convention), not the
   hardware's x0..x31: sp, ra, a0..a7, t0..t6, s0..s11 read as what they
   are used for. *)

type reg =
  (* the integer registers, by their ABI names *)
  | Zero                        (* x0, always zero *)
  | RA | SP | GP | TP           (* return address, stack, global, thread *)
  | T of int                    (* t0..t6, caller-saved scratch *)
  | S of int                    (* s0..s11, callee-saved *)
  | A of int                    (* a0..a7, arguments and results *)
  (* the floating-point registers *)
  | FT of int                   (* ft0..ft11 *)
  | FS of int                   (* fs0..fs11 *)
  | FA of int                   (* fa0..fa7 *)

(* The width an instruction acts on.  RV64 spells these in the mnemonic
   -- lb, lh, lw, ld -- rather than in a suffix on the operands. *)
type width = B | H | W | D

(* An instruction's operands.  There is no memory operand in the x86
   sense: a load or a store names a register and an offset, and nothing
   else reaches memory. *)
type operand =
  | Imm of int64
  | Reg of reg
  | Mem of reg * int            (* offset(reg), the offset a signed 12 bits *)
  | Sym of string * int         (* a symbol and an addend, for %hi/%lo pairs *)

type instr =
  | Op of string * operand list (* a mnemonic and its operands, source last *)
  | Label of string
  | Directive of string * string list
  | Raw of string (* an exact line the assembler must see, for inline assembly *)
  | Loc of int * int (* .loc N line, for the line table *)
  | Cfi of string (* a .cfi_* directive, e.g. "def_cfa_offset 48" *)

(* The data side is ELF's rather than the machine's, so it is shaped as
   on the other machine: a named object with its binding, its section
   and its contents.  Only the spelling differs, and [Emit] owns that --
   notably [.align], which on this machine counts powers of two. *)

type section = Data | Bss | Rodata | Tdata | Tbss

type data_item =
  | Bytes of string
  | Zeros of int
  | Quad_sym of string * int64 (* a symbol plus an addend, eight bytes *)
  | Quad of int64
  | Long of int32

type data = {
  dname : string;
  dglobal : bool;
  dweak : bool;
  dhidden : bool;
  dalias : string option; (* this symbol is defined equal to that one *)
  dfunc : bool; (* the symbol has function type *)
  ddecl : bool; (* only the binding: no storage is defined *)
  dtls : bool;
  dalign : int; (* in bytes; [Emit] turns it into the power of two *)
  section : section;
  size : int;
  items : data_item list;
}

type func = {
  name : string;
  global : bool;
  weak : bool;
  hidden : bool;
  body : instr list;
  debug : Dwarf.func option; (* Some when the function is described to a debugger *)
}

type program = {
  pic : bool; (* position-independent: what "la" means, said in ".option" *)
  funcs : func list;
  data : data list;
  source : string option; (* Some when emitting debug information *)
  files : (int * string) list; (* the .file table for line information *)
  asm_blocks : string list; (* file-scope asm, emitted as written *)
  init_array : (int * string) list; (* constructors: priority, function *)
  fini_array : (int * string) list;
}
