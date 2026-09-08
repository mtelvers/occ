(* An abstract syntax for x86-64 assembly in AT&T syntax (source first).

   The code generator builds this rather than printing strings so that
   [Emit] is the only place that knows how GNU as spells things, and so
   that a later register allocator has data to work on.  Mnemonics for
   the ALU and SSE families are kept as strings: the assembler is the
   authority on their spelling and the structure of interest is in the
   operands. *)

type reg =
  | RAX | RBX | RCX | RDX | RSI | RDI | RBP | RSP
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  | XMM of int

type width = B | W | L | Q

type operand =
  | Imm of int64
  | Reg of reg
  | Mem of reg * int (* disp(%reg) *)
  | Mem_index of reg * reg * int (* (%base,%index,scale) *)
  | Rip of string * int (* sym+disp(%rip) *)
  | Got of string (* sym@GOTPCREL(%rip): the address of a global from the GOT *)
  | Plt of string (* sym@PLT, for calls *)
  | Tpoff of reg * string (* sym@tpoff(%reg): a thread-local's offset from the TLS base (local-exec) *)
  | Gottpoff of string (* sym@gottpoff(%rip): that offset read from the GOT (initial-exec) *)
  | Fs_zero (* %fs:0, the TLS base *)

type cc = CE | CNE | CL | CLE | CG | CGE | CB | CBE | CA | CAE | CO | CNO | CP | CNP | CS | CNS

type instr =
  | Mov of width * operand * operand
  | Movabs of int64 * reg
  | Movsx of width * width * operand * operand (* from, to *)
  | Movzx of width * width * operand * operand
  | Lea of operand * reg
  | Alu of string * width * operand * operand (* add sub and or xor imul cmp test, src, dst *)
  | Unary of string * width * operand (* neg not *)
  | Shift of string * width * operand * operand (* shl sar shr; count (imm or %cl), dst *)
  | Cqo | Cdq
  | Idiv of width * operand
  | Div of width * operand
  | Setcc of cc * operand
  | Jmp of string
  | Jmp_indirect of operand (* jmp *operand *)
  | Jcc of cc * string
  | Call of operand
  | Ret
  | Push of operand
  | Pop of operand
  | Sse of string * operand * operand (* movsd addsd cvtsi2sdq ucomisd ... , src, dst *)
  | Xchg of width * operand * operand
  | Lock of instr
  | Mfence
  | Rep_movsb
  | Rep_stosb
  | Ud2
  | Label of string
  | Raw of string (* an exact line the assembler must see, e.g. the TLS GD sequence *)
  | X87 of string * operand option (* an x87 instruction: fldt, fstpt, faddp, ... with at most one memory operand *)
  | Comment of string
  | Cfi of string (* a .cfi_* directive, e.g. "def_cfa_offset 16" *)
  | File of int * string (* .file N "name", for the line table *)
  | Loc of int * int (* .loc N line *)

(* DWARF types, as much as parameters and results need (see doc/phases.md). *)
type dwarf_type =
  | Dw_void
  | Dw_base of string * int * int (* name, DW_ATE encoding, byte size *)
  | Dw_pointer (* to void: pointee types are not described *)
  | Dw_struct of string
  | Dw_union of string

type dbg_location = At_cfa_offset of int (* DW_OP_fbreg *) | In_register of int (* DW_OP_regN, DWARF number *)

type dbg_param = { pname : string; ptype : dwarf_type; ploc : dbg_location }

type dbg_func = {
  dfile : int; (* index in the .file table *)
  dline : int;
  dparams : dbg_param list;
  dret : dwarf_type;
}

type func = { name : string; global : bool; weak : bool; hidden : bool; body : instr list; debug : dbg_func option }

type data_item =
  | Bytes of string | Zeros of int | Quad_sym of string * int64 | Quad of int64 | Long of int32
  | Word of int (* two bytes: the sign and exponent of a long double constant *)
  | Long_diff of string * string (* .long a - b: a position-independent table entry *)

type section = Data | Bss | Rodata | Tdata | Tbss

type data = { dname : string; dglobal : bool; dweak : bool; dhidden : bool; dalias : string option; dfunc : bool; ddecl : bool; dtls : bool; dalign : int; section : section; size : int; items : data_item list }

type program = {
  funcs : func list;
  data : data list;
  source : string option; (* Some when emitting debug info *)
  files : (int * string) list; (* the .file table for line information *)
  asm_blocks : string list; (* file-scope asm, emitted as written *)
  init_array : (int * string) list; (* constructors: priority, function *)
  fini_array : (int * string) list;
}
