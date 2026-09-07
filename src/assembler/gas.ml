(* The syntax of a GNU assembler source file, AT&T x86-64 dialect.

   This is the input language of the assembler: what occ's Emit, ocamlopt
   and the runtime's amd64.S all produce.  A file is a sequence of
   statements separated by newlines or semicolons; a statement is a label
   definition, a directive, or an instruction.  (GNU as manual, "Syntax".)

   Only the subset those three producers use is modelled; anything else
   is a syntax error at parse time rather than a silent misassembly. *)

(* ---- Registers ------------------------------------------------------------ *)

type reg_class =
  | Gpr           (* general purpose: rax .. r15 and their narrower views *)
  | Xmm           (* xmm0 .. xmm15 *)
  | Segment       (* fs, gs: only as segment overrides on memory operands *)
  | Rip           (* only as the base of rip-relative addressing *)

type reg = {
  rclass : reg_class;
  rnum : int;      (* hardware number 0..15; for Segment, 4 = fs, 5 = gs *)
  rwidth : int;    (* operand width in bits: 8, 16, 32, 64 (128 for xmm) *)
  rname : string;  (* as written, for messages *)
}

(* ---- Expressions ---------------------------------------------------------- *)

(* Constant expressions over integers and symbols (GNU as manual,
   "Expressions").  Values are 64-bit; the assembler decides after layout
   whether an expression is a plain number, a symbol plus offset, or a
   difference of two labels in one section. *)
type expr =
  | Num of int64
  | Sym of string * string option   (* symbol and optional @modifier: PLT, GOTPCREL, tpoff, gottpoff *)
  | Dot                             (* the current location, "." *)
  | Neg of expr
  | Not of expr
  | Bin of binop * expr * expr

and binop = Add | Sub | Mul | Div | Mod | And | Or | Xor | Shl | Shr

(* ---- Operands ------------------------------------------------------------- *)

type mem = {
  seg : reg option;       (* segment override: %fs:disp(...) *)
  disp : expr option;     (* displacement, possibly symbolic *)
  base : reg option;
  index : reg option;
  scale : int;            (* 1, 2, 4 or 8 *)
}

type operand =
  | Imm of expr           (* $expr *)
  | Reg of reg            (* %reg *)
  | Mem of mem            (* disp(base,index,scale); a bare symbol is a Mem with only disp *)
  | Indirect of operand   (* *operand, the target of an indirect jmp or call *)

(* ---- Statements ----------------------------------------------------------- *)

type section_spec = {
  sname : string;
  sflags : string option;       (* the "awx" string, when given *)
  stype : string option;        (* progbits, nobits, ... without the @ *)
  sextra : expr list;           (* entry size for "M" sections *)
}

type cfi =
  | Cfi_startproc of bool         (* true: "simple", no initial instructions *)
  | Cfi_endproc
  | Cfi_def_cfa of int * int      (* register, offset *)
  | Cfi_def_cfa_register of int
  | Cfi_def_cfa_offset of int
  | Cfi_adjust_cfa_offset of int
  | Cfi_offset of int * int       (* register saved at cfa + offset *)
  | Cfi_rel_offset of int * int
  | Cfi_restore of int
  | Cfi_same_value of int
  | Cfi_undefined of int
  | Cfi_register of int * int
  | Cfi_remember_state
  | Cfi_restore_state
  | Cfi_escape of expr list       (* raw bytes *)
  | Cfi_signal_frame

type directive =
  | Section of section_spec       (* .section, and .text/.data/.bss as shorthands *)
  | Previous
  | Global of string
  | Local of string
  | Weak of string
  | Visibility of string * string (* symbol, "hidden" | "protected" | "internal" *)
  | Type of string * string       (* symbol, "function" | "object" | "tls_object" | ... *)
  | Size of string * expr
  | Set of string * expr          (* .set / .equ / sym = expr *)
  | Comm of string * expr * expr option   (* symbol, size, alignment *)
  | Data of int * expr list       (* .byte .word .long .quad: width in bytes, values *)
  | Ascii of string list          (* .ascii: raw bytes *)
  | Asciz of string list          (* .asciz / .string: NUL-terminated *)
  | Zero of expr * int            (* .zero / .space / .skip: count and fill byte *)
  | Uleb128 of expr list
  | Sleb128 of expr list
  | Align of int * int option     (* .align / .balign: byte alignment, optional fill; .p2align converted *)
  | File of int option * string   (* .file "name" or .file n "name" *)
  | Loc of int * int * int        (* .loc file line column *)
  | Cfi of cfi
  | Ident of string
  | Ignored of string             (* directives accepted and dropped, listed in the parser *)

type statement =
  | Label of string
  | Directive of directive
  | Instruction of instruction

and instruction = {
  prefixes : string list;    (* lock, rep, repz, repnz *)
  mnemonic : string;         (* as written, with its size suffix if any *)
  operands : operand list;   (* in AT&T order: source first, destination last *)
}

type line = { stmt : statement; lineno : int }

(* ---- Register table ------------------------------------------------------- *)

let gpr_names = [|
  (* index = hardware number *)
  [| "al"; "ax"; "eax"; "rax" |]; [| "cl"; "cx"; "ecx"; "rcx" |];
  [| "dl"; "dx"; "edx"; "rdx" |]; [| "bl"; "bx"; "ebx"; "rbx" |];
  [| "spl"; "sp"; "esp"; "rsp" |]; [| "bpl"; "bp"; "ebp"; "rbp" |];
  [| "sil"; "si"; "esi"; "rsi" |]; [| "dil"; "di"; "edi"; "rdi" |];
  [| "r8b"; "r8w"; "r8d"; "r8" |]; [| "r9b"; "r9w"; "r9d"; "r9" |];
  [| "r10b"; "r10w"; "r10d"; "r10" |]; [| "r11b"; "r11w"; "r11d"; "r11" |];
  [| "r12b"; "r12w"; "r12d"; "r12" |]; [| "r13b"; "r13w"; "r13d"; "r13" |];
  [| "r14b"; "r14w"; "r14d"; "r14" |]; [| "r15b"; "r15w"; "r15d"; "r15" |];
|]

let register_of_name name =
  let found = ref None in
  Array.iteri (fun num names ->
      Array.iteri (fun i n ->
          if n = name then found := Some { rclass = Gpr; rnum = num; rwidth = 8 lsl i; rname = name }) names)
    gpr_names;
  match !found with
  | Some r -> Some r
  | None when List.mem name [ "ah"; "ch"; "dh"; "bh" ] ->
      (* the legacy high-byte registers share numbers 4-7 with spl/bpl/sil/dil;
         the encoder tells them apart by name *)
      Some { rclass = Gpr; rnum = 4 + String.index "acdb" name.[0]; rwidth = 8; rname = name }
  | None ->
      let n = String.length name in
      if n >= 4 && String.sub name 0 3 = "xmm" then
        (match int_of_string_opt (String.sub name 3 (n - 3)) with
         | Some k when k >= 0 && k < 16 -> Some { rclass = Xmm; rnum = k; rwidth = 128; rname = name }
         | _ -> None)
      else
        match name with
        | "fs" -> Some { rclass = Segment; rnum = 4; rwidth = 16; rname = name }
        | "gs" -> Some { rclass = Segment; rnum = 5; rwidth = 16; rname = name }
        | "rip" -> Some { rclass = Rip; rnum = 0; rwidth = 64; rname = name }
        | _ -> None

(* DWARF register numbers for .cfi directives (System V ABI, figure 3.36) *)
let dwarf_number_of_gpr = [| 0; 2; 1; 3; 7; 6; 4; 5; 8; 9; 10; 11; 12; 13; 14; 15 |]
