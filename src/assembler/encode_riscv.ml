(* Machine-code encoding of RV64 instructions (RISC-V ISA manual, volume
   I, "RV32I Base Integer Instruction Set" onwards and the instruction
   listings; the psABI for the relocations).

   The machine makes this a much smaller job than x86-64 did.  Every
   instruction is exactly four bytes, and there are six formats, which
   differ only in where the immediate's bits are scattered:

     R   funct7[31:25] rs2[24:20] rs1[19:15] funct3[14:12] rd[11:7] op[6:0]
     I   imm[11:0][31:20]         rs1        funct3        rd       op
     S   imm[11:5][31:25]   rs2   rs1        funct3   imm[4:0][11:7]  op
     B   imm[12|10:5]       rs2   rs1        funct3   imm[4:1|11]     op
     U   imm[31:12][31:12]                            rd       op
     J   imm[20|10:1|11|19:12][31:12]                 rd       op

   So the tables below give an opcode and a few function codes, and the
   formats do the rest.  Of the hundred and thirty mnemonics the three
   producers use -- occ's own output, OCaml's runtime/riscv.S and
   ocamlopt's emitter -- most are pseudo-instructions standing for one of
   these with a register or an immediate fixed, and they are listed as
   such rather than given encodings of their own.

   Where a choice exists, it follows GNU as, since the two assemblers are
   compared byte for byte (tools/ascheck.sh).  The one real choice is
   [load_const], gas's sequence for `li', which is reproduced here
   because a program that builds a constant differently is a program
   whose bytes differ.

   This assembler does not relax, and emits no R_RISCV_RELAX: relaxation
   is an invitation to the linker, and a linker need not accept it.  The
   comparison is therefore against `as -mno-relax'. *)

open Gas

type encoded = Fixup.result = Fixed of string * Fixup.t list | Relaxable of { short : Fixup.form; long : Fixup.form }

exception Bad = Fixup.Bad
let bad = Fixup.bad

(* ---- Words ----------------------------------------------------------- *)

(* an instruction is four bytes, little-endian *)
let word w =
  let b k = Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical w k) 0xffl)) in
  let s = Bytes.create 4 in
  Bytes.set s 0 (b 0); Bytes.set s 1 (b 8); Bytes.set s 2 (b 16); Bytes.set s 3 (b 24);
  Bytes.to_string s

let bits v lo n = Int32.shift_left (Int32.logand v (Int32.sub (Int32.shift_left 1l n) 1l)) lo

(* [n] bits of [v] starting at bit [from], placed at bit [lo]: the
   immediates of the B and J formats are scattered like this *)
let field v lo n from = bits (Int32.shift_right v from) lo n

(* ---- The six formats -------------------------------------------------- *)

let r_type ~op ~f3 ~f7 ~rd ~rs1 ~rs2 =
  Int32.logor (Int32.of_int op)
    (Int32.logor (bits (Int32.of_int rd) 7 5)
       (Int32.logor (bits (Int32.of_int f3) 12 3)
          (Int32.logor (bits (Int32.of_int rs1) 15 5)
             (Int32.logor (bits (Int32.of_int rs2) 20 5) (bits (Int32.of_int f7) 25 7)))))

let i_type ~op ~f3 ~rd ~rs1 ~imm =
  Int32.logor (Int32.of_int op)
    (Int32.logor (bits (Int32.of_int rd) 7 5)
       (Int32.logor (bits (Int32.of_int f3) 12 3)
          (Int32.logor (bits (Int32.of_int rs1) 15 5) (bits imm 20 12))))

let s_type ~op ~f3 ~rs1 ~rs2 ~imm =
  Int32.logor (Int32.of_int op)
    (Int32.logor (bits imm 7 5)
       (Int32.logor (bits (Int32.of_int f3) 12 3)
          (Int32.logor (bits (Int32.of_int rs1) 15 5)
             (Int32.logor (bits (Int32.of_int rs2) 20 5) (field imm 25 7 5)))))

let b_type ~op ~f3 ~rs1 ~rs2 ~imm =
  Int32.logor (Int32.of_int op)
    (Int32.logor (field imm 8 4 1)                  (* imm[4:1] *)
       (Int32.logor (field imm 7 1 11)              (* imm[11] *)
          (Int32.logor (bits (Int32.of_int f3) 12 3)
             (Int32.logor (bits (Int32.of_int rs1) 15 5)
                (Int32.logor (bits (Int32.of_int rs2) 20 5)
                   (Int32.logor (field imm 25 6 5)  (* imm[10:5] *)
                      (field imm 31 1 12)))))))     (* imm[12] *)

let u_type ~op ~rd ~imm =
  Int32.logor (Int32.of_int op) (Int32.logor (bits (Int32.of_int rd) 7 5) (bits imm 12 20))

let j_type ~op ~rd ~imm =
  Int32.logor (Int32.of_int op)
    (Int32.logor (bits (Int32.of_int rd) 7 5)
       (Int32.logor (field imm 21 10 1)             (* imm[10:1] *)
          (Int32.logor (field imm 20 1 11)          (* imm[11] *)
             (Int32.logor (field imm 12 8 12)       (* imm[19:12] *)
                (field imm 31 1 20)))))             (* imm[20] *)

(* ---- Opcodes ---------------------------------------------------------- *)

let op_load = 0x03 and op_load_fp = 0x07 and op_misc_mem = 0x0f
let op_imm = 0x13 and op_auipc = 0x17 and op_imm32 = 0x1b
let op_store = 0x23 and op_store_fp = 0x27 and op_amo = 0x2f
let op_reg = 0x33 and op_lui = 0x37 and op_reg32 = 0x3b
let op_fp = 0x53 and op_branch = 0x63 and op_jalr = 0x67 and op_jal = 0x6f
let op_system = 0x73

(* ---- Operands --------------------------------------------------------- *)

let ireg = function
  | Reg ({ rclass = Ireg; _ } as r) -> r.rnum
  | Reg r -> bad "%s is not an integer register" r.rname
  | _ -> bad "expected an integer register"

let freg = function
  | Reg ({ rclass = Freg; _ } as r) -> r.rnum
  | Reg r -> bad "%s is not a floating-point register" r.rname
  | _ -> bad "expected a floating-point register"

(* the value of an expression, when it has to be known now *)
let rec const = function
  | Num v -> Some v
  | Neg e -> Option.map Int64.neg (const e)
  | Not e -> Option.map Int64.lognot (const e)
  | Bin (op, a, b) ->
      (match const a, const b with
       | Some a, Some b ->
           Some (match op with
               | Add -> Int64.add a b | Sub -> Int64.sub a b | Mul -> Int64.mul a b
               | Div -> Int64.div a b | Mod -> Int64.rem a b | And -> Int64.logand a b
               | Or -> Int64.logor a b | Xor -> Int64.logxor a b
               | Shl -> Int64.shift_left a (Int64.to_int b)
               | Shr -> Int64.shift_right_logical a (Int64.to_int b))
       | _ -> None)
  | Sym _ | Dot -> None

let imm_of = function
  | Imm e -> e
  | Mem { disp = Some e; base = None; _ } -> e
  | _ -> bad "expected an immediate"

(* a bare word in an operand position: a rounding mode, a fence ordering *)
let keyword = function
  | Imm (Sym (name, None)) | Mem { disp = Some (Sym (name, None)); base = None; _ } -> Some name
  | _ -> None

let constant op =
  match const (imm_of op) with
  | Some v -> v
  | None -> bad "expected a constant"

(* offset(base), the machine's one addressing mode; a bare (base) means
   an offset of zero *)
let address = function
  | Mem { disp; base = Some b; index = None; _ } when b.rclass = Ireg -> (disp, b.rnum)
  | Mem { base = Some b; _ } -> bad "%s is not an integer register" b.rname
  | _ -> bad "expected offset(register)"

let modifier = function
  | Sym (_, Some m) -> Some m
  | Bin (_, Sym (_, Some m), _) -> Some m
  | _ -> None

let rec has_symbol = function
  | Sym _ | Dot -> true
  | Num _ -> false
  | Neg e | Not e -> has_symbol e
  | Bin (_, a, b) -> has_symbol a || has_symbol b

(* ---- Fixups ----------------------------------------------------------- *)

let fix ~field ~target = Fixup.make ~at:0 ~size:4 ~field target

(* an I-format immediate: known now, or left to the linker *)
let i_imm ~op ~f3 ~rd ~rs1 e =
  match const e with
  | Some v ->
      if not (Fixup.fits_signed 12 v) then bad "immediate %Ld does not fit in twelve bits" v;
      Fixed (word (i_type ~op ~f3 ~rd ~rs1 ~imm:(Int64.to_int32 v)), [])
  | None -> Fixed (word (i_type ~op ~f3 ~rd ~rs1 ~imm:0l), [ fix ~field:Fixup.Rv_lo12_i ~target:e ])

let s_imm ~op ~f3 ~rs1 ~rs2 e =
  match const e with
  | Some v ->
      if not (Fixup.fits_signed 12 v) then bad "offset %Ld does not fit in twelve bits" v;
      Fixed (word (s_type ~op ~f3 ~rs1 ~rs2 ~imm:(Int64.to_int32 v)), [])
  | None -> Fixed (word (s_type ~op ~f3 ~rs1 ~rs2 ~imm:0l), [ fix ~field:Fixup.Rv_lo12_s ~target:e ])

let u_imm ~op ~rd e =
  match const e with
  | Some v -> Fixed (word (u_type ~op ~rd ~imm:(Int64.to_int32 v)), [])
  | None -> Fixed (word (u_type ~op ~rd ~imm:0l), [ fix ~field:Fixup.Rv_hi20 ~target:e ])

(* ---- li, whose sequence is the assembler's own ------------------------ *)

(* gas builds a constant as follows, and the bytes of a program depend on
   it: a value that fits in twelve signed bits is one addi; one that fits
   in thirty-two is lui and addiw; anything wider is built from the top
   down, shifting left and adding in the low twelve bits at each step.
   The shift skips whatever zero bits are there to skip, which is why a
   constant with a run of zeros in the middle takes fewer instructions
   than its width suggests.

   The twelve-bit case is the entry point's alone: reached from the
   recursion, the same value is written with addiw instead, and gas does
   that too -- it is why `li a0,1' and the tail of `li a0,0x80000000'
   differ in one bit. *)
let rec load_const_big rd v acc =
  let low = Int64.sub (Int64.logand v 0xfffL)
      (if Int64.logand v 0x800L <> 0L then 0x1000L else 0L) in   (* sign-extended low twelve *)
  let upper = Int64.sub v low in
  let sext32 = Int64.equal (Int64.shift_right (Int64.shift_left v 32) 32) v in
  if not sext32 then begin
    (* shift the upper part down as far as its zero bits allow *)
    let shift = ref 12 in
    while Int64.logand (Int64.shift_right upper !shift) 1L = 0L do incr shift done;
    let acc = load_const_big rd (Int64.shift_right upper !shift) acc in
    let acc = word (i_type ~op:op_imm ~f3:1 ~rd ~rs1:rd ~imm:(Int32.of_int !shift)) :: acc in
    if Int64.equal low 0L then acc
    else word (i_type ~op:op_imm ~f3:0 ~rd ~rs1:rd ~imm:(Int64.to_int32 low)) :: acc
  end else begin
    let acc =
      if Int64.equal upper 0L then acc
      else word (u_type ~op:op_lui ~rd ~imm:(Int64.to_int32 (Int64.shift_right upper 12))) :: acc in
    if Int64.equal low 0L && not (Int64.equal upper 0L) then acc
    else
      (* addiw, so that a value built this way stays sign-extended *)
      word (i_type ~op:op_imm32 ~f3:0 ~rd ~rs1:(if Int64.equal upper 0L then 0 else rd)
              ~imm:(Int64.to_int32 low)) :: acc
  end

let load_const rd v =
  if Fixup.fits_signed 12 v then word (i_type ~op:op_imm ~f3:0 ~rd ~rs1:0 ~imm:(Int64.to_int32 v))
  else String.concat "" (List.rev (load_const_big rd v []))

(* ---- Tables ----------------------------------------------------------- *)

(* mnemonic -> funct3 for the loads and the stores *)
let load_f3 = [ "lb", 0; "lh", 1; "lw", 2; "ld", 3; "lbu", 4; "lhu", 5; "lwu", 6 ]
let store_f3 = [ "sb", 0; "sh", 1; "sw", 2; "sd", 3 ]
let fload_f3 = [ "flw", 2; "fld", 3 ]
let fstore_f3 = [ "fsw", 2; "fsd", 3 ]

(* register-register arithmetic: funct3, funct7, and whether the "w" form *)
let reg_ops = [
  "add", (0, 0x00); "sub", (0, 0x20); "sll", (1, 0x00); "slt", (2, 0x00);
  "sltu", (3, 0x00); "xor", (4, 0x00); "srl", (5, 0x00); "sra", (5, 0x20);
  "or", (6, 0x00); "and", (7, 0x00);
  (* the M extension *)
  "mul", (0, 0x01); "mulh", (1, 0x01); "mulhsu", (2, 0x01); "mulhu", (3, 0x01);
  "div", (4, 0x01); "divu", (5, 0x01); "rem", (6, 0x01); "remu", (7, 0x01);
]

let reg32_ops = [
  "addw", (0, 0x00); "subw", (0, 0x20); "sllw", (1, 0x00); "srlw", (5, 0x00); "sraw", (5, 0x20);
  "mulw", (0, 0x01); "divw", (4, 0x01); "divuw", (5, 0x01); "remw", (6, 0x01); "remuw", (7, 0x01);
]

(* immediate arithmetic: funct3, and the funct7-like top bits of a shift *)
let imm_ops = [ "addi", 0; "slti", 2; "sltiu", 3; "xori", 4; "ori", 6; "andi", 7 ]
let shift_ops = [ "slli", (1, 0x00); "srli", (5, 0x00); "srai", (5, 0x10) ]
let shift32_ops = [ "slliw", (1, 0x00); "srliw", (5, 0x00); "sraiw", (5, 0x10) ]

let branch_f3 = [ "beq", 0; "bne", 1; "blt", 4; "bge", 5; "bltu", 6; "bgeu", 7 ]

(* the rounding modes, which are a funct3 *)
let rounding = [ "rne", 0; "rtz", 1; "rdn", 2; "rup", 3; "rmm", 4; "dyn", 7 ]

(* floating-point arithmetic: funct7, with the single and double forms one
   apart, and the rounding mode gas leaves dynamic *)
let fp_ops = [
  "fadd", 0x00; "fsub", 0x04; "fmul", 0x08; "fdiv", 0x0c; "fsqrt", 0x2c;
  "fsgnj", 0x10; "fmin", 0x14;
]

(* comparisons: funct7 and funct3 *)
let fcmp_ops = [ "feq", 2; "flt", 1; "fle", 0 ]

(* the ordering bits of a fence: i, o, r, w *)
let fence_bits s =
  let bit = function 'i' -> 8 | 'o' -> 4 | 'r' -> 2 | 'w' -> 1 | c -> bad "fence: %c is not one of iorw" c in
  String.fold_left (fun acc c -> acc lor bit c) 0 s

(* the atomic operations: funct5 *)
let amo_ops = [
  "amoswap", 0x01; "amoadd", 0x00; "amoxor", 0x04; "amoand", 0x0c; "amoor", 0x08;
  "amomin", 0x10; "amomax", 0x14; "amominu", 0x18; "amomaxu", 0x1c;
]

(* ---- Splitting a mnemonic --------------------------------------------- *)

(* "amoadd.d.aqrl" is an operation, a width and an ordering; "fadd.s" an
   operation and a precision; "lr.w.aqrl" likewise.  [suffixes] cuts a
   mnemonic at its dots. *)
let suffixes m = String.split_on_char '.' m

let ends_with suffix s =
  let ls = String.length s and lf = String.length suffix in
  ls >= lf && String.sub s (ls - lf) lf = suffix

(* ---- One instruction -------------------------------------------------- *)

let rd_of ops k = ireg (List.nth ops k)
let frd_of ops k = freg (List.nth ops k)

let rec instruction (i : instruction) : encoded =
  let m = i.mnemonic and ops = i.operands in
  let n = List.length ops in
  let want k = if n <> k then bad "%s takes %d operands, not %d" m k n in
  (* a floating-point instruction may name its rounding mode as one more
     operand than it otherwise takes *)
  let want_rm k = if n <> k && n <> k + 1 then bad "%s takes %d operands, not %d" m k n in
  let arg k = List.nth ops k in
  (* the rounding mode a floating-point instruction was given, or the
     dynamic one, which is what gas writes when none is named *)
  let rm ?(default = 7) k =
    if n > k then match keyword (arg k) with
      | Some name -> (try List.assoc name rounding with Not_found -> bad "%s is not a rounding mode" name)
      | None -> default
    else default in
  let plain w = Fixed (word w, []) in
  match m with
  (* loads and stores *)
  | _ when List.mem_assoc m load_f3 ->
      want 2;
      let disp, base = address (arg 1) in
      let e = Option.value disp ~default:(Num 0L) in
      i_imm ~op:op_load ~f3:(List.assoc m load_f3) ~rd:(rd_of ops 0) ~rs1:base e
  | _ when List.mem_assoc m fload_f3 ->
      want 2;
      let disp, base = address (arg 1) in
      let e = Option.value disp ~default:(Num 0L) in
      i_imm ~op:op_load_fp ~f3:(List.assoc m fload_f3) ~rd:(frd_of ops 0) ~rs1:base e
  | _ when List.mem_assoc m store_f3 ->
      want 2;
      let disp, base = address (arg 1) in
      let e = Option.value disp ~default:(Num 0L) in
      s_imm ~op:op_store ~f3:(List.assoc m store_f3) ~rs1:base ~rs2:(rd_of ops 0) e
  | _ when List.mem_assoc m fstore_f3 ->
      want 2;
      let disp, base = address (arg 1) in
      let e = Option.value disp ~default:(Num 0L) in
      s_imm ~op:op_store_fp ~f3:(List.assoc m fstore_f3) ~rs1:base ~rs2:(frd_of ops 0) e

  (* register-register arithmetic *)
  (* "add rd,rs1,tp,%tprel_add(x)": the same instruction, with a
     relocation that marks it as the middle of a thread-local sequence *)
  | "add" when n = 4 ->
      let w = r_type ~op:op_reg ~f3:0 ~f7:0 ~rd:(rd_of ops 0) ~rs1:(rd_of ops 1) ~rs2:(rd_of ops 2) in
      Fixed (word w, [ Fixup.make ~at:0 ~size:4 ~field:Fixup.Rv_none (imm_of (arg 3)) ])
  | _ when List.mem_assoc m reg_ops ->
      want 3;
      let f3, f7 = List.assoc m reg_ops in
      plain (r_type ~op:op_reg ~f3 ~f7 ~rd:(rd_of ops 0) ~rs1:(rd_of ops 1) ~rs2:(rd_of ops 2))
  | _ when List.mem_assoc m reg32_ops ->
      want 3;
      let f3, f7 = List.assoc m reg32_ops in
      plain (r_type ~op:op_reg32 ~f3 ~f7 ~rd:(rd_of ops 0) ~rs1:(rd_of ops 1) ~rs2:(rd_of ops 2))

  (* immediate arithmetic *)
  | _ when List.mem_assoc m imm_ops ->
      want 3;
      i_imm ~op:op_imm ~f3:(List.assoc m imm_ops) ~rd:(rd_of ops 0) ~rs1:(rd_of ops 1) (imm_of (arg 2))
  | "addiw" -> want 3; i_imm ~op:op_imm32 ~f3:0 ~rd:(rd_of ops 0) ~rs1:(rd_of ops 1) (imm_of (arg 2))
  | _ when List.mem_assoc m shift_ops ->
      want 3;
      let f3, top = List.assoc m shift_ops in
      let sh = constant (arg 2) in
      if Int64.compare sh 0L < 0 || Int64.compare sh 63L > 0 then bad "%s: shift out of range" m;
      plain (i_type ~op:op_imm ~f3 ~rd:(rd_of ops 0) ~rs1:(rd_of ops 1)
               ~imm:(Int32.logor (Int64.to_int32 sh) (Int32.of_int (top lsl 6))))
  | _ when List.mem_assoc m shift32_ops ->
      want 3;
      let f3, top = List.assoc m shift32_ops in
      let sh = constant (arg 2) in
      if Int64.compare sh 0L < 0 || Int64.compare sh 31L > 0 then bad "%s: shift out of range" m;
      plain (i_type ~op:op_imm32 ~f3 ~rd:(rd_of ops 0) ~rs1:(rd_of ops 1)
               ~imm:(Int32.logor (Int64.to_int32 sh) (Int32.of_int (top lsl 6))))

  (* the upper immediate *)
  | "lui" -> want 2; u_imm ~op:op_lui ~rd:(rd_of ops 0) (imm_of (arg 1))
  | "auipc" -> want 2; u_imm ~op:op_auipc ~rd:(rd_of ops 0) (imm_of (arg 1))

  (* the pseudo-instructions that stand for one of the above *)
  | "nop" -> want 0; plain (i_type ~op:op_imm ~f3:0 ~rd:0 ~rs1:0 ~imm:0l)
  | "mv" -> want 2; plain (i_type ~op:op_imm ~f3:0 ~rd:(rd_of ops 0) ~rs1:(rd_of ops 1) ~imm:0l)
  | "not" -> want 2; plain (i_type ~op:op_imm ~f3:4 ~rd:(rd_of ops 0) ~rs1:(rd_of ops 1) ~imm:(-1l))
  | "neg" -> want 2; plain (r_type ~op:op_reg ~f3:0 ~f7:0x20 ~rd:(rd_of ops 0) ~rs1:0 ~rs2:(rd_of ops 1))
  | "negw" -> want 2; plain (r_type ~op:op_reg32 ~f3:0 ~f7:0x20 ~rd:(rd_of ops 0) ~rs1:0 ~rs2:(rd_of ops 1))
  | "sext.w" -> want 2; plain (i_type ~op:op_imm32 ~f3:0 ~rd:(rd_of ops 0) ~rs1:(rd_of ops 1) ~imm:0l)
  | "seqz" -> want 2; plain (i_type ~op:op_imm ~f3:3 ~rd:(rd_of ops 0) ~rs1:(rd_of ops 1) ~imm:1l)
  | "snez" -> want 2; plain (r_type ~op:op_reg ~f3:3 ~f7:0 ~rd:(rd_of ops 0) ~rs1:0 ~rs2:(rd_of ops 1))
  | "sltz" -> want 2; plain (r_type ~op:op_reg ~f3:2 ~f7:0 ~rd:(rd_of ops 0) ~rs1:(rd_of ops 1) ~rs2:0)
  | "sgtz" -> want 2; plain (r_type ~op:op_reg ~f3:2 ~f7:0 ~rd:(rd_of ops 0) ~rs1:0 ~rs2:(rd_of ops 1))
  | "li" -> want 2; Fixed (load_const (rd_of ops 0) (constant (arg 1)), [])

  (* the system instructions the code generators use *)
  | "ecall" -> want 0; plain (i_type ~op:op_system ~f3:0 ~rd:0 ~rs1:0 ~imm:0l)
  | "ebreak" -> want 0; plain (i_type ~op:op_system ~f3:0 ~rd:0 ~rs1:0 ~imm:1l)
  (* gas writes the illegal instruction as "csrrw x0, cycle, x0" *)
  | "unimp" -> want 0; plain 0xc0001073l

  (* fences.  Without operands the ordering is the strongest one. *)
  | "fence" ->
      let pred, succ =
        if n = 0 then 0xf, 0xf
        else begin
          want 2;
          match keyword (arg 0), keyword (arg 1) with
          | Some a, Some b -> fence_bits a, fence_bits b
          | _ -> bad "fence takes two orderings"
        end in
      plain (i_type ~op:op_misc_mem ~f3:0 ~rd:0 ~rs1:0
               ~imm:(Int32.of_int ((pred lsl 4) lor succ)))
  | "fence.i" -> plain (i_type ~op:op_misc_mem ~f3:1 ~rd:0 ~rs1:0 ~imm:0l)

  (* floating point.  The precision is the last suffix, and the funct7 of
     a double is one more than that of the single it is named after. *)
  | _ when (match suffixes m with [ base; ("s" | "d") ] -> List.mem_assoc base fp_ops | _ -> false) ->
      let base, prec = match suffixes m with [ b; p ] -> b, p | _ -> assert false in
      let f7 = List.assoc base fp_ops + (if prec = "d" then 1 else 0) in
      if base = "fsqrt" then begin
        want_rm 2;
        plain (r_type ~op:op_fp ~f3:(rm 2) ~f7 ~rd:(frd_of ops 0) ~rs1:(frd_of ops 1) ~rs2:0)
      end else begin
        want_rm 3;
        plain (r_type ~op:op_fp ~f3:(rm 3) ~f7 ~rd:(frd_of ops 0) ~rs1:(frd_of ops 1) ~rs2:(frd_of ops 2))
      end
  (* fmv, fneg and fabs are the sign-injection instruction with both
     sources the same register *)
  | _ when (match suffixes m with [ ("fmv" | "fneg" | "fabs"); ("s" | "d") ] -> true | _ -> false) ->
      want 2;
      let base, prec = match suffixes m with [ b; p ] -> b, p | _ -> assert false in
      let f7 = 0x10 + (if prec = "d" then 1 else 0) in
      let f3 = match base with "fmv" -> 0 | "fneg" -> 1 | _ -> 2 in
      let rs = frd_of ops 1 in
      plain (r_type ~op:op_fp ~f3 ~f7 ~rd:(frd_of ops 0) ~rs1:rs ~rs2:rs)
  | _ when (match suffixes m with [ base; ("s" | "d") ] -> List.mem_assoc base fcmp_ops | _ -> false) ->
      want 3;
      let base, prec = match suffixes m with [ b; p ] -> b, p | _ -> assert false in
      let f7 = 0x50 + (if prec = "d" then 1 else 0) in
      plain (r_type ~op:op_fp ~f3:(List.assoc base fcmp_ops) ~f7
               ~rd:(rd_of ops 0) ~rs1:(frd_of ops 1) ~rs2:(frd_of ops 2))
  (* the conversions.  fcvt.a.b turns a b into an a, and which of the two
     is the integer decides which register bank each end names.  gas
     leaves the rounding dynamic except where the conversion cannot
     round, where it writes the nearest-even mode. *)
  | _ when (match suffixes m with "fcvt" :: _ :: _ :: [] -> true | _ -> false) ->
      want_rm 2;
      let into, from = match suffixes m with [ _; a; b ] -> a, b | _ -> assert false in
      let int_kind = function "w" -> Some 0 | "wu" -> Some 1 | "l" -> Some 2 | "lu" -> Some 3 | _ -> None in
      (match int_kind into, int_kind from with
       | Some _, Some _ -> bad "%s converts between two integers" m
       | Some k, None ->
           (* a floating-point value to an integer *)
           let f7 = if from = "s" then 0x60 else 0x61 in
           plain (r_type ~op:op_fp ~f3:(rm 2) ~f7 ~rd:(rd_of ops 0) ~rs1:(frd_of ops 1) ~rs2:k)
       | None, Some k ->
           (* an integer to a floating-point value; exact when a double
              takes a 32-bit integer, so gas rounds to nearest there *)
           let f7 = if into = "s" then 0x68 else 0x69 in
           let exact = into = "d" && (from = "w" || from = "wu") in
           plain (r_type ~op:op_fp ~f3:(rm ~default:(if exact then 0 else 7) 2) ~f7
                    ~rd:(frd_of ops 0) ~rs1:(rd_of ops 1) ~rs2:k)
       | None, None ->
           (* between the two floating-point formats; widening is exact *)
           let f7 = if into = "s" then 0x20 else 0x21 in
           let rs2 = if from = "s" then 0 else 1 in
           let exact = into = "d" in
           plain (r_type ~op:op_fp ~f3:(rm ~default:(if exact then 0 else 7) 2) ~f7
                    ~rd:(frd_of ops 0) ~rs1:(frd_of ops 1) ~rs2))
  (* moving bits between the banks, which is not a conversion *)
  | "fmv.x.w" -> want 2; plain (r_type ~op:op_fp ~f3:0 ~f7:0x70 ~rd:(rd_of ops 0) ~rs1:(frd_of ops 1) ~rs2:0)
  | "fmv.w.x" -> want 2; plain (r_type ~op:op_fp ~f3:0 ~f7:0x78 ~rd:(frd_of ops 0) ~rs1:(rd_of ops 1) ~rs2:0)
  | "fmv.x.d" -> want 2; plain (r_type ~op:op_fp ~f3:0 ~f7:0x71 ~rd:(rd_of ops 0) ~rs1:(frd_of ops 1) ~rs2:0)
  | "fmv.d.x" -> want 2; plain (r_type ~op:op_fp ~f3:0 ~f7:0x79 ~rd:(frd_of ops 0) ~rs1:(rd_of ops 1) ~rs2:0)
  | "fclass.s" -> want 2; plain (r_type ~op:op_fp ~f3:1 ~f7:0x70 ~rd:(rd_of ops 0) ~rs1:(frd_of ops 1) ~rs2:0)
  | "fclass.d" -> want 2; plain (r_type ~op:op_fp ~f3:1 ~f7:0x71 ~rd:(rd_of ops 0) ~rs1:(frd_of ops 1) ~rs2:0)

  (* the A extension: a load-reserved, a store-conditional and the
     read-modify-write operations, each with an ordering suffix *)
  | _ when (match suffixes m with ("lr" | "sc") :: _ -> true | _ -> false) ->
      let parts = suffixes m in
      let base = List.nth parts 0 and w = List.nth parts 1 in
      let order = if List.length parts > 2 then List.nth parts 2 else "" in
      let f3 = match w with "w" -> 2 | "d" -> 3 | _ -> bad "%s: width must be w or d" m in
      let aq = if ends_with "aq" order || order = "aqrl" then 1 else 0 in
      let rl = if ends_with "rl" order then 1 else 0 in
      let funct5 = if base = "lr" then 0x02 else 0x03 in
      let f7 = (funct5 lsl 2) lor (aq lsl 1) lor rl in
      if base = "lr" then begin
        want 2;
        let _, addr = address (arg 1) in
        plain (r_type ~op:op_amo ~f3 ~f7 ~rd:(rd_of ops 0) ~rs1:addr ~rs2:0)
      end else begin
        want 3;
        let _, addr = address (arg 2) in
        plain (r_type ~op:op_amo ~f3 ~f7 ~rd:(rd_of ops 0) ~rs1:addr ~rs2:(rd_of ops 1))
      end
  | _ when (match suffixes m with base :: _ -> List.mem_assoc base amo_ops | [] -> false) ->
      let parts = suffixes m in
      let base = List.nth parts 0 and w = List.nth parts 1 in
      let order = if List.length parts > 2 then List.nth parts 2 else "" in
      want 3;
      let f3 = match w with "w" -> 2 | "d" -> 3 | _ -> bad "%s: width must be w or d" m in
      let aq = if ends_with "aq" order || order = "aqrl" then 1 else 0 in
      let rl = if ends_with "rl" order then 1 else 0 in
      let f7 = ((List.assoc base amo_ops) lsl 2) lor (aq lsl 1) lor rl in
      let _, addr = address (arg 2) in
      plain (r_type ~op:op_amo ~f3 ~f7 ~rd:(rd_of ops 0) ~rs1:addr ~rs2:(rd_of ops 1))

  (* branches.  A target in this section is a thirteen-bit displacement;
     one the assembler cannot measure becomes the inverted branch around
     a jump, which is what gas writes too. *)
  | _ when List.mem_assoc m branch_f3 ->
      want 3;
      let f3 = List.assoc m branch_f3 in
      let rs1 = rd_of ops 0 and rs2 = rd_of ops 1 in
      let target = imm_of (arg 2) in
      branch ~f3 ~rs1 ~rs2 target
  | "beqz" -> want 2; branch ~f3:0 ~rs1:(rd_of ops 0) ~rs2:0 (imm_of (arg 1))
  | "bnez" -> want 2; branch ~f3:1 ~rs1:(rd_of ops 0) ~rs2:0 (imm_of (arg 1))
  | "bltz" -> want 2; branch ~f3:4 ~rs1:(rd_of ops 0) ~rs2:0 (imm_of (arg 1))
  | "bgez" -> want 2; branch ~f3:5 ~rs1:(rd_of ops 0) ~rs2:0 (imm_of (arg 1))
  | "bgtz" -> want 2; branch ~f3:4 ~rs1:0 ~rs2:(rd_of ops 0) (imm_of (arg 1))
  | "blez" -> want 2; branch ~f3:5 ~rs1:0 ~rs2:(rd_of ops 0) (imm_of (arg 1))
  (* the ones written the other way round: bgt a,b is blt b,a *)
  | "bgt" -> want 3; branch ~f3:4 ~rs1:(rd_of ops 1) ~rs2:(rd_of ops 0) (imm_of (arg 2))
  | "ble" -> want 3; branch ~f3:5 ~rs1:(rd_of ops 1) ~rs2:(rd_of ops 0) (imm_of (arg 2))
  | "bgtu" -> want 3; branch ~f3:6 ~rs1:(rd_of ops 1) ~rs2:(rd_of ops 0) (imm_of (arg 2))
  | "bleu" -> want 3; branch ~f3:7 ~rs1:(rd_of ops 1) ~rs2:(rd_of ops 0) (imm_of (arg 2))

  (* jumps *)
  | "j" -> want 1; jump ~rd:0 (imm_of (arg 0))
  | "jal" ->
      if n = 1 then jump ~rd:1 (imm_of (arg 0))
      else begin want 2; jump ~rd:(rd_of ops 0) (imm_of (arg 1)) end
  | "jr" -> want 1; plain (i_type ~op:op_jalr ~f3:0 ~rd:0 ~rs1:(rd_of ops 0) ~imm:0l)
  | "ret" -> want 0; plain (i_type ~op:op_jalr ~f3:0 ~rd:0 ~rs1:1 ~imm:0l)
  | "jalr" ->
      (match ops with
       | [ r ] -> plain (i_type ~op:op_jalr ~f3:0 ~rd:1 ~rs1:(ireg r) ~imm:0l)
       | [ rd; mem ] ->
           let disp, base = address mem in
           i_imm ~op:op_jalr ~f3:0 ~rd:(ireg rd) ~rs1:base (Option.value disp ~default:(Num 0L))
       | _ -> bad "jalr takes a register, or a register and an address")
  (* a call and a tail call: the pair the psABI reserves for them, which
     takes one relocation between them *)
  | "call" -> want 1; call ~rd:1 (imm_of (arg 0))
  | "tail" -> want 1; call ~rd:0 (imm_of (arg 0))

  | _ -> bad "unknown instruction %s" m

(* A branch: the short form is the B-type instruction, and the long form
   the opposite branch over a jump.  [Assemble] measures the short one
   and takes the long one when the target is out of reach -- or at once,
   when the target is a symbol it cannot measure. *)
and branch ~f3 ~rs1 ~rs2 target =
  let short =
    { Fixup.fbytes = word (b_type ~op:op_branch ~f3 ~rs1 ~rs2 ~imm:0l);
      ffixups = [ Fixup.make ~at:0 ~size:4 ~pcrel:true ~pcbase:0 ~field:Fixup.Rv_branch ~branch:true target ];
      fbits = 13 } in
  let long =
    (* the condition inverted (the low bit of funct3 does it), stepping
       over the jump, and then the jump itself *)
    { Fixup.fbytes =
        word (b_type ~op:op_branch ~f3:(f3 lxor 1) ~rs1 ~rs2 ~imm:8l)
        ^ word (j_type ~op:op_jal ~rd:0 ~imm:0l);
      ffixups = [ Fixup.make ~at:4 ~size:4 ~pcrel:true ~pcbase:4 ~field:Fixup.Rv_jal ~branch:true target ];
      fbits = 21 } in
  ignore (has_symbol target);
  Relaxable { short; long }

and jump ~rd target =
  Fixed (word (j_type ~op:op_jal ~rd ~imm:0l),
         [ Fixup.make ~at:0 ~size:4 ~pcrel:true ~pcbase:0 ~field:Fixup.Rv_jal ~branch:true target ])

and call ~rd target =
  (* auipc into the link register (or t1 for a tail call), then jalr
     through it; one R_RISCV_CALL_PLT covers the pair *)
  let scratch = if rd = 1 then 1 else 6 in
  Fixed (word (u_type ~op:op_auipc ~rd:scratch ~imm:0l)
         ^ word (i_type ~op:op_jalr ~f3:0 ~rd ~rs1:scratch ~imm:0l),
         [ Fixup.make ~at:0 ~size:8 ~pcrel:true ~pcbase:0 ~field:Fixup.Rv_call ~branch:true target ])

(* ---- Patching a value the assembler worked out itself ----------------- *)

(* A value goes into the instruction's fields rather than into whole
   bytes, so patching means rebuilding those fields.  The field is
   cleared first, so that this works for the linker too: it patches
   fields the assembler already wrote a provisional value into.

   Anything the assembler leaves to the linker keeps its provisional
   value and a relocation, which [reloc_type] names. *)
let patch (bytes : Bytes.t) at (f : Fixup.field) (v : int64) =
  let get k =
    let b i = Int32.of_int (Char.code (Bytes.get bytes (k + i))) in
    Int32.logor (b 0)
      (Int32.logor (Int32.shift_left (b 1) 8)
         (Int32.logor (Int32.shift_left (b 2) 16) (Int32.shift_left (b 3) 24))) in
  let set k w = Bytes.blit_string (word w) 0 bytes k 4 in
  (* what of the instruction is not the field *)
  let keep k mask = Int32.logand (get k) (Int32.lognot mask) in
  let i_mask = bits (-1l) 20 12 in
  let s_mask = Int32.logor (bits (-1l) 7 5) (bits (-1l) 25 7) in
  let u_mask = bits (-1l) 12 20 in
  let b_mask = Int32.logor (bits (-1l) 7 5) (bits (-1l) 25 7) in
  let j_mask = bits (-1l) 12 20 in
  let get k mask = keep k mask in
  let reach bits what =
    if not (Fixup.fits_signed bits v) then bad "%s is %Ld bytes away, too far" what v in
  let v32 = Int64.to_int32 v in
  (* the twenty high bits, with the low twelve's sign carried into them,
     which is what %hi means and what an auipc pair needs *)
  let hi20 () = Int64.to_int32 (Int64.shift_right (Int64.add v 0x800L) 12) in
  match f with
  | Fixup.Whole -> bad "a whole-byte field on this machine"
  | Fixup.Rv_none -> ()
  | Fixup.Rv_hi20 -> set at (Int32.logor (get at u_mask) (bits (hi20 ()) 12 20))
  | Fixup.Rv_lo12_i -> set at (Int32.logor (get at i_mask) (bits v32 20 12))
  | Fixup.Rv_lo12_s ->
      set at (Int32.logor (get at s_mask) (Int32.logor (bits v32 7 5) (field v32 25 7 5)))
  | Fixup.Rv_branch ->
      reach 13 "the branch target";
      set at (Int32.logor (get at b_mask)
                (Int32.logor (field v32 8 4 1)
                   (Int32.logor (field v32 7 1 11)
                      (Int32.logor (field v32 25 6 5) (field v32 31 1 12)))))
  | Fixup.Rv_jal ->
      reach 21 "the jump target";
      set at (Int32.logor (get at j_mask)
                (Int32.logor (field v32 21 10 1)
                   (Int32.logor (field v32 20 1 11)
                      (Int32.logor (field v32 12 8 12) (field v32 31 1 20)))))
  | Fixup.Rv_call ->
      (* the pair: the auipc takes the high twenty bits and the jalr the
         low twelve, both measured from the auipc *)
      set at (Int32.logor (get at u_mask) (bits (hi20 ()) 12 20));
      set (at + 4) (Int32.logor (get (at + 4) i_mask) (bits v32 20 12))

(* ---- Which relocation a field asks for -------------------------------- *)

(* The field decides it, together with the modifier the source wrote: the
   same twelve bits are a LO12_I when they follow a %lo and a
   PCREL_LO12_I when they follow a %pcrel_lo, and the linker treats the
   two quite differently. *)
let reloc_type ~(field : Fixup.field) ~modifier ~size ~pcrel =
  let open Elf in
  match field, modifier with
  | Fixup.Rv_hi20, Some ("hi" | "HI") -> r_riscv_hi20
  | Fixup.Rv_hi20, Some "pcrel_hi" -> r_riscv_pcrel_hi20
  | Fixup.Rv_hi20, Some "got_pcrel_hi" -> r_riscv_got_hi20
  | Fixup.Rv_hi20, Some "tls_ie_pcrel_hi" -> r_riscv_tls_got_hi20
  | Fixup.Rv_hi20, Some "tls_gd_pcrel_hi" -> r_riscv_tls_gd_hi20
  | Fixup.Rv_hi20, Some "tprel_hi" -> r_riscv_tprel_hi20
  | Fixup.Rv_hi20, None -> r_riscv_hi20
  | Fixup.Rv_lo12_i, Some "pcrel_lo" -> r_riscv_pcrel_lo12_i
  | Fixup.Rv_lo12_i, Some "tprel_lo" -> r_riscv_tprel_lo12_i
  | Fixup.Rv_lo12_i, (Some ("lo" | "LO") | None) -> r_riscv_lo12_i
  | Fixup.Rv_lo12_s, Some "pcrel_lo" -> r_riscv_pcrel_lo12_s
  | Fixup.Rv_lo12_s, Some "tprel_lo" -> r_riscv_tprel_lo12_s
  | Fixup.Rv_lo12_s, (Some ("lo" | "LO") | None) -> r_riscv_lo12_s
  | Fixup.Rv_branch, _ -> r_riscv_branch
  | Fixup.Rv_jal, _ -> r_riscv_jal
  | Fixup.Rv_call, _ -> r_riscv_call_plt
  (* "add a0,a0,tp,%tprel_add(x)" changes no field of the instruction:
     the fourth operand exists only so that the linker knows which symbol
     the twenty-high/twelve-low pair around it belongs to. *)
  | Fixup.Rv_none, Some "tprel_add" -> r_riscv_tprel_add
  | Fixup.Rv_none, _ -> bad "that relocation marks nothing"
  (* data, and the tables: whole bytes holding an address or a
     difference of two labels *)
  | Fixup.Whole, None ->
      (match size, pcrel with
       | 8, false -> r_riscv_64
       | 4, false -> r_riscv_32
       | 4, true -> r_riscv_32_pcrel
       | n, _ -> bad "no relocation for %d bytes on this machine" n)
  | Fixup.Whole, Some m -> bad "unsupported relocation modifier %%%s" m
  | _, Some m -> bad "%%%s does not belong on that instruction" m

(* ---- The address pseudo-instructions ---------------------------------- *)

(* "la rd, sym" is two instructions, and the second names the first by a
   label: the linker pairs a %pcrel_lo with the %pcrel_hi it belongs to
   by that label's address.  So these cannot be encoded here -- an
   encoder cannot define a label -- and [Assemble] expands them, asking
   this only what the pair should be.

   What "la" means depends on ".option": with position independence it
   loads the address from the global offset table, and without it the
   address is pc-relative arithmetic.  "lla" is always the second form,
   which is what a compiler uses for a symbol it knows is local; the two
   thread-local forms read their offset from the table. *)
(* What the pair is: the modifier on the high half, then either an
   "addi" that adds the low half to the same register, or a load or
   store whose address is the low half of the register the auipc landed
   in.  [scratch] is that register: the destination itself where it can
   be, and the one the source named where it cannot -- a float cannot
   hold an address, and a store's destination is memory. *)
type pair =
  | Add of string                                   (* hi modifier *)
  | Access of string * string * operand * reg       (* hi modifier, mnemonic, the value, scratch *)

let address_pair ~pic (i : instruction) =
  let symbolic = function
    | Imm (Sym _) | Imm (Bin (_, Sym _, _)) -> true
    | _ -> false in
  match i.mnemonic, i.operands with
  | ("la" | "lla" | "la.tls.ie" | "la.tls.gd"), [ Reg ({ rclass = Ireg; _ } as rd); _ ] ->
      (match i.mnemonic with
       | "lla" -> Some (Add "pcrel_hi")
       | "la" -> if pic then Some (Access ("got_pcrel_hi", "ld", Reg rd, rd)) else Some (Add "pcrel_hi")
       | "la.tls.ie" -> Some (Access ("tls_ie_pcrel_hi", "ld", Reg rd, rd))
       | _ -> Some (Add "tls_gd_pcrel_hi"))
  | ("la" | "lla" | "la.tls.ie" | "la.tls.gd"), _ ->
      bad "%s takes a register and a symbol" i.mnemonic
  (* An integer load from a symbol: the destination holds the address
     first, so it needs no scratch of its own. *)
  | m, [ (Reg ({ rclass = Ireg; _ } as r) as rd); sym ]
    when List.mem_assoc m load_f3 && symbolic sym ->
      Some (Access ("pcrel_hi", m, rd, r))
  (* A store, or a load into a floating-point register, which must be
     told which register to build the address in. *)
  | m, [ rd; sym; Reg ({ rclass = Ireg; _ } as tmp) ]
    when (List.mem_assoc m load_f3 || List.mem_assoc m store_f3
          || List.mem_assoc m fload_f3 || List.mem_assoc m fstore_f3) && symbolic sym ->
      Some (Access ("pcrel_hi", m, rd, tmp))
  | m, [ _; sym ] when (List.mem_assoc m fload_f3 || List.mem_assoc m store_f3
                        || List.mem_assoc m fstore_f3) && symbolic sym ->
      bad "%s from a symbol needs a register to build the address in" m
  | _ -> None

(* the modifier attached to a symbol, wherever it sits in an expression *)
let rec with_modifier m = function
  | Sym (name, _) -> Sym (name, Some m)
  | Bin (op, a, b) -> Bin (op, with_modifier m a, b)
  | e -> e
