(* Instruction selection for RV64: [Ir] to [Asm].

   The same chapter-one shape as the other machine: every IR virtual
   register lives in an eight-byte frame slot, and each instruction
   loads its operands into fixed scratch registers, computes, and stores
   the result back.  Nothing is kept in a register between
   instructions, so there is no register allocation to get wrong and the
   code reads like the IR it came from.

   What the machine imposes, and the reader should know before the code:

   - Every instruction is three-address and destination-first:
     `add a0,a1,a2' means a0 := a1 + a2.

   - There is one addressing mode, `offset(reg)', and the offset is a
     signed twelve bits.  A frame bigger than that cannot be reached
     directly, so [addr] builds the address in a register when it has
     to; nothing else in this file worries about the range.

   - An immediate in an arithmetic instruction is also twelve signed
     bits.  [li] loads any wider constant, leaving the assembler to
     choose the sequence, which is the one thing here that is easier in
     assembly than in machine code.

   - RV64 holds a 32-bit value sign-extended in its 64-bit register, and
     the `w' forms of the arithmetic keep it that way (`addw', `mulw').
     Using the plain forms on 32-bit values would be wrong, not merely
     slower: a comparison of the results would see the high bits.

   The registers used: t0 and t1 hold the operands of the instruction
   being selected, t2 an address, and a0..a7 the arguments of a call.
   s0 is the frame pointer, as in the code gcc generates here, so that a
   debugger and the unwinder see what they expect. *)

open Asm

type st = {
  pic : bool;
  debug : bool;
  mutable code : instr list;          (* reversed *)
  mutable regs : (int, int) Hashtbl.t; (* Ir register -> offset from s0 *)
  mutable slots : int array;          (* Ir slot -> offset from s0 *)
  mutable frame : int;                (* bytes below s0 given out so far *)
  mutable fname : string;
  mutable label_count : int;
  mutable float_consts : ((int64 * bool) * string) list;  (* bits and whether four bytes *)
  mutable wide : (int, int) Hashtbl.t;    (* long double register -> its sixteen-byte slot *)
  mutable wide_consts : ((int64 * int64) * string) list;
  mutable const_count : int;
  mutable hidden_ptr : int;           (* where the aggregate-return pointer was saved *)
  (* A va_list here is one pointer walking upwards, so the argument
     registers a variadic function saves have to sit immediately below
     the arguments its caller pushed: the save area is the top of the
     frame, and the return address and frame pointer go below it. *)
  mutable va_bytes : int;             (* size of that area, 0 if not variadic *)
  mutable named_int : int;            (* integer registers the named parameters took *)
  tls : (string, unit) Hashtbl.t;     (* thread-local symbols, defined or declared *)
  mutable moved_sp : bool;            (* the function moved sp itself, for a variable length array *)
  mutable consts : data list;         (* the read-only constants the functions needed *)
  mutable files : (string, int) Hashtbl.t;  (* source file -> its number in the .file table *)
  mutable next_file : int;
}

let emit st i = st.code <- i :: st.code
let op st m ops = emit st (Op (m, ops))
let fresh_label st hint =
  st.label_count <- st.label_count + 1;
  Printf.sprintf ".L%s.%s%d" st.fname hint st.label_count

let round_up n a = (n + a - 1) / a * a
let is_float = function Ir.F32 | Ir.F64 | Ir.F80 -> true | _ -> false
let width = function Ir.I8 -> 1 | Ir.I16 -> 2 | Ir.I32 -> 4 | Ir.F32 -> 4 | _ -> 8

(* ---- the frame ------------------------------------------------------ *)

(* Below s0 come the varargs save area (nothing, usually), then the
   return address and the caller's frame pointer, then the locals. *)
let saved_bytes = 16

let ra_offset st = - (st.va_bytes + 8)
let fp_offset st = - (st.va_bytes + 16)

let alloc st size align =
  st.frame <- round_up (st.frame + size) align;
  - st.frame

let reg_slot st r =
  match Hashtbl.find_opt st.regs r with
  | Some off -> off
  | None -> let off = alloc st 8 8 in Hashtbl.replace st.regs r off; off

(* A twelve-bit signed offset reaches most of a frame; beyond that the
   address has to be built.  Every load and store goes through this, so
   the rest of the file may pretend the offset always fits. *)
let fits12 n = n >= -2048 && n <= 2047

let addr st base off (scratch : reg) =
  if fits12 off then Mem (base, off)
  else begin
    op st "li" [ Reg scratch; Imm (Int64.of_int off) ];
    op st "add" [ Reg scratch; Reg base; Reg scratch ];
    Mem (scratch, 0)
  end

(* ---- loading and storing -------------------------------------------- *)

let load_mnemonic ty signed =
  match ty, signed with
  | Ir.I8, true -> "lb" | Ir.I8, false -> "lbu"
  | Ir.I16, true -> "lh" | Ir.I16, false -> "lhu"
  | Ir.I32, true -> "lw" | Ir.I32, false -> "lwu"
  | Ir.F32, _ -> "flw"
  | Ir.F64, _ | Ir.F80, _ -> "fld"
  | _ -> "ld"

let store_mnemonic ty =
  match ty with
  | Ir.I8 -> "sb" | Ir.I16 -> "sh" | Ir.I32 -> "sw"
  | Ir.F32 -> "fsw" | Ir.F64 | Ir.F80 -> "fsd"
  | _ -> "sd"

(* A floating-point constant goes in the read-only section and is
   loaded from there, since the machine has no instruction that takes
   one.  The width matters: a float is four bytes of its own encoding,
   not the top half of the double with the same value, and loading four
   bytes of a double's encoding gives a different number entirely --
   which is how the tests found this. *)
let float_const st (ty : Ir.ty) (v : float) =
  let narrow = ty = Ir.F32 in
  let bits = if narrow then Int64.of_int32 (Int32.bits_of_float v) else Int64.bits_of_float v in
  match List.assoc_opt (bits, narrow) st.float_consts with
  | Some l -> l
  | None ->
      st.const_count <- st.const_count + 1;
      let l = Printf.sprintf ".LC%s.%d" st.fname st.const_count in
      st.float_consts <- ((bits, narrow), l) :: st.float_consts;
      l

(* The address of a symbol.  Without position independence that is the
   twenty-high/twelve-low pair the machine is built around; with it, a
   load from the global offset table, which the assembler spells for us
   as one pseudo-instruction.

   A thread-local symbol is an offset from the thread pointer, tp, and
   the two forms are the same two choices the other machine makes.
   Without position independence the offset is known at link time and
   the sequence is the twenty-high/twelve-low pair again, with the
   relocations that name a thread-local offset -- the `add' carrying a
   third operand that exists only to tell the linker which symbol the
   pair belongs to, which is how this machine spells it.  With position
   independence the offset is read from the global offset table, which
   is initial-exec, the model that needs no call into the dynamic
   loader; `la.tls.ie' is the assembler's name for that load. *)
let load_sym st sym (dst : reg) =
  if Hashtbl.mem st.tls sym then
    if st.pic then begin
      op st "la.tls.ie" [ Reg dst; Sym (sym, 0) ];
      op st "add" [ Reg dst; Reg dst; Reg TP ]
    end else begin
      op st "lui" [ Reg dst; Sym ("%tprel_hi(" ^ sym ^ ")", 0) ];
      op st "add" [ Reg dst; Reg dst; Reg TP; Sym ("%tprel_add(" ^ sym ^ ")", 0) ];
      op st "addi" [ Reg dst; Reg dst; Sym ("%tprel_lo(" ^ sym ^ ")", 0) ]
    end
  else if st.pic then op st "la" [ Reg dst; Sym (sym, 0) ]
  else begin
    op st "lui" [ Reg dst; Sym ("%hi(" ^ sym ^ ")", 0) ];
    op st "addi" [ Reg dst; Reg dst; Sym ("%lo(" ^ sym ^ ")", 0) ]
  end

(* An immediate in the form the machine keeps a value of its type in.
   RV64 holds a 32-bit value sign-extended in its 64-bit register, and
   every load here sign-extends, so the whole back end may compare two
   registers whole and get an unsigned answer right.  An immediate has to
   join that convention: 4294967295 as an unsigned int is the pattern of
   -1, not 0x00000000ffffffff.  Materialising it the other way makes
   `u == 4294967295u' false, which is how this was found. *)
let narrow (ty : Ir.ty) (v : int64) =
  if is_float ty then v
  else
    let bits = 8 * width ty in
    if bits >= 64 then v
    else Int64.shift_right (Int64.shift_left v (64 - bits)) (64 - bits)

(* An operand into a named integer register. *)
let rec load_int st (ty : Ir.ty) (o : Ir.operand) (dst : reg) =
  match o with
  | Ir.Imm v -> op st "li" [ Reg dst; Imm (narrow ty v) ]
  | Ir.Reg r -> op st (load_mnemonic ty true) [ Reg dst; addr st (S 0) (reg_slot st r) dst ]
  | Ir.Slot k -> op st "addi" [ Reg dst; Reg (S 0); Imm (Int64.of_int st.slots.(k)) ]
  | Ir.Sym s -> load_sym st s dst
  | Ir.Fimm f -> load_int st ty (Ir.Imm (Int64.bits_of_float f)) dst

let load_addr st o dst =
  match o with
  | Ir.Slot k ->
      let off = st.slots.(k) in
      if fits12 off then op st "addi" [ Reg dst; Reg (S 0); Imm (Int64.of_int off) ]
      else begin
        op st "li" [ Reg dst; Imm (Int64.of_int off) ];
        op st "add" [ Reg dst; Reg (S 0); Reg dst ]
      end
  | _ -> load_int st Ir.I64 o dst

let load_float st (ty : Ir.ty) (o : Ir.operand) (dst : reg) =
  match o with
  | Ir.Fimm f ->
      let l = float_const st ty f in
      load_sym st l (T 2);
      op st (load_mnemonic ty true) [ Reg dst; Mem (T 2, 0) ]
  | Ir.Reg r -> op st (load_mnemonic ty true) [ Reg dst; addr st (S 0) (reg_slot st r) (T 2) ]
  | _ -> failwith "Riscv64.Select: a floating-point value from an integer operand"

let load st ty o dst_i dst_f = if is_float ty then load_float st ty o dst_f else load_int st ty o dst_i

let store st (ty : Ir.ty) (r : int) (src : reg) =
  op st (store_mnemonic ty) [ Reg src; addr st (S 0) (reg_slot st r) (T 2) ]

(* ---- long double ---------------------------------------------------- *)

(* long double here is IEEE binary128, which the machine has no
   instructions for: the arithmetic is a call into the compiler's
   support library, and a value travels in two integer registers, being
   sixteen bytes (the psABI passes it as it would any pair of words).
   So an F80 register -- the IR's name for long double, from the machine
   where it is x87's eighty bits -- lives in a sixteen-byte slot here,
   and everything below moves it two words at a time. *)

let slot16 st r =
  match Hashtbl.find_opt st.wide r with
  | Some off -> off
  | None -> let off = alloc st 16 16 in Hashtbl.replace st.wide r off; off

(* A double converted to binary128, exactly: every double is one.  The
   sign stays, the exponent is rebiased from 1023 to 16383, and the
   mantissa's fifty-two bits move up to the top of the hundred and
   twelve. *)
let binary128_of_double (v : float) : int64 * int64 =
  let bits = Int64.bits_of_float v in
  let sign = Int64.logand bits Int64.min_int in
  let exp = Int64.to_int (Int64.logand (Int64.shift_right_logical bits 52) 0x7ffL) in
  let mant = Int64.logand bits 0xfffffffffffffL in
  if exp = 0 && Int64.equal mant 0L then (0L, sign)          (* a zero, with its sign *)
  else if exp = 0x7ff then
    (* an infinity or a NaN: the exponent is all ones and the mantissa
       keeps its top bits, so a quiet NaN stays quiet *)
    (Int64.shift_left mant 60,
     Int64.logor sign (Int64.logor 0x7fff000000000000L (Int64.shift_right_logical mant 4)))
  else
    let exp' = exp - 1023 + 16383 in
    (Int64.shift_left mant 60,
     Int64.logor sign
       (Int64.logor (Int64.shift_left (Int64.of_int exp') 48) (Int64.shift_right_logical mant 4)))

let wide_const st (v : float) =
  let (lo, hi) = binary128_of_double v in
  match List.assoc_opt (lo, hi) st.wide_consts with
  | Some l -> l
  | None ->
      st.const_count <- st.const_count + 1;
      let l = Printf.sprintf ".LW%s.%d" st.fname st.const_count in
      st.wide_consts <- ((lo, hi), l) :: st.wide_consts;
      l

(* the two words of a long double, into a pair of integer registers *)
let load_wide st (o : Ir.operand) (lo : reg) (hi : reg) =
  match o with
  | Ir.Reg r ->
      let off = slot16 st r in
      op st "ld" [ Reg lo; addr st (S 0) off (T 2) ];
      op st "ld" [ Reg hi; addr st (S 0) (off + 8) (T 2) ]
  | Ir.Fimm v ->
      let l = wide_const st v in
      load_sym st l (T 2);
      op st "ld" [ Reg lo; Mem (T 2, 0) ];
      op st "ld" [ Reg hi; Mem (T 2, 8) ]
  | _ -> failwith "Riscv64.Select: a long double from an operand that is not one"

let store_wide st (r : int) (lo : reg) (hi : reg) =
  let off = slot16 st r in
  op st "sd" [ Reg lo; addr st (S 0) off (T 2) ];
  op st "sd" [ Reg hi; addr st (S 0) (off + 8) (T 2) ]

(* A call into the support library.  Each long double argument takes two
   integer registers, each narrower value one, and the result comes back
   the same way. *)
let soft_call st name (args : [ `Wide of Ir.operand | `Word of Ir.ty * Ir.operand ] list) =
  let next = ref 0 in
  List.iter (fun a ->
      match a with
      | `Wide o ->
          load_wide st o (A !next) (A (!next + 1));
          next := !next + 2
      | `Word (ty, o) ->
          if is_float ty then begin
            load_float st ty o (FT 0);
            op st (if ty = Ir.F32 then "fmv.x.w" else "fmv.x.d") [ Reg (A !next); Reg (FT 0) ]
          end else load_int st ty o (A !next);
          incr next)
    args;
  op st "call" [ Sym (name, 0) ]

(* ---- arithmetic ------------------------------------------------------ *)

(* The mnemonic for an operation at a type.  A 32-bit operation uses the
   `w' form so that its result stays sign-extended. *)
let int_mnemonic (b : Ir.binop) (ty : Ir.ty) =
  let w = ty = Ir.I32 in
  match b with
  | Ir.Add -> if w then "addw" else "add"
  | Ir.Sub -> if w then "subw" else "sub"
  | Ir.Mul -> if w then "mulw" else "mul"
  | Ir.Sdiv -> if w then "divw" else "div"
  | Ir.Udiv -> if w then "divuw" else "divu"
  | Ir.Srem -> if w then "remw" else "rem"
  | Ir.Urem -> if w then "remuw" else "remu"
  | Ir.And -> "and" | Ir.Or -> "or" | Ir.Xor -> "xor"
  | Ir.Shl -> if w then "sllw" else "sll"
  | Ir.Sshr -> if w then "sraw" else "sra"
  | Ir.Ushr -> if w then "srlw" else "srl"
  | Ir.Fadd | Ir.Fsub | Ir.Fmul | Ir.Fdiv ->
      failwith "Riscv64.Select: a floating-point operation asked for an integer mnemonic"

let float_mnemonic (b : Ir.binop) (ty : Ir.ty) =
  let s = if ty = Ir.F32 then ".s" else ".d" in
  match b with
  | Ir.Fadd -> "fadd" ^ s | Ir.Fsub -> "fsub" ^ s
  | Ir.Fmul -> "fmul" ^ s | Ir.Fdiv -> "fdiv" ^ s
  | _ -> failwith "Riscv64.Select: an integer operation asked for a floating-point mnemonic"

(* the support library's names, which are the ones gcc calls *)
let soft_binop = function
  | Ir.Fadd -> "__addtf3" | Ir.Fsub -> "__subtf3"
  | Ir.Fmul -> "__multf3" | Ir.Fdiv -> "__divtf3"
  | _ -> failwith "Riscv64.Select: that operation has no long double form"

(* ---- an operation that reports its own overflow --------------------- *)

(* This machine has no condition flags: overflow is detected in
   arithmetic.  Below the register's width there is nothing to detect,
   only something to notice: the exact result of an operation on two
   32-bit values fits in a 64-bit register, so the sequence computes it
   there and asks whether narrowing it back lost anything.  At the
   register's own width the answer comes from the operands' signs, as
   the RISC-V manual's commentary on the integer instructions describes,
   or from the high half of the product for a multiplication. *)

(* the w-bit value in [r], extended to fill the register *)
let extend st (ty : Ir.ty) signed (r : reg) =
  let bits = 64 - 8 * width ty in
  if bits = 0 then ()
  else if ty = Ir.I32 && signed then op st "sext.w" [ Reg r; Reg r ]
  else begin
    op st "slli" [ Reg r; Reg r; Imm (Int64.of_int bits) ];
    op st (if signed then "srai" else "srli") [ Reg r; Reg r; Imm (Int64.of_int bits) ]
  end

let binop_overflow st (b : Ir.binop) (ty : Ir.ty) signed (r : int) (flag : int) a c =
  (* t0 and t1 hold the operands at their exact values, t3 the result
     and t4 the flag; t5 is scratch.  t2 is left alone: [store] uses it
     to reach a slot a long way from the frame pointer. *)
  load_int st ty a (T 0); extend st ty signed (T 0);
  load_int st ty c (T 1); extend st ty signed (T 1);
  if width ty < 8 then begin
    op st (match b with Ir.Add -> "add" | Ir.Sub -> "sub" | _ -> "mul")
      [ Reg (T 3); Reg (T 0); Reg (T 1) ];
    op st "mv" [ Reg (T 5); Reg (T 3) ];
    extend st ty signed (T 5);
    op st "xor" [ Reg (T 4); Reg (T 3); Reg (T 5) ];
    op st "snez" [ Reg (T 4); Reg (T 4) ]
  end else begin
    match b, signed with
    | Ir.Add, true ->
        (* the sum has the wrong sign for both operands *)
        op st "add" [ Reg (T 3); Reg (T 0); Reg (T 1) ];
        op st "xor" [ Reg (T 4); Reg (T 0); Reg (T 3) ];
        op st "xor" [ Reg (T 5); Reg (T 1); Reg (T 3) ];
        op st "and" [ Reg (T 4); Reg (T 4); Reg (T 5) ];
        op st "slti" [ Reg (T 4); Reg (T 4); Imm 0L ]
    | Ir.Add, false ->
        (* a carry out: the sum came out below one of the addends *)
        op st "add" [ Reg (T 3); Reg (T 0); Reg (T 1) ];
        op st "sltu" [ Reg (T 4); Reg (T 3); Reg (T 0) ]
    | Ir.Sub, true ->
        (* the operands differ in sign and the difference agrees with the
           subtrahend rather than with what it was taken from *)
        op st "sub" [ Reg (T 3); Reg (T 0); Reg (T 1) ];
        op st "xor" [ Reg (T 4); Reg (T 0); Reg (T 1) ];
        op st "xor" [ Reg (T 5); Reg (T 0); Reg (T 3) ];
        op st "and" [ Reg (T 4); Reg (T 4); Reg (T 5) ];
        op st "slti" [ Reg (T 4); Reg (T 4); Imm 0L ]
    | Ir.Sub, false ->
        op st "sltu" [ Reg (T 4); Reg (T 0); Reg (T 1) ];
        op st "sub" [ Reg (T 3); Reg (T 0); Reg (T 1) ]
    | _, true ->
        (* the high half of a signed product is the sign of the low half
           repeated, and nothing else *)
        op st "mulh" [ Reg (T 4); Reg (T 0); Reg (T 1) ];
        op st "mul" [ Reg (T 3); Reg (T 0); Reg (T 1) ];
        op st "srai" [ Reg (T 5); Reg (T 3); Imm 63L ];
        op st "xor" [ Reg (T 4); Reg (T 4); Reg (T 5) ];
        op st "snez" [ Reg (T 4); Reg (T 4) ]
    | _, false ->
        op st "mulhu" [ Reg (T 4); Reg (T 0); Reg (T 1) ];
        op st "mul" [ Reg (T 3); Reg (T 0); Reg (T 1) ];
        op st "snez" [ Reg (T 4); Reg (T 4) ]
  end;
  store st ty r (T 3);
  store st Ir.I32 flag (T 4)

let binop st (b : Ir.binop) (ty : Ir.ty) (r : int) a c =
  if ty = Ir.F80 then begin
    soft_call st (soft_binop b) [ `Wide a; `Wide c ];
    store_wide st r (A 0) (A 1)
  end else
  match b with
  | Ir.Fadd | Ir.Fsub | Ir.Fmul | Ir.Fdiv ->
      load_float st ty a (FT 0);
      load_float st ty c (FT 1);
      op st (float_mnemonic b ty) [ Reg (FT 0); Reg (FT 0); Reg (FT 1) ];
      store st ty r (FT 0)
  | _ ->
      load_int st ty a (T 0);
      load_int st ty c (T 1);
      op st (int_mnemonic b ty) [ Reg (T 0); Reg (T 0); Reg (T 1) ];
      store st ty r (T 0)

(* A comparison leaves 0 or 1 in a register, which is what the IR asks
   for; the machine has set-less-than and nothing else, so the other
   nine conditions are built from it and from equality against zero. *)
let compare_int st (c : Ir.cond) ty a b (dst : reg) =
  load_int st ty a (T 0);
  load_int st ty b (T 1);
  let slt = "slt" and sltu = "sltu" in
  match c with
  | Ir.Eq -> op st "sub" [ Reg dst; Reg (T 0); Reg (T 1) ]; op st "seqz" [ Reg dst; Reg dst ]
  | Ir.Ne -> op st "sub" [ Reg dst; Reg (T 0); Reg (T 1) ]; op st "snez" [ Reg dst; Reg dst ]
  | Ir.Slt -> op st slt [ Reg dst; Reg (T 0); Reg (T 1) ]
  | Ir.Sgt -> op st slt [ Reg dst; Reg (T 1); Reg (T 0) ]
  | Ir.Sle -> op st slt [ Reg dst; Reg (T 1); Reg (T 0) ]; op st "xori" [ Reg dst; Reg dst; Imm 1L ]
  | Ir.Sge -> op st slt [ Reg dst; Reg (T 0); Reg (T 1) ]; op st "xori" [ Reg dst; Reg dst; Imm 1L ]
  | Ir.Ult -> op st sltu [ Reg dst; Reg (T 0); Reg (T 1) ]
  | Ir.Ugt -> op st sltu [ Reg dst; Reg (T 1); Reg (T 0) ]
  | Ir.Ule -> op st sltu [ Reg dst; Reg (T 1); Reg (T 0) ]; op st "xori" [ Reg dst; Reg dst; Imm 1L ]
  | Ir.Uge -> op st sltu [ Reg dst; Reg (T 0); Reg (T 1) ]; op st "xori" [ Reg dst; Reg dst; Imm 1L ]
  | Ir.Feq | Ir.Fne | Ir.Flt | Ir.Fle | Ir.Fgt | Ir.Fge ->
      failwith "Riscv64.Select: a floating-point comparison came to the integer path"

(* A comparison of long doubles is a call that answers like strcmp: a
   negative, zero or positive integer, and the condition is then a test
   of that.  __eqtf2 and __netf2 answer zero for equal, which is the
   same shape. *)
let compare_wide st (c : Ir.cond) a b (dst : reg) =
  let name = match c with
    | Ir.Feq | Ir.Fne -> "__eqtf2"
    | Ir.Flt -> "__lttf2" | Ir.Fle -> "__letf2"
    | Ir.Fgt -> "__gttf2" | Ir.Fge -> "__getf2"
    | _ -> failwith "Riscv64.Select: that comparison has no long double form" in
  soft_call st name [ `Wide a; `Wide b ];
  (match c with
   | Ir.Feq -> op st "seqz" [ Reg dst; Reg (A 0) ]
   | Ir.Fne -> op st "snez" [ Reg dst; Reg (A 0) ]
   | Ir.Flt -> op st "slti" [ Reg dst; Reg (A 0); Imm 0L ]
   | Ir.Fle -> op st "slti" [ Reg dst; Reg (A 0); Imm 1L ]
   | Ir.Fgt -> op st "sgtz" [ Reg dst; Reg (A 0) ]
   | Ir.Fge -> op st "slti" [ Reg dst; Reg (A 0); Imm 0L ];
               op st "xori" [ Reg dst; Reg dst; Imm 1L ]
   | _ -> ())

let compare_float st (c : Ir.cond) ty a b (dst : reg) =
  if ty = Ir.F80 then compare_wide st c a b dst
  else begin
  load_float st ty a (FT 0);
  load_float st ty b (FT 1);
  let s = if ty = Ir.F32 then ".s" else ".d" in
  match c with
  | Ir.Feq -> op st ("feq" ^ s) [ Reg dst; Reg (FT 0); Reg (FT 1) ]
  | Ir.Fne -> op st ("feq" ^ s) [ Reg dst; Reg (FT 0); Reg (FT 1) ]; op st "xori" [ Reg dst; Reg dst; Imm 1L ]
  | Ir.Flt -> op st ("flt" ^ s) [ Reg dst; Reg (FT 0); Reg (FT 1) ]
  | Ir.Fle -> op st ("fle" ^ s) [ Reg dst; Reg (FT 0); Reg (FT 1) ]
  | Ir.Fgt -> op st ("flt" ^ s) [ Reg dst; Reg (FT 1); Reg (FT 0) ]
  | Ir.Fge -> op st ("fle" ^ s) [ Reg dst; Reg (FT 1); Reg (FT 0) ]
  | _ -> failwith "Riscv64.Select: an integer comparison came to the floating-point path"
  end

(* ---- conversions ---------------------------------------------------- *)

let conv st (c : Ir.conv) (r : int) (o : Ir.operand) =
  match c with
  | Ir.Sext (from, _) -> load_int st from o (T 0); store st Ir.I64 r (T 0)
  | Ir.Zext (from, _) ->
      (* the load sign-extends, so the high bits are cleared by hand *)
      (match from with
       | Ir.I8 -> op st (load_mnemonic Ir.I8 false) [ Reg (T 0); addr st (S 0) (reg_slot st (match o with Ir.Reg x -> x | _ -> 0)) (T 2) ]
       | _ -> load_int st from o (T 0));
      (match from with
       | Ir.I8 -> ()
       | Ir.I16 -> op st "slli" [ Reg (T 0); Reg (T 0); Imm 48L ]; op st "srli" [ Reg (T 0); Reg (T 0); Imm 48L ]
       | Ir.I32 -> op st "slli" [ Reg (T 0); Reg (T 0); Imm 32L ]; op st "srli" [ Reg (T 0); Reg (T 0); Imm 32L ]
       | _ -> ());
      store st Ir.I64 r (T 0)
  | Ir.Trunc (from, into) -> load_int st from o (T 0); store st into r (T 0)
  | Ir.Fext -> load_float st Ir.F32 o (FT 0); op st "fcvt.d.s" [ Reg (FT 0); Reg (FT 0) ]; store st Ir.F64 r (FT 0)
  | Ir.Ftrunc -> load_float st Ir.F64 o (FT 0); op st "fcvt.s.d" [ Reg (FT 0); Reg (FT 0) ]; store st Ir.F32 r (FT 0)
  | Ir.Stof (from, Ir.F80) ->
      soft_call st (if width from <= 4 then "__floatsitf" else "__floatditf") [ `Word (from, o) ];
      store_wide st r (A 0) (A 1)
  | Ir.Utof (from, Ir.F80) ->
      soft_call st (if width from <= 4 then "__floatunsitf" else "__floatunditf") [ `Word (from, o) ];
      store_wide st r (A 0) (A 1)
  | Ir.Ftos (Ir.F80, into) ->
      soft_call st (if width into <= 4 then "__fixtfsi" else "__fixtfdi") [ `Wide o ];
      store st into r (A 0)
  | Ir.Ftou (Ir.F80, into) ->
      soft_call st (if width into <= 4 then "__fixunstfsi" else "__fixunstfdi") [ `Wide o ];
      store st into r (A 0)
  | Ir.Stof (from, into) ->
      load_int st from o (T 0);
      let s = if into = Ir.F32 then "s" else "d" and w = if width from <= 4 then "w" else "l" in
      op st (Printf.sprintf "fcvt.%s.%s" s w) [ Reg (FT 0); Reg (T 0) ];
      store st into r (FT 0)
  | Ir.Utof (from, into) ->
      load_int st from o (T 0);
      let s = if into = Ir.F32 then "s" else "d" and w = if width from <= 4 then "wu" else "lu" in
      op st (Printf.sprintf "fcvt.%s.%s" s w) [ Reg (FT 0); Reg (T 0) ];
      store st into r (FT 0)
  | Ir.Ftos (from, into) ->
      load_float st from o (FT 0);
      let s = if from = Ir.F32 then "s" else "d" and w = if width into <= 4 then "w" else "l" in
      (* C rounds toward zero when it converts a float to an integer
         (6.3.1.4p1), and the machine takes the mode as an operand *)
      op st (Printf.sprintf "fcvt.%s.%s" w s) [ Reg (T 0); Reg (FT 0); Sym ("rtz", 0) ];
      store st into r (T 0)
  | Ir.Ftou (from, into) ->
      load_float st from o (FT 0);
      let s = if from = Ir.F32 then "s" else "d" and w = if width into <= 4 then "wu" else "lu" in
      op st (Printf.sprintf "fcvt.%s.%s" w s) [ Reg (T 0); Reg (FT 0); Sym ("rtz", 0) ];
      store st into r (T 0)
  (* to and from long double, again through the support library.  The
     names say what they do: extend or truncate between the formats,
     float an integer into one, fix one into an integer. *)
  | Ir.Fconv (from, into) when into = Ir.F80 ->
      let name = match from with
        | Ir.F32 -> "__extendsftf2" | Ir.F64 -> "__extenddftf2"
        | Ir.I64 -> "__floatditf" | Ir.I32 -> "__floatsitf"
        | Ir.I8 | Ir.I16 -> "__floatsitf"
        | Ir.F80 -> "" in
      if name = "" then (load_wide st o (T 0) (T 1); store_wide st r (T 0) (T 1))
      else begin
        soft_call st name [ `Word (from, o) ];
        store_wide st r (A 0) (A 1)
      end
  | Ir.Fconv (from, into) when from = Ir.F80 ->
      let name = match into with
        | Ir.F32 -> "__trunctfsf2" | Ir.F64 -> "__trunctfdf2"
        | Ir.I64 -> "__fixtfdi" | Ir.I32 | Ir.I8 | Ir.I16 -> "__fixtfsi"
        | Ir.F80 -> "" in
      if name = "" then (load_wide st o (T 0) (T 1); store_wide st r (T 0) (T 1))
      else begin
        soft_call st name [ `Wide o ];
        (* the support library is built for this ABI, so a
           floating-point result arrives in fa0 and not in a0 *)
        if is_float into then store st into r (FA 0) else store st into r (A 0)
      end
  | Ir.Fconv (_, _) -> failwith "Riscv64.Select: a conversion between two long doubles"

(* ---- calls ---------------------------------------------------------- *)

(* Where each argument goes.  Integers and pointers take a0..a7,
   floating-point values fa0..fa7, and what is left goes on the stack in
   order; an aggregate travels as its pieces, or as a pointer if the ABI
   said memory.  A variadic call passes everything after the named
   parameters in the integer registers and then the stack, which is what
   the ABI asks for and what makes a va_list a plain pointer. *)
type place = In_int of int | In_float of int | On_stack of int

let assign_args ?(named = None) ~hidden (args : Ir.arg list) =
  let ni = ref (if hidden then 1 else 0) and nf = ref 0 and stack = ref 0 in
  let index = ref 0 in
  (* an argument matching the ellipsis takes an integer register even if
     it is a floating-point value (the psABI's hardware floating-point
     calling convention) *)
  let by_ellipsis () =
    let i = !index in
    match named with Some n -> i >= n | None -> false in
  let places =
    List.map (fun (a : Ir.arg) ->
        let ellipsis = by_ellipsis () in
        incr index;
        match a with
        (* long double is sixteen bytes and travels as two words, in
           integer registers or on the stack, never in a floating-point
           register: the machine has none that wide *)
        | Ir.Scalar (Ir.F80, _) ->
            (* An argument whose alignment is two words starts at an
               even-numbered register, the psABI's rule for a register
               pair; long double is the one scalar that asks for it, and
               printf's first such argument lands in a2 and a3 rather
               than a1 and a2 because of it. *)
            if !ni land 1 = 1 then incr ni;
            if !ni + 2 <= 8 then
              (let p = [ In_int !ni; In_int (!ni + 1) ] in ni := !ni + 2; p)
            else begin
              stack := round_up !stack 16;
              let p = [ On_stack !stack; On_stack (!stack + 8) ] in
              stack := !stack + 16; p
            end
        | Ir.Scalar (ty, _) when is_float ty && ellipsis ->
            if !ni < 8 then (let p = In_int !ni in incr ni; [ p ])
            else (let p = On_stack !stack in stack := !stack + 8; [ p ])
        | Ir.Scalar (ty, _) when is_float ty && !nf < 8 -> let p = In_float !nf in incr nf; [ p ]
        | Ir.Scalar (ty, _) when not (is_float ty) && !ni < 8 -> let p = In_int !ni in incr ni; [ p ]
        | Ir.Scalar _ -> let p = On_stack !stack in stack := !stack + 8; [ p ]
        | Ir.Aggregate a ->
            (match a.passing with
             | Ir.In_memory ->
                 if !ni < 8 then (let p = In_int !ni in incr ni; [ p ])
                 else (let p = On_stack !stack in stack := !stack + 8; [ p ])
             | Ir.In_registers pieces ->
                 (* every piece or none: if they do not all fit, the
                    whole object goes on the stack *)
                 let want_i = List.length (List.filter (fun (p : Ir.piece) -> not p.pfloat) pieces)
                 and want_f = List.length (List.filter (fun (p : Ir.piece) -> p.pfloat) pieces) in
                 if !ni + want_i <= 8 && !nf + want_f <= 8 then
                   List.map (fun (p : Ir.piece) ->
                       if p.pfloat then (let q = In_float !nf in incr nf; q)
                       else (let q = In_int !ni in incr ni; q))
                     pieces
                 else
                   List.map (fun _ ->
                       let q = On_stack !stack in stack := !stack + 8; q)
                     pieces))
      args in
  (places, !ni, !nf, !stack)

let call st (res : Ir.result option) (callee : Ir.operand) (args : Ir.arg list) named =
  let hidden = match res with Some (Ir.Ret_aggregate a) -> a.passing = Ir.In_memory | _ -> false in
  let places, _, _, stack_bytes = assign_args ~named ~hidden args in
  (* room for the arguments that did not fit in registers, kept to the
     sixteen-byte alignment the ABI asks of sp at a call *)
  let area = round_up stack_bytes 16 in
  if area > 0 then op st "addi" [ Reg SP; Reg SP; Imm (Int64.of_int (- area)) ];
  (* the arguments, into their registers or onto the stack *)
  List.iter2 (fun (a : Ir.arg) ps ->
      match a, ps with
      | Ir.Scalar (Ir.F80, o), [ In_int i; In_int j ] -> load_wide st o (A i) (A j)
      | Ir.Scalar (Ir.F80, o), [ On_stack a; On_stack b ] ->
          load_wide st o (T 0) (T 1);
          op st "sd" [ Reg (T 0); Mem (SP, a) ];
          op st "sd" [ Reg (T 1); Mem (SP, b) ]
      | Ir.Scalar (ty, o), [ In_int i ] when is_float ty ->
          (* a floating-point value in an integer register: its bits *)
          load_float st ty o (FT 0);
          op st (if ty = Ir.F32 then "fmv.x.w" else "fmv.x.d") [ Reg (A i); Reg (FT 0) ]
      | Ir.Scalar (ty, o), [ In_int i ] -> load_int st ty o (A i)
      | Ir.Scalar (ty, o), [ In_float i ] -> load_float st ty o (FA i)
      | Ir.Scalar (ty, o), [ On_stack off ] ->
          load st ty o (T 0) (FT 0);
          op st (store_mnemonic ty) [ Reg (if is_float ty then FT 0 else T 0); Mem (SP, off) ]
      (* An object too big for two registers is passed by reference, and
         the reference must be to a copy: the callee may write to its
         parameter, and writing through to the caller's object would be
         wrong.  gcc's callee here uses the pointer in place, so the
         copy has to be made on this side. *)
      | Ir.Aggregate a, [ In_int i ] when a.passing = Ir.In_memory ->
          let tmp = alloc st a.size 8 in
          load_addr st a.addr (T 4);
          op st "addi" [ Reg (T 3); Reg (S 0); Imm (Int64.of_int tmp) ];
          for k = 0 to a.size - 1 do
            op st "lbu" [ Reg (T 0); Mem (T 4, k) ];
            op st "sb" [ Reg (T 0); Mem (T 3, k) ]
          done;
          op st "addi" [ Reg (A i); Reg (S 0); Imm (Int64.of_int tmp) ]
      | Ir.Aggregate a, [ On_stack off ] when a.passing = Ir.In_memory ->
          let tmp = alloc st a.size 8 in
          load_addr st a.addr (T 4);
          op st "addi" [ Reg (T 3); Reg (S 0); Imm (Int64.of_int tmp) ];
          for k = 0 to a.size - 1 do
            op st "lbu" [ Reg (T 0); Mem (T 4, k) ];
            op st "sb" [ Reg (T 0); Mem (T 3, k) ]
          done;
          op st "sd" [ Reg (T 3); Mem (SP, off) ]
      | Ir.Aggregate a, ps ->
          (* the pieces, read from the object a piece at a time *)
          let pieces = match a.passing with Ir.In_registers l -> l | Ir.In_memory -> [] in
          load_addr st a.addr (T 2);
          List.iter2 (fun (p : Ir.piece) place ->
              let ty = if p.pfloat then (if p.psize = 4 then Ir.F32 else Ir.F64)
                else (match p.psize with 1 -> Ir.I8 | 2 -> Ir.I16 | 4 -> Ir.I32 | _ -> Ir.I64) in
              match place with
              | In_int i -> op st (load_mnemonic ty true) [ Reg (A i); Mem (T 2, p.poff) ]
              | In_float i -> op st (load_mnemonic ty true) [ Reg (FA i); Mem (T 2, p.poff) ]
              | On_stack off ->
                  op st (load_mnemonic ty true) [ Reg (T 0); Mem (T 2, p.poff) ];
                  op st (store_mnemonic ty) [ Reg (T 0); Mem (SP, off) ])
            pieces ps
      | _ -> failwith "Riscv64.Select: an argument and its place disagree")
    args places;
  if hidden then
    (match res with
     | Some (Ir.Ret_aggregate a) -> load_addr st a.addr (A 0)
     | _ -> ());
  (* the call itself *)
  (match callee with
   | Ir.Sym s -> op st "call" [ Sym ((if st.pic then s ^ "@plt" else s), 0) ]
   | o -> load_int st Ir.I64 o (T 2); op st "jalr" [ Reg (T 2) ]);
  if area > 0 then op st "addi" [ Reg SP; Reg SP; Imm (Int64.of_int area) ];
  (* and its result *)
  match res with
  | None -> ()
  | Some (Ir.Ret_scalar (Ir.F80, r)) -> store_wide st r (A 0) (A 1)
  | Some (Ir.Ret_scalar (ty, r)) -> store st ty r (if is_float ty then FA 0 else A 0)
  | Some (Ir.Ret_aggregate a) ->
      (match a.passing with
       | Ir.In_memory -> ()                     (* the callee wrote it *)
       | Ir.In_registers pieces ->
           load_addr st a.addr (T 2);
           let ni = ref 0 and nf = ref 0 in
           List.iter (fun (p : Ir.piece) ->
               let ty = if p.pfloat then (if p.psize = 4 then Ir.F32 else Ir.F64)
                 else (match p.psize with 1 -> Ir.I8 | 2 -> Ir.I16 | 4 -> Ir.I32 | _ -> Ir.I64) in
               if p.pfloat then (op st (store_mnemonic ty) [ Reg (FA !nf); Mem (T 2, p.poff) ]; incr nf)
               else (op st (store_mnemonic ty) [ Reg (A !ni); Mem (T 2, p.poff) ]; incr ni))
             pieces)

(* ---- one instruction ------------------------------------------------ *)

let memcopy st (dst : Ir.operand) (src : Ir.operand) bytes =
  (* a byte at a time, which is what a first back end should do: the
     runtime's copies are small and correctness is the point *)
  load_addr st dst (T 3);
  load_addr st src (T 4);
  for i = 0 to bytes - 1 do
    op st "lbu" [ Reg (T 0); Mem (T 4, i) ];
    op st "sb" [ Reg (T 0); Mem (T 3, i) ]
  done

let memzero st (dst : Ir.operand) bytes =
  load_addr st dst (T 3);
  for i = 0 to bytes - 1 do op st "sb" [ Reg Zero; Mem (T 3, i) ] done

let not_yet what = failwith ("Riscv64.Select: " ^ what ^ " is not implemented yet")

let instr st (i : Ir.instr) =
  match i with
  | Ir.Mov (Ir.F80, r, o) -> load_wide st o (T 0) (T 1); store_wide st r (T 0) (T 1)
  | Ir.Mov (ty, r, o) ->
      load st ty o (T 0) (FT 0);
      store st ty r (if is_float ty then FT 0 else T 0)
  | Ir.Binop (b, ty, r, a, c) -> binop st b ty r a c
  | Ir.Neg (Ir.F80, r, o) ->
      (* the sign is the top bit of the high word *)
      load_wide st o (T 0) (T 1);
      op st "li" [ Reg (T 2); Imm 1L ];
      op st "slli" [ Reg (T 2); Reg (T 2); Imm 63L ];
      op st "xor" [ Reg (T 1); Reg (T 1); Reg (T 2) ];
      store_wide st r (T 0) (T 1)
  | Ir.Neg (ty, r, o) when is_float ty ->
      load_float st ty o (FT 0);
      op st (if ty = Ir.F32 then "fneg.s" else "fneg.d") [ Reg (FT 0); Reg (FT 0) ];
      store st ty r (FT 0)
  | Ir.Neg (ty, r, o) ->
      load_int st ty o (T 0);
      op st (if ty = Ir.I32 then "negw" else "neg") [ Reg (T 0); Reg (T 0) ];
      store st ty r (T 0)
  | Ir.Not (ty, r, o) ->
      load_int st ty o (T 0);
      op st "not" [ Reg (T 0); Reg (T 0) ];
      store st ty r (T 0)
  | Ir.Cmp (c, ty, r, a, b) ->
      if is_float ty then compare_float st c ty a b (T 0) else compare_int st c ty a b (T 0);
      store st Ir.I32 r (T 0)
  | Ir.Conv (c, r, o) -> conv st c r o
  | Ir.Load (Ir.F80, r, o) ->
      load_addr st o (T 2);
      op st "ld" [ Reg (T 0); Mem (T 2, 0) ];
      op st "ld" [ Reg (T 1); Mem (T 2, 8) ];
      store_wide st r (T 0) (T 1)
  | Ir.Store (Ir.F80, a, v) ->
      load_wide st v (T 0) (T 1);
      load_addr st a (T 2);
      op st "sd" [ Reg (T 0); Mem (T 2, 0) ];
      op st "sd" [ Reg (T 1); Mem (T 2, 8) ]
  | Ir.Load (ty, r, o) ->
      load_addr st o (T 2);
      op st (load_mnemonic ty true) [ Reg (if is_float ty then FT 0 else T 0); Mem (T 2, 0) ];
      store st ty r (if is_float ty then FT 0 else T 0)
  | Ir.Store (ty, a, v) ->
      load st ty v (T 0) (FT 0);
      load_addr st a (T 2);
      op st (store_mnemonic ty) [ Reg (if is_float ty then FT 0 else T 0); Mem (T 2, 0) ]
  | Ir.Memcpy (dst, src, n) -> memcopy st dst src n
  | Ir.Memzero (dst, n) -> memzero st dst n
  | Ir.Call (res, callee, args, named) -> call st res callee args named
  | Ir.Label l -> emit st (Label l)
  | Ir.Jump l -> op st "j" [ Sym (l, 0) ]
  | Ir.Branch (o, t, e) ->
      load_int st Ir.I32 o (T 0);
      op st "bnez" [ Reg (T 0); Sym (t, 0) ];
      op st "j" [ Sym (e, 0) ]
  | Ir.Switch (ty, o, cases, default) ->
      (* a chain of comparisons: a jump table can come later *)
      load_int st ty o (T 0);
      List.iter (fun (v, l) ->
          op st "li" [ Reg (T 1); Imm v ];
          op st "beq" [ Reg (T 0); Reg (T 1); Sym (l, 0) ]) cases;
      op st "j" [ Sym (default, 0) ]
  | Ir.Ret None -> op st "j" [ Sym (".Lreturn." ^ st.fname, 0) ]
  | Ir.Ret (Some (Ir.Rv_scalar (Ir.F80, o))) ->
      load_wide st o (A 0) (A 1);
      op st "j" [ Sym (".Lreturn." ^ st.fname, 0) ]
  | Ir.Ret (Some (Ir.Rv_scalar (ty, o))) ->
      load st ty o (A 0) (FA 0);
      op st "j" [ Sym (".Lreturn." ^ st.fname, 0) ]
  | Ir.Ret (Some (Ir.Rv_aggregate a)) ->
      (match a.passing with
       | Ir.In_memory ->
           (* the caller gave us where to put it, and we saved that *)
           op st "ld" [ Reg (T 3); addr st (S 0) st.hidden_ptr (T 2) ];
           load_addr st a.addr (T 4);
           let n = a.size in
           for k = 0 to n - 1 do
             op st "lbu" [ Reg (T 0); Mem (T 4, k) ];
             op st "sb" [ Reg (T 0); Mem (T 3, k) ]
           done;
           op st "mv" [ Reg (A 0); Reg (T 3) ]
       | Ir.In_registers pieces ->
           load_addr st a.addr (T 2);
           let ni = ref 0 and nf = ref 0 in
           List.iter (fun (p : Ir.piece) ->
               let ty = if p.pfloat then (if p.psize = 4 then Ir.F32 else Ir.F64)
                 else (match p.psize with 1 -> Ir.I8 | 2 -> Ir.I16 | 4 -> Ir.I32 | _ -> Ir.I64) in
               if p.pfloat then (op st (load_mnemonic ty true) [ Reg (FA !nf); Mem (T 2, p.poff) ]; incr nf)
               else (op st (load_mnemonic ty true) [ Reg (A !ni); Mem (T 2, p.poff) ]; incr ni))
             pieces);
      op st "j" [ Sym (".Lreturn." ^ st.fname, 0) ]
  | Ir.Line _ -> ()                            (* debug lines come later *)
  | Ir.Trap -> op st "unimp" []
  | Ir.Intrinsic (Ir.Fabs, ty, r, o) ->
      load_float st ty o (FT 0);
      op st (if ty = Ir.F32 then "fabs.s" else "fabs.d") [ Reg (FT 0); Reg (FT 0) ];
      store st ty r (FT 0)
  | Ir.Intrinsic (Ir.Fsqrt, ty, r, o) ->
      load_float st ty o (FT 0);
      op st (if ty = Ir.F32 then "fsqrt.s" else "fsqrt.d") [ Reg (FT 0); Reg (FT 0) ];
      store st ty r (FT 0)
  | Ir.Fence _ -> op st "fence" [ Sym ("rw, rw", 0) ]
  | Ir.Return_address r -> store st Ir.I64 r RA
  | Ir.Binop_overflow (b, ty, signed, r, flag, a, c) -> binop_overflow st b ty signed r flag a c
  (* 7.17, mapped onto the A extension the way the machine's own
     compiler does it: a fence on each side of a sequentially consistent
     load, a fence before such a store, and the acquire-release forms of
     the read-modify-write instructions.  A relaxed operation needs no
     fence and no suffix.  The machine has these at four and eight bytes
     only; a narrower one would need a masked loop on the containing
     word, and nothing here asks for that yet. *)
  | Ir.Atomic_load (ty, r, a, order) ->
      if width ty < 4 then not_yet "an atomic narrower than four bytes";
      let ordered = order <> Ir.Relaxed in
      if ordered then op st "fence" [ Sym ("rw,rw", 0) ];
      load_addr st a (T 2);
      op st (load_mnemonic ty true) [ Reg (if is_float ty then FT 0 else T 0); Mem (T 2, 0) ];
      if ordered then op st "fence" [ Sym ("r,rw", 0) ];
      store st ty r (if is_float ty then FT 0 else T 0)
  | Ir.Atomic_store (ty, a, v, order) ->
      if width ty < 4 then not_yet "an atomic narrower than four bytes";
      load st ty v (T 0) (FT 0);
      load_addr st a (T 2);
      if order <> Ir.Relaxed then op st "fence" [ Sym ("rw,w", 0) ];
      op st (store_mnemonic ty) [ Reg (if is_float ty then FT 0 else T 0); Mem (T 2, 0) ]
  | Ir.Atomic_rmw (b, ty, r, a, v, order) ->
      if width ty < 4 then not_yet "an atomic narrower than four bytes";
      let suffix = (if width ty = 4 then ".w" else ".d") ^ (if order = Ir.Relaxed then "" else ".aqrl") in
      load_int st ty v (T 1);
      load_addr st a (T 2);
      let mnemonic =
        match b with
        | Ir.Add -> Some "amoadd"
        | Ir.Sub -> op st "neg" [ Reg (T 1); Reg (T 1) ]; Some "amoadd"
        | Ir.And -> Some "amoand" | Ir.Or -> Some "amoor" | Ir.Xor -> Some "amoxor"
        | _ -> None in
      (match mnemonic with
       | Some m -> op st (m ^ suffix) [ Reg (T 0); Reg (T 1); Mem (T 2, 0) ]
       | None -> not_yet "that operation applied atomically");
      store st ty r (T 0)
  | Ir.Atomic_xchg (ty, r, a, v, order) ->
      if width ty < 4 then not_yet "an atomic narrower than four bytes";
      let suffix = (if width ty = 4 then ".w" else ".d") ^ (if order = Ir.Relaxed then "" else ".aqrl") in
      load_int st ty v (T 1);
      load_addr st a (T 2);
      op st ("amoswap" ^ suffix) [ Reg (T 0); Reg (T 1); Mem (T 2, 0) ];
      store st ty r (T 0)
  | Ir.Atomic_cmpxchg (ty, r, a, expected, desired, order) ->
      if width ty < 4 then not_yet "an atomic narrower than four bytes";
      (* the reserve-and-store loop: read, compare, try to store, and go
         round again only if the reservation was lost *)
      let w = if width ty = 4 then ".w" else ".d" in
      let aq = if order = Ir.Relaxed then "" else ".aqrl" in
      let rl = if order = Ir.Relaxed then "" else ".rl" in
      let again = fresh_label st "cas" and out = fresh_label st "casout" in
      load_addr st a (T 2);
      load_addr st expected (T 4);
      op st (load_mnemonic ty true) [ Reg (T 5); Mem (T 4, 0) ];   (* what we expect *)
      load_int st ty desired (T 6);
      emit st (Label again);
      op st ("lr" ^ w ^ aq) [ Reg (T 0); Mem (T 2, 0) ];
      op st "bne" [ Reg (T 0); Reg (T 5); Sym (out, 0) ];
      op st ("sc" ^ w ^ rl) [ Reg (T 1); Reg (T 6); Mem (T 2, 0) ];
      op st "bnez" [ Reg (T 1); Sym (again, 0) ];
      emit st (Label out);
      (* the result is whether it succeeded, and a failure writes back
         what was there (7.17.7.4p3) *)
      op st "sub" [ Reg (T 1); Reg (T 0); Reg (T 5) ];
      op st "seqz" [ Reg (T 1); Reg (T 1) ];
      op st (store_mnemonic ty) [ Reg (T 0); Mem (T 4, 0) ];
      store st Ir.I32 r (T 1)
  | Ir.Va_start ap ->
      (* the list is one pointer, at the first saved argument register *)
      op st "addi" [ Reg (T 0); Reg (S 0); Imm (Int64.of_int (- st.va_bytes)) ];
      load_addr st ap (T 2);
      op st "sd" [ Reg (T 0); Mem (T 2, 0) ]
  | Ir.Va_arg (ty, r, ap) ->
      (* read where the pointer points, then advance it by one word: the
         saved registers and the arguments on the stack are contiguous,
         so nothing here has to know which it read *)
      load_addr st ap (T 2);
      op st "ld" [ Reg (T 3); Mem (T 2, 0) ];
      (match ty with
       | Ir.F32 ->
           (* a float matching an ellipsis arrived as the low bits of a word *)
           op st "lw" [ Reg (T 0); Mem (T 3, 0) ];
           op st "fmv.w.x" [ Reg (FT 0); Reg (T 0) ];
           store st ty r (FT 0)
       | Ir.F64 ->
           op st "ld" [ Reg (T 0); Mem (T 3, 0) ];
           op st "fmv.d.x" [ Reg (FT 0); Reg (T 0) ];
           store st ty r (FT 0)
       | Ir.F80 ->
           (* aligned to sixteen in the list, as the ABI asks *)
           op st "addi" [ Reg (T 3); Reg (T 3); Imm 15L ];
           op st "andi" [ Reg (T 3); Reg (T 3); Imm (-16L) ];
           op st "ld" [ Reg (T 0); Mem (T 3, 0) ];
           op st "ld" [ Reg (T 1); Mem (T 3, 8) ];
           store_wide st r (T 0) (T 1);
           op st "addi" [ Reg (T 3); Reg (T 3); Imm 8L ]  (* the other eight below *)
       | _ ->
           op st (load_mnemonic ty true) [ Reg (T 0); Mem (T 3, 0) ];
           store st ty r (T 0));
      op st "addi" [ Reg (T 3); Reg (T 3); Imm 8L ];
      op st "sd" [ Reg (T 3); Mem (T 2, 0) ]
  | Ir.Va_arg_aggregate (dst, size, passing, ap) ->
      load_addr st ap (T 2);
      op st "ld" [ Reg (T 3); Mem (T 2, 0) ];
      let words =
        match passing with
        | Ir.In_memory -> 1                (* a pointer to it was passed *)
        | Ir.In_registers _ -> (size + 7) / 8 in
      (match passing with
       | Ir.In_memory ->
           op st "ld" [ Reg (T 4); Mem (T 3, 0) ];
           load_addr st dst (T 5);
           for k = 0 to size - 1 do
             op st "lbu" [ Reg (T 0); Mem (T 4, k) ];
             op st "sb" [ Reg (T 0); Mem (T 5, k) ]
           done
       | Ir.In_registers _ ->
           load_addr st dst (T 5);
           for k = 0 to size - 1 do
             op st "lbu" [ Reg (T 0); Mem (T 3, k) ];
             op st "sb" [ Reg (T 0); Mem (T 5, k) ]
           done);
      op st "addi" [ Reg (T 3); Reg (T 3); Imm (Int64.of_int (8 * words)) ];
      op st "sd" [ Reg (T 3); Mem (T 2, 0) ]
  | Ir.Alloca (r, size) ->
      (* fresh stack below sp, kept to the ABI's sixteen bytes.  Nothing
         gives it back before the function returns, which the epilogue
         then does by restoring sp from the frame pointer. *)
      st.moved_sp <- true;
      load_int st Ir.I64 size (T 0);
      op st "addi" [ Reg (T 0); Reg (T 0); Imm 15L ];
      op st "andi" [ Reg (T 0); Reg (T 0); Imm (-16L) ];
      op st "sub" [ Reg SP; Reg SP; Reg (T 0) ];
      store st Ir.I64 r SP
  | Ir.Inline_asm _ -> not_yet "inline assembly"

(* ---- a function ----------------------------------------------------- *)

(* The frame, laid out as the code gcc generates here does so that a
   debugger and the unwinder find what they expect:

       sp+size-8   the return address
       sp+size-16  the caller's frame pointer
       s0 = sp + size, and the locals below it
       sp          the arguments to calls that did not fit in registers

   Nothing here is kept in a register across instructions, so ra and s0
   are the only registers that have to be saved. *)
let func st (f : Ir.func) : func =
  st.code <- []; st.regs <- Hashtbl.create 64; st.wide <- Hashtbl.create 8;
  st.frame <- saved_bytes; st.moved_sp <- false;
  st.fname <- f.name; st.label_count <- 0; st.hidden_ptr <- 0;
  let hidden = match f.returns_aggregate with Some (_, p) -> p = Ir.In_memory | None -> false in
  (* the parameters arrive where a caller would have put them *)
  let as_args = List.map (function
      | Ir.P_scalar (ty, r) -> Ir.Scalar (ty, Ir.Reg r)
      | Ir.P_aggregate (slot, size, passing) -> Ir.Aggregate { Ir.addr = Ir.Slot slot; size; passing })
      f.params in
  let places, named_int, _, _ = assign_args ~hidden as_args in
  st.named_int <- named_int;
  (* a variadic function saves the argument registers its named
     parameters did not take, immediately below s0 *)
  st.va_bytes <- if f.variadic then 8 * (8 - min 8 named_int) else 0;
  st.frame <- max st.frame (st.va_bytes + saved_bytes);
  (* the body first, so that the frame's size is known before the
     prologue that establishes it is written *)
  (* the slots come after the save area, whose size is now known *)
  st.slots <- Array.map (fun (s : Ir.slot) -> alloc st s.size (max s.align 1)) f.slots;
  if hidden then st.hidden_ptr <- alloc st 8 8;
  let saved = st.code in
  st.code <- [];
  if f.variadic then
    for i = min 8 named_int to 7 do
      op st "sd" [ Reg (A i); Mem (S 0, - st.va_bytes + 8 * (i - min 8 named_int)) ]
    done;
  if hidden then op st "sd" [ Reg (A 0); addr st (S 0) st.hidden_ptr (T 2) ];
  List.iter2 (fun (p : Ir.param) ps ->
      match p, ps with
      | Ir.P_scalar (Ir.F80, r), [ In_int i; In_int j ] -> store_wide st r (A i) (A j)
      | Ir.P_scalar (Ir.F80, r), [ On_stack a; On_stack b ] ->
          op st "ld" [ Reg (T 0); Mem (S 0, a) ];
          op st "ld" [ Reg (T 1); Mem (S 0, b) ];
          store_wide st r (T 0) (T 1)
      | Ir.P_scalar (ty, r), [ In_int i ] -> store st ty r (A i)
      | Ir.P_scalar (ty, r), [ In_float i ] -> store st ty r (FA i)
      | Ir.P_scalar (ty, r), [ On_stack off ] ->
          (* above s0: the caller's outgoing area *)
          op st (load_mnemonic ty true) [ Reg (if is_float ty then FT 0 else T 0); Mem (S 0, off) ];
          store st ty r (if is_float ty then FT 0 else T 0)
      | Ir.P_aggregate (slot, size, Ir.In_memory), [ In_int i ] ->
          (* what arrived is the address of the caller's copy; the body
             addresses a slot, so the object is copied into it *)
          op st "mv" [ Reg (T 4); Reg (A i) ];
          op st "addi" [ Reg (T 3); Reg (S 0); Imm (Int64.of_int st.slots.(slot)) ];
          for k = 0 to size - 1 do
            op st "lbu" [ Reg (T 0); Mem (T 4, k) ];
            op st "sb" [ Reg (T 0); Mem (T 3, k) ]
          done
      | Ir.P_aggregate (slot, size, Ir.In_memory), [ On_stack off ] ->
          op st "ld" [ Reg (T 4); Mem (S 0, off) ];
          op st "addi" [ Reg (T 3); Reg (S 0); Imm (Int64.of_int st.slots.(slot)) ];
          for k = 0 to size - 1 do
            op st "lbu" [ Reg (T 0); Mem (T 4, k) ];
            op st "sb" [ Reg (T 0); Mem (T 3, k) ]
          done
      | Ir.P_aggregate (slot, _, Ir.In_registers pieces), ps ->
          load_addr st (Ir.Slot slot) (T 2);
          List.iter2 (fun (pc : Ir.piece) place ->
              let ty = if pc.pfloat then (if pc.psize = 4 then Ir.F32 else Ir.F64)
                else (match pc.psize with 1 -> Ir.I8 | 2 -> Ir.I16 | 4 -> Ir.I32 | _ -> Ir.I64) in
              match place with
              | In_int i -> op st (store_mnemonic ty) [ Reg (A i); Mem (T 2, pc.poff) ]
              | In_float i -> op st (store_mnemonic ty) [ Reg (FA i); Mem (T 2, pc.poff) ]
              | On_stack off ->
                  op st (load_mnemonic ty true) [ Reg (T 0); Mem (S 0, off) ];
                  op st (store_mnemonic ty) [ Reg (T 0); Mem (T 2, pc.poff) ])
            pieces ps
      | _ -> failwith "Riscv64.Select: a parameter and its place disagree")
    f.params places;
  List.iter (instr st) f.body;
  let body = List.rev st.code in
  st.code <- saved;
  (* the frame: the locals and the two saved registers, rounded to the
     sixteen the ABI asks of sp.  Arguments that do not fit in registers
     are not here: each call makes room for its own just below sp, as
     the other machine does, so that a variable length array may move sp
     without disturbing them. *)
  let size = round_up st.frame 16 in
  let prologue =
    [ Op ("addi", [ Reg SP; Reg SP; Imm (Int64.of_int (- size)) ]);
      Op ("sd", [ Reg RA; Mem (SP, size + ra_offset st) ]);
      Op ("sd", [ Reg (S 0); Mem (SP, size + fp_offset st) ]);
      Op ("addi", [ Reg (S 0); Reg SP; Imm (Int64.of_int size) ]) ] in
  let epilogue =
    [ Label (".Lreturn." ^ f.name) ]
    (* A function that moved sp itself -- one with a variable length
       array -- cannot undo that by adding the frame's size back, so the
       stack pointer comes from the frame pointer, which is what a frame
       pointer is for.  gcc here writes the same `addi sp,s0,-size'. *)
    @ (if st.moved_sp then [ Op ("addi", [ Reg SP; Reg (S 0); Imm (Int64.of_int (- size)) ]) ] else [])
    @ [ Op ("ld", [ Reg RA; Mem (SP, size + ra_offset st) ]);
        Op ("ld", [ Reg (S 0); Mem (SP, size + fp_offset st) ]);
        Op ("addi", [ Reg SP; Reg SP; Imm (Int64.of_int size) ]);
        Op ("ret", []) ] in
  (* the constants this function needed, as read-only objects *)
  let const name align size items =
    { dname = name; dglobal = false; dweak = false; dhidden = false; dalias = None;
      dfunc = false; ddecl = false; dtls = false; dalign = align; section = Rodata; size; items } in
  st.consts <-
    List.map (fun ((bits, narrow), l) ->
        if narrow then const l 4 4 [ Long (Int64.to_int32 bits) ] else const l 8 8 [ Quad bits ])
      st.float_consts
    @ List.map (fun ((lo, hi), l) -> const l 16 16 [ Quad lo; Quad hi ]) st.wide_consts
    @ st.consts;
  st.float_consts <- []; st.wide_consts <- [];
  { name = f.name; global = f.global; weak = f.flink.weak; hidden = f.flink.hidden;
    body = prologue @ body @ epilogue; debug = false }

(* ---- data ----------------------------------------------------------- *)

let data_of_global (g : Ir.global) : data option =
  if not g.gdefined then
    (* an undefined reference declared weak or hidden: emit just the
       binding, so the assembler records it *)
    (if g.glink.weak || g.glink.hidden then
       Some { dname = g.gname; dglobal = false; dweak = g.glink.weak; dhidden = g.glink.hidden;
              dalias = None; dfunc = false; ddecl = true; dtls = false; dalign = 1;
              section = Data; size = 0; items = [] }
     else None)
  else if g.glink.alias <> None then
    (* an alias defines no storage; it is a .set to its target *)
    Some { dname = g.gname; dglobal = g.gglobal; dweak = g.glink.weak; dhidden = g.glink.hidden;
           dalias = g.glink.alias; dfunc = g.gfunc; ddecl = false; dtls = g.gtls; dalign = 1;
           section = Data; size = 0; items = [] }
  else
    let items = match g.ginit with
      | None -> [ Zeros (max g.gsize 1) ]
      | Some ds ->
          List.map (function
              | Ir.Bytes s -> Bytes s | Ir.Zeros n -> Zeros n | Ir.Addr (s, o) -> Quad_sym (s, o)) ds in
    let zero = g.ginit = None || List.for_all (function Zeros _ -> true | _ -> false) items in
    let section = match g.gtls, zero with
      | true, true -> Tbss | true, false -> Tdata | false, true -> Bss | false, false -> Data in
    Some { dname = g.gname; dglobal = g.gglobal; dweak = g.glink.weak; dhidden = g.glink.hidden;
           dalias = None; dfunc = false; ddecl = false; dtls = g.gtls; dalign = g.galign;
           section; size = max g.gsize 1; items }

(* ---- a program ------------------------------------------------------ *)

let program ~pic ~debug (p : Ir.program) : program =
  let tls = Hashtbl.create 16 in
  List.iter (fun (g : Ir.global) -> if g.gtls then Hashtbl.replace tls g.gname ()) p.globals;
  let st = { pic; debug; tls; code = []; regs = Hashtbl.create 64; slots = [||]; frame = 0;
             fname = ""; label_count = 0; float_consts = []; const_count = 0;
             hidden_ptr = 0; va_bytes = 0; named_int = 0; moved_sp = false;
             wide = Hashtbl.create 8; wide_consts = []; consts = [];
             files = Hashtbl.create 8; next_file = 1 } in
  let funcs = List.map (fun f -> func st f) p.funcs in
  let data = List.filter_map data_of_global p.globals in
  let files = List.sort compare (Hashtbl.fold (fun name n acc -> (n, name) :: acc) st.files []) in
  { funcs; data = data @ List.rev st.consts;
    source = (if debug then Some p.source else None); files;
    asm_blocks = p.asm_blocks; init_array = p.init_array; fini_array = p.fini_array }
