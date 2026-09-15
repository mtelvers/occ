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
  mutable outgoing : int;             (* bytes above sp for arguments that do not fit in registers *)
  mutable fname : string;
  mutable label_count : int;
  mutable float_consts : (int64 * string) list;
  mutable const_count : int;
  mutable hidden_ptr : int;           (* where the aggregate-return pointer was saved *)
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

(* The first sixteen bytes below s0 hold the return address and the
   caller's frame pointer, so a local starts below them. *)
let saved_bytes = 16

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

let float_const st (v : float) =
  let bits = Int64.bits_of_float v in
  match List.assoc_opt bits st.float_consts with
  | Some l -> l
  | None ->
      st.const_count <- st.const_count + 1;
      let l = Printf.sprintf ".LC%s.%d" st.fname st.const_count in
      st.float_consts <- (bits, l) :: st.float_consts;
      l

(* The address of a symbol.  Without position independence that is the
   twenty-high/twelve-low pair the machine is built around; with it, a
   load from the global offset table, which the assembler spells for us
   as one pseudo-instruction. *)
let load_sym st sym (dst : reg) =
  if st.pic then op st "la" [ Reg dst; Sym (sym, 0) ]
  else begin
    op st "lui" [ Reg dst; Sym ("%hi(" ^ sym ^ ")", 0) ];
    op st "addi" [ Reg dst; Reg dst; Sym ("%lo(" ^ sym ^ ")", 0) ]
  end

(* An operand into a named integer register. *)
let rec load_int st (ty : Ir.ty) (o : Ir.operand) (dst : reg) =
  match o with
  | Ir.Imm v -> op st "li" [ Reg dst; Imm v ]
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
      let l = float_const st f in
      load_sym st l (T 2);
      op st (load_mnemonic ty true) [ Reg dst; Mem (T 2, 0) ]
  | Ir.Reg r -> op st (load_mnemonic ty true) [ Reg dst; addr st (S 0) (reg_slot st r) (T 2) ]
  | _ -> failwith "Riscv64.Select: a floating-point value from an integer operand"

let load st ty o dst_i dst_f = if is_float ty then load_float st ty o dst_f else load_int st ty o dst_i

let store st (ty : Ir.ty) (r : int) (src : reg) =
  op st (store_mnemonic ty) [ Reg src; addr st (S 0) (reg_slot st r) (T 2) ]

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

let binop st (b : Ir.binop) (ty : Ir.ty) (r : int) a c =
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

let compare_float st (c : Ir.cond) ty a b (dst : reg) =
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
  | Ir.Fconv _ -> failwith "Riscv64.Select: long double is not implemented yet"

(* ---- calls ---------------------------------------------------------- *)

(* Where each argument goes.  Integers and pointers take a0..a7,
   floating-point values fa0..fa7, and what is left goes on the stack in
   order; an aggregate travels as its pieces, or as a pointer if the ABI
   said memory.  A variadic call passes everything after the named
   parameters in the integer registers and then the stack, which is what
   the ABI asks for and what makes a va_list a plain pointer. *)
type place = In_int of int | In_float of int | On_stack of int

let assign_args ~hidden (args : Ir.arg list) =
  let ni = ref (if hidden then 1 else 0) and nf = ref 0 and stack = ref 0 in
  let places =
    List.map (fun (a : Ir.arg) ->
        match a with
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

let call st (res : Ir.result option) (callee : Ir.operand) (args : Ir.arg list) variadic =
  ignore variadic;
  let hidden = match res with Some (Ir.Ret_aggregate a) -> a.passing = Ir.In_memory | _ -> false in
  let places, _, _, stack_bytes = assign_args ~hidden args in
  if stack_bytes > st.outgoing then st.outgoing <- round_up stack_bytes 16;
  (* the arguments, into their registers or onto the stack *)
  List.iter2 (fun (a : Ir.arg) ps ->
      match a, ps with
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
   | Ir.Sym s -> op st (if st.pic then "call" else "call") [ Sym ((if st.pic then s ^ "@plt" else s), 0) ]
   | o -> load_int st Ir.I64 o (T 2); op st "jalr" [ Reg (T 2) ]);
  (* and its result *)
  match res with
  | None -> ()
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
  | Ir.Mov (ty, r, o) ->
      load st ty o (T 0) (FT 0);
      store st ty r (if is_float ty then FT 0 else T 0)
  | Ir.Binop (b, ty, r, a, c) -> binop st b ty r a c
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
  | Ir.Call (res, callee, args, variadic) -> call st res callee args variadic
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
  | Ir.Binop_overflow _ -> not_yet "an operation with an overflow flag"
  | Ir.Atomic_load _ | Ir.Atomic_store _ | Ir.Atomic_rmw _
  | Ir.Atomic_xchg _ | Ir.Atomic_cmpxchg _ -> not_yet "the atomic operations"
  | Ir.Va_start _ | Ir.Va_arg _ | Ir.Va_arg_aggregate _ -> not_yet "variable arguments"
  | Ir.Alloca _ -> not_yet "a variable length array"
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
  st.code <- []; st.regs <- Hashtbl.create 64; st.frame <- saved_bytes; st.outgoing <- 0;
  st.fname <- f.name; st.label_count <- 0; st.hidden_ptr <- 0;
  st.slots <- Array.map (fun (s : Ir.slot) -> alloc st s.size (max s.align 1)) f.slots;
  let hidden = match f.returns_aggregate with Some (_, p) -> p = Ir.In_memory | None -> false in
  if hidden then st.hidden_ptr <- alloc st 8 8;
  (* the parameters arrive where a caller would have put them *)
  let as_args = List.map (function
      | Ir.P_scalar (ty, r) -> Ir.Scalar (ty, Ir.Reg r)
      | Ir.P_aggregate (slot, size, passing) -> Ir.Aggregate { Ir.addr = Ir.Slot slot; size; passing })
      f.params in
  let places, _, _, _ = assign_args ~hidden as_args in
  (* the body first, so that the frame's size is known before the
     prologue that establishes it is written *)
  let saved = st.code in
  st.code <- [];
  if hidden then op st "sd" [ Reg (A 0); addr st (S 0) st.hidden_ptr (T 2) ];
  List.iter2 (fun (p : Ir.param) ps ->
      match p, ps with
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
  (* the frame: locals, the two saved registers, and room for outgoing
     arguments, rounded to the sixteen the ABI asks of sp *)
  let size = round_up (st.frame + st.outgoing) 16 in
  let prologue =
    [ Directive ("text", []) ]
    @ (if f.global then [ Directive ("globl", [ f.name ]) ] else [])
    @ [ Directive ("type", [ f.name; "@function" ]); Label f.name;
        Op ("addi", [ Reg SP; Reg SP; Imm (Int64.of_int (- size)) ]);
        Op ("sd", [ Reg RA; Mem (SP, size - 8) ]);
        Op ("sd", [ Reg (S 0); Mem (SP, size - 16) ]);
        Op ("addi", [ Reg (S 0); Reg SP; Imm (Int64.of_int size) ]) ] in
  let epilogue =
    [ Label (".Lreturn." ^ f.name);
      Op ("ld", [ Reg RA; Mem (SP, size - 8) ]);
      Op ("ld", [ Reg (S 0); Mem (SP, size - 16) ]);
      Op ("addi", [ Reg SP; Reg SP; Imm (Int64.of_int size) ]);
      Op ("ret", []) ]
    @ (if f.global then [ Directive ("size", [ f.name; ".-" ^ f.name ]) ] else []) in
  (* the floating-point constants this function needed *)
  let consts =
    List.concat_map (fun (bits, l) ->
        [ Directive ("section", [ ".rodata" ]); Directive ("align", [ "3" ]);
          Label l; Directive ("quad", [ Int64.to_string bits ]) ])
      st.float_consts in
  st.float_consts <- [];
  { name = f.name; body = prologue @ body @ epilogue @ consts }

(* ---- data ----------------------------------------------------------- *)

let data_of_global (g : Ir.global) : instr list =
  if not g.gdefined then []
  else
    let align = [ Directive ("align", [ string_of_int (max 0 (int_of_float (log (float_of_int (max 1 g.galign)) /. log 2.))) ]) ] in
    let head =
      (if g.gglobal then [ Directive ("globl", [ g.gname ]) ] else [])
      @ [ Directive ("type", [ g.gname; if g.gfunc then "@function" else "@object" ]) ] in
    match g.ginit with
    | None ->
        [ Directive ("bss", []) ] @ head @ align
        @ [ Label g.gname; Directive ("zero", [ string_of_int (max 1 g.gsize) ]) ]
    | Some items ->
        let body =
          List.concat_map (function
              | Ir.Bytes s ->
                  [ Directive ("ascii", [ "\"" ^ String.concat ""
                        (List.map (fun c ->
                             let c = Char.code c in
                             if c = 34 then "\\\"" else if c = 92 then "\\\\"
                             else if c >= 32 && c < 127 then String.make 1 (Char.chr c)
                             else Printf.sprintf "\\%03o" c)
                           (List.init (String.length s) (String.get s))) ^ "\"" ]) ]
              | Ir.Zeros n -> [ Directive ("zero", [ string_of_int n ]) ]
              | Ir.Addr (s, 0L) -> [ Directive ("quad", [ s ]) ]
              | Ir.Addr (s, n) -> [ Directive ("quad", [ Printf.sprintf "%s+%Ld" s n ]) ])
            items in
        [ Directive ("data", []) ] @ head @ align @ [ Label g.gname ] @ body

(* ---- a program ------------------------------------------------------ *)

let program ~pic ~debug (p : Ir.program) : program =
  let st = { pic; debug; code = []; regs = Hashtbl.create 64; slots = [||]; frame = 0;
             outgoing = 0; fname = ""; label_count = 0; float_consts = []; const_count = 0;
             hidden_ptr = 0 } in
  let funcs = List.map (fun f -> func st f) (List.filter (fun (f : Ir.func) -> not f.discardable || true) p.funcs) in
  let data = List.concat_map data_of_global p.globals in
  { funcs; data }
