open Asm

type st = {
  pic : bool;
  debug : bool;
  files : (string, int) Hashtbl.t; (* source file -> .file index *)
  mutable next_file : int;
  locals : (string, unit) Hashtbl.t; (* symbols defined in this unit with internal linkage *)
  tls : (string, unit) Hashtbl.t; (* thread-local symbols, defined or declared *)
  mutable code : instr list; (* reversed *)
  mutable regs : (int, int) Hashtbl.t; (* spilled Ir reg -> rbp offset *)
  mutable alloc : Regalloc.assignment; (* where each Ir register lives *)
  mutable saved : (reg * int) list; (* callee-saved registers and their save slots *)
  mutable slots : int array; (* Ir slot -> rbp offset *)
  mutable frame : int; (* bytes below rbp allocated so far *)
  mutable float_consts : (int64 * string) list;
  mutable f80_consts : (int64 * string) list; (* long double constants, keyed by the double's bits *)
  mutable f80 : (int, int) Hashtbl.t; (* long double Ir reg -> its 16-byte slot *)
  mutable scratch : int; (* a 16-byte slot for moving values between the FPU and the integer unit *)
  mutable tables : data list; (* jump tables, one data object each *)
  mutable const_count : int;
  mutable label_count : int;
  mutable fname : string;
  mutable hidden_ptr : int; (* rbp offset holding the aggregate-return pointer *)
  mutable save_area : int; (* rbp offset of the varargs register save area *)
  mutable va_gp : int; (* integer registers used by named parameters *)
  mutable va_fp : int;
  mutable va_stack : int; (* bytes of named parameters passed on the stack *)
}

let emit st i = st.code <- i :: st.code
let fresh_label st hint = st.label_count <- st.label_count + 1; Printf.sprintf ".L%s.%s%d" st.fname hint st.label_count

let width_of = function Ir.I8 -> B | Ir.I16 -> W | Ir.I32 -> L | Ir.I64 -> Q | Ir.F32 -> L | Ir.F64 | Ir.F80 -> Q
let is_float = function Ir.F32 | Ir.F64 | Ir.F80 -> true | _ -> false
let sse_suffix = function Ir.F32 -> "ss" | _ -> "sd"
let round_up n a = (n + a - 1) / a * a

let alloc st size align =
  st.frame <- round_up (st.frame + size) align;
  - st.frame

(* Where an IR register lives: a callee-saved register chosen by
   [Regalloc], or a frame slot.  Slots are 8 bytes, shared over time
   between spilled registers whose live ranges do not overlap. *)
let location st r =
  match Hashtbl.find_opt st.alloc.where r with
  | Some (Regalloc.Register p) -> `Reg p
  | Some (Regalloc.Spill k) ->
      (match Hashtbl.find_opt st.regs k with
       | Some off -> `Mem off
       | None -> let off = alloc st 8 8 in Hashtbl.replace st.regs k off; `Mem off)
  | None -> (* never mentioned by the allocator: a register only written *)
      (match Hashtbl.find_opt st.regs (-1 - r) with
       | Some off -> `Mem off
       | None -> let off = alloc st 8 8 in Hashtbl.replace st.regs (-1 - r) off; `Mem off)

let reg_slot st r = match location st r with `Mem off -> off | `Reg _ -> failwith "Select.reg_slot: in a register"

let fits32 v = Int64.compare v (-2147483648L) >= 0 && Int64.compare v 2147483647L <= 0

let float_const st (ty : Ir.ty) f =
  let bits = if ty = Ir.F32 then Int64.of_int32 (Int32.bits_of_float f) else Int64.bits_of_float f in
  let key = if ty = Ir.F32 then Int64.logor bits 0x1_0000_0000L (* distinguish widths *) else bits in
  match List.assoc_opt key st.float_consts with
  | Some l -> l
  | None ->
      st.const_count <- st.const_count + 1;
      let l = Printf.sprintf ".LCF%d" st.const_count in
      st.float_consts <- (key, l) :: st.float_consts; l

(* The address of a symbol, into a register.  A thread-local's address is
   the thread pointer (%fs:0) plus its offset: local-exec in an executable,
   initial-exec through the GOT in position-independent code.  The
   initial-exec model suffices because every thread-local the OCaml build
   defines lives in a module present at program start; a dlopen'ed object
   defining its own thread-locals would need the general-dynamic model. *)
let load_sym st sym (dst : reg) =
  if Hashtbl.mem st.tls sym then begin
    emit st (Mov (Q, Fs_zero, Reg dst));
    if st.pic then (emit st (Mov (Q, Gottpoff sym, Reg R10)); emit st (Alu ("add", Q, Reg R10, Reg dst)))
    else emit st (Lea (Tpoff (dst, sym), dst))
  end
  else if st.pic && not (Hashtbl.mem st.locals sym) then emit st (Mov (Q, Got sym, Reg dst))
  else emit st (Lea (Rip (sym, 0), dst))

(* Load an integer-class operand into a general register. *)
let load_int st (ty : Ir.ty) (op : Ir.operand) (dst : reg) =
  match op with
  | Ir.Reg r ->
      (match location st r with
       | `Reg p -> emit st (Mov (Q, Reg p, Reg dst))
       | `Mem off -> emit st (Mov (width_of ty, Mem (RBP, off), Reg dst)))
  | Ir.Imm v -> if fits32 v then emit st (Mov (Q, Imm v, Reg dst)) else emit st (Movabs (v, dst))
  | Ir.Fimm _ -> failwith "Select: float immediate in integer context"
  | Ir.Sym s -> load_sym st s dst
  | Ir.Slot k -> emit st (Lea (Mem (RBP, st.slots.(k)), dst))

(* Load a 64-bit address operand. *)
let load_addr st op dst = load_int st Ir.I64 op dst

let load_float st (ty : Ir.ty) (op : Ir.operand) (dst : reg) =
  let mov = "mov" ^ sse_suffix ty in
  match op with
  | Ir.Reg r -> emit st (Sse (mov, Mem (RBP, reg_slot st r), Reg dst))
  | Ir.Fimm f -> emit st (Sse (mov, Rip (float_const st ty f, 0), Reg dst))
  | Ir.Imm v -> emit st (Sse (mov, Rip (float_const st ty (Int64.to_float v), 0), Reg dst))
  | Ir.Sym s -> failwith ("Select: symbol " ^ s ^ " in float context")
  | Ir.Slot k -> failwith (Printf.sprintf "Select: slot %d in float context" k)

let load st ty op dst = if is_float ty then load_float st ty op dst else load_int st ty op dst

(* An integer operand for an ALU instruction whose other operand is rax:
   the register the value already lives in, or a 32-bit immediate, used
   directly; anything else is loaded into rcx first. *)
let src_operand st (ty : Ir.ty) (op : Ir.operand) : operand =
  match op with
  | Ir.Reg r -> (match location st r with `Reg p -> Reg p | `Mem _ -> load_int st ty op RCX; Reg RCX)
  | Ir.Imm v when fits32 v -> Imm v
  | _ -> load_int st ty op RCX; Reg RCX

(* The memory operand of a load or store when the address is a frame slot
   or, outside position-independent code, a symbol: no address computation
   is needed. *)
let mem_operand st (addr : Ir.operand) : operand =
  match addr with
  | Ir.Slot k -> Mem (RBP, st.slots.(k))
  | Ir.Sym s when not st.pic && not (Hashtbl.mem st.tls s) -> Rip (s, 0)
  | Ir.Reg r -> (match location st r with `Reg p -> Mem (p, 0) | `Mem _ -> load_addr st addr RCX; Mem (RCX, 0))
  | _ -> load_addr st addr RCX; Mem (RCX, 0)

let store_int st (ty : Ir.ty) (r : int) (src : reg) =
  match location st r with
  | `Reg p -> emit st (Mov (Q, Reg src, Reg p)) (* the full register: consumers read the width they need *)
  | `Mem off -> emit st (Mov (width_of ty, Reg src, Mem (RBP, off)))
let store_float st (ty : Ir.ty) (r : int) (src : reg) = emit st (Sse ("mov" ^ sse_suffix ty, Reg src, Mem (RBP, reg_slot st r)))
let store st ty r src = if is_float ty then store_float st ty r src else store_int st ty r src

(* ---- long double: the x87 unit ----------------------------------------------------

   long double is the 80-bit extended format, class X87 in the ABI, kept
   in 16-byte frame slots and passed in memory.  Each operation pushes its
   operands on the FPU register stack, computes, and pops the result back
   to a slot, so the stack is empty between IR instructions.  GNU as's
   AT&T spellings are used: "fsubrp" is st(1) - st(0). *)

let f80_slot st r =
  match Hashtbl.find_opt st.f80 r with
  | Some off -> off
  | None -> let off = alloc st 16 16 in Hashtbl.replace st.f80 r off; off

let scratch st = if st.scratch = 0 then st.scratch <- alloc st 16 16; st.scratch

let x87 st m op = emit st (X87 (m, op))

(* the 80-bit encoding of a double: sign and 15-bit biased exponent, then a
   64-bit significand with an explicit integer bit *)
let f80_of_float f =
  let bits = Int64.bits_of_float f in
  let sign = Int64.to_int (Int64.shift_right_logical bits 63) lsl 15 in
  let exp = Int64.to_int (Int64.logand (Int64.shift_right_logical bits 52) 0x7ffL) in
  let frac = Int64.logand bits 0xfffffffffffffL in
  if exp = 0 && frac = 0L then 0L, sign
  else if exp = 0x7ff then Int64.logor Int64.min_int (Int64.shift_left frac 11), sign lor 0x7fff
  else if exp = 0 then begin
    let m = ref frac and e = ref (1 - 1023 + 16383) in
    while Int64.logand !m 0x10000000000000L = 0L do m := Int64.shift_left !m 1; decr e done;
    Int64.shift_left !m 11, sign lor !e
  end
  else Int64.logor Int64.min_int (Int64.shift_left frac 11), sign lor (exp - 1023 + 16383)

let f80_const st f =
  let bits = Int64.bits_of_float f in
  match List.assoc_opt bits st.f80_consts with
  | Some l -> l
  | None ->
      st.const_count <- st.const_count + 1;
      let l = Printf.sprintf ".LCL%d" st.const_count in
      st.f80_consts <- (bits, l) :: st.f80_consts; l

(* push a long double operand on the FPU stack *)
let fpush st (op : Ir.operand) =
  match op with
  | Ir.Reg r -> x87 st "fldt" (Some (Mem (RBP, f80_slot st r)))
  | Ir.Fimm f -> x87 st "fldt" (Some (Rip (f80_const st f, 0)))
  | Ir.Imm v -> x87 st "fldt" (Some (Rip (f80_const st (Int64.to_float v), 0)))
  | Ir.Sym _ | Ir.Slot _ -> failwith "Select: address in long double context"

(* pop st(0) into a long double register *)
let fpop_to st r = x87 st "fstpt" (Some (Mem (RBP, f80_slot st r)))

(* A 0/1 result from the flags, into an I32 register. *)
let set_flag st cc (r : int) =
  emit st (Setcc (cc, Reg RAX));
  emit st (Movzx (B, L, Reg RAX, Reg RAX));
  store_int st Ir.I32 r RAX

(* The .file index of a source file, emitting the directive on first use. *)
let file_index st name =
  match Hashtbl.find_opt st.files name with
  | Some n -> n
  | None ->
      let n = st.next_file in
      st.next_file <- n + 1;
      Hashtbl.replace st.files name n;
      n

(* What DWARF is told about a C type: scalars exactly, aggregates by name. *)
let dwarf_type (t : Ctype.t) : dwarf_type =
  match t.u with
  | Ctype.Void -> Dw_void
  | Ctype.Integer k ->
      let size = Target.size_of_ikind k in
      let enc = match k with
        | Ctype.Bool -> 2 | Ctype.Char | Ctype.SChar -> 6 | Ctype.UChar -> 8
        | k when Ctype.is_signed k -> 5 | _ -> 7 in
      Dw_base (Ctype.ikind_to_string k, enc, size)
  | Ctype.Floating k -> Dw_base (Ctype.fkind_to_string k, 4, Target.size_of_fkind k)
  | Ctype.Enum _ -> Dw_base ("unsigned int", 7, 4)
  | Ctype.Pointer _ | Ctype.Array _ | Ctype.Vla _ | Ctype.Func _ -> Dw_pointer
  | Ctype.Struct tag -> Dw_struct (Option.value tag.name ~default:"<anonymous>")
  | Ctype.Union tag -> Dw_union (Option.value tag.name ~default:"<anonymous>")

(* ---- Calling convention (ABI 3.2.3) ------------------------------------------- *)

let int_arg_regs = [| RDI; RSI; RDX; RCX; R8; R9 |]

(* Assign registers to a list of arguments, returning for each either the
   registers it uses or its stack position. *)
type place = In_regs of reg list | On_stack of int (* offset in the argument area *)

let assign_args ~(hidden : bool) (args : Ir.arg list) : place list * int * int * int =
  let ni = ref (if hidden then 1 else 0) and nf = ref 0 and stack = ref 0 in
  let places = List.map (fun a ->
      match a with
      | Ir.Scalar (Ir.F80, _) ->
          (* class X87: in memory, 16-byte aligned *)
          stack := round_up !stack 16;
          let o = !stack in stack := !stack + 16; On_stack o
      | Ir.Scalar (ty, _) when is_float ty ->
          if !nf < 8 then (let r = XMM !nf in incr nf; In_regs [ r ])
          else (let o = !stack in stack := !stack + 8; On_stack o)
      | Ir.Scalar _ ->
          if !ni < 6 then (let r = int_arg_regs.(!ni) in incr ni; In_regs [ r ])
          else (let o = !stack in stack := !stack + 8; On_stack o)
      | Ir.Aggregate a ->
          let need_i = List.length (List.filter (( = ) Ir.Integer) a.classes)
          and need_f = List.length (List.filter (( = ) Ir.Sse) a.classes) in
          if List.mem Ir.Memory a.classes || !ni + need_i > 6 || !nf + need_f > 8 then
            (let o = !stack in stack := !stack + round_up a.size 8; On_stack o)
          else
            In_regs (List.map (function
                | Ir.Integer -> let r = int_arg_regs.(!ni) in incr ni; r
                | Ir.Sse -> let r = XMM !nf in incr nf; r
                | Ir.Memory -> assert false) a.classes)) args in
  places, !ni, !nf, !stack

(* Move eightbyte [i] of the object at [base] (a register holding its
   address) into or out of [r]. *)
let eightbyte_load st base i (r : reg) size =
  match r with
  | XMM _ -> emit st (Sse ((if size - 8 * i >= 8 then "movsd" else "movss"), Mem (base, 8 * i), Reg r))
  | _ ->
      let remaining = size - 8 * i in
      if remaining >= 8 then emit st (Mov (Q, Mem (base, 8 * i), Reg r))
      else begin
        (* assemble a partial eightbyte byte by byte to avoid reading past
           the object; r10 is the scratch, since r11 may be [base] *)
        emit st (Mov (Q, Imm 0L, Reg r));
        for b = remaining - 1 downto 0 do
          emit st (Shift ("shl", Q, Imm 8L, Reg r));
          emit st (Movzx (B, L, Mem (base, 8 * i + b), Reg R10));
          emit st (Alu ("or", Q, Reg R10, Reg r))
        done
      end

let eightbyte_store st base i (r : reg) size =
  match r with
  | XMM _ -> emit st (Sse ((if size - 8 * i >= 8 then "movsd" else "movss"), Reg r, Mem (base, 8 * i)))
  | _ ->
      let remaining = size - 8 * i in
      if remaining >= 8 then emit st (Mov (Q, Reg r, Mem (base, 8 * i)))
      else
        for b = 0 to remaining - 1 do
          emit st (Mov (B, Reg r, Mem (base, 8 * i + b)));
          if b < remaining - 1 then emit st (Shift ("shr", Q, Imm 8L, Reg r))
        done

let memcpy st = emit st Rep_movsb (* rdi, rsi, rcx set by the caller *)

let call st (res : Ir.result option) (callee : Ir.operand) (args : Ir.arg list) variadic =
  let hidden = match res with Some (Ir.Ret_aggregate a) -> List.mem Ir.Memory a.classes | _ -> false in
  let places, _ni, nf, stack_bytes = assign_args ~hidden args in
  let area = round_up stack_bytes 16 in
  if area > 0 then emit st (Alu ("sub", Q, Imm (Int64.of_int area), Reg RSP));
  (* stack arguments first, while the argument registers are still free *)
  List.iter2 (fun a place ->
      match a, place with
      | Ir.Scalar (Ir.F80, op), On_stack off -> fpush st op; x87 st "fstpt" (Some (Mem (RSP, off)))
      | Ir.Scalar (ty, op), On_stack off ->
          if is_float ty then (load_float st ty op (XMM 0); emit st (Sse ("mov" ^ sse_suffix ty, Reg (XMM 0), Mem (RSP, off))))
          else (load_int st ty op RAX; emit st (Mov (Q, Reg RAX, Mem (RSP, off))))
      | Ir.Aggregate ag, On_stack off ->
          load_addr st ag.addr RSI;
          emit st (Lea (Mem (RSP, off), RDI));
          emit st (Mov (Q, Imm (Int64.of_int ag.size), Reg RCX));
          memcpy st
      | _ -> ()) args places;
  (* aggregates in registers: load their eightbytes via r11 *)
  List.iter2 (fun a place ->
      match a, place with
      | Ir.Aggregate ag, In_regs regs ->
          load_addr st ag.addr R11;
          List.iteri (fun i r -> eightbyte_load st R11 i r ag.size) regs
      | _ -> ()) args places;
  (* scalars in registers: those in general registers may not clobber
     r11, and loading an operand only touches the destination *)
  List.iter2 (fun a place ->
      match a, place with
      | Ir.Scalar (ty, op), In_regs [ r ] -> load st ty op r
      | _ -> ()) args places;
  (match res with
   | Some (Ir.Ret_aggregate a) when hidden -> load_addr st a.addr RDI
   | _ -> ());
  if variadic then emit st (Mov (L, Imm (Int64.of_int nf), Reg RAX));
  (match callee with
   | Ir.Sym s -> if Hashtbl.mem st.locals s then emit st (Call (Rip (s, 0))) else emit st (Call (Plt s))
   | op -> load_addr st op R11; emit st (Call (Reg R11)));
  if area > 0 then emit st (Alu ("add", Q, Imm (Int64.of_int area), Reg RSP));
  match res with
  | None -> ()
  | Some (Ir.Ret_scalar (Ir.F80, r)) -> fpop_to st r   (* returned in st(0) *)
  | Some (Ir.Ret_scalar (ty, r)) -> store st ty r (if is_float ty then XMM 0 else RAX)
  | Some (Ir.Ret_aggregate a) when hidden -> ignore a (* the callee wrote through the hidden pointer *)
  | Some (Ir.Ret_aggregate a) ->
      (* returned in rax/rdx and xmm0/xmm1 by class *)
      load_addr st a.addr R11;
      let ni = ref 0 and nf = ref 0 in
      List.iteri (fun i c ->
          let r = match c with
            | Ir.Integer -> let r = [| RAX; RDX |].(!ni) in incr ni; r
            | Ir.Sse -> let r = XMM !nf in incr nf; r
            | Ir.Memory -> assert false in
          eightbyte_store st R11 i r a.size) a.classes

(* ---- Conversions -------------------------------------------------------------- *)

let conv st (c : Ir.conv) (r : int) (op : Ir.operand) =
  match c with
  | Ir.Fconv (Ir.F80, t) -> fpush st op; x87 st (if t = Ir.F32 then "fstps" else "fstpl") (Some (Mem (RBP, reg_slot st r)))
  | Ir.Fconv (f, Ir.F80) ->
      load_float st f op (XMM 0);
      let sc = scratch st in
      emit st (Sse ("mov" ^ sse_suffix f, Reg (XMM 0), Mem (RBP, sc)));
      x87 st (if f = Ir.F32 then "flds" else "fldl") (Some (Mem (RBP, sc)));
      fpop_to st r
  | Ir.Fconv (f, t) -> failwith (Printf.sprintf "Select: Fconv %s" (if f = t then "identity" else "between SSE types"))
  | Ir.Stof (f, Ir.F80) ->
      load_int st f op RAX;
      (match f with Ir.I8 | Ir.I16 | Ir.I32 -> emit st (Movsx (width_of f, Q, Reg RAX, Reg RAX)) | _ -> ());
      let sc = scratch st in
      emit st (Mov (Q, Reg RAX, Mem (RBP, sc))); x87 st "fildll" (Some (Mem (RBP, sc))); fpop_to st r
  | Ir.Utof (f, Ir.F80) ->
      load_int st f op RAX;
      (match f with
       | Ir.I8 | Ir.I16 -> emit st (Movzx (width_of f, L, Reg RAX, Reg RAX))
       | Ir.I32 -> emit st (Mov (L, Reg RAX, Reg RAX))
       | _ -> ());
      let sc = scratch st in
      emit st (Mov (Q, Reg RAX, Mem (RBP, sc))); x87 st "fildll" (Some (Mem (RBP, sc)));
      if f = Ir.I64 then begin
        (* a value with the top bit set was loaded as negative: add 2^64 *)
        let ok = fresh_label st "utof" in
        emit st (Alu ("test", Q, Reg RAX, Reg RAX)); emit st (Jcc (CNS, ok));
        x87 st "fadds" (Some (Rip (float_const st Ir.F32 18446744073709551616.0, 0)));
        emit st (Label ok)
      end;
      fpop_to st r
  | Ir.Ftos (Ir.F80, t) ->
      fpush st op;
      let sc = scratch st in
      x87 st "fisttpll" (Some (Mem (RBP, sc)));
      emit st (Mov (Q, Mem (RBP, sc), Reg RAX)); store_int st t r RAX
  | Ir.Ftou (Ir.F80, t) ->
      fpush st op;
      let sc = scratch st in
      if t = Ir.I64 then begin
        (* from 2^63 up: convert x - 2^63 and put the top bit back *)
        let small = fresh_label st "ftou" and done_ = fresh_label st "ftoud" in
        let two63 = float_const st Ir.F64 9223372036854775808.0 in
        x87 st "fldl" (Some (Rip (two63, 0)));                    (* st0 = 2^63, st1 = x *)
        emit st (Raw "\tfucomip\t%st(1), %st");                  (* CF: 2^63 < x *)
        emit st (Jcc (CA, small));                                 (* 2^63 > x *)
        x87 st "fsubl" (Some (Rip (two63, 0)));
        x87 st "fisttpll" (Some (Mem (RBP, sc)));
        emit st (Mov (Q, Mem (RBP, sc), Reg RAX));
        emit st (Movabs (Int64.min_int, RCX)); emit st (Alu ("xor", Q, Reg RCX, Reg RAX));
        emit st (Jmp done_);
        emit st (Label small);
        x87 st "fisttpll" (Some (Mem (RBP, sc)));
        emit st (Mov (Q, Mem (RBP, sc), Reg RAX));
        emit st (Label done_)
      end else begin
        x87 st "fisttpll" (Some (Mem (RBP, sc)));
        emit st (Mov (Q, Mem (RBP, sc), Reg RAX))
      end;
      store_int st t r RAX
  | Ir.Sext (f, t) -> load_int st f op RAX; emit st (Movsx (width_of f, width_of t, Reg RAX, Reg RAX)); store_int st t r RAX
  | Ir.Zext (f, t) ->
      load_int st f op RAX;
      (match f with
       | Ir.I32 -> emit st (Mov (L, Reg RAX, Reg RAX)) (* a 32-bit move zero-extends *)
       | _ -> emit st (Movzx (width_of f, L, Reg RAX, Reg RAX)));
      store_int st t r RAX
  | Ir.Trunc (f, t) -> load_int st f op RAX; store_int st t r RAX
  | Ir.Fext -> load_float st Ir.F32 op (XMM 0); emit st (Sse ("cvtss2sd", Reg (XMM 0), Reg (XMM 0))); store_float st Ir.F64 r (XMM 0)
  | Ir.Ftrunc -> load_float st Ir.F64 op (XMM 0); emit st (Sse ("cvtsd2ss", Reg (XMM 0), Reg (XMM 0))); store_float st Ir.F32 r (XMM 0)
  | Ir.Stof (f, t) ->
      load_int st f op RAX;
      (match f with Ir.I8 | Ir.I16 -> emit st (Movsx (width_of f, L, Reg RAX, Reg RAX)) | _ -> ());
      emit st (Sse ("cvtsi2" ^ sse_suffix t ^ (if f = Ir.I64 then "q" else "l"), Reg RAX, Reg (XMM 0)));
      store_float st t r (XMM 0)
  | Ir.Utof (f, t) ->
      load_int st f op RAX;
      (match f with
       | Ir.I64 ->
           (* a value with the top bit set is halved, converted and doubled *)
           let neg = fresh_label st "utof" and done_ = fresh_label st "utofd" in
           emit st (Alu ("test", Q, Reg RAX, Reg RAX));
           emit st (Jcc (CS, neg));
           emit st (Sse ("cvtsi2" ^ sse_suffix t ^ "q", Reg RAX, Reg (XMM 0)));
           emit st (Jmp done_);
           emit st (Label neg);
           emit st (Mov (Q, Reg RAX, Reg RCX));
           emit st (Shift ("shr", Q, Imm 1L, Reg RCX));
           emit st (Alu ("and", Q, Imm 1L, Reg RAX));
           emit st (Alu ("or", Q, Reg RAX, Reg RCX));
           emit st (Sse ("cvtsi2" ^ sse_suffix t ^ "q", Reg RCX, Reg (XMM 0)));
           emit st (Sse ("add" ^ sse_suffix t, Reg (XMM 0), Reg (XMM 0)));
           emit st (Label done_)
       | Ir.I32 -> emit st (Mov (L, Reg RAX, Reg RAX)); emit st (Sse ("cvtsi2" ^ sse_suffix t ^ "q", Reg RAX, Reg (XMM 0)))
       | _ -> emit st (Movzx (width_of f, L, Reg RAX, Reg RAX)); emit st (Sse ("cvtsi2" ^ sse_suffix t ^ "l", Reg RAX, Reg (XMM 0))));
      store_float st t r (XMM 0)
  | Ir.Ftos (f, t) ->
      load_float st f op (XMM 0);
      emit st (Sse ("cvtt" ^ sse_suffix f ^ "2si" ^ (if t = Ir.I64 then "q" else "l"), Reg (XMM 0), Reg RAX));
      store_int st t r RAX
  | Ir.Ftou (f, t) ->
      load_float st f op (XMM 0);
      (match t with
       | Ir.I64 ->
           (* values >= 2^63 are shifted down by 2^63, converted, and the bit restored *)
           let big = fresh_label st "ftou" and done_ = fresh_label st "ftoud" in
           let two63 = float_const st f 9223372036854775808.0 in
           emit st (Sse ("mov" ^ sse_suffix f, Rip (two63, 0), Reg (XMM 1)));
           emit st (Sse ("ucomi" ^ sse_suffix f, Reg (XMM 1), Reg (XMM 0)));
           emit st (Jcc (CAE, big));
           emit st (Sse ("cvtt" ^ sse_suffix f ^ "2siq", Reg (XMM 0), Reg RAX));
           emit st (Jmp done_);
           emit st (Label big);
           emit st (Sse ("sub" ^ sse_suffix f, Reg (XMM 1), Reg (XMM 0)));
           emit st (Sse ("cvtt" ^ sse_suffix f ^ "2siq", Reg (XMM 0), Reg RAX));
           emit st (Movabs (Int64.min_int, RCX));
           emit st (Alu ("xor", Q, Reg RCX, Reg RAX));
           emit st (Label done_)
       | _ -> emit st (Sse ("cvtt" ^ sse_suffix f ^ "2siq", Reg (XMM 0), Reg RAX)));
      store_int st t r RAX

(* ---- Instructions ------------------------------------------------------------- *)

(* A switch is dense enough for a table when the table would be at most
   twice the number of cases, plus a few. *)
let dense (cases : (int64 * string) list) =
  List.length cases >= 4 &&
  let lo = List.fold_left (fun m (c, _) -> min m c) Int64.max_int cases in
  let hi = List.fold_left (fun m (c, _) -> max m c) Int64.min_int cases in
  let span = Int64.sub hi lo in
  Int64.compare span 0L >= 0 && Int64.compare span (Int64.of_int (2 * List.length cases + 8)) <= 0

let alu_of = function
  | Ir.Add -> "add" | Ir.Sub -> "sub" | Ir.Mul -> "imul" | Ir.And -> "and" | Ir.Or -> "or" | Ir.Xor -> "xor"
  | Ir.Fadd -> "add" | Ir.Fsub -> "sub" | Ir.Fmul -> "mul" | Ir.Fdiv -> "div"
  | _ -> assert false

let cc_of_int = function
  | Ir.Eq -> CE | Ir.Ne -> CNE | Ir.Slt -> CL | Ir.Sle -> CLE | Ir.Sgt -> CG | Ir.Sge -> CGE
  | Ir.Ult -> CB | Ir.Ule -> CBE | Ir.Ugt -> CA | Ir.Uge -> CAE
  | _ -> assert false

let binop st op ty r a b =
  match op with
  | Ir.Add | Ir.Sub | Ir.Mul | Ir.And | Ir.Or | Ir.Xor ->
      load_int st ty a RAX;
      let w = match ty with Ir.I8 | Ir.I16 when op = Ir.Mul -> L | _ -> width_of ty in
      emit st (Alu (alu_of op, w, src_operand st ty b, Reg RAX));
      store_int st ty r RAX
  | Ir.Fadd | Ir.Fsub | Ir.Fmul | Ir.Fdiv ->
      load_float st ty a (XMM 0); load_float st ty b (XMM 1);
      emit st (Sse (alu_of op ^ sse_suffix ty, Reg (XMM 1), Reg (XMM 0)));
      store_float st ty r (XMM 0)
  | Ir.Sdiv | Ir.Srem | Ir.Udiv | Ir.Urem ->
      let signed = (op = Ir.Sdiv || op = Ir.Srem) in
      load_int st ty a RAX; load_int st ty b RCX;
      (* 8- and 16-bit division is done in 32 bits *)
      let w = match ty with Ir.I8 | Ir.I16 -> L | _ -> width_of ty in
      (match ty with
       | Ir.I8 | Ir.I16 ->
           if signed then (emit st (Movsx (width_of ty, L, Reg RAX, Reg RAX)); emit st (Movsx (width_of ty, L, Reg RCX, Reg RCX)))
           else (emit st (Movzx (width_of ty, L, Reg RAX, Reg RAX)); emit st (Movzx (width_of ty, L, Reg RCX, Reg RCX)))
       | _ -> ());
      if signed then (emit st (if w = Q then Cqo else Cdq); emit st (Idiv (w, Reg RCX)))
      else (emit st (Mov (L, Imm 0L, Reg RDX)); emit st (Div (w, Reg RCX)));
      store_int st ty r (if op = Ir.Sdiv || op = Ir.Udiv then RAX else RDX)
  | Ir.Shl | Ir.Sshr | Ir.Ushr ->
      load_int st ty a RAX;
      let m = match op with Ir.Shl -> "shl" | Ir.Sshr -> "sar" | _ -> "shr" in
      (match b with
       | Ir.Imm v -> emit st (Shift (m, width_of ty, Imm (Int64.logand v 63L), Reg RAX))
       | _ -> load_int st ty b RCX; emit st (Shift (m, width_of ty, Reg RCX, Reg RAX)));
      store_int st ty r RAX

(* ---- Inline assembly (extension) ----------------------------------------------

   The template is text for the assembler with operands to fill in.  Each
   operand gets a register from its constraint: a fixed one ("a" is rax,
   "D" is rdi, "{r10}" names one), or one from the pool of caller-saved
   registers for "r", or an xmm register for "x"; "m" operands are memory
   references and "i" immediates; a digit ties an input to an earlier
   operand's register.  Inputs are loaded before the text and outputs
   stored after it.  The register allocator treats the statement as
   clobbering every caller-saved register; callee-saved registers the
   template names are saved around it. *)

let reg_of_num = [| RAX; RCX; RDX; RBX; RSP; RBP; RSI; RDI; R8; R9; R10; R11; R12; R13; R14; R15 |]

let inline_asm st (a : Ir.asm) =
  let n = Array.length a.operands in
  let constr = function
    | Ir.Asm_in (c, _, _) | Ir.Asm_out (c, _, _) | Ir.Asm_inout (c, _, _, _) | Ir.Asm_mem (c, _) -> c
    | Ir.Asm_imm _ -> "i" in
  let named name = match Gas.register_of_name name with
    | Some { Gas.rclass = Gas.Gpr; rnum; _ } -> Some reg_of_num.(rnum)
    | Some { Gas.rclass = Gas.Xmm; rnum; _ } -> Some (XMM rnum)
    | _ -> None in
  let fixed c =
    match c with
    | "a" -> Some RAX | "b" -> Some RBX | "c" -> Some RCX | "d" -> Some RDX | "S" -> Some RSI | "D" -> Some RDI
    | _ when String.length c > 2 && c.[0] = '{' -> named (String.sub c 1 (String.length c - 2))
    | _ -> None in
  let clobbered = List.filter_map named a.clobbers in
  let regs = Array.make n None in
  Array.iteri (fun i op -> regs.(i) <- fixed (constr op)) a.operands;
  let taken = List.filter_map Fun.id (Array.to_list regs) @ clobbered in
  let pool = ref (List.filter (fun r -> not (List.mem r taken)) [ RAX; RCX; RDX; RSI; RDI; R8; R9; R10; R11 ]) in
  let xmm_pool = ref (List.filter (fun r -> not (List.mem r taken)) (List.init 16 (fun k -> XMM k))) in
  let take pool = match !pool with r :: rest -> pool := rest; r | [] -> failwith "inline asm: out of registers" in
  Array.iteri (fun i op ->
      if regs.(i) = None then
        match constr op, op with
        | _, Ir.Asm_imm _ -> ()
        | c, Ir.Asm_mem _ when String.contains c 'm' -> regs.(i) <- Some (take pool)   (* holds the address *)
        | ("r" | "q" | "g" | "X"), _ -> regs.(i) <- Some (take pool)
        | "x", _ -> regs.(i) <- Some (take xmm_pool)
        | ("t" | "u"), _ -> ()   (* on the FPU stack *)
        | c, _ when String.length c = 1 && c.[0] >= '0' && c.[0] <= '9' -> ()
        | c, _ -> failwith ("inline asm: unsupported constraint " ^ c)) a.operands;
  (* digits: the same register as the operand they name *)
  Array.iteri (fun i op ->
      let c = constr op in
      if String.length c = 1 && c.[0] >= '0' && c.[0] <= '9' then regs.(i) <- regs.(Char.code c.[0] - 48)) a.operands;
  let reg i = match regs.(i) with Some r -> r | None -> failwith "inline asm: operand without a register" in
  (* callee-saved registers the template may change *)
  let saved = List.filter (fun r -> List.mem r [ RBX; R12; R13; R14; R15 ])
      (List.sort_uniq compare (clobbered @ List.filter_map Fun.id (Array.to_list regs))) in
  List.iter (fun r -> emit st (Push (Reg r))) saved;
  (* inputs *)
  let on_fpu c = c = "t" || c = "u" in
  Array.iteri (fun i op ->
      match op with
      | Ir.Asm_in (c, ty, v) | Ir.Asm_inout (c, ty, _, v) -> if not (on_fpu c) then load st ty v (reg i)
      | Ir.Asm_mem (_, addr) -> load_addr st addr (reg i)
      | Ir.Asm_out _ | Ir.Asm_imm _ -> ()) a.operands;
  (* FPU operands: "u" is st(1), "t" is st(0), so u is pushed first *)
  let fpu_ops c = List.filter (fun i -> constr a.operands.(i) = c) (List.init n Fun.id) in
  List.iter (fun i ->
      match a.operands.(i) with
      | Ir.Asm_in (_, _, v) | Ir.Asm_inout (_, _, _, v) -> fpush st v
      | _ -> ()) (fpu_ops "u" @ fpu_ops "t");
  (* the text, with %0 .. %9 (and %k0, %q0, %w0, %b0) substituted *)
  let width_of_ty = function Ir.I8 -> B | Ir.I16 -> W | Ir.I32 -> L | _ -> Q in
  let text i modifier =
    match a.operands.(i) with
    | Ir.Asm_imm v -> "$" ^ Int64.to_string v
    | Ir.Asm_mem _ -> "(" ^ Emit.reg Q (reg i) ^ ")"
    | Ir.Asm_in (c, _, _) | Ir.Asm_out (c, _, _) | Ir.Asm_inout (c, _, _, _) when on_fpu c -> if c = "t" then "%st" else "%st(1)"
    | Ir.Asm_in (_, ty, _) | Ir.Asm_out (_, ty, _) | Ir.Asm_inout (_, ty, _, _) ->
        (match reg i with
         | XMM k -> Printf.sprintf "%%xmm%d" k
         | r ->
             let w = match modifier with
               | Some 'b' -> B | Some 'w' -> W | Some 'k' -> L | Some 'q' -> Q
               | _ -> width_of_ty ty in
             Emit.reg w r) in
  let b = Buffer.create (String.length a.template) in
  let t = a.template in
  let len = String.length t in
  let i = ref 0 in
  while !i < len do
    if t.[!i] = '%' && !i + 1 < len then begin
      let c = t.[!i + 1] in
      if c = '%' then (Buffer.add_char b '%'; i := !i + 2)
      else if c >= '0' && c <= '9' then (Buffer.add_string b (text (Char.code c - 48) None); i := !i + 2)
      else if (c = 'k' || c = 'q' || c = 'w' || c = 'b') && !i + 2 < len && t.[!i + 2] >= '0' && t.[!i + 2] <= '9' then
        (Buffer.add_string b (text (Char.code t.[!i + 2] - 48) (Some c)); i := !i + 3)
      else (Buffer.add_char b '%'; incr i)
    end else (Buffer.add_char b t.[!i]; incr i)
  done;
  List.iter (fun line -> if String.trim line <> "" then emit st (Raw ("\t" ^ String.trim line)))
    (String.split_on_char '\n' (Buffer.contents b));
  (* outputs: an FPU result is popped first, then the pushed inputs the
     template did not consume ("st" among the clobbers says it did) *)
  Array.iteri (fun i op ->
      match op with
      | Ir.Asm_out (c, _, r) | Ir.Asm_inout (c, _, r, _) when on_fpu c -> fpop_to st r
      | Ir.Asm_out (_, ty, r) | Ir.Asm_inout (_, ty, r, _) -> store st ty r (reg i)
      | _ -> ()) a.operands;
  let consumed = List.mem "st" a.clobbers in
  List.iter (fun i ->
      match a.operands.(i) with
      | Ir.Asm_in (c, _, _) when c = "u" || not consumed -> emit st (Raw "\tfstp\t%st(0)")
      | _ -> ()) (fpu_ops "t" @ fpu_ops "u");
  List.iter (fun r -> emit st (Pop (Reg r))) (List.rev saved)

let instr st (i : Ir.instr) =
  match i with
  | Ir.Mov (Ir.F80, r, op) -> fpush st op; fpop_to st r
  | Ir.Binop ((Ir.Fadd | Ir.Fsub | Ir.Fmul | Ir.Fdiv) as op, Ir.F80, r, a, b) ->
      fpush st a; fpush st b;
      x87 st (match op with Ir.Fadd -> "faddp" | Ir.Fsub -> "fsubrp" | Ir.Fmul -> "fmulp" | _ -> "fdivrp") None;
      fpop_to st r
  | Ir.Neg (Ir.F80, r, op) -> fpush st op; x87 st "fchs" None; fpop_to st r
  | Ir.Cmp (c, Ir.F80, r, a, b) ->
      (* fucomip compares st(0) with st(1) and pops: CF for below, ZF for
         equal, PF for unordered; the comparison is arranged so that the
         test is false on NaN except for != *)
      let against x y = fpush st y; fpush st x; emit st (Raw "\tfucomip\t%st(1), %st"); emit st (Raw "\tfstp\t%st(0)") in
      (match c with
       | Ir.Feq -> against a b; emit st (Setcc (CE, Reg RAX)); emit st (Setcc (CNP, Reg RCX)); emit st (Alu ("and", B, Reg RCX, Reg RAX))
       | Ir.Fne -> against a b; emit st (Setcc (CNE, Reg RAX)); emit st (Setcc (CP, Reg RCX)); emit st (Alu ("or", B, Reg RCX, Reg RAX))
       | Ir.Fgt -> against a b; emit st (Setcc (CA, Reg RAX))
       | Ir.Fge -> against a b; emit st (Setcc (CAE, Reg RAX))
       | Ir.Flt -> against b a; emit st (Setcc (CA, Reg RAX))
       | Ir.Fle -> against b a; emit st (Setcc (CAE, Reg RAX))
       | _ -> assert false);
      emit st (Movzx (B, L, Reg RAX, Reg RAX));
      store_int st Ir.I32 r RAX
  | Ir.Load (Ir.F80, r, addr) -> let m = mem_operand st addr in x87 st "fldt" (Some m); fpop_to st r
  | Ir.Store (Ir.F80, addr, v) -> fpush st v; let m = mem_operand st addr in x87 st "fstpt" (Some m)
  | Ir.Va_arg (Ir.F80, r, ap) ->
      (* always in the overflow area, 16-byte aligned (ABI 3.5.7) *)
      load_addr st ap RCX;
      emit st (Mov (Q, Mem (RCX, 8), Reg RDX));
      emit st (Alu ("add", Q, Imm 15L, Reg RDX)); emit st (Alu ("and", Q, Imm (-16L), Reg RDX));
      x87 st "fldt" (Some (Mem (RDX, 0)));
      emit st (Alu ("add", Q, Imm 16L, Reg RDX)); emit st (Mov (Q, Reg RDX, Mem (RCX, 8)));
      fpop_to st r
  | Ir.Intrinsic (intr, Ir.F80, r, op) -> fpush st op; x87 st (if intr = Ir.Fabs then "fabs" else "fsqrt") None; fpop_to st r
  | Ir.Mov (ty, r, op) ->
      if is_float ty then (load_float st ty op (XMM 0); store_float st ty r (XMM 0))
      else (load_int st ty op RAX; store_int st ty r RAX)
  | Ir.Binop (op, ty, r, a, b) -> binop st op ty r a b
  | Ir.Binop_overflow (op, ty, signed, r, flag, a, b) ->
      load_int st ty a RAX; load_int st ty b RCX;
      let w = width_of ty in
      (match op, signed with
       | Ir.Mul, false ->
           (* one-operand unsigned multiply: rdx:rax = rax * rcx, CF/OF set if rdx is used *)
           emit st (Unary ("mul", w, Reg RCX))
       | _ -> emit st (Alu (alu_of op, w, Reg RCX, Reg RAX)));
      store_int st ty r RAX;
      set_flag st (if signed || op = Ir.Mul then CO else CB) flag
  | Ir.Neg (ty, r, op) ->
      if is_float ty then begin
        load_float st ty op (XMM 0);
        let sign = float_const st ty (if ty = Ir.F32 then Int32.float_of_bits 0x8000_0000l else Int64.float_of_bits Int64.min_int) in
        emit st (Sse ("mov" ^ sse_suffix ty, Rip (sign, 0), Reg (XMM 1)));
        emit st (Sse ("xorp" ^ (if ty = Ir.F32 then "s" else "d"), Reg (XMM 1), Reg (XMM 0)));
        store_float st ty r (XMM 0)
      end else (load_int st ty op RAX; emit st (Unary ("neg", width_of ty, Reg RAX)); store_int st ty r RAX)
  | Ir.Not (ty, r, op) -> load_int st ty op RAX; emit st (Unary ("not", width_of ty, Reg RAX)); store_int st ty r RAX
  | Ir.Cmp (c, ty, r, a, b) ->
      if is_float ty then begin
        load_float st ty a (XMM 0); load_float st ty b (XMM 1);
        let uc = "ucomi" ^ sse_suffix ty in
        (* ucomisd src, dst sets flags for dst ? src; unordered sets ZF, PF and CF,
           so tests are arranged to be false on NaN except for != *)
        (match c with
         | Ir.Feq -> emit st (Sse (uc, Reg (XMM 1), Reg (XMM 0))); emit st (Setcc (CE, Reg RAX)); emit st (Setcc (CNP, Reg RCX)); emit st (Alu ("and", B, Reg RCX, Reg RAX))
         | Ir.Fne -> emit st (Sse (uc, Reg (XMM 1), Reg (XMM 0))); emit st (Setcc (CNE, Reg RAX)); emit st (Setcc (CP, Reg RCX)); emit st (Alu ("or", B, Reg RCX, Reg RAX))
         | Ir.Flt -> emit st (Sse (uc, Reg (XMM 0), Reg (XMM 1))); emit st (Setcc (CA, Reg RAX))
         | Ir.Fle -> emit st (Sse (uc, Reg (XMM 0), Reg (XMM 1))); emit st (Setcc (CAE, Reg RAX))
         | Ir.Fgt -> emit st (Sse (uc, Reg (XMM 1), Reg (XMM 0))); emit st (Setcc (CA, Reg RAX))
         | Ir.Fge -> emit st (Sse (uc, Reg (XMM 1), Reg (XMM 0))); emit st (Setcc (CAE, Reg RAX))
         | _ -> assert false);
        emit st (Movzx (B, L, Reg RAX, Reg RAX));
        store_int st Ir.I32 r RAX
      end else begin
        load_int st ty a RAX;
        emit st (Alu ("cmp", width_of ty, src_operand st ty b, Reg RAX));
        set_flag st (cc_of_int c) r
      end
  | Ir.Conv (c, r, op) -> conv st c r op
  | Ir.Load (ty, r, addr) ->
      let m = mem_operand st addr in
      if is_float ty then (emit st (Sse ("mov" ^ sse_suffix ty, m, Reg (XMM 0))); store_float st ty r (XMM 0))
      else (emit st (Mov (width_of ty, m, Reg RAX)); store_int st ty r RAX)
  | Ir.Store (ty, addr, v) ->
      (* the value first: computing the address may use rcx *)
      if is_float ty then (load_float st ty v (XMM 0); let m = mem_operand st addr in emit st (Sse ("mov" ^ sse_suffix ty, Reg (XMM 0), m)))
      else (load_int st ty v RAX; let m = mem_operand st addr in emit st (Mov (width_of ty, Reg RAX, m)))
  | Ir.Memcpy (dst, src, n) ->
      load_addr st dst RDI; load_addr st src RSI;
      emit st (Mov (Q, Imm (Int64.of_int n), Reg RCX)); memcpy st
  | Ir.Memzero (dst, n) ->
      load_addr st dst RDI;
      emit st (Mov (L, Imm 0L, Reg RAX));
      emit st (Mov (Q, Imm (Int64.of_int n), Reg RCX));
      emit st Rep_stosb
  | Ir.Call (res, callee, args, variadic) -> call st res callee args variadic
  | Ir.Inline_asm a -> inline_asm st a
  | Ir.Label l -> emit st (Label l)
  | Ir.Jump l -> emit st (Jmp l)
  | Ir.Branch (c, t, f) ->
      (match c with
       | Ir.Reg r when (match location st r with `Reg _ -> true | `Mem _ -> false) ->
           let p = (match location st r with `Reg p -> p | `Mem _ -> assert false) in
           emit st (Alu ("test", L, Reg p, Reg p))
       | _ -> load_int st Ir.I32 c RAX; emit st (Alu ("test", L, Reg RAX, Reg RAX)));
      emit st (Jcc (CNE, t)); emit st (Jmp f)
  | Ir.Switch (ty, v, cases, default) when dense cases ->
      (* a jump table: index = value - smallest case, bounds-checked
         unsigned so one comparison also rejects values below the range.
         Entries are offsets from the table so the code is position
         independent either way. *)
      let lo = List.fold_left (fun m (c, _) -> min m c) Int64.max_int cases in
      let hi = List.fold_left (fun m (c, _) -> max m c) Int64.min_int cases in
      load_int st ty v RAX;
      let w = width_of ty in
      if lo <> 0L then emit st (Alu ("sub", w, Imm (Int64.of_int32 (Int64.to_int32 lo)), Reg RAX));
      (match ty with Ir.I64 -> () | _ -> emit st (Mov (L, Reg RAX, Reg RAX)));
      emit st (Alu ("cmp", Q, Imm (Int64.sub hi lo), Reg RAX));
      emit st (Jcc (CA, default));
      st.const_count <- st.const_count + 1;
      let table = Printf.sprintf ".LJT%d" st.const_count in
      let entries = Array.make (Int64.to_int (Int64.sub hi lo) + 1) default in
      List.iter (fun (c, l) -> entries.(Int64.to_int (Int64.sub c lo)) <- l) cases;
      st.tables <- { dname = table; dglobal = false; dweak = false; dhidden = false; dalias = None; dfunc = false; ddecl = false; dtls = false; dalign = 4; section = Rodata; size = 4 * Array.length entries;
                     items = Array.to_list (Array.map (fun l -> Long_diff (l, table)) entries) } :: st.tables;
      emit st (Lea (Rip (table, 0), RCX));
      emit st (Movsx (L, Q, Mem_index (RCX, RAX, 4), Reg RAX));
      emit st (Alu ("add", Q, Reg RCX, Reg RAX));
      emit st (Jmp_indirect (Reg RAX))
  | Ir.Switch (ty, v, cases, default) ->
      (* compare at the type's width: the case constants are already
         reduced to it, so signedness does not matter *)
      load_int st ty v RAX;
      List.iter (fun (value, l) ->
          (match ty with
           | Ir.I64 ->
               if fits32 value then emit st (Alu ("cmp", Q, Imm value, Reg RAX))
               else (emit st (Movabs (value, RCX)); emit st (Alu ("cmp", Q, Reg RCX, Reg RAX)))
           | _ ->
               (* the low 32 bits as a signed immediate *)
               let low = Int64.of_int32 (Int64.to_int32 value) in
               emit st (Alu ("cmp", width_of ty, Imm low, Reg RAX)));
          emit st (Jcc (CE, l))) cases;
      emit st (Jmp default)
  | Ir.Ret v ->
      (match v with
       | None -> ()
       | Some (Ir.Rv_scalar (Ir.F80, op)) -> fpush st op
       | Some (Ir.Rv_scalar (ty, op)) -> load st ty op (if is_float ty then XMM 0 else RAX)
       | Some (Ir.Rv_aggregate a) ->
           if List.mem Ir.Memory a.classes then begin
             emit st (Mov (Q, Mem (RBP, st.hidden_ptr), Reg RDI));
             load_addr st a.addr RSI;
             emit st (Mov (Q, Imm (Int64.of_int a.size), Reg RCX));
             memcpy st;
             emit st (Mov (Q, Mem (RBP, st.hidden_ptr), Reg RAX))
           end else begin
             load_addr st a.addr R11;
             let ni = ref 0 and nf = ref 0 in
             List.iteri (fun i c ->
                 let r = match c with
                   | Ir.Integer -> let r = [| RAX; RDX |].(!ni) in incr ni; r
                   | Ir.Sse -> let r = XMM !nf in incr nf; r
                   | Ir.Memory -> assert false in
                 eightbyte_load st R11 i r a.size) a.classes
           end);
      emit st (Jmp (".L" ^ st.fname ^ ".ret"))
  | Ir.Atomic_load (ty, r, addr, _) ->
      (* loads are acquire on x86-64; seq_cst needs nothing extra given how stores are done *)
      load_addr st addr RCX;
      if is_float ty then (emit st (Sse ("mov" ^ sse_suffix ty, Mem (RCX, 0), Reg (XMM 0))); store_float st ty r (XMM 0))
      else (emit st (Mov (width_of ty, Mem (RCX, 0), Reg RAX)); store_int st ty r RAX)
  | Ir.Atomic_store (ty, addr, v, order) ->
      load_addr st addr RCX; load_int st ty v RAX;
      (* a seq_cst store is an xchg, which is a full barrier *)
      if order = Ir.Seq_cst then emit st (Xchg (width_of ty, Reg RAX, Mem (RCX, 0)))
      else emit st (Mov (width_of ty, Reg RAX, Mem (RCX, 0)))
  | Ir.Atomic_xchg (ty, r, addr, v, _) ->
      load_addr st addr RCX; load_int st ty v RAX;
      emit st (Xchg (width_of ty, Reg RAX, Mem (RCX, 0)));
      store_int st ty r RAX
  | Ir.Atomic_rmw (op, ty, r, addr, v, _) ->
      load_addr st addr RCX; load_int st ty v RDX;
      let w = width_of ty in
      (match op with
       | Ir.Add | Ir.Sub ->
           if op = Ir.Sub then emit st (Unary ("neg", w, Reg RDX));
           emit st (Lock (Alu ("xadd", w, Reg RDX, Mem (RCX, 0))));
           store_int st ty r RDX
       | Ir.And | Ir.Or | Ir.Xor ->
           (* compare-and-swap loop: old in rax, new in rsi *)
           let loop = fresh_label st "rmw" in
           emit st (Mov (w, Mem (RCX, 0), Reg RAX));
           emit st (Label loop);
           emit st (Mov (Q, Reg RAX, Reg RSI));
           emit st (Alu (alu_of op, w, Reg RDX, Reg RSI));
           emit st (Lock (Alu ("cmpxchg", w, Reg RSI, Mem (RCX, 0))));
           emit st (Jcc (CNE, loop));
           store_int st ty r RAX
       | _ -> assert false)
  | Ir.Atomic_cmpxchg (ty, r, addr, expected, desired, _) ->
      (* rax = *expected; lock cmpxchg desired, *addr; on failure *expected = rax *)
      load_addr st addr RCX; load_addr st expected RSI; load_int st ty desired RDX;
      let w = width_of ty in
      let fail = fresh_label st "cas" and done_ = fresh_label st "casd" in
      emit st (Mov (w, Mem (RSI, 0), Reg RAX));
      emit st (Lock (Alu ("cmpxchg", w, Reg RDX, Mem (RCX, 0))));
      emit st (Jcc (CNE, fail));
      emit st (Mov (L, Imm 1L, Reg RAX));
      emit st (Jmp done_);
      emit st (Label fail);
      emit st (Mov (w, Reg RAX, Mem (RSI, 0)));
      emit st (Mov (L, Imm 0L, Reg RAX));
      emit st (Label done_);
      store_int st Ir.I32 r RAX
  | Ir.Fence order -> if order = Ir.Seq_cst then emit st Mfence
  | Ir.Va_start ap ->
      (* ABI 3.5.7: gp_offset, fp_offset, overflow_arg_area, reg_save_area *)
      load_addr st ap RAX;
      emit st (Mov (L, Imm (Int64.of_int (8 * st.va_gp)), Mem (RAX, 0)));
      emit st (Mov (L, Imm (Int64.of_int (48 + 16 * st.va_fp)), Mem (RAX, 4)));
      emit st (Lea (Mem (RBP, 16 + st.va_stack), RCX));
      emit st (Mov (Q, Reg RCX, Mem (RAX, 8)));
      emit st (Lea (Mem (RBP, st.save_area), RCX));
      emit st (Mov (Q, Reg RCX, Mem (RAX, 16)))
  | Ir.Va_arg (ty, r, ap) ->
      load_addr st ap RCX;
      let overflow = fresh_label st "vaov" and done_ = fresh_label st "vad" in
      let off_field, limit, step = if is_float ty then 4, 176, 16 else 0, 48, 8 in
      emit st (Mov (L, Mem (RCX, off_field), Reg RAX));
      emit st (Alu ("cmp", L, Imm (Int64.of_int limit), Reg RAX));
      emit st (Jcc (CAE, overflow));
      emit st (Mov (Q, Mem (RCX, 16), Reg RDX));
      emit st (Alu ("add", Q, Reg RAX, Reg RDX)); (* rdx = reg_save_area + offset *)
      emit st (Alu ("add", L, Imm (Int64.of_int step), Mem (RCX, off_field)));
      emit st (Jmp done_);
      emit st (Label overflow);
      emit st (Mov (Q, Mem (RCX, 8), Reg RDX));
      emit st (Lea (Mem (RDX, 8), RSI));
      emit st (Mov (Q, Reg RSI, Mem (RCX, 8)));
      emit st (Label done_);
      if is_float ty then (emit st (Sse ("mov" ^ sse_suffix ty, Mem (RDX, 0), Reg (XMM 0))); store_float st ty r (XMM 0))
      else (emit st (Mov (width_of ty, Mem (RDX, 0), Reg RAX)); store_int st ty r RAX)
  | Ir.Alloca (r, size) ->
      (* stack space for a variable length array, kept 16-byte aligned; the
         epilogue restores rsp from rbp, so nothing is freed before return *)
      load_int st Ir.I64 size RAX;
      emit st (Alu ("add", Q, Imm 15L, Reg RAX));
      emit st (Alu ("and", Q, Imm (-16L), Reg RAX));
      emit st (Alu ("sub", Q, Reg RAX, Reg RSP));
      emit st (Mov (Q, Reg RSP, Reg RAX));
      store_int st Ir.I64 r RAX
  | Ir.Va_arg_aggregate (dst, size, classes, ap) ->
      (* ABI 3.5.7 step by step: an aggregate whose eightbytes all fit in the
         remaining register save area is copied from there, one class at a
         time; otherwise it is taken from the overflow area *)
      load_addr st ap RCX;
      load_addr st dst RDI;
      let overflow = fresh_label st "vaov" and done_ = fresh_label st "vad" in
      let n_int = List.length (List.filter (( = ) Ir.Integer) classes) and n_sse = List.length (List.filter (( = ) Ir.Sse) classes) in
      if List.mem Ir.Memory classes || size > 16 then emit st (Jmp overflow)
      else begin
        if n_int > 0 then begin
          emit st (Mov (L, Mem (RCX, 0), Reg RAX));
          emit st (Alu ("cmp", L, Imm (Int64.of_int (48 - 8 * n_int)), Reg RAX)); emit st (Jcc (CA, overflow))
        end;
        if n_sse > 0 then begin
          emit st (Mov (L, Mem (RCX, 4), Reg RDX));
          emit st (Alu ("cmp", L, Imm (Int64.of_int (176 - 16 * n_sse)), Reg RDX)); emit st (Jcc (CA, overflow))
        end;
        emit st (Mov (Q, Mem (RCX, 16), Reg RSI));   (* reg_save_area *)
        List.iteri (fun i cls ->
            let field = if cls = Ir.Integer then 0 else 4 in
            emit st (Mov (L, Mem (RCX, field), Reg RAX));
            emit st (Mov (Q, Mem_index (RSI, RAX, 1), Reg R8));
            emit st (Mov (Q, Reg R8, Mem (RDI, 8 * i)));
            emit st (Alu ("add", L, Imm (if cls = Ir.Integer then 8L else 16L), Mem (RCX, field)))) classes;
        emit st (Jmp done_)
      end;
      emit st (Label overflow);
      emit st (Mov (Q, Mem (RCX, 8), Reg RSI));      (* overflow_arg_area *)
      let words = (size + 7) / 8 in
      for i = 0 to words - 1 do
        emit st (Mov (Q, Mem (RSI, 8 * i), Reg R8));
        emit st (Mov (Q, Reg R8, Mem (RDI, 8 * i)))
      done;
      emit st (Alu ("add", Q, Imm (Int64.of_int (8 * words)), Reg RSI));
      emit st (Mov (Q, Reg RSI, Mem (RCX, 8)));
      emit st (Label done_)
  | Ir.Intrinsic (Ir.Fsqrt, ty, r, op) ->
      load_float st ty op (XMM 0);
      emit st (Sse ("sqrt" ^ sse_suffix ty, Reg (XMM 0), Reg (XMM 0)));
      store_float st ty r (XMM 0)
  | Ir.Intrinsic (Ir.Fabs, ty, r, op) ->
      (* clear the sign bit *)
      load_float st ty op (XMM 0);
      let mask = float_const st ty (if ty = Ir.F32 then Int32.float_of_bits 0x7FFF_FFFFl else Int64.float_of_bits Int64.max_int) in
      emit st (Sse ("mov" ^ sse_suffix ty, Rip (mask, 0), Reg (XMM 1)));
      emit st (Sse ("andp" ^ (if ty = Ir.F32 then "s" else "d"), Reg (XMM 1), Reg (XMM 0)));
      store_float st ty r (XMM 0)
  | Ir.Line loc -> if st.debug then emit st (Loc (file_index st loc.Loc.file, loc.Loc.line))
  | Ir.Trap -> emit st Ud2
  | Ir.Return_address r -> emit st (Mov (Q, Mem (RBP, 8), Reg RAX)); store_int st Ir.I64 r RAX

(* ---- Functions ------------------------------------------------------------------- *)

let func st (f : Ir.func) : func =
  st.code <- []; st.regs <- Hashtbl.create 64; st.f80 <- Hashtbl.create 8; st.scratch <- 0; st.frame <- 0; st.fname <- f.name; st.label_count <- 0;
  (* frame: IR slots; spill slots are allocated as they are first used *)
  st.slots <- Array.map (fun (s : Ir.slot) -> alloc st s.size (max s.align 1)) f.slots;
  st.alloc <- Regalloc.allocate f;
  st.saved <- List.map (fun p -> p, alloc st 8 8) st.alloc.used;
  (* parameters arrive per the same assignment a caller makes *)
  let hidden = match f.returns_aggregate with Some (_, classes) -> List.mem Ir.Memory classes | None -> false in
  let as_args = List.map (function
      | Ir.P_scalar (ty, r) -> Ir.Scalar (ty, Ir.Reg r)
      | Ir.P_aggregate (slot, size, classes) -> Ir.Aggregate { Ir.addr = Ir.Slot slot; size; classes }) f.params in
  let places, ni, nf, stack_bytes = assign_args ~hidden as_args in
  st.va_gp <- ni; st.va_fp <- nf; st.va_stack <- stack_bytes;
  if hidden then (st.hidden_ptr <- alloc st 8 8);
  if f.variadic then st.save_area <- alloc st 176 16;
  let body_start = st.code in
  ignore body_start;
  (* prologue body: spill incoming registers before anything clobbers them *)
  if hidden then emit st (Mov (Q, Reg RDI, Mem (RBP, st.hidden_ptr)));
  List.iter2 (fun p place ->
      match p, place with
      | Ir.P_scalar (ty, r), In_regs [ reg ] -> store st ty r reg
      | Ir.P_scalar (Ir.F80, r), On_stack off -> x87 st "fldt" (Some (Mem (RBP, 16 + off))); fpop_to st r
      | Ir.P_scalar (ty, r), On_stack off ->
          if is_float ty then (emit st (Sse ("mov" ^ sse_suffix ty, Mem (RBP, 16 + off), Reg (XMM 0))); store_float st ty r (XMM 0))
          else (emit st (Mov (width_of ty, Mem (RBP, 16 + off), Reg RAX)); store_int st ty r RAX)
      | Ir.P_aggregate (slot, size, _), In_regs regs ->
          emit st (Lea (Mem (RBP, st.slots.(slot)), R11));
          List.iteri (fun i reg -> eightbyte_store st R11 i reg size) regs
      | Ir.P_aggregate (slot, size, _), On_stack off ->
          emit st (Lea (Mem (RBP, 16 + off), RSI));
          emit st (Lea (Mem (RBP, st.slots.(slot)), RDI));
          emit st (Mov (Q, Imm (Int64.of_int size), Reg RCX));
          memcpy st
      | _ -> assert false) f.params places;
  if f.variadic then begin
    (* save every argument register: the callee cannot know which were used *)
    Array.iteri (fun i r -> emit st (Mov (Q, Reg r, Mem (RBP, st.save_area + 8 * i)))) int_arg_regs;
    for i = 0 to 7 do emit st (Sse ("movaps", Reg (XMM i), Mem (RBP, st.save_area + 48 + 16 * i))) done
  end;
  (* a comparison whose only use is the branch that follows it becomes a
     conditional jump, without materialising 0 or 1 *)
  let uses = Hashtbl.create 64 in
  List.iter (fun i -> let _, us = Regalloc.regs_of_instr i in List.iter (fun r -> Hashtbl.replace uses r (1 + Option.value (Hashtbl.find_opt uses r) ~default:0)) us) f.body;
  let rec select = function
    | Ir.Cmp (c, ty, r, a, b) :: Ir.Branch (Ir.Reg r', t, e) :: rest when r = r' && Hashtbl.find_opt uses r = Some 1 && not (is_float ty) ->
        load_int st ty a RAX;
        emit st (Alu ("cmp", width_of ty, src_operand st ty b, Reg RAX));
        emit st (Jcc (cc_of_int c, t)); emit st (Jmp e);
        select rest
    | i :: rest -> instr st i; select rest
    | [] -> () in
  select f.body;
  let body = List.rev st.code in
  let frame = round_up st.frame 16 in
  (* the standard frame, described to the unwinder: after the push the CFA
     is rsp+16 and the caller's rbp is saved at CFA-16; then rbp holds it *)
  let dwarf_reg = function RBX -> 3 | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15 | RSI -> 4 | RDI -> 5 | R8 -> 8 | R9 -> 9 | _ -> assert false in
  let prologue =
    (if st.debug then [ Loc (file_index st f.loc.Loc.file, f.loc.Loc.line) ] else [])
    @ [ Cfi "startproc"; Push (Reg RBP); Cfi "def_cfa_offset 16"; Cfi "offset 6, -16";
        Mov (Q, Reg RSP, Reg RBP); Cfi "def_cfa_register 6" ]
    @ (if frame > 0 then [ Alu ("sub", Q, Imm (Int64.of_int frame), Reg RSP) ] else [])
    (* callee-saved registers the allocator uses are preserved in the frame *)
    @ List.concat_map (fun (p, off) -> [ Mov (Q, Reg p, Mem (RBP, off)); Cfi (Printf.sprintf "offset %d, %d" (dwarf_reg p) (off - 16)) ]) st.saved in
  let epilogue =
    [ Label (".L" ^ f.name ^ ".ret") ]
    @ List.map (fun (p, off) -> Mov (Q, Mem (RBP, off), Reg p)) st.saved
    @ [ Mov (Q, Reg RBP, Reg RSP); Pop (Reg RBP); Cfi "def_cfa 7, 8"; Ret; Cfi "endproc" ] in
  let debug =
    if not st.debug then None
    else begin
      (* parameters live in frame slots at rbp+off; the frame base for
         DWARF is the CFA, which is rbp+16 *)
      let where = function
        | Ir.P_scalar (_, r) -> (match location st r with `Reg p -> In_register (dwarf_reg p) | `Mem off -> At_cfa_offset (off - 16))
        | Ir.P_aggregate (k, _, _) -> At_cfa_offset (st.slots.(k) - 16) in
      let dparams = List.map2 (fun p (pname, ty) -> { pname; ptype = dwarf_type ty; ploc = where p }) f.params f.params_dbg in
      Some { dfile = file_index st f.loc.Loc.file; dline = f.loc.Loc.line; dparams; dret = dwarf_type f.ret_dbg }
    end in
  Peephole.func { name = f.name; global = f.global; weak = f.flink.weak; hidden = f.flink.hidden; body = prologue @ body @ epilogue; debug }

let data_of_global (g : Ir.global) : data option =
  if not g.gdefined then
    (* an undefined reference declared weak or hidden: emit just the
       binding, so the assembler records it (e.g. musl's weak _DYNAMIC) *)
    (if g.glink.weak || g.glink.hidden then
       Some { dname = g.gname; dglobal = false; dweak = g.glink.weak; dhidden = g.glink.hidden;
              dalias = None; dfunc = false; ddecl = true; dtls = false; dalign = 1; section = Data; size = 0; items = [] }
     else None)
  else if g.glink.alias <> None then
    (* an alias defines no storage; it is a .set to its target *)
    Some { dname = g.gname; dglobal = g.gglobal; dweak = g.glink.weak; dhidden = g.glink.hidden; dalias = g.glink.alias;
           dfunc = g.gfunc; ddecl = false; dtls = g.gtls; dalign = 1; section = Data; size = 0; items = [] }
  else
    let items = match g.ginit with
      | None -> [ Zeros (max g.gsize 1) ]
      | Some ds -> List.map (function
          | Ir.Bytes s -> Bytes s | Ir.Zeros n -> Zeros n | Ir.Addr (s, o) -> Quad_sym (s, o)) ds in
    let zero = g.ginit = None || List.for_all (function Zeros _ -> true | _ -> false) items in
    let section = match g.gtls, zero with
      | true, true -> Tbss | true, false -> Tdata | false, true -> Bss | false, false -> Data in
    Some { dname = g.gname; dglobal = g.gglobal; dweak = g.glink.weak; dhidden = g.glink.hidden; dalias = None; dfunc = false; ddecl = false; dtls = g.gtls; dalign = g.galign; section; size = max g.gsize 1; items }

let program ~pic ~debug (p : Ir.program) : program =
  let locals = Hashtbl.create 64 and tls = Hashtbl.create 16 in
  List.iter (fun (g : Ir.global) ->
      if g.gdefined && not g.gglobal then Hashtbl.replace locals g.gname ();
      if g.gtls then Hashtbl.replace tls g.gname ()) p.globals;
  List.iter (fun (f : Ir.func) -> if not f.global then Hashtbl.replace locals f.name ()) p.funcs;
  let st = { pic; debug; files = Hashtbl.create 8; next_file = 1; locals; tls; code = []; tables = [];
             alloc = { Regalloc.where = Hashtbl.create 1; spill_slots = 0; used = [] }; saved = []; regs = Hashtbl.create 64; slots = [||]; frame = 0; float_consts = []; f80_consts = []; f80 = Hashtbl.create 8; scratch = 0;
             const_count = 0; label_count = 0; fname = ""; hidden_ptr = 0; save_area = 0; va_gp = 0; va_fp = 0; va_stack = 0 } in
  let funcs = List.map (func st) p.funcs in
  let data = List.filter_map data_of_global p.globals in
  let consts = List.rev_map (fun (bits, name) ->
      let is32 = Int64.logand bits 0x1_0000_0000L <> 0L && Int64.shift_right_logical bits 33 = 0L in
      if is32 then { dname = name; dglobal = false; dweak = false; dhidden = false; dalias = None; dfunc = false; ddecl = false; dtls = false; dalign = 4; section = Rodata; size = 4; items = [ Long (Int64.to_int32 bits) ] }
      else { dname = name; dglobal = false; dweak = false; dhidden = false; dalias = None; dfunc = false; ddecl = false; dtls = false; dalign = 8; section = Rodata; size = 8; items = [ Quad bits ] }) st.float_consts in
  let long_consts = List.rev_map (fun (bits, name) ->
      let m, se = f80_of_float (Int64.float_of_bits bits) in
      { dname = name; dglobal = false; dweak = false; dhidden = false; dalias = None; dfunc = false; ddecl = false; dtls = false; dalign = 16; section = Rodata; size = 16; items = [ Quad m; Word se; Zeros 6 ] }) st.f80_consts in
  let files = List.sort compare (Hashtbl.fold (fun name n acc -> (n, name) :: acc) st.files []) in
  { funcs; data = data @ consts @ long_consts @ List.rev st.tables; source = (if debug then Some p.source else None); files; asm_blocks = p.asm_blocks; init_array = p.init_array; fini_array = p.fini_array }
