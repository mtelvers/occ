(* A readable rendering of [Ir] for --dump=ir. *)

open Ir

let ty = function I8 -> "i8" | I16 -> "i16" | I32 -> "i32" | I64 -> "i64" | F32 -> "f32" | F64 -> "f64" | F80 -> "f80"

let operand = function
  | Reg r -> Printf.sprintf "%%%d" r
  | Imm i -> Int64.to_string i
  | Fimm f -> Printf.sprintf "%h" f
  | Sym s -> "@" ^ s
  | Slot k -> Printf.sprintf "slot%d" k

let binop = function
  | Add -> "add" | Sub -> "sub" | Mul -> "mul" | Sdiv -> "sdiv" | Udiv -> "udiv" | Srem -> "srem" | Urem -> "urem"
  | And -> "and" | Or -> "or" | Xor -> "xor" | Shl -> "shl" | Sshr -> "sshr" | Ushr -> "ushr"
  | Fadd -> "fadd" | Fsub -> "fsub" | Fmul -> "fmul" | Fdiv -> "fdiv"

let cond = function
  | Eq -> "eq" | Ne -> "ne" | Slt -> "slt" | Sle -> "sle" | Sgt -> "sgt" | Sge -> "sge"
  | Ult -> "ult" | Ule -> "ule" | Ugt -> "ugt" | Uge -> "uge"
  | Feq -> "feq" | Fne -> "fne" | Flt -> "flt" | Fle -> "fle" | Fgt -> "fgt" | Fge -> "fge"

let order = function
  | Relaxed -> "relaxed" | Consume -> "consume" | Acquire -> "acquire"
  | Release -> "release" | Acq_rel -> "acq_rel" | Seq_cst -> "seq_cst"

let conv = function
  | Sext (a, b) -> Printf.sprintf "sext %s->%s" (ty a) (ty b) | Zext (a, b) -> Printf.sprintf "zext %s->%s" (ty a) (ty b)
  | Trunc (a, b) -> Printf.sprintf "trunc %s->%s" (ty a) (ty b) | Fconv (a, b) -> Printf.sprintf "fconv %s->%s" (ty a) (ty b)
  | Fext -> "fext" | Ftrunc -> "ftrunc"
  | Stof (a, b) -> Printf.sprintf "stof %s->%s" (ty a) (ty b) | Utof (a, b) -> Printf.sprintf "utof %s->%s" (ty a) (ty b)
  | Ftos (a, b) -> Printf.sprintf "ftos %s->%s" (ty a) (ty b) | Ftou (a, b) -> Printf.sprintf "ftou %s->%s" (ty a) (ty b)

let cls = function Integer -> "int" | Sse -> "sse" | Memory -> "mem"
let agg (a : agg) = Printf.sprintf "agg[%d:%s] %s" a.size (String.concat "," (List.map cls a.classes)) (operand a.addr)

let arg = function
  | Scalar (t, o) -> Printf.sprintf "%s %s" (ty t) (operand o)
  | Aggregate a -> agg a

let instr ppf i =
  let p fmt = Format.fprintf ppf fmt in
  match i with
  | Mov (t, r, o) -> p "  %%%d = mov.%s %s" r (ty t) (operand o)
  | Binop (op, t, r, a, b) -> p "  %%%d = %s.%s %s, %s" r (binop op) (ty t) (operand a) (operand b)
  | Binop_overflow (op, t, s, r, f, a, b) -> p "  %%%d, %%%d = %s.%s.%s.overflow %s, %s" r f (binop op) (ty t) (if s then "s" else "u") (operand a) (operand b)
  | Neg (t, r, o) -> p "  %%%d = neg.%s %s" r (ty t) (operand o)
  | Not (t, r, o) -> p "  %%%d = not.%s %s" r (ty t) (operand o)
  | Cmp (c, t, r, a, b) -> p "  %%%d = cmp.%s.%s %s, %s" r (cond c) (ty t) (operand a) (operand b)
  | Conv (c, r, o) -> p "  %%%d = %s %s" r (conv c) (operand o)
  | Load (t, r, a) -> p "  %%%d = load.%s [%s]" r (ty t) (operand a)
  | Store (t, a, v) -> p "  store.%s [%s], %s" (ty t) (operand a) (operand v)
  | Memcpy (d, s, n) -> p "  memcpy %s, %s, %d" (operand d) (operand s) n
  | Memzero (d, n) -> p "  memzero %s, %d" (operand d) n
  | Call (res, f, args, va) ->
      let res = match res with
        | None -> "" | Some (Ret_scalar (t, r)) -> Printf.sprintf "%%%d:%s = " r (ty t)
        | Some (Ret_aggregate a) -> agg a ^ " = " in
      p "  %scall%s %s(%s)" res (if va then ".variadic" else "") (operand f) (String.concat ", " (List.map arg args))
  | Label l -> p "%s:" l
  | Jump l -> p "  jump %s" l
  | Branch (c, a, b) -> p "  branch %s ? %s : %s" (operand c) a b
  | Switch (t, v, cases, d) ->
      p "  switch.%s %s [%s] default %s" (ty t) (operand v)
        (String.concat "; " (List.map (fun (v, l) -> Printf.sprintf "%Ld: %s" v l) cases)) d
  | Ret None -> p "  ret"
  | Ret (Some (Rv_scalar (t, o))) -> p "  ret.%s %s" (ty t) (operand o)
  | Ret (Some (Rv_aggregate a)) -> p "  ret.%s" (agg a)
  | Atomic_load (t, r, a, o) -> p "  %%%d = atomic_load.%s.%s [%s]" r (ty t) (order o) (operand a)
  | Atomic_store (t, a, v, o) -> p "  atomic_store.%s.%s [%s], %s" (ty t) (order o) (operand a) (operand v)
  | Atomic_rmw (op, t, r, a, v, o) -> p "  %%%d = atomic_%s.%s.%s [%s], %s" r (binop op) (ty t) (order o) (operand a) (operand v)
  | Atomic_xchg (t, r, a, v, o) -> p "  %%%d = atomic_xchg.%s.%s [%s], %s" r (ty t) (order o) (operand a) (operand v)
  | Atomic_cmpxchg (t, r, a, e, d, o) -> p "  %%%d = atomic_cmpxchg.%s.%s [%s], [%s], %s" r (ty t) (order o) (operand a) (operand e) (operand d)
  | Fence o -> p "  fence.%s" (order o)
  | Va_start a -> p "  va_start %s" (operand a)
  | Alloca (r, n) -> p "  %s = alloca %s" (operand (Reg r)) (operand n)
  | Va_arg_aggregate (dst, size, _, ap) -> p "  va_arg_aggregate %s, %d, %s" (operand dst) size (operand ap)
  | Va_arg (t, r, a) -> p "  %%%d = va_arg.%s %s" r (ty t) (operand a)
  | Trap -> p "  trap"
  | Inline_asm a ->
      p "  asm %S" a.template;
      Array.iter (function
          | Asm_in (c, _, o) -> p " in %s(%s)" c (operand o)
          | Asm_out (c, _, r) -> p " out %s(%s)" c (operand (Reg r))
          | Asm_inout (c, _, r, o) -> p " inout %s(%s<-%s)" c (operand (Reg r)) (operand o)
          | Asm_mem (c, o) -> p " mem %s(%s)" c (operand o)
          | Asm_imm v -> p " imm %Ld" v) a.operands;
      if a.clobbers <> [] then p " clobbers %s" (String.concat "," a.clobbers)
  | Return_address r -> p "  %%%d = return_address" r
  | Intrinsic (f, t, r, o) -> p "  %%%d = %s.%s %s" r (match f with Fabs -> "fabs" | Fsqrt -> "fsqrt") (ty t) (operand o)
  | Line l -> p "  # line %d" l.Loc.line

let func ppf (f : func) =
  Format.fprintf ppf "%sfunction %s(%s)%s%s@."
    (if f.global then "global " else "") f.name
    (String.concat ", " (List.map (function P_scalar (t, r) -> Printf.sprintf "%s %%%d" (ty t) r | P_aggregate (k, s, c) -> Printf.sprintf "agg[%d:%s] slot%d" s (String.concat "," (List.map cls c)) k) f.params))
    (if f.variadic then ", ..." else "")
    (match f.returns_aggregate with Some (n, c) -> Printf.sprintf " returns agg[%d:%s]" n (String.concat "," (List.map cls c)) | None -> "");
  Array.iteri (fun i (s : slot) -> Format.fprintf ppf "  slot%d: %d bytes align %d@." i s.size s.align) f.slots;
  List.iter (fun i -> instr ppf i; Format.fprintf ppf "@.") f.body

let data = function
  | Bytes s -> Printf.sprintf "bytes %S" s
  | Zeros n -> Printf.sprintf "zeros %d" n
  | Addr (s, o) -> Printf.sprintf "addr @%s+%Ld" s o

let program ppf (p : program) =
  List.iter (fun (g : global) ->
      Format.fprintf ppf "%s%sdata %s: %d bytes align %d%s@."
        (if g.gglobal then "global " else "") (if g.gtls then "tls " else "") g.gname g.gsize g.galign
        (match g.ginit with
         | None -> if g.gdefined then " (zero)" else " (extern)"
         | Some d -> " = " ^ String.concat ", " (List.map data d))) p.globals;
  List.iter (func ppf) p.funcs
