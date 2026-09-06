module T = Typed
module C = Ctype

(* ---- Per-function state ---------------------------------------------------- *)

type fn = {
  env : Env.t;
  mutable code : Ir.instr list; (* reversed *)
  mutable next_reg : int;
  mutable slots : Ir.slot list; (* reversed *)
  mutable next_slot : int;
  locals : (int, int) Hashtbl.t; (* symbol id -> slot, for locals in memory *)
  vars : (int, int) Hashtbl.t; (* symbol id -> IR register, for scalar locals never addressed *)
  addressed : (int, unit) Hashtbl.t; (* locals whose address is taken *)
  mutable breaks : string list; (* innermost first *)
  mutable continues : string list;
  mutable cases : ((int64 * string) list * string option) list; (* per enclosing switch, innermost first *)
  fname : string;
  mutable next_label : int;
  mutable last_line : Loc.t;
}

(* Translation-unit state: interned string literals and static data. *)
type tu = {
  tenv : Env.t;
  mutable strings : (string * Ir.global) list; (* content key -> global *)
  mutable extra_globals : Ir.global list;
  mutable string_count : int;
}

let emit fn i = fn.code <- i :: fn.code

(* Record a source position for the debug line table when it changes. *)
let mark fn (loc : Loc.t) =
  if loc.line > 0 && (loc.line <> fn.last_line.line || loc.file <> fn.last_line.file) then begin
    fn.last_line <- loc; emit fn (Ir.Line loc)
  end
let fresh fn = fn.next_reg <- fn.next_reg + 1; fn.next_reg
let label fn hint = fn.next_label <- fn.next_label + 1; Printf.sprintf ".L%s.%s%d" fn.fname hint fn.next_label
let new_slot fn size align =
  fn.slots <- { Ir.size = max size 1; align } :: fn.slots;
  let k = fn.next_slot in fn.next_slot <- k + 1; k

(* The assembly-level name of a symbol.  A static local has no linkage
   and may share its name with others, so its id is appended. *)
let asm_name (s : T.symbol) =
  match s.asm_name, s.storage with
  | Some l, _ -> l
  | None, T.Static { linkage = T.No_linkage; _ } -> Printf.sprintf "%s.%d" s.name s.id
  | None, _ -> s.name

(* ---- Types ---------------------------------------------------------------- *)

let size_of env (t : C.t) = Env.size_of env Loc.none t
let align_of env (t : C.t) = Env.align_of env Loc.none t

let underlying env (t : C.t) : C.t =
  match t.u with
  | C.Enum tag -> (match (Env.tag_info env tag).underlying with Some u -> u | None -> C.uint)
  | _ -> t

(* The machine type of a scalar. *)
let ir_type env (t : C.t) : Ir.ty =
  match (underlying env t).u with
  | C.Integer k -> (match Target.size_of_ikind k with 1 -> Ir.I8 | 2 -> Ir.I16 | 4 -> Ir.I32 | _ -> Ir.I64)
  | C.Floating C.Float -> Ir.F32
  | C.Floating (C.Double | C.LongDouble) -> Ir.F64 (* long double is treated as double *)
  | C.Pointer _ | C.Array _ | C.Func _ -> Ir.I64
  | _ -> failwith ("Lower.ir_type: not a scalar: " ^ C.to_string t)

let is_signed env (t : C.t) =
  match (underlying env t).u with C.Integer k -> C.is_signed k | _ -> false

let is_float (t : C.t) = C.is_floating t
let is_aggregate (t : C.t) = C.is_record t || C.is_array t

(* ---- Constants and static data ------------------------------------------------- *)

let le_bytes n v =
  String.init n (fun i -> Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical v (8 * i)) 0xFFL)))

(* Re-encode a UTF-8 string as an array of [elem]-sized code units. *)
let encode_string env (elem : C.t) (s : string) : string =
  match size_of env elem with
  | 1 -> s
  | n ->
      let b = Buffer.create (String.length s * n) in
      let i = ref 0 in
      while !i < String.length s do
        let c = Char.code s.[!i] in
        let len, init =
          if c < 0x80 then 1, c else if c land 0xE0 = 0xC0 then 2, c land 0x1F
          else if c land 0xF0 = 0xE0 then 3, c land 0x0F else 4, c land 0x07 in
        let cp = ref init in
        for k = 1 to len - 1 do cp := (!cp lsl 6) lor (Char.code s.[!i + k] land 0x3F) done;
        i := !i + len;
        Buffer.add_string b (le_bytes n (Int64.of_int !cp))
      done;
      Buffer.contents b

let intern_string tu (s : T.expr) : string =
  let elem = match s.ty.u with C.Array (e, _) -> e | _ -> assert false in
  let bytes = match s.e with T.String b -> b | _ -> assert false in
  let n = match s.ty.u with C.Array (_, Some n) -> n | _ -> assert false in
  let image = encode_string tu.tenv elem bytes ^ String.make (size_of tu.tenv elem) '\000' in
  let key = string_of_int (size_of tu.tenv elem) ^ ":" ^ image in
  match List.assoc_opt key tu.strings with
  | Some g -> g.Ir.gname
  | None ->
      tu.string_count <- tu.string_count + 1;
      let g = { Ir.gname = Printf.sprintf ".LC%d" tu.string_count; gglobal = false; galign = align_of tu.tenv elem;
                gtls = false; gsize = n * size_of tu.tenv elem; ginit = Some [ Ir.Bytes image ]; gdefined = true } in
      tu.strings <- (key, g) :: tu.strings;
      g.Ir.gname

(* A static initializer becomes a byte image with relocations.  Items are
   applied in order so a later one overrides an earlier overlapping one. *)
type image = { bytes : Bytes.t; mutable relocs : (int * string * int64) list }

let rec fill_image tu (img : image) (base : int) (ty : C.t) (init : T.init) =
  let env = tu.tenv in
  match init with
  | T.Init_string s ->
      let elem = match ty.u with C.Array (e, _) -> e | _ -> assert false in
      let data = encode_string env elem s in
      let n = min (String.length data) (size_of env ty) in
      Bytes.blit_string data 0 img.bytes base n
  | T.Init_agg items ->
      List.iter (fun (it : T.init_item) ->
          match it.bits with
          | None -> fill_image tu img (base + it.off) it.ity it.init
          | Some (bit, width) ->
              let v = match it.init with
                | T.Init_scalar e -> (match Const_eval.eval env e with Some (Const_eval.Int v) -> v | _ -> Diag.error e.loc "initializer element is not constant")
                | _ -> assert false in
              let unit_size = size_of env it.ity in
              let cur = ref 0L in
              for i = unit_size - 1 downto 0 do
                cur := Int64.logor (Int64.shift_left !cur 8) (Int64.of_int (Char.code (Bytes.get img.bytes (base + it.off + i))))
              done;
              let mask = Int64.shift_left (Int64.sub (Int64.shift_left 1L width) 1L) bit in
              let v = Int64.logand (Int64.shift_left v bit) mask in
              let merged = Int64.logor (Int64.logand !cur (Int64.lognot mask)) v in
              Bytes.blit_string (le_bytes unit_size merged) 0 img.bytes (base + it.off) unit_size) items
  | T.Init_scalar e ->
      let size = size_of env ty in
      (match Const_eval.eval env e with
       | Some (Const_eval.Int v) -> Bytes.blit_string (le_bytes size v) 0 img.bytes base size
       | Some (Const_eval.Float f) ->
           let v = if size = 4 then Int64.of_int32 (Int32.bits_of_float f) else Int64.bits_of_float f in
           Bytes.blit_string (le_bytes (min size 8) v) 0 img.bytes base (min size 8)
       | Some (Const_eval.Addr (s, off)) -> img.relocs <- (base, asm_name s, off) :: img.relocs
       | Some (Const_eval.Str (bytes, sty, off)) ->
           let name = intern_string tu { e with e = T.String bytes; ty = sty } in
           img.relocs <- (base, name, off) :: img.relocs
       | None -> Diag.error e.loc "initializer element is not constant")

let data_of_image (img : image) : Ir.data list =
  let n = Bytes.length img.bytes in
  let relocs = List.sort (fun (a, _, _) (b, _, _) -> compare a b) img.relocs in
  let rec go pos relocs acc =
    if pos >= n then List.rev acc
    else
      match relocs with
      | (off, sym, add) :: rest when off = pos -> go (pos + 8) rest (Ir.Addr (sym, add) :: acc)
      | (off, _, _) :: _ -> go off relocs (Ir.Bytes (Bytes.sub_string img.bytes pos (off - pos)) :: acc)
      | [] -> go n [] (Ir.Bytes (Bytes.sub_string img.bytes pos (n - pos)) :: acc) in
  (* collapse all-zero chunks so .bss-like data stays small in the assembly *)
  List.map (function
      | Ir.Bytes s when String.for_all (fun c -> c = '\000') s -> Ir.Zeros (String.length s)
      | d -> d) (go 0 relocs [])

let global_of tu (g : T.global) : Ir.global =
  let env = tu.tenv in
  let sym = g.gsym in
  let tls = match sym.storage with T.Static { tls; _ } -> tls | _ -> false in
  let gglobal = match sym.storage with T.Static { linkage = T.External; _ } -> true | _ -> false in
  let size = match Env.size_align env sym.ty with Some (s, _) -> s | None -> 0 in
  let align = max (match Env.size_align env sym.ty with Some (_, a) -> a | None -> 1) (Option.value sym.align ~default:1) in
  let ginit = Option.map (fun init ->
      let img = { bytes = Bytes.make size '\000'; relocs = [] } in
      fill_image tu img 0 sym.ty init;
      data_of_image img) g.ginit in
  { Ir.gname = asm_name sym; gglobal; galign = align; gtls = tls; gsize = size; ginit; gdefined = g.defined }

(* ---- Expressions ----------------------------------------------------------- *)

(* Where an lvalue lives: in an IR register, for a scalar local whose
   address is never taken (the register allocator may then keep it in a
   machine register), or in memory at an address, possibly a bit-field. *)
type place = In_reg of Ir.reg | In_mem of Ir.operand * (int * int) option

let registerable fn (s : T.symbol) =
  Sys.getenv_opt "OCC_NO_REGVARS" = None (* debugging aid: keep every local in memory *)
  && s.storage = T.Local && not (Hashtbl.mem fn.addressed s.id) && not s.ty.q.volatile
  && (match s.ty.u with C.Integer _ | C.Enum _ | C.Pointer _ | C.Floating _ -> true | _ -> false)

let var_reg fn (s : T.symbol) =
  match Hashtbl.find_opt fn.vars s.id with
  | Some r -> r
  | None -> let r = fresh fn in Hashtbl.replace fn.vars s.id r; r

let rec address fn tu (e : T.expr) : Ir.operand =
  match e.e with
  | T.Var s ->
      (match s.storage with
       | T.Local ->
           if registerable fn s then Diag.error e.loc "internal: address of a register variable";
           Ir.Slot (local_slot fn s)
       | T.Static _ -> Ir.Sym (asm_name s))
  | T.String _ -> Ir.Sym (intern_string tu e)
  | T.Deref p -> value fn tu p
  | T.Member (base, f) ->
      let b = if base.lvalue then address fn tu base else materialise fn tu base in
      if f.offset = 0 then b
      else let r = fresh fn in emit fn (Ir.Binop (Ir.Add, Ir.I64, r, b, Ir.Imm (Int64.of_int f.offset))); Ir.Reg r
  | T.Compound_literal (sym, init) ->
      (match sym.storage with
       | T.Static _ -> Ir.Sym (asm_name sym)
       | T.Local ->
           let slot = local_slot fn sym in
           initialise fn tu (Ir.Slot slot) sym.ty init;
           Ir.Slot slot)
  | T.Convert x when C.is_array x.ty || C.is_function x.ty -> address fn tu x
  | _ when not e.lvalue && is_aggregate e.ty -> materialise fn tu e
  | _ -> Diag.error e.loc "internal: address of a non-lvalue"

(* An rvalue struct (a call result or conditional) gets a temporary. *)
and materialise fn tu (e : T.expr) : Ir.operand =
  let size = size_of fn.env e.ty and align = align_of fn.env e.ty in
  let slot = new_slot fn size align in
  copy_into fn tu (Ir.Slot slot) e;
  Ir.Slot slot

and local_slot fn (s : T.symbol) =
  match Hashtbl.find_opt fn.locals s.id with
  | Some k -> k
  | None ->
      let size, align = match Env.size_align fn.env s.ty with Some sa -> sa | None -> 8, 8 in
      let k = new_slot fn size (max align (Option.value s.align ~default:1)) in
      Hashtbl.replace fn.locals s.id k; k

(* Evaluate an aggregate-typed expression into memory at [dst]. *)
and copy_into fn tu (dst : Ir.operand) (e : T.expr) =
  let size = size_of fn.env e.ty in
  match e.e with
  | T.Call (f, args) -> call fn tu ~dst:(Some dst) e.ty f args |> ignore
  | T.Cond (c, a, b) ->
      let l1 = label fn "cond" and l2 = label fn "condelse" and l3 = label fn "condend" in
      emit fn (Ir.Branch (truth fn tu c, l1, l2));
      emit fn (Ir.Label l1); copy_into fn tu dst a; emit fn (Ir.Jump l3);
      emit fn (Ir.Label l2); copy_into fn tu dst b;
      emit fn (Ir.Label l3)
  | T.Assign (l, r) ->
      let la = address fn tu l in
      copy_into fn tu la r;
      emit fn (Ir.Memcpy (dst, la, size))
  | T.Binop (Syntax.Comma, a, b) -> ignore (side_effect fn tu a); copy_into fn tu dst b
  | T.Compound_literal (_, init) -> initialise fn tu dst e.ty init
  | T.Va_arg (ap, ty) ->
      Diag.error e.loc "va_arg of aggregate type '%a' is not supported" C.pp ty |> ignore; ignore ap
  | _ ->
      let src = address fn tu e in
      emit fn (Ir.Memcpy (dst, src, size))

(* Load the value of an lvalue, including bit-fields. *)
and load fn (ty : C.t) (addr : Ir.operand) (bits : (int * int) option) : Ir.operand =
  let it = ir_type fn.env ty in
  let r = fresh fn in
  match bits with
  | None -> emit fn (Ir.Load (it, r, addr)); Ir.Reg r
  | Some (bit, width) ->
      (* shift left to drop higher bits, then right (arithmetic when signed) to drop lower *)
      let unit_bits = 8 * size_of fn.env ty in
      emit fn (Ir.Load (it, r, addr));
      let r2 = fresh fn and r3 = fresh fn in
      emit fn (Ir.Binop (Ir.Shl, it, r2, Ir.Reg r, Ir.Imm (Int64.of_int (unit_bits - bit - width))));
      emit fn (Ir.Binop ((if is_signed fn.env ty then Ir.Sshr else Ir.Ushr), it, r3, Ir.Reg r2, Ir.Imm (Int64.of_int (unit_bits - width))));
      Ir.Reg r3

and store fn (ty : C.t) (addr : Ir.operand) (bits : (int * int) option) (v : Ir.operand) =
  let it = ir_type fn.env ty in
  match bits with
  | None -> emit fn (Ir.Store (it, addr, v))
  | Some (bit, width) ->
      let mask = Int64.shift_left (Int64.sub (Int64.shift_left 1L width) 1L) bit in
      let old = fresh fn and cleared = fresh fn and shifted = fresh fn and masked = fresh fn and merged = fresh fn in
      emit fn (Ir.Load (it, old, addr));
      emit fn (Ir.Binop (Ir.And, it, cleared, Ir.Reg old, Ir.Imm (Int64.lognot mask)));
      emit fn (Ir.Binop (Ir.Shl, it, shifted, v, Ir.Imm (Int64.of_int bit)));
      emit fn (Ir.Binop (Ir.And, it, masked, Ir.Reg shifted, Ir.Imm mask));
      emit fn (Ir.Binop (Ir.Or, it, merged, Ir.Reg cleared, Ir.Reg masked));
      emit fn (Ir.Store (it, addr, Ir.Reg merged))

and bits_of (e : T.expr) = match e.e with T.Member (_, f) -> f.bits | _ -> None

and place fn tu (e : T.expr) : place =
  match e.e with
  | T.Var s when registerable fn s -> In_reg (var_reg fn s)
  | _ -> In_mem (address fn tu e, bits_of e)

and read fn (ty : C.t) = function
  | In_reg r -> Ir.Reg r
  | In_mem (a, bits) -> load fn ty a bits

and write fn (ty : C.t) (pl : place) (v : Ir.operand) =
  match pl with
  | In_reg r -> emit fn (Ir.Mov (ir_type fn.env ty, r, v))
  | In_mem (a, bits) -> store fn ty a bits v

(* Conversions between scalar machine types (6.3.1). *)
and convert fn (from : C.t) (to_ : C.t) (v : Ir.operand) : Ir.operand =
  let env = fn.env in
  let fi = ir_type env from and ti = ir_type env to_ in
  let to_u = underlying env to_ in
  let size = function Ir.I8 -> 1 | Ir.I16 -> 2 | Ir.I32 -> 4 | Ir.I64 -> 8 | Ir.F32 -> 4 | Ir.F64 -> 8 in
  let r = fresh fn in
  match to_u.u, fi, ti with
  | C.Integer C.Bool, _, _ ->
      (* 6.3.1.2: nonzero becomes 1 *)
      let zero = if is_float from then Ir.Fimm 0.0 else Ir.Imm 0L in
      emit fn (Ir.Cmp ((if is_float from then Ir.Fne else Ir.Ne), fi, r, v, zero));
      Ir.Reg r
  | _, (Ir.F32 | Ir.F64), (Ir.F32 | Ir.F64) ->
      if fi = ti then v
      else (emit fn (Ir.Conv ((if ti = Ir.F64 then Ir.Fext else Ir.Ftrunc), r, v)); Ir.Reg r)
  | _, (Ir.F32 | Ir.F64), _ ->
      emit fn (Ir.Conv ((if is_signed env to_ then Ir.Ftos (fi, ti) else Ir.Ftou (fi, ti)), r, v)); Ir.Reg r
  | _, _, (Ir.F32 | Ir.F64) ->
      emit fn (Ir.Conv ((if is_signed env from then Ir.Stof (fi, ti) else Ir.Utof (fi, ti)), r, v)); Ir.Reg r
  | _ ->
      if size fi = size ti then v
      else if size fi > size ti then (emit fn (Ir.Conv (Ir.Trunc (fi, ti), r, v)); Ir.Reg r)
      else (emit fn (Ir.Conv ((if is_signed env from then Ir.Sext (fi, ti) else Ir.Zext (fi, ti)), r, v)); Ir.Reg r)

and binop_of env (op : Syntax.binop) (ty : C.t) : Ir.binop =
  let f = is_float ty and s = is_signed env ty in
  match op with
  | Syntax.Add -> if f then Ir.Fadd else Ir.Add
  | Syntax.Sub -> if f then Ir.Fsub else Ir.Sub
  | Syntax.Mul -> if f then Ir.Fmul else Ir.Mul
  | Syntax.Div -> if f then Ir.Fdiv else if s then Ir.Sdiv else Ir.Udiv
  | Syntax.Mod -> if s then Ir.Srem else Ir.Urem
  | Syntax.Band -> Ir.And | Syntax.Bor -> Ir.Or | Syntax.Bxor -> Ir.Xor
  | Syntax.Shl -> Ir.Shl | Syntax.Shr -> if s then Ir.Sshr else Ir.Ushr
  | _ -> assert false

and cond_of env (op : Syntax.binop) (ty : C.t) : Ir.cond =
  let f = is_float ty and s = is_signed env ty in
  match op with
  | Syntax.Eq -> if f then Ir.Feq else Ir.Eq
  | Syntax.Ne -> if f then Ir.Fne else Ir.Ne
  | Syntax.Lt -> if f then Ir.Flt else if s then Ir.Slt else Ir.Ult
  | Syntax.Le -> if f then Ir.Fle else if s then Ir.Sle else Ir.Ule
  | Syntax.Gt -> if f then Ir.Fgt else if s then Ir.Sgt else Ir.Ugt
  | Syntax.Ge -> if f then Ir.Fge else if s then Ir.Sge else Ir.Uge
  | _ -> assert false

(* The arithmetic of [a op b] on values already converted to [ty], with
   pointer operands scaled by the pointee size (6.5.6p8-9). *)
and arith fn (op : Syntax.binop) (ty : C.t) (a_ty : C.t) (b_ty : C.t) (a : Ir.operand) (b : Ir.operand) : Ir.operand =
  let env = fn.env in
  let r = fresh fn in
  match op, a_ty.u, b_ty.u with
  | (Syntax.Add | Syntax.Sub), C.Pointer p, (C.Integer _ | C.Enum _) ->
      let size = size_of env p in
      let scaled =
        match b with
        | _ when size = 1 -> b
        | Ir.Imm i -> Ir.Imm (Int64.mul i (Int64.of_int size)) (* a constant index *)
        | _ -> let s = fresh fn in emit fn (Ir.Binop (Ir.Mul, Ir.I64, s, b, Ir.Imm (Int64.of_int size))); Ir.Reg s in
      emit fn (Ir.Binop ((if op = Syntax.Add then Ir.Add else Ir.Sub), Ir.I64, r, a, scaled));
      Ir.Reg r
  | Syntax.Sub, C.Pointer p, C.Pointer _ ->
      let diff = fresh fn in
      emit fn (Ir.Binop (Ir.Sub, Ir.I64, diff, a, b));
      emit fn (Ir.Binop (Ir.Sdiv, Ir.I64, r, Ir.Reg diff, Ir.Imm (Int64.of_int (size_of env p))));
      Ir.Reg r
  | (Syntax.Eq | Syntax.Ne | Syntax.Lt | Syntax.Le | Syntax.Gt | Syntax.Ge), _, _ ->
      emit fn (Ir.Cmp (cond_of env op a_ty, ir_type env a_ty, r, a, b)); Ir.Reg r
  | _ -> emit fn (Ir.Binop (binop_of env op ty, ir_type env ty, r, a, b)); Ir.Reg r

(* Evaluate for side effects only. *)
and side_effect fn tu (e : T.expr) =
  if is_aggregate e.ty && not e.lvalue then ignore (materialise fn tu e)
  else if e.ty.u = C.Void then ignore (value_void fn tu e)
  else ignore (value fn tu e)

and value_void fn tu (e : T.expr) : Ir.operand =
  match e.e with
  | T.Call (f, args) -> call fn tu ~dst:None e.ty f args
  | T.Convert x -> side_effect fn tu x; Ir.Imm 0L
  | T.Cond (c, a, b) ->
      let l1 = label fn "vc" and l2 = label fn "vce" and l3 = label fn "vcend" in
      emit fn (Ir.Branch (truth fn tu c, l1, l2));
      emit fn (Ir.Label l1); side_effect fn tu a; emit fn (Ir.Jump l3);
      emit fn (Ir.Label l2); side_effect fn tu b; emit fn (Ir.Label l3); Ir.Imm 0L
  | T.Binop (Syntax.Comma, a, b) -> side_effect fn tu a; value_void fn tu b
  | T.Builtin _ | T.Atomic_op _ -> value fn tu e
  | _ -> Diag.error e.loc "internal: void expression"

(* The value of a scalar expression, as an operand. *)
and value fn tu (e : T.expr) : Ir.operand =
  let env = fn.env in
  match e.e with
  | T.Int v -> if is_float e.ty then Ir.Fimm (Int64.to_float v) else Ir.Imm v
  | T.Float f -> Ir.Fimm f
  | T.Var s when C.is_function s.ty -> Ir.Sym (asm_name s)
  | T.Deref p when C.is_function e.ty -> value fn tu p
  | T.Var _ | T.Deref _ | T.Member _ | T.String _ ->
      (* an object: its address if it is an aggregate, else its contents
         (lvalue conversion, 6.3.2.1p2) *)
      if is_aggregate e.ty then address fn tu e
      else read fn e.ty (place fn tu e)
  | T.Convert x ->
      if C.is_array x.ty || C.is_function x.ty then address fn tu x
      else if e.ty.u = C.Void then (side_effect fn tu x; Ir.Imm 0L)
      else convert fn x.ty e.ty (value fn tu x)
  | T.Unop (Syntax.Neg, x) -> let r = fresh fn in emit fn (Ir.Neg (ir_type env e.ty, r, value fn tu x)); Ir.Reg r
  | T.Unop (Syntax.Not, x) -> let r = fresh fn in emit fn (Ir.Not (ir_type env e.ty, r, value fn tu x)); Ir.Reg r
  | T.Unop (Syntax.Lnot, x) ->
      let r = fresh fn in
      let zero = if is_float x.ty then Ir.Fimm 0.0 else Ir.Imm 0L in
      emit fn (Ir.Cmp ((if is_float x.ty then Ir.Feq else Ir.Eq), ir_type env x.ty, r, value fn tu x, zero)); Ir.Reg r
  | T.Unop ((Syntax.Preinc | Syntax.Predec | Syntax.Postinc | Syntax.Postdec) as op, x) ->
      (* 6.5.2.4, 6.5.3.1: like x += 1, yielding the old or the new value *)
      let pl = place fn tu x in
      let old = read fn x.ty pl in
      let one = if is_float x.ty then Ir.Fimm 1.0 else Ir.Imm 1L in
      let sub = (op = Syntax.Predec || op = Syntax.Postdec) in
      let int_one_ty = C.long in
      let nv =
        if C.is_pointer x.ty then arith fn (if sub then Syntax.Sub else Syntax.Add) x.ty x.ty int_one_ty old one
        else arith fn (if sub then Syntax.Sub else Syntax.Add) x.ty x.ty x.ty old one in
      let nv = if (match (underlying env x.ty).u with C.Integer C.Bool -> true | _ -> false) then convert fn C.int x.ty nv else nv in
      (* a register variable's old value must be copied before it is overwritten *)
      let old = match pl, old with
        | In_reg _, Ir.Reg _ when not (op = Syntax.Preinc || op = Syntax.Predec) ->
            let t = fresh fn in emit fn (Ir.Mov (ir_type env x.ty, t, old)); Ir.Reg t
        | _ -> old in
      write fn x.ty pl nv;
      if op = Syntax.Preinc || op = Syntax.Predec then nv else old
  | T.Unop ((Syntax.Addr | Syntax.Deref), _) -> assert false (* Elab produces Addr/Deref nodes *)
  | T.Binop (Syntax.Land, a, b) | T.Binop (Syntax.Lor, a, b) ->
      (* 6.5.13-14: short circuit; result 0 or 1 *)
      let is_and = (match e.e with T.Binop (Syntax.Land, _, _) -> true | _ -> false) in
      let r = fresh fn in
      let l_rhs = label fn "sc" and l_short = label fn "scs" and l_end = label fn "scend" in
      let av = truth fn tu a in
      if is_and then emit fn (Ir.Branch (av, l_rhs, l_short)) else emit fn (Ir.Branch (av, l_short, l_rhs));
      emit fn (Ir.Label l_rhs);
      let bv = truth fn tu b in
      emit fn (Ir.Mov (Ir.I32, r, bv)); emit fn (Ir.Jump l_end);
      emit fn (Ir.Label l_short);
      emit fn (Ir.Mov (Ir.I32, r, Ir.Imm (if is_and then 0L else 1L)));
      emit fn (Ir.Label l_end);
      Ir.Reg r
  | T.Binop (Syntax.Comma, a, b) -> side_effect fn tu a; value fn tu b
  | T.Binop (op, a, b) ->
      let av = value fn tu a in
      let bv = value fn tu b in
      arith fn op e.ty a.ty b.ty av bv
  | T.Assign (l, r) ->
      if is_aggregate l.ty then (let la = address fn tu l in copy_into fn tu la r; la)
      else begin
        let rv = value fn tu r in
        let pl = place fn tu l in
        write fn l.ty pl rv;
        (* the value of the assignment is the new value of the object *)
        (match pl with In_reg reg -> Ir.Reg reg | In_mem _ -> rv)
      end
  | T.Compound_assign (op, l, r, comp_ty) ->
      let pl = place fn tu l in
      let old = read fn l.ty pl in
      let rv = value fn tu r in
      let nv =
        if C.is_pointer l.ty then arith fn op l.ty l.ty r.ty old rv
        else begin
          let old_c = convert fn l.ty comp_ty old in
          let res = arith fn op comp_ty comp_ty r.ty old_c rv in
          convert fn comp_ty l.ty res
        end in
      write fn l.ty pl nv;
      nv
  | T.Cond (c, a, b) ->
      let r = fresh fn in
      let l1 = label fn "c" and l2 = label fn "ce" and l3 = label fn "cend" in
      emit fn (Ir.Branch (truth fn tu c, l1, l2));
      let ty = ir_type env e.ty in
      emit fn (Ir.Label l1); let av = value fn tu a in emit fn (Ir.Mov (ty, r, av)); emit fn (Ir.Jump l3);
      emit fn (Ir.Label l2); let bv = value fn tu b in emit fn (Ir.Mov (ty, r, bv));
      emit fn (Ir.Label l3);
      Ir.Reg r
  | T.Call (f, args) -> call fn tu ~dst:None e.ty f args
  | T.Addr x -> address fn tu x
  | T.Compound_literal _ -> address fn tu e
  | T.Va_arg (ap, ty) ->
      let r = fresh fn in
      emit fn (Ir.Va_arg (ir_type env ty, r, value fn tu ap)); Ir.Reg r
  | T.Builtin (name, args) -> builtin fn tu e.loc name args
  | T.Atomic_op (op, orders, args) -> atomic fn tu e.ty op orders args

(* A scalar's truth value as an I32 0/1 when it is not already one. *)
and truth fn tu (e : T.expr) : Ir.operand =
  let v = value fn tu e in
  match e.e with
  | T.Binop ((Syntax.Eq | Syntax.Ne | Syntax.Lt | Syntax.Le | Syntax.Gt | Syntax.Ge | Syntax.Land | Syntax.Lor), _, _)
  | T.Unop (Syntax.Lnot, _) -> v
  | _ ->
      let r = fresh fn in
      let zero = if is_float e.ty then Ir.Fimm 0.0 else Ir.Imm 0L in
      emit fn (Ir.Cmp ((if is_float e.ty then Ir.Fne else Ir.Ne), ir_type fn.env e.ty, r, v, zero));
      Ir.Reg r

and call fn tu ~dst (ret_ty : C.t) (f : T.expr) (args : T.expr list) : Ir.operand =
  let env = fn.env in
  match f.e, args with
  | T.Convert { e = T.Var { name = ("fabs" | "fabsf" | "sqrt" | "sqrtf") as name; storage = T.Static { linkage = T.External; _ }; _ }; _ }, [ a ]
    when is_float a.ty && is_float ret_ty ->
      (* 7.1.4: a library function may be implemented as a builtin; these
         two are single instructions and gcc never emits calls for them, so
         programs link without -lm *)
      let r = fresh fn in
      let ty = ir_type env ret_ty in
      let v = convert fn a.ty ret_ty (value fn tu a) in
      emit fn (Ir.Intrinsic ((if String.sub name 0 4 = "fabs" then Ir.Fabs else Ir.Fsqrt), ty, r, v));
      Ir.Reg r
  | _ ->
  call_general fn tu ~dst ret_ty f args

and call_general fn tu ~dst (ret_ty : C.t) (f : T.expr) (args : T.expr list) : Ir.operand =
  let env = fn.env in
  let fty = match f.ty.u with C.Pointer { u = C.Func ft; _ } -> ft | _ -> assert false in
  let callee = value fn tu f in
  let agg (a : T.expr) addr = { Ir.addr; size = size_of env a.ty; classes = Abi.classify env a.ty } in
  let args = List.map (fun (a : T.expr) ->
      if is_aggregate a.ty then Ir.Aggregate (agg a (address fn tu a))
      else Ir.Scalar (ir_type env a.ty, value fn tu a)) args in
  if ret_ty.u = C.Void then (emit fn (Ir.Call (None, callee, args, fty.variadic)); Ir.Imm 0L)
  else if is_aggregate ret_ty then begin
    let dst = match dst with Some d -> d | None -> Ir.Slot (new_slot fn (size_of env ret_ty) (align_of env ret_ty)) in
    emit fn (Ir.Call (Some (Ir.Ret_aggregate { Ir.addr = dst; size = size_of env ret_ty; classes = Abi.classify env ret_ty }), callee, args, fty.variadic));
    dst
  end else begin
    let r = fresh fn in
    emit fn (Ir.Call (Some (Ir.Ret_scalar (ir_type env ret_ty, r)), callee, args, fty.variadic));
    Ir.Reg r
  end

and builtin fn tu loc name (args : T.expr list) : Ir.operand =
  let env = fn.env in
  match name, args with
  | ("__builtin_trap" | "__builtin_unreachable"), [] -> emit fn Ir.Trap; Ir.Imm 0L
  | "__builtin_prefetch", _ -> List.iter (side_effect fn tu) args; Ir.Imm 0L
  | "__builtin_return_address", [ _ ] -> let r = fresh fn in emit fn (Ir.Return_address r); Ir.Reg r
  | ("__builtin_add_overflow" | "__builtin_sub_overflow" | "__builtin_mul_overflow"), [ a; b; res ] ->
      let rty = match res.ty.u with C.Pointer t -> t | _ -> assert false in
      if not (C.compatible (C.strip a.ty) (C.strip rty) && C.compatible (C.strip b.ty) (C.strip rty)) then
        Diag.error loc "%s: operands must have the result's type '%a' (mixed types are not supported)" name C.pp rty;
      let op = match name with "__builtin_add_overflow" -> Ir.Add | "__builtin_sub_overflow" -> Ir.Sub | _ -> Ir.Mul in
      let r = fresh fn and flag = fresh fn in
      let av = value fn tu a and bv = value fn tu b in
      let addr = value fn tu res in
      emit fn (Ir.Binop_overflow (op, ir_type env rty, is_signed env rty, r, flag, av, bv));
      emit fn (Ir.Store (ir_type env rty, addr, Ir.Reg r));
      Ir.Reg flag
  | "__builtin_va_start", [ ap; _ ] -> emit fn (Ir.Va_start (value fn tu ap)); Ir.Imm 0L
  | "__builtin_va_end", [ ap ] -> side_effect fn tu ap; Ir.Imm 0L
  | "__builtin_va_copy", [ d; s ] -> emit fn (Ir.Memcpy (value fn tu d, value fn tu s, 24)); Ir.Imm 0L
  | ("__builtin_setjmp" | "__builtin_longjmp"), _ ->
      let name = if name = "__builtin_setjmp" then "_setjmp" else "longjmp" in
      let args = List.map (fun (a : T.expr) -> Ir.Scalar (ir_type env a.ty, value fn tu a)) args in
      let r = fresh fn in
      emit fn (Ir.Call (Some (Ir.Ret_scalar (Ir.I32, r)), Ir.Sym name, args, false)); Ir.Reg r
  | _ -> Diag.error loc "internal: unknown builtin %s" name

and atomic fn tu (ty : C.t) (op : T.atomic_op) (orders : Ir.memory_order list) (args : T.expr list) : Ir.operand =
  let env = fn.env in
  (* the operation's width is that of the atomic object, never of the
     result: compare-exchange returns _Bool whatever it compares *)
  let it = match args with
    | p :: _ -> ir_type env (match p.ty.u with C.Pointer t -> t | _ -> assert false)
    | [] -> Ir.I32 in
  let order = List.hd orders in
  match op, args with
  | T.Load, [ p ] -> let r = fresh fn in emit fn (Ir.Atomic_load (it, r, value fn tu p, order)); Ir.Reg r
  | T.Store, [ p; v ] -> let pv = value fn tu p in emit fn (Ir.Atomic_store (it, pv, value fn tu v, order)); Ir.Imm 0L
  | T.Exchange, [ p; v ] -> let r = fresh fn in let pv = value fn tu p in emit fn (Ir.Atomic_xchg (it, r, pv, value fn tu v, order)); Ir.Reg r
  | T.Compare_exchange _, [ p; expected; desired ] ->
      let r = fresh fn in
      let pv = value fn tu p in
      let ev = value fn tu expected in
      emit fn (Ir.Atomic_cmpxchg (it, r, pv, ev, value fn tu desired, order));
      (* the IR result is an I32 flag; the C result is _Bool *)
      let b = fresh fn in
      emit fn (Ir.Conv (Ir.Trunc (Ir.I32, ir_type env ty), b, Ir.Reg r)); Ir.Reg b
  | T.Fetch bop, [ p; v ] ->
      let r = fresh fn in
      let pv = value fn tu p in
      let vv = value fn tu v in
      let vv = match ty.u with
        | C.Pointer t -> let s = fresh fn in emit fn (Ir.Binop (Ir.Mul, Ir.I64, s, vv, Ir.Imm (Int64.of_int (size_of env t)))); Ir.Reg s
        | _ -> vv in
      let irop = match bop with Syntax.Add -> Ir.Add | Syntax.Sub -> Ir.Sub | Syntax.Bor -> Ir.Or | Syntax.Bxor -> Ir.Xor | _ -> Ir.And in
      emit fn (Ir.Atomic_rmw (irop, it, r, pv, vv, order)); Ir.Reg r
  | T.Fence, [] -> emit fn (Ir.Fence order); Ir.Imm 0L
  | T.Signal_fence, [] -> Ir.Imm 0L
  | _ -> assert false

(* ---- Initialisation of automatic objects (6.7.9) ------------------------------- *)

and initialise fn tu (dst : Ir.operand) (ty : C.t) (init : T.init) =
  let env = fn.env in
  match init with
  | T.Init_scalar e when is_aggregate ty -> copy_into fn tu dst e
  | T.Init_scalar e -> store fn ty dst None (value fn tu e)
  | T.Init_string _ ->
      (* copy the whole array image from a private data object *)
      let size = size_of env ty in
      let img = { bytes = Bytes.make size '\000'; relocs = [] } in
      fill_image tu img 0 ty init;
      tu.string_count <- tu.string_count + 1;
      let name = Printf.sprintf ".LC%d" tu.string_count in
      tu.extra_globals <- { Ir.gname = name; gglobal = false; galign = align_of env ty; gtls = false; gsize = size;
                            ginit = Some (data_of_image img); gdefined = true } :: tu.extra_globals;
      emit fn (Ir.Memcpy (dst, Ir.Sym name, size))
  | T.Init_agg items ->
      (* 6.7.9p21: members without an initializer are zero, so clear first *)
      emit fn (Ir.Memzero (dst, size_of env ty));
      List.iter (fun (it : T.init_item) ->
          let addr = if it.off = 0 then dst else (let r = fresh fn in emit fn (Ir.Binop (Ir.Add, Ir.I64, r, dst, Ir.Imm (Int64.of_int it.off))); Ir.Reg r) in
          match it.bits, it.init with
          | Some _, T.Init_scalar e -> store fn it.ity addr it.bits (value fn tu e)
          | _ -> initialise fn tu addr it.ity it.init) items

(* ---- Statements (6.8) -------------------------------------------------------- *)

let rec stmt fn tu (s : T.stmt) =
  (* the line table entry for a statement comes from its first expression *)
  (match s with
   | T.Expr e | T.If (e, _, _) | T.Switch (e, _) | T.While (e, _) | T.Return (Some e) -> mark fn e.loc
   | T.Decl (_, Some (T.Init_scalar e)) -> mark fn e.loc
   | T.For (_, Some e, _, _) -> mark fn e.loc
   | _ -> ());
  match s with
  | T.Expr e -> side_effect fn tu e
  | T.Decl (sym, init) when registerable fn sym ->
      (match init with
       | Some (T.Init_scalar e) -> write fn sym.ty (In_reg (var_reg fn sym)) (value fn tu e)
       | Some _ -> assert false
       | None -> ())
  | T.Decl (sym, init) ->
      let slot = local_slot fn sym in
      (match init with Some i -> initialise fn tu (Ir.Slot slot) sym.ty i | None -> ())
  | T.Block ss -> List.iter (stmt fn tu) ss
  | T.If (c, a, b) ->
      let lt = label fn "then" and lf = label fn "else" and le = label fn "endif" in
      emit fn (Ir.Branch (truth fn tu c, lt, lf));
      emit fn (Ir.Label lt); stmt fn tu a; emit fn (Ir.Jump le);
      emit fn (Ir.Label lf); (match b with Some b -> stmt fn tu b | None -> ());
      emit fn (Ir.Label le)
  | T.While (c, body) ->
      let lc = label fn "while" and lb = label fn "body" and le = label fn "endwhile" in
      emit fn (Ir.Label lc);
      emit fn (Ir.Branch (truth fn tu c, lb, le));
      emit fn (Ir.Label lb);
      loop fn tu ~brk:le ~cont:lc body;
      emit fn (Ir.Jump lc);
      emit fn (Ir.Label le)
  | T.Do (body, c) ->
      let lb = label fn "do" and lc = label fn "docond" and le = label fn "enddo" in
      emit fn (Ir.Label lb);
      loop fn tu ~brk:le ~cont:lc body;
      emit fn (Ir.Label lc);
      emit fn (Ir.Branch (truth fn tu c, lb, le));
      emit fn (Ir.Label le)
  | T.For (init, c, step, body) ->
      (match init with Some i -> stmt fn tu i | None -> ());
      let lc = label fn "for" and lb = label fn "forbody" and ls = label fn "forstep" and le = label fn "endfor" in
      emit fn (Ir.Label lc);
      (match c with Some c -> emit fn (Ir.Branch (truth fn tu c, lb, le)) | None -> ());
      emit fn (Ir.Label lb);
      loop fn tu ~brk:le ~cont:ls body;
      emit fn (Ir.Label ls);
      (match step with Some e -> side_effect fn tu e | None -> ());
      emit fn (Ir.Jump lc);
      emit fn (Ir.Label le)
  | T.Switch (c, body) ->
      let v = value fn tu c in
      let le = label fn "endswitch" in
      (* the body is emitted first into its own list so the cases are known *)
      let saved = fn.code in
      fn.code <- [];
      fn.cases <- ([], None) :: fn.cases;
      fn.breaks <- le :: fn.breaks;
      stmt fn tu body;
      fn.breaks <- List.tl fn.breaks;
      let cases, default = List.hd fn.cases in
      fn.cases <- List.tl fn.cases;
      let body_code = fn.code in
      fn.code <- saved;
      emit fn (Ir.Switch (ir_type fn.env c.ty, v, List.rev cases, Option.value default ~default:le));
      fn.code <- body_code @ fn.code;
      emit fn (Ir.Label le)
  | T.Case (v, body) ->
      let l = label fn "case" in
      (match fn.cases with
       | (cs, d) :: rest -> fn.cases <- ((v, l) :: cs, d) :: rest
       | [] -> assert false);
      emit fn (Ir.Label l); stmt fn tu body
  | T.Default body ->
      let l = label fn "default" in
      (match fn.cases with
       | (cs, _) :: rest -> fn.cases <- (cs, Some l) :: rest
       | [] -> assert false);
      emit fn (Ir.Label l); stmt fn tu body
  | T.Label (name, body) -> emit fn (Ir.Label (Printf.sprintf ".L%s.%s" fn.fname name)); stmt fn tu body
  | T.Goto name -> emit fn (Ir.Jump (Printf.sprintf ".L%s.%s" fn.fname name))
  | T.Continue -> emit fn (Ir.Jump (List.hd fn.continues))
  | T.Break -> emit fn (Ir.Jump (List.hd fn.breaks))
  | T.Return None -> emit fn (Ir.Ret None)
  | T.Return (Some e) ->
      if is_aggregate e.ty then
        emit fn (Ir.Ret (Some (Ir.Rv_aggregate { Ir.addr = address fn tu e; size = size_of fn.env e.ty; classes = Abi.classify fn.env e.ty })))
      else emit fn (Ir.Ret (Some (Ir.Rv_scalar (ir_type fn.env e.ty, value fn tu e))))
  | T.Asm _ -> Diag.error Loc.none "inline assembly is not supported"

and loop fn tu ~brk ~cont body =
  fn.breaks <- brk :: fn.breaks;
  fn.continues <- cont :: fn.continues;
  stmt fn tu body;
  fn.breaks <- List.tl fn.breaks;
  fn.continues <- List.tl fn.continues

(* ---- Functions and the program ------------------------------------------------ *)

(* Locals whose address is taken must live in memory. *)
let rec find_addressed fn (s : T.stmt) =
  let rec expr (e : T.expr) =
    match e.e with
    | T.Addr { e = T.Var s; _ } -> Hashtbl.replace fn.addressed s.id ()
    | T.Addr x -> expr x
    | T.Var _ | T.Int _ | T.Float _ | T.String _ -> ()
    | T.Convert x | T.Unop (_, x) | T.Deref x | T.Member (x, _) -> expr x
    | T.Binop (_, a, b) | T.Assign (a, b) | T.Compound_assign (_, a, b, _) -> expr a; expr b
    | T.Cond (a, b, c) -> expr a; expr b; expr c
    | T.Call (f, args) -> expr f; List.iter expr args
    | T.Compound_literal (_, i) -> init i
    | T.Va_arg (x, _) -> expr x
    | T.Builtin (_, args) | T.Atomic_op (_, _, args) -> List.iter expr args
  and init = function
    | T.Init_scalar e -> expr e
    | T.Init_agg items -> List.iter (fun (it : T.init_item) -> init it.init) items
    | T.Init_string _ -> () in
  match s with
  | T.Expr e -> expr e
  | T.Decl (_, Some i) -> init i
  | T.Decl (_, None) | T.Goto _ | T.Continue | T.Break | T.Return None | T.Asm _ -> ()
  | T.Block ss -> List.iter (find_addressed fn) ss
  | T.If (c, a, b) -> expr c; find_addressed fn a; Option.iter (find_addressed fn) b
  | T.Switch (c, b) | T.While (c, b) | T.Do (b, c) -> expr c; find_addressed fn b
  | T.Case (_, b) | T.Default b | T.Label (_, b) -> find_addressed fn b
  | T.For (i, c, st, b) -> Option.iter (find_addressed fn) i; Option.iter expr c; Option.iter expr st; find_addressed fn b
  | T.Return (Some e) -> expr e

let func tu (f : T.func) : Ir.func =
  let env = tu.tenv in
  let fn = { env; code = []; next_reg = 0; slots = []; next_slot = 0; locals = Hashtbl.create 32;
             vars = Hashtbl.create 32; addressed = Hashtbl.create 16;
             breaks = []; continues = []; cases = []; fname = asm_name f.fsym; next_label = 0; last_line = f.loc } in
  find_addressed fn f.body;
  (* the prologue and parameter stores belong to the definition's line,
     which Select records at the function's start; the next line entry is
     the first statement's, so a debugger skipping the prologue lands there *)
  let fty = match f.fsym.ty.u with C.Func ft -> ft | _ -> assert false in
  (* parameters: scalars arrive in registers and are spilled to their slot;
     aggregates are copied by the prologue into their slot *)
  let params = List.map (fun (p : T.symbol) ->
      if is_aggregate p.ty then Ir.P_aggregate (local_slot fn p, size_of env p.ty, Abi.classify env p.ty)
      else if registerable fn p then Ir.P_scalar (ir_type env p.ty, var_reg fn p) (* arrives in its register *)
      else begin
        let r = fresh fn in
        emit fn (Ir.Store (ir_type env p.ty, Ir.Slot (local_slot fn p), Ir.Reg r));
        Ir.P_scalar (ir_type env p.ty, r)
      end) f.params in
  stmt fn tu f.body;
  (* 5.1.2.2.3: reaching the end of main returns 0; otherwise the value is
     undefined, and returning nothing in particular is fine *)
  (match fn.code with
   | Ir.Ret _ :: _ -> ()
   | _ ->
       if f.fsym.name = "main" then emit fn (Ir.Ret (Some (Ir.Rv_scalar (Ir.I32, Ir.Imm 0L))))
       else emit fn (Ir.Ret None));
  { Ir.name = fn.fname; params; variadic = fty.variadic;
    returns_aggregate = (if is_aggregate fty.ret then Some (size_of env fty.ret, Abi.classify env fty.ret) else None);
    slots = Array.of_list (List.rev fn.slots); body = List.rev fn.code;
    global = (match f.fsym.storage with T.Static { linkage = T.External; _ } -> true | _ -> false);
    discardable = f.inline; loc = f.loc;
    variables = Hashtbl.fold (fun _ r acc -> r :: acc) fn.vars [];
    params_dbg = List.map (fun (p : T.symbol) -> p.name, p.ty) f.params; ret_dbg = fty.ret }

(* Symbols an instruction refers to. *)
let syms_of_instr (i : Ir.instr) : string list =
  let op = function Ir.Sym s -> [ s ] | _ -> [] in
  let arg = function Ir.Scalar (_, o) -> op o | Ir.Aggregate a -> op a.addr in
  match i with
  | Ir.Mov (_, _, o) | Ir.Neg (_, _, o) | Ir.Not (_, _, o) | Ir.Conv (_, _, o) | Ir.Load (_, _, o)
  | Ir.Va_start o | Ir.Va_arg (_, _, o) | Ir.Branch (o, _, _) | Ir.Switch (_, o, _, _)
  | Ir.Atomic_load (_, _, o, _) -> op o
  | Ir.Binop (_, _, _, a, b) | Ir.Binop_overflow (_, _, _, _, _, a, b) | Ir.Cmp (_, _, _, a, b) | Ir.Store (_, a, b)
  | Ir.Memcpy (a, b, _) | Ir.Atomic_store (_, a, b, _) | Ir.Atomic_rmw (_, _, _, a, b, _) | Ir.Atomic_xchg (_, _, a, b, _) -> op a @ op b
  | Ir.Atomic_cmpxchg (_, _, a, b, c, _) -> op a @ op b @ op c
  | Ir.Memzero (o, _) -> op o
  | Ir.Call (res, f, args, _) ->
      op f @ List.concat_map arg args @ (match res with Some (Ir.Ret_aggregate a) -> op a.addr | _ -> [])
  | Ir.Ret (Some (Ir.Rv_scalar (_, o))) -> op o
  | Ir.Ret (Some (Ir.Rv_aggregate a)) -> op a.addr
  | Ir.Intrinsic (_, _, _, o) -> op o
  | Ir.Ret None | Ir.Label _ | Ir.Jump _ | Ir.Fence _ | Ir.Trap | Ir.Return_address _ | Ir.Line _ -> []

(* Drop inline definitions nothing refers to, transitively. *)
let prune (funcs : Ir.func list) (globals : Ir.global list) : Ir.func list =
  let referenced = Hashtbl.create 64 in
  let mark s = Hashtbl.replace referenced s () in
  List.iter (fun (g : Ir.global) ->
      Option.iter (List.iter (function Ir.Addr (s, _) -> mark s | _ -> ())) g.ginit) globals;
  let by_name = Hashtbl.create 64 in
  List.iter (fun (f : Ir.func) -> Hashtbl.replace by_name f.name f) funcs;
  let visited = Hashtbl.create 64 in
  let rec visit (f : Ir.func) =
    if not (Hashtbl.mem visited f.name) then begin
      Hashtbl.replace visited f.name ();
      List.iter (fun i -> List.iter (fun s ->
          mark s;
          match Hashtbl.find_opt by_name s with Some g -> visit g | None -> ()) (syms_of_instr i)) f.body
    end in
  List.iter (fun (f : Ir.func) -> if not f.discardable then visit f) funcs;
  (* functions only reached from data initializers *)
  List.iter (fun (f : Ir.func) -> if Hashtbl.mem referenced f.name then visit f) funcs;
  List.filter (fun (f : Ir.func) -> not f.discardable || Hashtbl.mem visited f.name) funcs

let program ~source env (tu : T.translation_unit) : Ir.program =
  let st = { tenv = env; strings = []; extra_globals = []; string_count = 0 } in
  let funcs = List.map (func st) tu.funcs in
  let globals = List.map (global_of st) tu.globals in
  let funcs = prune funcs globals in
  let strings = List.rev_map snd st.strings in
  { Ir.funcs; globals = globals @ strings @ List.rev st.extra_globals; source }
