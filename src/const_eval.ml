type value =
  | Int of int64
  | Float of float
  | Addr of Typed.symbol * int64
  | Str of string * Ctype.t * int64

let normalise (t : Ctype.t) v =
  match t.u with
  | Integer Bool -> if v = 0L then 0L else 1L
  | Integer k ->
      let bits = 8 * Target.size_of_ikind k in
      if bits = 64 then v
      else
        let shift = 64 - bits in
        if Ctype.is_signed k then Int64.shift_right (Int64.shift_left v shift) shift
        else Int64.shift_right_logical (Int64.shift_left v shift) shift
  | Enum _ -> Int64.shift_right_logical (Int64.shift_left v 32) 32 |> fun u ->
      (* enums are 32-bit; treat as unsigned int or int per Target *)
      Int64.shift_right (Int64.shift_left u 32) 32
  | _ -> v

let is_unsigned (t : Ctype.t) =
  match t.u with
  | Integer k -> not (Ctype.is_signed k)
  | Pointer _ -> true
  | _ -> false

let to_float (t : Ctype.t) v =
  if is_unsigned t && v < 0L then Int64.to_float (Int64.shift_right_logical v 1) *. 2.0 +. Int64.to_float (Int64.logand v 1L)
  else Int64.to_float v

let rec eval env (e : Typed.expr) : value option =
  let ( let* ) = Option.bind in
  match e.e with
  | Int v -> Some (Int v)
  | Float f -> Some (Float f)
  | String s -> Some (Str (s, e.ty, 0L))
  | Var { storage = Static _; _ } when Ctype.is_array e.ty || Ctype.is_function e.ty ->
      (* an array or function designator is already an address *)
      (match e.e with Var s -> Some (Addr (s, 0L)) | _ -> None)
  | Addr { e = Var ({ storage = Static _; _ } as s); _ } -> Some (Addr (s, 0L))
  | Addr { e = Deref p; _ } -> eval env p
  | Addr { e = Member (base, f); _ } ->
      let* b = eval env { base with e = Addr base; ty = Ctype.pointer base.ty; lvalue = false } in
      (match b with
       | Addr (s, o) -> Some (Addr (s, Int64.add o (Int64.of_int f.offset)))
       | Str (s, t, o) -> Some (Str (s, t, Int64.add o (Int64.of_int f.offset)))
       | _ -> None)
  | Convert x ->
      let* v = eval env x in
      (match v, e.ty.u with
       | Int i, (Integer _ | Enum _) ->
           if Ctype.is_floating x.ty then None else Some (Int (normalise e.ty i))
       | Int i, Pointer _ -> Some (Int i)
       | Int i, Floating _ -> Some (Float (to_float x.ty i))
       | Float f, (Integer _ | Enum _) -> Some (Int (normalise e.ty (Int64.of_float f)))
       | Float f, Floating Float -> Some (Float (Int32.float_of_bits (Int32.bits_of_float f)))
       | Float f, Floating _ -> Some (Float f)
       | (Addr _ | Str _), Pointer _ -> Some v
       | (Addr _ | Str _), Integer (Long | ULong | LLong | ULLong) -> Some v
       | _ -> None)
  | Unop (op, x) ->
      let* v = eval env x in
      (match op, v with
       | Neg, Int i -> Some (Int (normalise e.ty (Int64.neg i)))
       | Neg, Float f -> Some (Float (-. f))
       | Not, Int i -> Some (Int (normalise e.ty (Int64.lognot i)))
       | Lnot, Int i -> Some (Int (if i = 0L then 1L else 0L))
       | Lnot, Float f -> Some (Int (if f = 0.0 then 1L else 0L))
       | Lnot, (Addr _ | Str _) -> Some (Int 0L)
       | _ -> None)
  | Binop (op, a, b) ->
      let* va = eval env a in
      let* vb = eval env b in
      binop env e.ty a.ty op va vb
  | Cond (c, a, b) ->
      let* vc = eval env c in
      let truth = match vc with Int i -> i <> 0L | Float f -> f <> 0.0 | Addr _ | Str _ -> true in
      eval env (if truth then a else b)
  | _ -> None

and binop env (rty : Ctype.t) (aty : Ctype.t) op va vb =
  let open Syntax in
  let unsigned = is_unsigned aty in
  let elem_size (t : Ctype.t) =
    match t.u with Pointer p -> Int64.of_int (Env.size_of env Loc.none p) | _ -> 1L in
  match va, vb with
  | Int x, Int y ->
      let int_result v = Some (Int (normalise rty v)) in
      let cmp c = Some (Int (if c then 1L else 0L)) in
      let ucmp = Int64.unsigned_compare x y and scmp = compare x y in
      let c = if unsigned then ucmp else scmp in
      (match op with
       | Add -> int_result (Int64.add x y) | Sub -> int_result (Int64.sub x y)
       | Mul -> int_result (Int64.mul x y)
       | Div -> if y = 0L then None else int_result (if unsigned then Int64.unsigned_div x y else Int64.div x y)
       | Mod -> if y = 0L then None else int_result (if unsigned then Int64.unsigned_rem x y else Int64.rem x y)
       | Shl -> int_result (Int64.shift_left x (Int64.to_int y))
       | Shr -> int_result (if unsigned then Int64.shift_right_logical x (Int64.to_int y) else Int64.shift_right x (Int64.to_int y))
       | Band -> int_result (Int64.logand x y) | Bor -> int_result (Int64.logor x y) | Bxor -> int_result (Int64.logxor x y)
       | Lt -> cmp (c < 0) | Gt -> cmp (c > 0) | Le -> cmp (c <= 0) | Ge -> cmp (c >= 0)
       | Eq -> cmp (x = y) | Ne -> cmp (x <> y)
       | Land -> cmp (x <> 0L && y <> 0L) | Lor -> cmp (x <> 0L || y <> 0L)
       | Comma -> Some (Int y))
  | Float x, Float y ->
      let r f = Some (Float f) and cmp c = Some (Int (if c then 1L else 0L)) in
      (match op with
       | Add -> r (x +. y) | Sub -> r (x -. y) | Mul -> r (x *. y) | Div -> r (x /. y)
       | Lt -> cmp (x < y) | Gt -> cmp (x > y) | Le -> cmp (x <= y) | Ge -> cmp (x >= y)
       | Eq -> cmp (x = y) | Ne -> cmp (x <> y)
       | Land -> cmp (x <> 0.0 && y <> 0.0) | Lor -> cmp (x <> 0.0 || y <> 0.0)
       | _ -> None)
  | Addr (s, o), Int i when op = Add -> Some (Addr (s, Int64.add o (Int64.mul i (elem_size aty))))
  | Addr (s, o), Int i when op = Sub -> Some (Addr (s, Int64.sub o (Int64.mul i (elem_size aty))))
  | Str (s, t, o), Int i when op = Add -> Some (Str (s, t, Int64.add o i))
  | _ -> None

let int_const env e =
  match eval env e with
  | Some (Int v) -> v
  | _ -> Diag.error e.loc "expression is not an integer constant expression"
