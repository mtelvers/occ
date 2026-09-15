(* How an aggregate is passed and returned, which is the one part of a
   calling convention the front end has to know: it decides the shape of
   the IR for a call before any machine code is in sight.

   The answer is a list of classes, one per eightbyte, and both machines
   are asked the same question even though their rules differ (x86-64
   System V 3.2.3; RISC-V calling convention "Hardware floating-point
   calling convention").  Flatten an object into (offset, kind) leaves,
   then merge per eightbyte. *)
type leaf = Int_leaf | Float_leaf | X87_leaf

let rec leaves env (t : Ctype.t) base acc =
  match t.u with
  | Ctype.Integer _ | Ctype.Enum _ | Ctype.Pointer _ -> (base, Int_leaf) :: acc
  | Ctype.Floating Ctype.LongDouble -> (base, X87_leaf) :: acc
  | Ctype.Floating _ -> (base, Float_leaf) :: acc
  | Ctype.Array (e, Some n) ->
      let s = Env.size_of env Loc.none e in
      let acc = ref acc in
      for i = 0 to n - 1 do acc := leaves env e (base + i * s) !acc done;
      !acc
  | Ctype.Struct tag | Ctype.Union tag ->
      (match (Env.tag_info env tag).layout with
       | Some l -> List.fold_left (fun acc (f : Ctype.field) -> leaves env f.ftype (base + f.offset) acc) acc l.fields
       | None -> acc)
  | Ctype.Array (_, None) | Ctype.Vla _ | Ctype.Void | Ctype.Func _ -> acc

let classify_amd64 env (t : Ctype.t) : Ir.cls list =
  let size = Env.size_of env Loc.none t in
  if size > 16 || size = 0 then [ Ir.Memory ]
  else begin
    let ls = leaves env t 0 [] in
    if List.exists (fun (_, k) -> k = X87_leaf) ls then [ Ir.Memory ]
    else begin
      let n = (size + 7) / 8 in
      let classes = Array.make n Ir.Sse in
      List.iter (fun (off, k) -> if k = Int_leaf then classes.(off / 8) <- Ir.Integer) ls;
      (* an eightbyte with no leaves (padding only) is INTEGER, ABI 3.2.3p4 *)
      for i = 0 to n - 1 do
        if not (List.exists (fun (off, _) -> off / 8 = i) ls) then classes.(i) <- Ir.Integer
      done;
      Array.to_list classes
    end
  end

let classify env (t : Ctype.t) : Ir.cls list =
  match !Target.machine with
  | Target.Amd64 -> classify_amd64 env t
  | Target.Riscv64 -> classify_amd64 env t   (* replaced when that backend lands *)
