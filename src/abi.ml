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

(* x86-64 System V 3.2.3: one class per eightbyte.  Every piece is a
   whole eightbyte, the last one included, which is what the back end
   has always loaded and stored. *)
let classify_amd64 env (t : Ctype.t) : Ir.passing =
  let size = Env.size_of env Loc.none t in
  if size > 16 || size = 0 then Ir.In_memory
  else begin
    let ls = leaves env t 0 [] in
    if List.exists (fun (_, k) -> k = X87_leaf) ls then Ir.In_memory
    else begin
      let n = (size + 7) / 8 in
      let float_piece = Array.make n true in
      List.iter (fun (off, k) -> if k = Int_leaf then float_piece.(off / 8) <- false) ls;
      (* an eightbyte with no leaves (padding only) is INTEGER, ABI 3.2.3p4 *)
      for i = 0 to n - 1 do
        if not (List.exists (fun (off, _) -> off / 8 = i) ls) then float_piece.(i) <- false
      done;
      Ir.In_registers
        (List.init n (fun i -> { Ir.poff = 8 * i; psize = 8; pfloat = float_piece.(i) }))
    end
  end

(* RISC-V's rule (the psABI's "Hardware floating-point calling
   convention"), which is asked of the flattened members rather than of
   eightbytes and so cannot be said in the other machine's terms:

     at most two members, all floating-point   one FP register each
     exactly two, one floating and one integer one of each
     anything else up to two words             integer registers
     larger than two words                     passed by reference

   Measured on the machine to be sure of the two rows that differ from
   x86-64: {float,float} takes two FP registers where x86-64 puts both
   halves in one, and {float,float,float,float} takes integer registers
   where x86-64 takes two SSE ones.  A union never qualifies -- only a
   struct's members are looked through -- and neither does long double,
   which is binary128 here and travels as integers. *)
let rec has_union env (t : Ctype.t) =
  match t.u with
  | Ctype.Union _ -> true
  | Ctype.Array (e, _) -> has_union env e
  | Ctype.Struct tag ->
      (match (Env.tag_info env tag).layout with
       | Some l -> List.exists (fun (f : Ctype.field) -> has_union env f.ftype) l.fields
       | None -> false)
  | _ -> false

let classify_riscv64 env (t : Ctype.t) : Ir.passing =
  let xlen = 8 in
  let size = Env.size_of env Loc.none t in
  if size = 0 || size > 2 * xlen then Ir.In_memory
  else begin
    let integer_pieces () =
      let n = (size + xlen - 1) / xlen in
      Ir.In_registers
        (List.init n (fun i ->
             { Ir.poff = xlen * i; psize = min xlen (size - xlen * i); pfloat = false })) in
    if has_union env t then integer_pieces ()
    else
      let ls = List.rev (leaves env t 0 []) in     (* in offset order *)
      let floats = List.filter (fun (_, k) -> k = Float_leaf) ls in
      let x87 = List.exists (fun (_, k) -> k = X87_leaf) ls in
      if x87 then integer_pieces ()
      else if List.length ls <= 2 && List.length floats = List.length ls && ls <> [] then
        (* one register per floating-point member, each its own size *)
        Ir.In_registers
          (List.map (fun (off, _) ->
               let rest = size - off in
               let psize = List.fold_left (fun acc (o, _) -> if o > off && o - off < acc then o - off else acc) rest ls in
               { Ir.poff = off; psize; pfloat = true })
             ls)
      else if List.length ls = 2 && List.length floats = 1 then
        (* one floating-point member and one integer one: a register of
           each kind, in the order they appear *)
        Ir.In_registers
          (List.map (fun (off, k) ->
               let rest = size - off in
               let psize = List.fold_left (fun acc (o, _) -> if o > off && o - off < acc then o - off else acc) rest ls in
               { Ir.poff = off; psize; pfloat = k = Float_leaf })
             ls)
      else integer_pieces ()
  end

let classify env (t : Ctype.t) : Ir.passing =
  match !Target.machine with
  | Target.Amd64 -> classify_amd64 env t
  | Target.Riscv64 -> classify_riscv64 env t
