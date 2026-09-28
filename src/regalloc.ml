(* Linear-scan register allocation over the IR (Poletto and Sarkar's
   algorithm, in one page).

   None of this is machine knowledge and it is the same job on both
   machines, so the machines supply only four things: which registers may
   be allocated and which of those a call destroys, which registers an
   instruction's selected code writes for itself, and how to name one in
   a dump.  Everything else -- what an instruction defines and uses,
   which values are floating-point, where each value's live interval
   begins and ends, and which value wins a register when two want one --
   is read off the IR.

   Floating-point values are not allocated: they live in frame slots, on
   both machines, which is where the second pass puts them. *)

type 'r location = Register of 'r | Spill of int

type 'r assignment = { where : (int, 'r location) Hashtbl.t; spill_slots : int; used : 'r list }

(* Registers defined and used by an instruction. *)
let regs_of_instr (i : Ir.instr) : int list * int list =
  let u = function Ir.Reg r -> [ r ] | _ -> [] in
  let arg = function Ir.Scalar (_, _, o) -> u o | Ir.Aggregate a -> u a.addr in
  match i with
  | Ir.Mov (_, r, o) | Ir.Neg (_, r, o) | Ir.Not (_, r, o) | Ir.Conv (_, r, o) | Ir.Load (_, r, o)
  | Ir.Va_arg (_, r, o) | Ir.Atomic_load (_, r, o, _) | Ir.Intrinsic (_, _, r, o) | Ir.Alloca (r, o) -> [ r ], u o
  | Ir.Binop (_, _, r, a, b) | Ir.Cmp (_, _, r, a, b) | Ir.Atomic_rmw (_, _, r, a, b, _) | Ir.Atomic_xchg (_, r, a, b, _) -> [ r ], u a @ u b
  | Ir.Binop_overflow (_, _, _, r, f, a, b) -> [ r; f ], u a @ u b
  | Ir.Atomic_cmpxchg (_, r, a, b, c, _) -> [ r ], u a @ u b @ u c
  | Ir.Store (_, a, b) | Ir.Memcpy (a, b, _) | Ir.Atomic_store (_, a, b, _) | Ir.Va_arg_aggregate (a, _, _, b) -> [], u a @ u b
  | Ir.Memzero (o, _) | Ir.Va_start o | Ir.Branch (o, _, _) | Ir.Switch (_, o, _, _) -> [], u o
  | Ir.Inline_asm a ->
      Array.fold_left (fun (d, us) op ->
          match op with
          | Ir.Asm_in (_, _, o) | Ir.Asm_mem (_, o) -> d, us @ u o
          | Ir.Asm_out (_, _, r) -> r :: d, us
          | Ir.Asm_inout (_, _, r, o) -> r :: d, us @ u o
          | Ir.Asm_imm _ -> d, us) ([], []) a.operands
  | Ir.Call (res, f, args, _) ->
      (match res with Some (Ir.Ret_scalar (_, r)) -> [ r ] | _ -> []),
      u f @ List.concat_map arg args @ (match res with Some (Ir.Ret_aggregate a) -> u a.addr | _ -> [])
  | Ir.Ret (Some (Ir.Rv_scalar (_, _, o))) -> [], u o
  | Ir.Ret (Some (Ir.Rv_aggregate a)) -> [], u a.addr
  | Ir.Return_address r -> [ r ], []
  | Ir.Label_addr (r, _) -> [ r ], []
  | Ir.Jump_indirect o -> [], u o
  | Ir.Ret None | Ir.Label _ | Ir.Jump _ | Ir.Fence _ | Ir.Trap | Ir.Line _ -> [], []

(* Which registers hold floating-point values: those defined with an F type. *)
let float_regs (f : Ir.func) =
  let fl = Hashtbl.create 16 in
  let is_f = function Ir.F32 | Ir.F64 | Ir.F80 -> true | _ -> false in
  List.iter (function Ir.P_scalar (t, r) when is_f t -> Hashtbl.replace fl r () | _ -> ()) f.params;
  List.iter (fun i ->
      match i with
      | Ir.Mov (t, r, _) | Ir.Neg (t, r, _) | Ir.Load (t, r, _) | Ir.Va_arg (t, r, _) | Ir.Atomic_load (t, r, _, _)
      | Ir.Intrinsic (_, t, r, _) | Ir.Binop (_, t, r, _, _) when is_f t -> Hashtbl.replace fl r ()
      | Ir.Conv ((Ir.Fext | Ir.Ftrunc | Ir.Stof _ | Ir.Utof _ | Ir.Fconv _), r, _) -> Hashtbl.replace fl r ()
      | Ir.Call (Some (Ir.Ret_scalar (t, r)), _, _, _) when is_f t -> Hashtbl.replace fl r ()
      | Ir.Inline_asm a ->
          Array.iter (function
              | Ir.Asm_out (_, t, r) | Ir.Asm_inout (_, t, r, _) when is_f t -> Hashtbl.replace fl r ()
              | _ -> ()) a.operands
      | _ -> ()) f.body;
  fl

(* Live intervals: (start, stop, register), sorted by start.

   A value's interval has to cover every position where the value is
   live, and liveness is a property of the control flow, not of the order
   the instructions happen to be written in: a value defined at the
   bottom of a loop and read at the top is live across the back edge,
   and a value whose every use is inside one arm of a switch is not live
   in the others.  So the flow graph is built and live-variable analysis
   run over it (the backward dataflow of the dragon book, chapter 9),
   rather than approximated.

   The approximation this replaces -- a value touched anywhere inside a
   loop is live over the whole loop -- is safe but ruinous where the
   whole function is one loop with many arms, which is what a bytecode
   interpreter is: each of its hundred-odd opcode handlers has locals of
   its own, and treating them all as live at once leaves a hundred
   values competing for twelve registers.  Under the analysis each
   handler's locals live only in that handler, and only what the loop
   really carries -- the interpreter's pc, sp and accumulator -- stays
   live throughout.

   Intervals remain one range apiece rather than a set of ranges with
   holes: that is still conservative, and it keeps the scan below in the
   one page it takes. *)
let intervals (f : Ir.func) =
  let body = Array.of_list f.body in
  let n = Array.length body in
  let def_use = Array.map regs_of_instr body in
  (* the label each name stands for *)
  let labels = Hashtbl.create 32 in
  Array.iteri (fun i ins -> match ins with Ir.Label l -> Hashtbl.replace labels l i | _ -> ()) body;
  (* Every label whose address is taken is a possible target of every
     indirect jump: that is the whole of what a computed goto does to the
     flow graph. *)
  let addressed = ref [] in
  Array.iter (function
      | Ir.Label_addr (_, l) -> (match Hashtbl.find_opt labels l with Some j -> addressed := j :: !addressed | None -> ())
      | _ -> ()) body;
  let target l = Hashtbl.find_opt labels l in
  (* Successors of position [i]: where control may go next.  A return
     goes nowhere; a trap ends the function too. *)
  let succs i =
    let fallthrough = if i + 1 < n then [ i + 1 ] else [] in
    match body.(i) with
    | Ir.Jump l -> (match target l with Some j -> [ j ] | None -> [])
    | Ir.Branch (_, a, b) -> List.filter_map target [ a; b ]
    | Ir.Switch (_, _, cases, d) -> List.filter_map target (d :: List.map snd cases)
    | Ir.Jump_indirect _ -> !addressed
    | Ir.Ret _ -> []
    | _ -> fallthrough in
  (* Live-in sets, as one bitset per position over the registers in use.
     The fixpoint is reached by sweeping backwards until nothing changes,
     which for a body written in source order is a handful of sweeps. *)
  let nregs = 1 + Array.fold_left (fun m (ds, us) -> List.fold_left max m (ds @ us)) 0 def_use in
  let words = (nregs + 7) / 8 in
  let live = Array.init (n + 1) (fun _ -> Bytes.make words '\000') in
  let get b r = Char.code (Bytes.get b (r / 8)) land (1 lsl (r land 7)) <> 0 in
  let set b r = Bytes.set b (r / 8) (Char.chr (Char.code (Bytes.get b (r / 8)) lor (1 lsl (r land 7)))) in
  let clear b r = Bytes.set b (r / 8) (Char.chr (Char.code (Bytes.get b (r / 8)) land lnot (1 lsl (r land 7)) land 0xff)) in
  let changed = ref true in
  while !changed do
    changed := false;
    for i = n - 1 downto 0 do
      (* live-out is the union of the successors' live-in *)
      let out = Bytes.make words '\000' in
      List.iter (fun s ->
          let ls = live.(s) in
          for k = 0 to words - 1 do
            Bytes.set out k (Char.chr (Char.code (Bytes.get out k) lor Char.code (Bytes.get ls k)))
          done) (succs i);
      let defs, uses = def_use.(i) in
      List.iter (fun r -> clear out r) defs;
      List.iter (fun r -> set out r) uses;
      if Bytes.compare out live.(i) <> 0 then begin live.(i) <- out; changed := true end
    done
  done;
  (* The interval of a value runs from where it is first defined or live
     to where it is last live or used. *)
  let first = Hashtbl.create 64 and last = Hashtbl.create 64 in
  let note r i = if not (Hashtbl.mem first r) then Hashtbl.replace first r i; Hashtbl.replace last r i in
  (* a parameter is live from before the first instruction *)
  List.iter (function Ir.P_scalar (_, r) -> note r (-1) | Ir.P_aggregate _ -> ()) f.params;
  for i = 0 to n - 1 do
    let defs, uses = def_use.(i) in
    List.iter (fun r -> note r i) defs;
    List.iter (fun r -> note r i) uses;
    for r = 0 to nregs - 1 do if get live.(i) r then note r i done
  done;
  List.sort compare (Hashtbl.fold (fun r s acc -> (s, Hashtbl.find last r, r) :: acc) first [])

let allocate ~callee_saved ~caller_saved ~clobbers ~name (f : Ir.func) =
  let allocatable = callee_saved @ caller_saved in
  let where = Hashtbl.create 64 in
  let floats = float_regs f in
  (* for each caller-saved register, the positions that clobber it *)
  let clobbered_at = Hashtbl.create 8 in
  List.iteri (fun i ins -> List.iter (fun p -> Hashtbl.add clobbered_at p i) (clobbers ins)) f.body;
  let safe start stop p =
    List.mem p callee_saved
    || not (List.exists (fun i -> i >= start && i <= stop) (Hashtbl.find_all clobbered_at p)) in
  (* the prologue stores parameters after the callee-saved saves, from the
     argument registers: a parameter may not live in one of those *)
  let used = ref [] in
  (* registers: active intervals as (stop, reg, physical) *)
  let active = ref [] in
  let free = ref allocatable in
  (* spill slots: active spills as (stop, slot), and a free list *)
  let spilled = ref [] and free_slots = ref [] and slots = ref 0 in
  (* a spill starting now takes a slot whose previous occupant has ended *)
  let spill (stop, r) =
    let slot = match !free_slots with s :: rest -> free_slots := rest; s | [] -> let s = !slots in incr slots; s in
    Hashtbl.replace where r (Spill slot);
    spilled := (stop, slot) :: !spilled in
  let all = intervals f in
  let uses = Hashtbl.create 64 in
  List.iter (fun i -> let ds, us = regs_of_instr i in
              List.iter (fun r -> Hashtbl.replace uses r (1 + Option.value (Hashtbl.find_opt uses r) ~default:0)) (ds @ us)) f.body;
  let use_count r = Option.value (Hashtbl.find_opt uses r) ~default:0 in
  let is_var = Hashtbl.create 32 in
  List.iter (fun r -> Hashtbl.replace is_var r ()) f.variables;
  (* pass 1: variables, with eviction of the one ending last *)
  List.iter (fun (start, stop, r) ->
      let expired, still = List.partition (fun (e, _, _) -> e < start) !active in
      active := still;
      List.iter (fun (_, _, p) -> free := p :: !free) expired;
      if Hashtbl.mem floats r then ()
      else
        match List.find_opt (fun p -> safe start stop p && not (start < 0 && List.mem p caller_saved)) !free with
        | Some p ->
            free := List.filter (( <> ) p) !free;
            if not (List.mem p !used) then used := p :: !used;
            Hashtbl.replace where r (Register p);
            active := (stop, r, p) :: !active
        | None ->
            (* evict, among callee-saved holders, the least used variable,
               if this one is used more: in a loop that spans the function
               every variable ends at the same place, and use count is
               what separates the interpreter's pc from a rarely read flag *)
            let candidates = List.filter (fun (_, _, p) -> List.mem p callee_saved) !active in
            (match candidates with
             | [] -> ()
             | c :: cs ->
                 let (_, r', p) = List.fold_left (fun ((_, x, _) as m) ((_, y, _) as c) -> if use_count y < use_count x then c else m) c cs in
                 if use_count r > use_count r' then begin
                   Hashtbl.replace where r (Register p);
                   active := (stop, r, p) :: List.filter (fun (_, x, _) -> x <> r') !active;
                   Hashtbl.remove where r' (* spilled below, in interval order *)
                 end))
    (List.filter (fun (_, _, r) -> Hashtbl.mem is_var r) all);
  (* the registers' occupancy after pass 1, as intervals *)
  let taken = List.filter_map (fun (start, stop, r) ->
      match Hashtbl.find_opt where r with Some (Register p) -> Some (start, stop, p) | _ -> None) all in
  (* pass 2: everything else, in interval order.  A temporary gets a
     register free over its whole interval or is spilled; a variable that
     lost its register in pass 1 is spilled here.  Spills are assigned in
     interval order so that slot sharing sees each interval from its start. *)
  let taken = ref taken in
  let busy start stop p = List.exists (fun (s, e, q) -> q = p && s <= stop && start <= e) !taken in
  List.iter (fun (start, stop, r) ->
      let expired_spills, live_spills = List.partition (fun (e, _) -> e < start) !spilled in
      spilled := live_spills;
      List.iter (fun (_, s) -> free_slots := s :: !free_slots) expired_spills;
      match Hashtbl.find_opt where r with
      | Some (Register _) -> () (* a variable placed in pass 1 *)
      | _ ->
          if Hashtbl.mem floats r || Hashtbl.mem is_var r then spill (stop, r)
          else
            match List.find_opt (fun p -> safe start stop p && not (busy start stop p) && not (start < 0 && List.mem p caller_saved)) allocatable with
            | Some p ->
                if not (List.mem p !used) then used := p :: !used;
                Hashtbl.replace where r (Register p);
                taken := (start, stop, p) :: !taken
            | None -> spill (stop, r)) all;
  ignore active;
  if Sys.getenv_opt "OCC_DUMP_ALLOC" = Some f.name then
    List.iter (fun (start, stop, r) ->
        Printf.eprintf "%%%d [%d,%d] -> %s\n" r start stop
          (match Hashtbl.find where r with Register p -> name p | Spill k -> "slot" ^ string_of_int k))
      (intervals f);
  { where; spill_slots = !slots; used = List.sort compare (List.filter (fun p -> List.mem p callee_saved) !used) }
