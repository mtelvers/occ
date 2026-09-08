open Asm

type location = Register of Asm.reg | Spill of int

type assignment = { where : (int, location) Hashtbl.t; spill_slots : int; used : Asm.reg list }

let callee_saved = [ RBX; R12; R13; R14; R15 ]
let caller_saved = [ R8; R9; RSI; RDI ]
let allocatable = callee_saved @ caller_saved

(* The caller-saved registers an instruction's selected code may write,
   besides the scratch registers rax, rcx, rdx, r10 and r11 that are never
   allocated.  Mirrors [Select]. *)
let clobbers (i : Ir.instr) : Asm.reg list =
  match i with
  | Ir.Call _ | Ir.Inline_asm _ -> caller_saved
  | Ir.Memcpy _ | Ir.Memzero _ -> [ RDI; RSI ]
  | Ir.Atomic_rmw _ | Ir.Atomic_cmpxchg _ | Ir.Va_arg _ -> [ RSI ]
  | Ir.Va_arg_aggregate _ -> [ RSI; RDI; R8 ]
  | Ir.Ret (Some (Ir.Rv_aggregate _)) -> [ RDI; RSI ]
  | _ -> []

(* Registers defined and used by an instruction. *)
let regs_of_instr (i : Ir.instr) : int list * int list =
  let u = function Ir.Reg r -> [ r ] | _ -> [] in
  let arg = function Ir.Scalar (_, o) -> u o | Ir.Aggregate a -> u a.addr in
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
  | Ir.Ret (Some (Ir.Rv_scalar (_, o))) -> [], u o
  | Ir.Ret (Some (Ir.Rv_aggregate a)) -> [], u a.addr
  | Ir.Return_address r -> [ r ], []
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

(* Live intervals: (start, stop, register), sorted by start. *)
let intervals (f : Ir.func) =
  let first = Hashtbl.create 64 and last = Hashtbl.create 64 in
  let note r i = if not (Hashtbl.mem first r) then Hashtbl.replace first r i; Hashtbl.replace last r i in
  List.iter (function Ir.P_scalar (_, r) -> note r (-1) | Ir.P_aggregate _ -> ()) f.params;
  List.iteri (fun i ins ->
      let defs, uses = regs_of_instr ins in
      List.iter (fun r -> note r i) defs;
      List.iter (fun r -> note r i) uses) f.body;
  (* A backward jump closes a loop: every register touched inside it must
     stay live over the whole loop, since the next iteration may read it
     before this one's definition in program order. *)
  let labels = Hashtbl.create 32 in
  List.iteri (fun i ins -> match ins with Ir.Label l -> Hashtbl.replace labels l i | _ -> ()) f.body;
  let loops = ref [] in
  List.iteri (fun i ins ->
      let targets = match ins with
        | Ir.Jump l -> [ l ] | Ir.Branch (_, a, b) -> [ a; b ]
        | Ir.Switch (_, _, cases, d) -> d :: List.map snd cases | _ -> [] in
      List.iter (fun l -> match Hashtbl.find_opt labels l with
          | Some j when j <= i -> loops := (j, i) :: !loops
          | _ -> ()) targets) f.body;
  (* Only variables need this: a temporary is defined once and used after
     its definition within the same iteration, so its linear interval is
     already right, and extending it would keep it alive over the whole
     loop for nothing. *)
  let touched = Hashtbl.create 64 in (* register -> positions *)
  List.iteri (fun i ins ->
      let defs, uses = regs_of_instr ins in
      List.iter (fun r -> Hashtbl.add touched r i) (defs @ uses)) f.body;
  List.iter (fun r ->
      if Hashtbl.mem first r then
        List.iter (fun (lo, hi) ->
            if List.exists (fun pos -> pos >= lo && pos <= hi) (Hashtbl.find_all touched r) then begin
              if Hashtbl.find first r > lo then Hashtbl.replace first r lo;
              if Hashtbl.find last r < hi then Hashtbl.replace last r hi
            end) !loops) f.variables;
  List.sort compare (Hashtbl.fold (fun r s acc -> (s, Hashtbl.find last r, r) :: acc) first [])

let allocate (f : Ir.func) : assignment =
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
          (match Hashtbl.find where r with Register p -> (match p with RBX -> "rbx" | R12 -> "r12" | R13 -> "r13" | R14 -> "r14" | R15 -> "r15" | _ -> "?") | Spill k -> "slot" ^ string_of_int k))
      (intervals f);
  { where; spill_slots = !slots; used = List.sort compare (List.filter (fun p -> List.mem p callee_saved) !used) }
