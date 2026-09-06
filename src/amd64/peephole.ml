open Asm

(* Frame slots read by the body, with a count. *)
let slot_reads (body : instr list) =
  let reads = Hashtbl.create 64 in
  let note = function
    | Mem (RBP, off) -> Hashtbl.replace reads off (1 + Option.value (Hashtbl.find_opt reads off) ~default:0)
    | _ -> () in
  List.iter (fun i ->
      match i with
      | Mov (_, src, _) | Movsx (_, _, src, _) | Movzx (_, _, src, _) | Lea (src, _) | Sse (_, src, _) -> note src
      | Alu (m, _, a, b) -> note a; if m <> "mov" then note b
      | Unary (_, _, a) | Shift (_, _, _, a) | Idiv (_, a) | Div (_, a) | Push a | Call a | Xchg (_, _, a) -> note a
      | Lock (Alu (_, _, a, b)) -> note a; note b
      | _ -> ()) body;
  (* stores into a slot are not reads; but an ALU or SSE instruction whose
     destination is a slot reads it as well, so those count above *)
  reads

let func (f : func) : func =
  let reads = slot_reads f.body in
  let rec go = function
    (* store then reload of the same slot at the same width *)
    | Mov (w, Reg r, Mem (RBP, o)) :: Mov (w', Mem (RBP, o'), Reg r') :: rest when o = o' && w = w' ->
        let single = Hashtbl.find_opt reads o = Some 1 in
        let copy = if r = r' then [] else [ Mov (w, Reg r, Reg r') ] in
        if single then go (copy @ rest) else Mov (w, Reg r, Mem (RBP, o)) :: go (copy @ rest)
    | Sse (m, Reg r, Mem (RBP, o)) :: Sse (m', Mem (RBP, o'), Reg r') :: rest
      when o = o' && m = m' && (m = "movsd" || m = "movss") ->
        let single = Hashtbl.find_opt reads o = Some 1 in
        let copy = if r = r' then [] else [ Sse (m, Reg r, Reg r') ] in
        if single then go (copy @ rest) else Sse (m, Reg r, Mem (RBP, o)) :: go (copy @ rest)
    (* a 64-bit register copied to itself does nothing; a 32-bit one
       zero-extends and must stay *)
    | Mov (Q, Reg r, Reg r') :: rest when r = r' -> go rest
    (* a value copied to a register and straight back: the copy back is redundant *)
    | Mov (Q, Reg a, Reg b) :: Mov (Q, Reg b', Reg a') :: rest when a = a' && b = b' && a <> b ->
        go (Mov (Q, Reg a, Reg b) :: rest)
    | i :: rest -> i :: go rest
    | [] -> [] in
  { f with body = go f.body }
