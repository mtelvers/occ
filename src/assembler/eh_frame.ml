(* Call frame information: .cfi directives to an .eh_frame section
   (DWARF 4 section 6.4, and the Linux Standard Base for the .eh_frame
   variant with its "zR" augmentation).

   Each function's directives become a Frame Description Entry: a list of
   call frame instructions interleaved with DW_CFA_advance_loc, computed
   from the distance between the labels at which the directives appeared.
   All FDEs share one Common Information Entry describing the initial
   state at function entry: the CFA is rsp+8 and the return address is at
   CFA-8.  Signal frames get a second CIE whose augmentation carries "S". *)

open Gas

type frame = {
  start : string;                   (* label at .cfi_startproc *)
  finish : string;                  (* label at .cfi_endproc *)
  signal : bool;
  ops : (string * cfi) list;        (* label at which each directive applies *)
}

let dw_eh_pe_pcrel_sdata4 = 0x1b

(* Entries are padded with DW_CFA_nop to 4 bytes, except the last FDE,
   which gas pads to the address size; matching that keeps the sections
   byte-identical. *)
let pad b n = while Buffer.length b mod n <> 0 do Buffer.add_char b '\000' done
let align_up n a = (n + a - 1) / a * a

(* A CIE for the given augmentation; returns its offset in the section. *)
let cie out signal =
  let start = Buffer.length out in
  let b = Buffer.create 32 in
  Buffer.add_char b '\001';                         (* version *)
  Buffer.add_string b (if signal then "zRS\000" else "zR\000");
  Leb.uleb b 1;                                     (* code alignment factor *)
  Leb.sleb b (-8);                                  (* data alignment factor *)
  Leb.uleb b 16;                                    (* return address register: rip *)
  Leb.uleb b 1;                                     (* augmentation data length *)
  Buffer.add_char b (Char.chr dw_eh_pe_pcrel_sdata4);
  Buffer.add_string b "\x0c\x07\x08";               (* DW_CFA_def_cfa rsp, 8 *)
  Buffer.add_string b "\x90\x01";                   (* DW_CFA_offset rip, cfa-8 *)
  let finish = align_up (start + 4 + 4 + Buffer.length b) 4 in
  Leb.u32 out (finish - start - 4);                 (* length: everything after this field *)
  Leb.u32 out 0;                                    (* CIE id *)
  Buffer.add_buffer out b;
  pad out 4;
  start

(* the call frame instructions of one frame *)
let instructions ~offset f =
  let b = Buffer.create 32 in
  let loc = ref (offset f.start) and cfa = ref 8 and stack = ref [] in
  let advance pos =
    let d = pos - !loc in
    if d < 0 then failwith "cfi directive before its function";
    if d > 0 then begin
      if d < 0x40 then Buffer.add_char b (Char.chr (0x40 lor d))
      else if d < 0x100 then begin Buffer.add_char b '\002'; Buffer.add_char b (Char.chr d) end
      else if d < 0x10000 then begin Buffer.add_char b '\003'; Leb.u16 b d end
      else begin Buffer.add_char b '\004'; Leb.u32 b d end
    end;
    loc := pos in
  let saved_at reg off =
    (* DW_CFA_offset with the factored offset when it is a non-negative
       multiple of the data alignment, else the signed extended form *)
    if off mod 8 <> 0 then failwith "cfi offset is not a multiple of 8";
    let factored = off / -8 in
    if factored >= 0 && reg < 64 then begin Buffer.add_char b (Char.chr (0x80 lor reg)); Leb.uleb b factored end
    else begin Buffer.add_char b '\x11'; Leb.uleb b reg; Leb.sleb b factored end in
  List.iter (fun (label, op) ->
      (match op with Cfi_startproc _ | Cfi_endproc | Cfi_signal_frame -> () | _ -> advance (offset label));
      match op with
      | Cfi_startproc _ | Cfi_endproc | Cfi_signal_frame -> ()
      | Cfi_def_cfa (r, off) -> Buffer.add_char b '\x0c'; Leb.uleb b r; Leb.uleb b off; cfa := off
      | Cfi_def_cfa_register r -> Buffer.add_char b '\x0d'; Leb.uleb b r
      | Cfi_def_cfa_offset n -> Buffer.add_char b '\x0e'; Leb.uleb b n; cfa := n
      | Cfi_adjust_cfa_offset n -> cfa := !cfa + n; Buffer.add_char b '\x0e'; Leb.uleb b !cfa
      | Cfi_offset (r, off) -> saved_at r off
      | Cfi_rel_offset (r, off) -> saved_at r (off - !cfa)
      | Cfi_restore r -> if r < 64 then Buffer.add_char b (Char.chr (0xc0 lor r)) else begin Buffer.add_char b '\x06'; Leb.uleb b r end
      | Cfi_same_value r -> Buffer.add_char b '\x08'; Leb.uleb b r
      | Cfi_undefined r -> Buffer.add_char b '\x07'; Leb.uleb b r
      | Cfi_register (r1, r2) -> Buffer.add_char b '\x09'; Leb.uleb b r1; Leb.uleb b r2
      | Cfi_remember_state -> Buffer.add_char b '\x0a'; stack := !cfa :: !stack
      | Cfi_restore_state ->
          Buffer.add_char b '\x0b';
          (match !stack with c :: rest -> cfa := c; stack := rest | [] -> failwith ".cfi_restore_state without .cfi_remember_state")
      | Cfi_escape bytes ->
          List.iter (fun e ->
              match Encode.const e with
              | Some v -> Buffer.add_char b (Char.chr (Int64.to_int v land 0xff))
              | None -> failwith ".cfi_escape needs constants") bytes) f.ops;
  Buffer.contents b

(* The section: FDEs in order, each preceded by its CIE the first time
   that CIE is needed (as gas does), with a fixup for each pc_begin. *)
let generate ~offset frames =
  let out = Buffer.create 256 in
  let fixups = ref [] in
  let cies = ref [] in
  let n = List.length frames in
  List.iteri (fun i f ->
      let cie_off = match List.assoc_opt f.signal !cies with
        | Some c -> c
        | None -> let c = cie out f.signal in cies := (f.signal, c) :: !cies; c in
      let body = instructions ~offset f in
      let align = if i = n - 1 then 8 else 4 in
      let start = Buffer.length out in
      let finish = align_up (start + 4 + 4 + 4 + 4 + 1 + String.length body) align in   (* length, cie pointer, pc_begin, pc_range, aug length, body *)
      Leb.u32 out (finish - start - 4);
      let here = Buffer.length out in
      Leb.u32 out (here - cie_off);                     (* CIE pointer: distance back to the CIE *)
      fixups := { Encode.at = Buffer.length out; size = 4; target = Sym (f.start, None); pcrel = true;
                  pcbase = Buffer.length out; signed = true; relaxable = false; branch = false } :: !fixups;
      Leb.u32 out 0;                                    (* pc_begin, pc-relative *)
      Leb.u32 out (offset f.finish - offset f.start);   (* pc_range *)
      Leb.uleb out 0;                                   (* augmentation data length *)
      Buffer.add_string out body;
      pad out align) frames;
  Buffer.contents out, List.rev !fixups
