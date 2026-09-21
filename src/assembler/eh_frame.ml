(* Call frame information: .cfi directives to an .eh_frame section
   (DWARF 4 section 6.4, and the Linux Standard Base for the .eh_frame
   variant with its "zR" augmentation).

   Each function's directives become a Frame Description Entry: a list of
   call frame instructions interleaved with DW_CFA_advance_loc, computed
   from the distance between the labels at which the directives appeared.
   All FDEs share one Common Information Entry describing the initial
   state at function entry, which is the machine's: on x86-64 the call
   pushed the return address, so the CFA is rsp+8 and the address is at
   CFA-8; on RISC-V nothing was pushed -- the CFA is sp and the return
   address is in ra -- so the entry says only which register the CFA is.
   Signal frames get a second CIE whose augmentation carries "S". *)

open Gas

type frame = {
  start : string;                   (* label at .cfi_startproc *)
  finish : string;                  (* label at .cfi_endproc *)
  signal : bool;
  ops : (string * cfi) list;        (* label at which each directive applies *)
}

let dw_eh_pe_pcrel_sdata4 = 0x1b

(* What the machine puts in the CIE.  The data alignment factor is the
   unit the register-save offsets are counted in, which is the size of
   what a push or a store moves: eight bytes on x86-64, four on RISC-V.
   The version and the initial instructions are what each machine's gas
   writes, measured. *)
type machine_cie = { version : char; data_align : int; ret_column : int; initial : string }

let machine_cie () =
  match !Target.machine with
  | Target.Amd64 ->
      { version = '\001'; data_align = -8; ret_column = 16;
        initial = "\x0c\x07\x08" (* DW_CFA_def_cfa rsp, 8 *) ^ "\x90\x01" (* DW_CFA_offset rip, cfa-8 *) }
  | Target.Riscv64 ->
      { version = '\003'; data_align = -4; ret_column = 1;
        initial = "\x0d\x02" (* DW_CFA_def_cfa_register sp *) }

(* Entries are padded with DW_CFA_nop to 4 bytes, except the last FDE,
   which gas pads to the address size; matching that keeps the sections
   byte-identical. *)
let pad b n = while Buffer.length b mod n <> 0 do Buffer.add_char b '\000' done
let align_up n a = (n + a - 1) / a * a

(* A CIE for the given augmentation; returns its offset in the section. *)
let cie out signal =
  let start = Buffer.length out in
  let m = machine_cie () in
  let b = Buffer.create 32 in
  Buffer.add_char b m.version;
  Buffer.add_string b (if signal then "zRS\000" else "zR\000");
  Leb.uleb b 1;                                     (* code alignment factor *)
  Leb.sleb b m.data_align;
  Leb.uleb b m.ret_column;                          (* the return address's register *)
  Leb.uleb b 1;                                     (* augmentation data length *)
  Buffer.add_char b (Char.chr dw_eh_pe_pcrel_sdata4);
  Buffer.add_string b m.initial;
  let finish = align_up (start + 4 + 4 + Buffer.length b) 4 in
  Leb.u32 out (finish - start - 4);                 (* length: everything after this field *)
  Leb.u32 out 0;                                    (* CIE id *)
  Buffer.add_buffer out b;
  pad out 4;
  start

(* the call frame instructions of one frame *)
let instructions ~offset f =
  let b = Buffer.create 32 in
  (* The canonical frame address's offset at entry, which is where
     ".cfi_adjust_cfa_offset" counts from: on x86-64 the call pushed the
     return address, so it is already eight bytes past the stack
     pointer, and on RISC-V nothing was pushed. *)
  let loc = ref (offset f.start)
  and cfa = ref (match !Target.machine with Target.Amd64 -> 8 | Target.Riscv64 -> 0)
  and stack = ref [] in
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
  (* The frame address's offset from the stack pointer.  It is normally
     positive and DW_CFA_def_cfa_offset takes it unsigned -- but a
     function with two exits adjusts down once per exit while adjusting
     up once, so the running offset can go below zero, and then the
     signed factored form is the one that can say it.  gas writes
     DW_CFA_def_cfa_offset_sf there too; OCaml's runtime/riscv.S is where
     this turns up. *)
  let def_cfa_offset n =
    let align = (machine_cie ()).data_align in
    if n >= 0 && n mod align = 0 then begin Buffer.add_char b '\x0e'; Leb.uleb b n end
    else begin
      (* DW_CFA_def_cfa_offset_sf, whose operand is factored by the data
         alignment (DWARF 4, 6.4.2.2) *)
      Buffer.add_char b '\x13'; Leb.sleb b (n / align)
    end in
  let saved_at reg off =
    (* DW_CFA_offset with the factored offset when it is a non-negative
       multiple of the data alignment, else the signed extended form *)
    if off mod 8 <> 0 then failwith "cfi offset is not a multiple of 8";
    let factored = off / (machine_cie ()).data_align in
    if factored >= 0 && reg < 64 then begin Buffer.add_char b (Char.chr (0x80 lor reg)); Leb.uleb b factored end
    else begin Buffer.add_char b '\x11'; Leb.uleb b reg; Leb.sleb b factored end in
  List.iter (fun (label, op) ->
      (match op with Cfi_startproc _ | Cfi_endproc | Cfi_signal_frame -> () | _ -> advance (offset label));
      match op with
      | Cfi_startproc _ | Cfi_endproc | Cfi_signal_frame -> ()
      | Cfi_def_cfa (r, off) -> Buffer.add_char b '\x0c'; Leb.uleb b r; Leb.uleb b off; cfa := off
      | Cfi_def_cfa_register r -> Buffer.add_char b '\x0d'; Leb.uleb b r
      | Cfi_def_cfa_offset n -> def_cfa_offset n; cfa := n
      | Cfi_adjust_cfa_offset n -> cfa := !cfa + n; def_cfa_offset !cfa
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
      fixups := Fixup.make ~at:(Buffer.length out) ~size:4 ~pcrel:true
                  ~pcbase:(Buffer.length out) ~signed:true (Sym (f.start, None)) :: !fixups;
      Leb.u32 out 0;                                    (* pc_begin, pc-relative *)
      Leb.u32 out (offset f.finish - offset f.start);   (* pc_range *)
      Leb.uleb out 0;                                   (* augmentation data length *)
      Buffer.add_string out body;
      pad out align) frames;
  Buffer.contents out, List.rev !fixups
