(* Line number information: .file and .loc directives to a .debug_line
   section (DWARF 3 section 6.2; the version 3 table is read by every
   consumer and needs no DWARF 5 directory tables).

   The line program is a little byte-coded machine: a state (address,
   file, line, column) is advanced by opcodes, and DW_LNS_copy appends a
   row to the table.  A "special opcode" advances both address and line
   in one byte when the deltas are small, which is the common case. *)

open Gas

type entry = { at : string; file : int; line : int; col : int }   (* label and position *)

type sequence = { entries : entry list; finish : string }         (* one per code section *)

let line_base = -5 and line_range = 14 and opcode_base = 13

(* The file table splits each name into a directory, numbered by first
   appearance, and a base name; a name with no directory uses entry 0. *)
let header files =
  let last = List.fold_left (fun m (n, _) -> max m n) 0 files in
  let dirs = ref [] in
  let entries = List.init last (fun i ->
      let name = Option.value (List.assoc_opt (i + 1) files) ~default:"" in
      let dir = Filename.dirname name in
      if String.contains name '/' then begin
        if not (List.mem dir !dirs) then dirs := !dirs @ [ dir ];
        Filename.basename name, 1 + Option.get (List.find_index (( = ) dir) !dirs)
      end else name, 0) in
  let b = Buffer.create 64 in
  Buffer.add_char b '\001';                  (* minimum_instruction_length *)
  Buffer.add_char b '\001';                  (* default_is_stmt *)
  Buffer.add_char b (Char.chr (line_base land 0xff));
  Buffer.add_char b (Char.chr line_range);
  Buffer.add_char b (Char.chr opcode_base);
  List.iter (fun n -> Buffer.add_char b (Char.chr n)) [ 0; 1; 1; 1; 1; 0; 0; 0; 1; 0; 0; 1 ];   (* standard_opcode_lengths *)
  List.iter (fun d -> Buffer.add_string b d; Buffer.add_char b '\000') !dirs;   (* include_directories *)
  Buffer.add_char b '\000';
  List.iter (fun (base, dir) ->
      Buffer.add_string b base;
      Buffer.add_char b '\000';
      Leb.uleb b dir; Leb.uleb b 0; Leb.uleb b 0   (* directory, mtime, length *)) entries;
  Buffer.add_char b '\000';
  Buffer.contents b

(* Advance the address and line by the given deltas and append a row.
   This follows gas's choice of encodings exactly: a special opcode when
   the deltas allow, DW_LNS_const_add_pc to extend its reach, and the
   standard advance opcodes otherwise. *)
let max_special_addr_delta = (255 - opcode_base) / line_range

let inc_line_addr b line_delta addr_delta =
  let tmp = ref (line_delta - line_base) and need_copy = ref false and line_delta = ref line_delta in
  if !tmp >= line_range || !tmp < 0 then begin
    Buffer.add_char b '\003'; Leb.sleb b !line_delta;      (* DW_LNS_advance_line *)
    line_delta := 0; tmp := 0 - line_base; need_copy := true
  end;
  if !line_delta = 0 && addr_delta = 0 then Buffer.add_char b '\001'   (* DW_LNS_copy *)
  else begin
    tmp := !tmp + opcode_base;
    let special = !tmp + addr_delta * line_range in
    let extended = !tmp + (addr_delta - max_special_addr_delta) * line_range in
    if addr_delta < 256 + max_special_addr_delta && special <= 255 then Buffer.add_char b (Char.chr special)
    else if addr_delta < 256 + max_special_addr_delta && extended <= 255 then begin
      Buffer.add_char b '\008'; Buffer.add_char b (Char.chr extended)   (* DW_LNS_const_add_pc *)
    end else begin
      Buffer.add_char b '\002'; Leb.uleb b addr_delta;     (* DW_LNS_advance_pc *)
      if !need_copy then Buffer.add_char b '\001' else Buffer.add_char b (Char.chr !tmp)
    end
  end

let program ~offset fixups_ref base seq =
  let b = Buffer.create 256 in
  (match seq.entries with
   | [] -> ()
   | first :: _ ->
       let address = ref (offset first.at) and file = ref 1 and line = ref 1 and col = ref 0 in
       List.iteri (fun i e ->
           if e.file <> !file then begin Buffer.add_char b '\004'; Leb.uleb b e.file; file := e.file end;
           if e.col <> !col then begin Buffer.add_char b '\005'; Leb.uleb b e.col; col := e.col end;
           if i = 0 then begin
             (* DW_LNE_set_address to the first row's address *)
             Buffer.add_char b '\000'; Leb.uleb b 9; Buffer.add_char b '\002';
             fixups_ref := { Encode.at = base + Buffer.length b; size = 8; target = Sym (first.at, None); pcrel = false;
                             pcbase = 0; signed = false; relaxable = false; branch = false } :: !fixups_ref;
             Leb.u64 b 0
           end;
           inc_line_addr b (e.line - !line) (offset e.at - !address);
           address := offset e.at; line := e.line) seq.entries;
       let d = offset seq.finish - !address in
       if d = max_special_addr_delta then Buffer.add_char b '\008'
       else if d > 0 then begin Buffer.add_char b '\002'; Leb.uleb b d end;
       Buffer.add_string b "\000\001\001");                (* DW_LNE_end_sequence *)
  Buffer.contents b

let generate ~files ~offset sequences =
  let hdr = header files in
  let fixups = ref [] in
  (* unit_length (4), version (2), header_length (4), then the header and program *)
  let programs_base = 4 + 2 + 4 + String.length hdr in
  let progs = Buffer.create 256 in
  List.iter (fun seq -> Buffer.add_string progs (program ~offset fixups (programs_base + Buffer.length progs) seq)) sequences;
  let out = Buffer.create 512 in
  Leb.u32 out (2 + 4 + String.length hdr + Buffer.length progs);
  Leb.u16 out 3;
  Leb.u32 out (String.length hdr);
  Buffer.add_string out hdr;
  Buffer.add_buffer out progs;
  Buffer.contents out, List.rev !fixups
