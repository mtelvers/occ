(* A minimal compilation unit for files that have line information but no
   .debug_info of their own (DWARF 3 sections 3.1 and 3.3).

   ocamlopt emits .file and .loc but no debugging information entries.  A
   debugger only consults a line table through a compile unit's
   DW_AT_stmt_list, so, like gas, the assembler supplies one: a
   DW_TAG_compile_unit covering the text section, a DW_TAG_subprogram for
   every function symbol with a size, and an address range table.  Names
   go in .debug_str and are referred to with DW_FORM_strp. *)

open Gas

type func = { fname : string; global : bool; fsize : int }

type input = {
  name : string;                (* the source file *)
  comp_dir : string;
  producer : string;
  text_end : string;            (* label at the end of the text section *)
  text_size : int;
  funcs : func list;
  line_label : string;          (* labels at the start of the sections referred to *)
  abbrev_label : string;
  info_label : string;
  str_label : string;
}

(* an absolute reference to a symbol plus offset *)
let fixup at size target = { Encode.at; size; target; pcrel = false; pcbase = 0; signed = false; relaxable = false; branch = false }

let abbrev =
  let b = Buffer.create 64 in
  let attrs code tag children l =
    Leb.uleb b code; Leb.uleb b tag; Buffer.add_char b (if children then '\001' else '\000');
    List.iter (fun (a, f) -> Leb.uleb b a; Leb.uleb b f) l;
    Buffer.add_string b "\000\000" in
  (* 1: compile unit *)
  attrs 1 0x11 true [ 0x10, 0x06; 0x11, 0x01; 0x12, 0x01; 0x03, 0x0e; 0x1b, 0x0e; 0x25, 0x0e; 0x13, 0x05 ];
  (* 2: subprogram: name, external, type, low_pc, high_pc *)
  attrs 2 0x2e false [ 0x03, 0x0e; 0x3f, 0x0c; 0x49, 0x15; 0x11, 0x01; 0x12, 0x01 ];   (* DW_FORM_ref_udata for the type *)
  (* 3: unspecified type *)
  attrs 3 0x3b false [];
  Buffer.add_char b '\000';
  Buffer.contents b

(* .debug_str, and the offset of each string in it *)
let strings i =
  let b = Buffer.create 256 in
  let add s = let off = Buffer.length b in Buffer.add_string b s; Buffer.add_char b '\000'; off in
  let name = add i.name and dir = add i.comp_dir and producer = add i.producer in
  let funcs = List.map (fun f -> add f.fname) i.funcs in
  Buffer.contents b, name, dir, producer, funcs

let info i ~str_offsets:(name_off, dir_off, prod_off, func_offs) =
  let b = Buffer.create 256 in
  let fixups = ref [] in
  let strp off = fixups := fixup (Buffer.length b) 4 (Bin (Add, Sym (i.str_label, None), Num (Int64.of_int off))) :: !fixups; Leb.u32 b 0 in
  let addr e = fixups := fixup (Buffer.length b) 8 e :: !fixups; Leb.u64 b 0 in
  let text_start = Bin (Sub, Sym (i.text_end, None), Num (Int64.of_int i.text_size)) in
  (* header *)
  Leb.u32 b 0;                          (* unit_length, patched below *)
  Leb.u16 b 3;                          (* version *)
  fixups := fixup (Buffer.length b) 4 (Sym (i.abbrev_label, None)) :: !fixups; Leb.u32 b 0;   (* debug_abbrev_offset *)
  Buffer.add_char b '\008';             (* address_size *)
  (* the compile unit *)
  Leb.uleb b 1;
  fixups := fixup (Buffer.length b) 4 (Sym (i.line_label, None)) :: !fixups; Leb.u32 b 0;     (* stmt_list *)
  addr text_start;
  addr (Sym (i.text_end, None));
  strp name_off; strp dir_off; strp prod_off;
  Leb.u16 b 0x8001;                     (* DW_LANG_Mips_Assembler *)
  (* subprograms; the type reference points past them, so its LEB128 size
     is found by trying each size until the offsets agree *)
  let die_size type_ref_size = 1 + 4 + 1 + type_ref_size + 8 + 8 in
  let rec type_offset k =
    let off = Buffer.length b + List.length i.funcs * die_size k in
    let tmp = Buffer.create 4 in Leb.uleb tmp off;
    if Buffer.length tmp = k then off else type_offset (k + 1) in
  let type_off = type_offset 1 in
  List.iter2 (fun f off ->
      Leb.uleb b 2;
      strp off;
      Buffer.add_char b (if f.global then '\001' else '\000');
      Leb.uleb b type_off;
      addr (Sym (f.fname, None));
      addr (Bin (Add, Sym (f.fname, None), Num (Int64.of_int f.fsize)))) i.funcs func_offs;
  assert (Buffer.length b = type_off);
  Leb.uleb b 3;
  Buffer.add_char b '\000';             (* end of the compile unit's children *)
  let bytes = Buffer.to_bytes b in
  Bytes.set_int32_le bytes 0 (Int32.of_int (Bytes.length bytes - 4));
  Bytes.to_string bytes, List.rev !fixups

let aranges i =
  let b = Buffer.create 48 in
  let fixups = ref [] in
  Leb.u32 b 44;                         (* length of what follows *)
  Leb.u16 b 2;                          (* version *)
  fixups := fixup (Buffer.length b) 4 (Sym (i.info_label, None)) :: !fixups; Leb.u32 b 0;
  Buffer.add_char b '\008'; Buffer.add_char b '\000';   (* address size, segment size *)
  Leb.u32 b 0;                          (* padding to a tuple boundary *)
  fixups := fixup (Buffer.length b) 8 (Bin (Sub, Sym (i.text_end, None), Num (Int64.of_int i.text_size))) :: !fixups;
  Leb.u64 b 0;
  Leb.u64 b i.text_size;
  Leb.u64 b 0; Leb.u64 b 0;             (* terminator *)
  Buffer.contents b, List.rev !fixups

(* the four sections: (name, bytes, fixups) *)
let generate i =
  let str, name_off, dir_off, prod_off, func_offs = strings i in
  let info_bytes, info_fixups = info i ~str_offsets:(name_off, dir_off, prod_off, func_offs) in
  let ar_bytes, ar_fixups = aranges i in
  [ ".debug_abbrev", abbrev, [];
    ".debug_info", info_bytes, info_fixups;
    ".debug_aranges", ar_bytes, ar_fixups;
    ".debug_str", str, [] ]
