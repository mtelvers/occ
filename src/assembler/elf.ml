(* Writing an ELF64 relocatable object file (ELF specification 1.2, and
   the System V x86-64 ABI supplement chapter 4 for the machine parts).

   A relocatable file is an ELF header, the bodies of the sections, and a
   section header table.  There are no program headers: the linker builds
   those.  The symbol and string tables are ordinary sections whose bodies
   are built here from the assembler's symbol list. *)

(* ---- Constants ------------------------------------------------------------- *)

let sht_progbits = 1 and sht_symtab = 2 and sht_strtab = 3 and sht_rela = 4 and sht_nobits = 8 and sht_note = 7
let shf_write = 0x1 and shf_alloc = 0x2 and shf_execinstr = 0x4 and shf_merge = 0x10 and shf_strings = 0x20
and shf_info_link = 0x40 and shf_tls = 0x400 and shf_group = 0x200

let stb_local = 0 and stb_global = 1 and stb_weak = 2
let stt_notype = 0 and stt_object = 1 and stt_func = 2 and stt_section = 3 and stt_file = 4 and stt_tls = 6
let stv_default = 0 and stv_internal = 1 and stv_hidden = 2 and stv_protected = 3
let shn_undef = 0 and shn_abs = 0xfff1 and shn_common = 0xfff2

(* relocation types (ABI table 4.9) *)
let r_x86_64_64 = 1 and r_x86_64_pc32 = 2 and r_x86_64_plt32 = 4 and r_x86_64_gotpcrel = 9
and r_x86_64_32 = 10 and r_x86_64_32s = 11 and r_x86_64_16 = 12 and r_x86_64_pc16 = 13
and r_x86_64_8 = 14 and r_x86_64_pc8 = 15 and r_x86_64_gottpoff = 22 and r_x86_64_tpoff32 = 23
and r_x86_64_pc64 = 24 and r_x86_64_gotpcrelx = 41 and r_x86_64_rex_gotpcrelx = 42

(* ---- Little-endian encoding ------------------------------------------------ *)

let add_u8 b v = Buffer.add_char b (Char.chr (v land 0xff))
let add_u16 b v = add_u8 b v; add_u8 b (v lsr 8)
let add_u32 b v = add_u16 b (v land 0xffff); add_u16 b ((v lsr 16) land 0xffff)
let add_u64 b v = for i = 0 to 7 do add_u8 b (Int64.to_int (Int64.shift_right_logical v (8 * i))) done
let add_int b v = add_u64 b (Int64.of_int v)

(* ---- Sections -------------------------------------------------------------- *)

type section = {
  name : string;
  typ : int;
  flags : int;
  link : int;          (* sh_link: for .symtab the .strtab index; for .rela the .symtab index *)
  info : int;          (* sh_info: for .rela the target section; for .symtab the first global *)
  align : int;
  entsize : int;
  body : string;       (* file content; empty for SHT_NOBITS *)
  size : int;          (* sh_size, which for SHT_NOBITS exceeds the body *)
}

let section ?(link = 0) ?(info = 0) ?(entsize = 0) ?(size = -1) ~typ ~flags ~align name body =
  { name; typ; flags; link; info; align; entsize; body; size = (if size < 0 then String.length body else size) }

(* ---- Symbol table entries (Elf64_Sym, 24 bytes) ------------------------------ *)

type symbol = {
  sname : string;
  bind : int;
  stype : int;
  other : int;         (* visibility *)
  shndx : int;
  value : int64;
  ssize : int64;
}

(* a string table: NUL, then each name NUL-terminated; returns the offsets *)
let string_table names =
  let b = Buffer.create 256 in
  Buffer.add_char b '\000';
  let offsets = List.map (fun n ->
      if n = "" then 0
      else begin let off = Buffer.length b in Buffer.add_string b n; Buffer.add_char b '\000'; off end) names in
  Buffer.contents b, offsets

let symtab_body symbols =
  let strtab, offsets = string_table (List.map (fun s -> s.sname) symbols) in
  let b = Buffer.create (24 * (List.length symbols + 1)) in
  Buffer.add_string b (String.make 24 '\000');   (* the null symbol *)
  List.iter2 (fun s off ->
      add_u32 b off;
      add_u8 b ((s.bind lsl 4) lor s.stype);
      add_u8 b s.other;
      add_u16 b s.shndx;
      add_u64 b s.value;
      add_u64 b s.ssize) symbols offsets;
  Buffer.contents b, strtab

(* ---- Relocation entries (Elf64_Rela, 24 bytes) -------------------------------- *)

type reloc = { offset : int; rtype : int; sym : int; addend : int64 }

let rela_body relocs =
  let b = Buffer.create (24 * List.length relocs) in
  List.iter (fun r ->
      add_int b r.offset;
      add_u64 b (Int64.logor (Int64.shift_left (Int64.of_int r.sym) 32) (Int64.of_int r.rtype));
      add_u64 b r.addend) relocs;
  Buffer.contents b

(* ---- The file ---------------------------------------------------------------- *)

let align_to n a = if a <= 1 then n else (n + a - 1) / (a - 1 + 1) * a

(* [sections] excludes the null section 0 and the section name table, both
   added here; [shstrndx] is filled in.  Section i in the list has index
   i + 1 in the file. *)
let write sections =
  let names = List.map (fun s -> s.name) sections @ [ ".shstrtab" ] in
  let shstrtab, name_offsets = string_table names in
  let sections = sections @ [ section ~typ:sht_strtab ~flags:0 ~align:1 ".shstrtab" shstrtab ] in
  let out = Buffer.create 65536 in
  (* ELF header: 64 bytes *)
  Buffer.add_string out "\x7fELF\x02\x01\x01\x00";   (* magic, 64-bit, little-endian, version 1, System V *)
  Buffer.add_string out (String.make 8 '\000');
  add_u16 out 1;          (* e_type: ET_REL *)
  add_u16 out 62;         (* e_machine: EM_X86_64 *)
  add_u32 out 1;          (* e_version *)
  add_u64 out 0L;         (* e_entry *)
  add_u64 out 0L;         (* e_phoff *)
  let shoff_pos = Buffer.length out in
  add_u64 out 0L;         (* e_shoff: patched below *)
  add_u32 out 0;          (* e_flags *)
  add_u16 out 64;         (* e_ehsize *)
  add_u16 out 0; add_u16 out 0;   (* e_phentsize, e_phnum *)
  add_u16 out 64;         (* e_shentsize *)
  add_u16 out (List.length sections + 1);   (* e_shnum *)
  add_u16 out (List.length sections);       (* e_shstrndx: the last one *)
  (* section bodies, each aligned *)
  let offsets = List.map (fun s ->
      let pad = align_to (Buffer.length out) s.align - Buffer.length out in
      Buffer.add_string out (String.make pad '\000');
      let off = Buffer.length out in
      if s.typ <> sht_nobits then Buffer.add_string out s.body;
      off) sections in
  (* section header table *)
  let pad = align_to (Buffer.length out) 8 - Buffer.length out in
  Buffer.add_string out (String.make pad '\000');
  let shoff = Buffer.length out in
  Buffer.add_string out (String.make 64 '\000');   (* the null section header *)
  List.iteri (fun i s ->
      add_u32 out (List.nth name_offsets i);
      add_u32 out s.typ;
      add_int out s.flags;
      add_u64 out 0L;                       (* sh_addr *)
      add_int out (List.nth offsets i);     (* sh_offset *)
      add_int out s.size;
      add_u32 out s.link;
      add_u32 out s.info;
      add_int out s.align;
      add_int out s.entsize) sections;
  let bytes = Buffer.to_bytes out in
  Bytes.set_int64_le bytes shoff_pos (Int64.of_int shoff);
  Bytes.to_string bytes
