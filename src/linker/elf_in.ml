(* Reading ELF64 relocatable objects for the linker (ELF specification
   1.2; System V x86-64 ABI supplement chapter 4).

   An input object contributes sections, symbols and relocations.  The
   reader keeps the raw section bodies and indexes; the linker decides
   what goes where.  Only the little-endian, 64-bit, x86-64 relocatable
   form is accepted, since that is all our tools produce. *)

let u8 s off = Char.code s.[off]
let u16 s off = u8 s off lor (u8 s (off + 1) lsl 8)
let u32 s off = u16 s off lor (u16 s (off + 2) lsl 16)
let u64 s off = u32 s off lor (u32 s (off + 4) lsl 32)
let s64 s off = Int64.to_int (String.get_int64_le s off)

type section = {
  index : int;
  name : string;
  typ : int;
  flags : int;
  addralign : int;
  entsize : int;
  info : int;
  link : int;
  body : string;           (* empty for SHT_NOBITS *)
  size : int;              (* sh_size, meaningful for NOBITS *)
}

type symbol = {
  sname : string;
  bind : int;              (* STB_* *)
  stype : int;             (* STT_* *)
  other : int;
  shndx : int;             (* SHN_UNDEF, SHN_ABS, SHN_COMMON or a section index *)
  value : int;
  ssize : int;
}

type reloc = {
  offset : int;            (* within the section relocated *)
  rtype : int;             (* R_X86_64_* *)
  sym : int;               (* symbol table index *)
  addend : int;
}

type t = {
  file : string;           (* for messages: path, or archive(member) *)
  sections : section array;
  symbols : symbol array;
  relocs : (int * reloc array) list;   (* target section index, entries *)
  groups : (string * int list) list;   (* COMDAT signature, member section indices *)
}

let is_elf s =
  String.length s >= 64 && String.sub s 0 4 = "\x7fELF" && s.[4] = '\002' && s.[5] = '\001'

let is_object s = is_elf s && u16 s 16 = 1          (* ET_REL *)
let is_shared s = is_elf s && u16 s 16 = 3          (* ET_DYN *)

let cstring s off =
  match String.index_from_opt s off '\000' with
  | Some e -> String.sub s off (e - off)
  | None -> String.sub s off (String.length s - off)

(* A shared object as an input.  The linker does not take it apart: it
   reads what the object offers, so that references to those names
   resolve, and the name to record for it, so that the loader knows what
   to load.  That name is the object's own SONAME if it has one, which
   is what makes a library's version travel with the program that used
   it, and otherwise the path as it was given.

   What it offers comes with a version, and that is not a nicety.  A C
   library keeps its old behaviour under an old version name: glibc
   offers realpath@@GLIBC_2.3, which takes a null second argument, and
   realpath@GLIBC_2.2.5, which does not.  A reference that names no
   version is bound by the loader to whichever it finds first, and it
   finds the old one.  So the version each name is offered under is read
   here, and the default one -- the one written @@ -- is what a
   reference to that name means. *)
type provided = {
  psym : symbol;
  pversion : string;        (* the version it is offered under, "" for none *)
  pdefault : bool;          (* and whether that is the default for the name *)
}

type shared = {
  sfile : string;
  soname : string;
  provides : provided array;
}

let read_shared file (s : string) : shared =
  if not (is_shared s) then failwith (file ^ ": not an ELF64 shared object");
  let shoff = u64 s 0x28 and shentsize = u16 s 0x3a and shnum = u16 s 0x3c and shstrndx = u16 s 0x3e in
  let hdr i = shoff + i * shentsize in
  let shstr = u64 s (hdr shstrndx + 0x18) in
  let body i =
    let h = hdr i in
    let typ = u32 s (h + 4) in
    if typ = 8 || typ = 0 then "" else String.sub s (u64 s (h + 0x18)) (u64 s (h + 0x20)) in
  let named name =
    let found = ref None in
    for i = 0 to shnum - 1 do
      if cstring s (shstr + u32 s (hdr i)) = name then found := Some i
    done;
    !found in
  let dynsym = named ".dynsym" and dynstr = named ".dynstr" in
  let strings = match dynstr with Some i -> body i | None -> "" in
  let symbols =
    match dynsym with
    | None -> [||]
    | Some i ->
        let b = body i in
        Array.init (String.length b / 24) (fun k ->
            let e = k * 24 in
            let info = u8 b (e + 4) in
            { sname = cstring strings (u32 b e); bind = info lsr 4; stype = info land 0xf;
              other = u8 b (e + 5); shndx = u16 b (e + 6); value = u64 b (e + 8); ssize = u64 b (e + 16) }) in
  (* The version each symbol is offered under: .gnu.version holds an
     index per symbol into the definitions in .gnu.version_d, with the
     top bit set on a name that is not the default for that symbol. *)
  let versym = match named ".gnu.version" with Some i -> body i | None -> "" in
  let verdef_names =
    match named ".gnu.version_d" with
    | None -> [||]
    | Some i ->
        let b = body i in
        let names = Array.make 64 "" in
        let grow a n = if n < Array.length a then a
          else (let bigger = Array.make (2 * n + 2) "" in Array.blit a 0 bigger 0 (Array.length a); bigger) in
        let table = ref names in
        let rec go off =
          if off + 20 <= String.length b then begin
            let flags = u16 b (off + 2) and ndx = u16 b (off + 4) in
            let aux = u32 b (off + 12) and next = u32 b (off + 16) in
            (* the base entry names the file itself, not a version *)
            if flags land 1 = 0 && off + aux + 4 <= String.length b then begin
              table := grow !table ndx;
              !table.(ndx) <- cstring strings (u32 b (off + aux))
            end;
            if next > 0 then go (off + next)
          end in
        go 0;
        !table in
  let version_of k =
    if 2 * k + 2 > String.length versym then ("", true)
    else
      let v = u16 versym (2 * k) in
      let ndx = v land 0x7fff and hidden = v land 0x8000 <> 0 in
      if ndx < 2 || ndx >= Array.length verdef_names then ("", true)
      else (verdef_names.(ndx), not hidden) in
  let provides =
    Array.mapi (fun k sy ->
        let (pversion, pdefault) = version_of k in
        { psym = sy; pversion; pdefault })
      symbols in
  (* DT_SONAME (tag 14) names an offset in .dynstr *)
  let soname =
    match named ".dynamic" with
    | None -> ""
    | Some i ->
        let b = body i in
        let rec go off =
          if off + 16 > String.length b then ""
          else
            let tag = u64 b off and value = u64 b (off + 8) in
            if tag = 0 then "" else if tag = 14 then cstring strings value else go (off + 16) in
        go 0 in
  { sfile = file; soname = (if soname <> "" then soname else Filename.basename file); provides }

let read file (s : string) : t =
  if not (is_object s) then failwith (file ^ ": not an ELF64 relocatable object");
  if u16 s 18 <> 62 then failwith (file ^ ": not an x86-64 object");
  let shoff = u64 s 0x28 and shentsize = u16 s 0x3a and shnum = u16 s 0x3c and shstrndx = u16 s 0x3e in
  let hdr i = shoff + i * shentsize in
  let shstr = u64 s (hdr shstrndx + 0x18) in
  let sections = Array.init shnum (fun i ->
      let h = hdr i in
      let typ = u32 s (h + 4) and off = u64 s (h + 0x18) and size = u64 s (h + 0x20) in
      { index = i; name = cstring s (shstr + u32 s h); typ; flags = u64 s (h + 8);
        addralign = max 1 (u64 s (h + 0x30)); entsize = u64 s (h + 0x38); info = u32 s (h + 0x2c); link = u32 s (h + 0x28);
        body = (if typ = 8 (* NOBITS *) || typ = 0 then "" else String.sub s off size); size }) in
  let symbols = ref [||] in
  let relocs = ref [] and groups = ref [] in
  Array.iter (fun sec ->
      match sec.typ with
      | 2 ->   (* SHT_SYMTAB *)
          let strtab = sections.(sec.link).body in
          symbols := Array.init (String.length sec.body / 24) (fun k ->
              let e = k * 24 in
              let info = u8 sec.body (e + 4) in
              { sname = cstring strtab (u32 sec.body e); bind = info lsr 4; stype = info land 0xf; other = u8 sec.body (e + 5);
                shndx = u16 sec.body (e + 6); value = u64 sec.body (e + 8); ssize = u64 sec.body (e + 16) })
      | 4 ->   (* SHT_RELA *)
          let entries = Array.init (String.length sec.body / 24) (fun k ->
              let e = k * 24 in
              let info = String.get_int64_le sec.body (e + 8) in
              { offset = u64 sec.body e; rtype = Int64.to_int (Int64.logand info 0xffffffffL);
                sym = Int64.to_int (Int64.shift_right_logical info 32); addend = s64 sec.body (e + 16) }) in
          relocs := (sec.info, entries) :: !relocs
      | 17 ->  (* SHT_GROUP: flags word then member indices; the signature is a symbol *)
          let n = String.length sec.body / 4 in
          let members = List.init (n - 1) (fun k -> u32 sec.body (4 * (k + 1))) in
          if u32 sec.body 0 land 1 = 1 then begin   (* GRP_COMDAT *)
            (* the signature symbol is named by sh_info in the symbol table sh_link *)
            let symtab = sections.(sec.link) in
            let strtab = sections.(symtab.link).body in
            let e = sec.info * 24 in
            groups := (cstring strtab (u32 symtab.body e), members) :: !groups
          end
      | _ -> ()) sections;
  { file; sections; symbols = !symbols; relocs = List.rev !relocs; groups = !groups }
