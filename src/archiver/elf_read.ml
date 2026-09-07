(* Reading what an archive needs from an ELF64 relocatable object: the
   names of the symbols it defines for other objects to use (ELF
   specification 1.2, sections "ELF header", "Section header" and
   "Symbol table").  The writer of these files is Elf; this is its
   inverse for the one table the archiver indexes. *)

let u16 s off = Char.code s.[off] lor (Char.code s.[off + 1] lsl 8)
let u32 s off = u16 s off lor (u16 s (off + 2) lsl 16)
let u64 s off = u32 s off lor (u32 s (off + 4) lsl 32)   (* fits: file offsets are small *)

let is_elf64 s =
  String.length s >= 64 && String.sub s 0 4 = "\x7fELF" && s.[4] = '\002' && s.[5] = '\001'

(* the string at [off] in a NUL-terminated string table *)
let cstring s off =
  match String.index_from_opt s off '\000' with
  | Some e -> String.sub s off (e - off)
  | None -> String.sub s off (String.length s - off)

(* The exported symbols, in symbol table order: global, weak or
   GNU-unique symbols that are defined, including common ones, and
   excluding section and file entries.  These are what a linker looks up
   an archive member by, so they are what the archive index lists. *)
let exported_symbols s =
  if not (is_elf64 s) then []
  else begin
    let shoff = u64 s 0x28 and shentsize = u16 s 0x3a and shnum = u16 s 0x3c in
    let section i = shoff + i * shentsize in
    let result = ref [] in
    for i = 0 to shnum - 1 do
      let sh = section i in
      if u32 s (sh + 4) = 2 then begin   (* SHT_SYMTAB *)
        let off = u64 s (sh + 0x18) and size = u64 s (sh + 0x20) and link = u32 s (sh + 0x28) in
        let strtab = u64 s (section link + 0x18) in
        let n = size / 24 in
        for k = 1 to n - 1 do
          let e = off + k * 24 in
          let info = Char.code s.[e + 4] in
          let bind = info lsr 4 and typ = info land 0xf in
          let shndx = u16 s (e + 6) in
          if (bind = 1 || bind = 2 || bind = 10) && shndx <> 0 && typ <> 3 && typ <> 4 then
            result := cstring s (strtab + u32 s e) :: !result
        done
      end
    done;
    List.rev !result
  end
