(* The tables a dynamic loader reads (ELF specification 1.2, "Dynamic
   linking"; System V x86-64 ABI supplement chapter 5).

   A statically linked executable needs none of this: every address is
   known when it is linked.  A shared object, and an executable that
   loads one, need the loader to finish the job, and these are what it
   reads to do so:

     .dynsym   the symbols the object offers and the ones it wants,
               in the same format as .symtab
     .dynstr   their names
     .hash     a table for finding a name in .dynsym
     .dynamic  where all of the above are, and what else to load
     .rela.dyn the places the loader must put an address into
     .rela.plt the same for the entries of the procedure linkage table

   Two hash tables are in use: the original one here, which DT_HASH
   names, and GNU's, which DT_GNU_HASH names.  A loader that finds only
   DT_HASH uses it, so this emits only that one; it is a few lines
   rather than a few hundred, and the cost is a slower first lookup of
   each symbol.  Symbol versions (DT_VERNEED, DT_VERSYM) are likewise
   left out: without them the loader binds each name to the default
   version, which is what a program that names no versions wants. *)

(* The hash of a symbol name, as the specification defines it.  Every
   loader computes this the same way, so the function is fixed and not a
   choice. *)
let hash name =
  let h = ref 0 in
  String.iter (fun c ->
      h := ((!h lsl 4) + Char.code c) land 0xffffffff;
      let g = !h land 0xf0000000 in
      if g <> 0 then h := !h lxor (g lsr 24);
      h := !h land (lnot g) land 0xffffffff)
    name;
  !h

(* ---- writing the little-endian words these tables are made of ---- *)

let u32 b v = Buffer.add_int32_le b (Int32.of_int (v land 0xffffffff))
let u64 b v = Buffer.add_int64_le b (Int64.of_int v)

(* ---- .dynstr ---- *)

(* A string table and the offset of each name in it.  The empty name is
   at offset 0, which is what a symbol with no name refers to. *)
type strtab = { text : Buffer.t; offsets : (string, int) Hashtbl.t }

let strtab () =
  let b = Buffer.create 256 in
  Buffer.add_char b '\000';
  { text = b; offsets = Hashtbl.create 256 }

let intern st name =
  if name = "" then 0
  else
    match Hashtbl.find_opt st.offsets name with
    | Some o -> o
    | None ->
        let o = Buffer.length st.text in
        Buffer.add_string st.text name;
        Buffer.add_char st.text '\000';
        Hashtbl.replace st.offsets name o;
        o

let strtab_contents st = Buffer.contents st.text

(* ---- .dynsym ---- *)

(* One entry.  [name] is an offset into .dynstr, filled in by the
   caller; the rest is as in .symtab. *)
type sym = { nameoff : int; info : int; other : int; shndx : int; value : int; size : int }

let null_sym = { nameoff = 0; info = 0; other = 0; shndx = 0; value = 0; size = 0 }

let dynsym syms =
  let b = Buffer.create (24 * List.length syms) in
  List.iter (fun s ->
      u32 b s.nameoff;
      Buffer.add_char b (Char.chr (s.info land 0xff));
      Buffer.add_char b (Char.chr (s.other land 0xff));
      Buffer.add_int16_le b (s.shndx land 0xffff);
      u64 b s.value;
      u64 b s.size)
    syms;
  Buffer.contents b

(* ---- .hash ---- *)

(* The table has a bucket per hash value modulo the bucket count and a
   chain per symbol: bucket[h mod n] is the first symbol with that hash,
   and chain[i] the next one after symbol i.  The names are given in the
   order they appear in .dynsym, index 0 being the null entry, which is
   in no chain. *)
let hash_table names =
  let n = List.length names in                    (* including index 0 *)
  (* one bucket per few symbols, which is what the linkers settle on *)
  let nbucket = max 1 (n / 4) in
  let buckets = Array.make nbucket 0 in
  let chain = Array.make (max n 1) 0 in
  (* built from the last symbol back, so that each bucket ends up
     holding the first of its chain and the order within a chain is the
     order in the table *)
  let names = Array.of_list names in
  for i = n - 1 downto 1 do
    let h = hash names.(i) mod nbucket in
    chain.(i) <- buckets.(h);
    buckets.(h) <- i
  done;
  let b = Buffer.create (8 + 4 * (nbucket + n)) in
  u32 b nbucket;
  u32 b n;
  Array.iter (fun v -> u32 b v) buckets;
  Array.iter (fun v -> u32 b v) chain;
  Buffer.contents b

(* the size the table will have, needed before the names are known *)
let hash_size n = 8 + 4 * (max 1 (n / 4)) + 4 * n

(* ---- .rela.dyn and .rela.plt ---- *)

(* A relocation for the loader: where, what kind, against which symbol
   of .dynsym (0 for none), and the addend.  R_X86_64_RELATIVE takes no
   symbol and its addend is the address as this link left it, which the
   loader adds the load address to. *)
type rel = { where : int; rtype : int; rsym : int; addend : int }

let r_x86_64_64 = 1
let r_x86_64_glob_dat = 6
let r_x86_64_jump_slot = 7
let r_x86_64_relative = 8
let r_x86_64_dtpmod64 = 16
let r_x86_64_dtpoff64 = 17
let r_x86_64_tpoff64 = 18

let rela rels =
  let b = Buffer.create (24 * List.length rels) in
  List.iter (fun r ->
      u64 b r.where;
      u64 b (r.rtype lor (r.rsym lsl 32));
      u64 b r.addend)
    rels;
  Buffer.contents b

(* Relocations that name no symbol come first, and DT_RELACOUNT says how
   many, so that a loader can apply them without looking at each one.
   This is the order GNU ld uses as well. *)
let sort_rels rels =
  let relative, rest = List.partition (fun r -> r.rtype = r_x86_64_relative) rels in
  relative @ rest

(* ---- .dynamic ---- *)

let dt_null = 0 and dt_needed = 1 and dt_pltrelsz = 2 and dt_pltgot = 3
and dt_hash = 4 and dt_strtab = 5 and dt_symtab = 6 and dt_rela = 7
and dt_relasz = 8 and dt_relaent = 9 and dt_strsz = 10 and dt_syment = 11
and dt_init = 12 and dt_fini = 13 and dt_soname = 14 and dt_rpath = 15
and dt_symbolic = 16 and dt_pltrel = 20 and dt_textrel = 22 and dt_jmprel = 23
and dt_init_array = 25 and dt_fini_array = 26 and dt_init_arraysz = 27
and dt_fini_arraysz = 28 and dt_runpath = 29 and dt_flags = 30
and dt_relacount = 0x6ffffff9

let dynamic entries =
  let b = Buffer.create (16 * (List.length entries + 1)) in
  List.iter (fun (tag, value) -> u64 b tag; u64 b value) entries;
  u64 b dt_null; u64 b 0;
  Buffer.contents b

let dynamic_size entries = 16 * (entries + 1)
