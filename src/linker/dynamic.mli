(** The tables a dynamic loader reads: .dynsym, .dynstr, .hash,
    .dynamic, and the relocations it applies.  See the header comment of
    dynamic.ml. *)

val hash : string -> int
(** the hash of a symbol name, as the ELF specification defines it *)

type strtab

val strtab : unit -> strtab
val intern : strtab -> string -> int
(** the offset of a name in the table, adding it if it is not there *)

val strtab_contents : strtab -> string

type sym = { nameoff : int; info : int; other : int; shndx : int; value : int; size : int }

val null_sym : sym
val dynsym : sym list -> string

val hash_table : string list -> string
(** the table for finding those names, given in .dynsym order with the
    null entry first *)

val hash_size : int -> int
(** the size [hash_table] will produce for that many symbols *)

type rel = { where : int; rtype : int; rsym : int; addend : int }

(** The loader's relocation types, by what they mean rather than by
    their numbers, which differ between the machines: put an address
    here, bind a symbol's address into the table, add the load address
    to what is here, copy a variable in, put a thread-local's offset
    here. *)
val r_absolute : unit -> int
val r_glob_dat : unit -> int
val r_jump_slot : unit -> int
val r_relative : unit -> int
val r_copy : unit -> int
val r_tpoff : unit -> int

val rela : rel list -> string
val sort_rels : rel list -> rel list
(** the ones naming no symbol first, which is what DT_RELACOUNT counts *)

val dt_null : int
val dt_needed : int
val dt_pltrelsz : int
val dt_pltgot : int
val dt_hash : int
val dt_strtab : int
val dt_symtab : int
val dt_rela : int
val dt_relasz : int
val dt_relaent : int
val dt_strsz : int
val dt_syment : int
val dt_init : int
val dt_fini : int
val dt_soname : int
val dt_rpath : int
val dt_symbolic : int
val dt_pltrel : int
val dt_textrel : int
val dt_jmprel : int
val dt_init_array : int
val dt_fini_array : int
val dt_init_arraysz : int
val dt_fini_arraysz : int
val dt_runpath : int
val dt_flags : int
val dt_relacount : int
val dt_versym : int
val dt_verneed : int
val dt_verneednum : int

val ver_ndx_global : int
(** the index for a name with no version of its own *)

val versym : int list -> string
(** .gnu.version: one index per .dynsym entry, in that order *)

type need = { file : int; versions : (int * int * int) list }
(** an object named in .dynstr, and the versions wanted from it as
    (name offset, hash, index) *)

val verneed : need list -> string
val verneed_size : need list -> int

val dynamic : (int * int) list -> string
(** the entries, with DT_NULL added at the end *)

val dynamic_size : int -> int
