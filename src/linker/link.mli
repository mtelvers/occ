(** A static linker for x86-64 ELF.  See the header comment of link.ml
    for the five steps. *)

type item =
  | Object of string    (** a relocatable object file *)
  | Archive of string   (** a static library, or a GNU ld script naming some *)
  | Library of string   (** -lname, found on the search path *)
  | Shared of string    (** a shared object: what it offers, not what is in it *)
  | Named of string     (** a name a linker script gave, to look for on the path *)
  | Named_shared of string  (** the same, for a shared object *)

val read_file : string -> string
(** the whole of a file, as bytes *)

val find_library : ?shared:bool -> string list -> string -> string
(** [find_library search name] is the path of libname.a on the search
    path, or of libname.so before it with [~shared:true].  Raises
    [Failure] if it is not there. *)

val script_items : string -> item list
(** the items a GNU ld script names, for the libm.a that is one *)

val link :
  ?shared:bool -> ?soname:string -> ?export_all:bool -> ?prefer_shared:bool -> ?rpath:string ->
  output:string -> entry:string option -> search:string list -> item list -> unit
(** Link the items, in order, into a statically linked executable at
    [output] whose entry point is the symbol [entry].  Archive members are
    included only when they define a symbol still undefined.  Raises
    [Failure] with a message for undefined symbols, duplicate definitions,
    overflowing relocations and unsupported inputs.

    With [~shared:true] the output is a shared object instead: ET_DYN,
    starting at address zero, with the tables a dynamic loader reads
    (see dynamic.ml).  It has no entry point, and [~soname] gives the
    name the loader is to record for it.

    An executable linked against a shared object gets those tables too,
    and a loader to read them.  [~export_all] (ld's -E) puts every
    global into .dynsym, which is what an executable that loads objects
    expecting to bind back to it needs; [~prefer_shared] makes -lname
    look for libname.so before libname.a, as ld does unless it is
    linking statically; [~rpath] is where the loader is to look for what
    it needs. *)
