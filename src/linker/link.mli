(** A static linker for x86-64 ELF.  See the header comment of link.ml
    for the five steps. *)

type item =
  | Object of string    (** a relocatable object file *)
  | Archive of string   (** a static library, or a GNU ld script naming some *)
  | Library of string   (** -lname, found on the search path *)
  | Shared of string    (** a shared object: what it offers, not what is in it *)

val read_file : string -> string
(** the whole of a file, as bytes *)

val find_library : ?shared:bool -> string list -> string -> string
(** [find_library search name] is the path of libname.a on the search
    path, or of libname.so before it with [~shared:true].  Raises
    [Failure] if it is not there. *)

val script_items : string -> item list
(** the items a GNU ld script names, for the libm.a that is one *)

val link :
  ?shared:bool -> ?soname:string ->
  output:string -> entry:string option -> search:string list -> item list -> unit
(** Link the items, in order, into a statically linked executable at
    [output] whose entry point is the symbol [entry].  Archive members are
    included only when they define a symbol still undefined.  Raises
    [Failure] with a message for undefined symbols, duplicate definitions,
    overflowing relocations and unsupported inputs.

    With [~shared:true] the output is a shared object instead: ET_DYN,
    starting at address zero, with the tables a dynamic loader reads
    (see dynamic.ml).  It has no entry point, and [~soname] gives the
    name the loader is to record for it. *)
