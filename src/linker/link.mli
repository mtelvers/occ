(** A static linker for x86-64 ELF.  See the header comment of link.ml
    for the five steps. *)

type item =
  | Object of string    (** a relocatable object file *)
  | Archive of string   (** a static library, or a GNU ld script naming some *)
  | Library of string   (** -lname, found as libname.a on the search path *)

val link : output:string -> entry:string -> search:string list -> item list -> unit
(** Link the items, in order, into a statically linked executable at
    [output] whose entry point is the symbol [entry].  Archive members are
    included only when they define a symbol still undefined.  Raises
    [Failure] with a message for undefined symbols, duplicate definitions,
    overflowing relocations and unsupported inputs. *)
