(** The assembler: GNU as syntax (x86-64, AT&T) to an ELF relocatable
    object.  See the header comment of assemble.ml for the four passes. *)

val run : string -> string -> string
(** [run name text] assembles the source [text] (named [name] in
    diagnostics) and returns the bytes of the object file.  Raises
    [Diag.Error] on the first problem. *)

val files : string list -> string -> unit
(** [files inputs output] assembles the concatenation of [inputs] into the
    file [output], as [as inputs -o output] would. *)
