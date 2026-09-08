(** Partial linking: `ld -r'.  See the header comment of partial.ml. *)

val link : output:string -> search:string list -> Link.item list -> unit
(** Concatenate the items into another relocatable object at [output]:
    the sections of the same name are joined, the symbol tables merged,
    and every relocation rewritten to its new place and symbol.  Archive
    members are included only when they define a symbol still undefined,
    as in a final link.  Raises [Failure] with a message for a duplicate
    definition or an unsupported input. *)
