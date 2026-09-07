(** Parsing GNU assembler source. *)

val parse : string -> string -> Gas.line list
(** [parse file text] is the statement list of [text], with source line
    numbers; [file] names the input in diagnostics.  Numeric local labels
    are renamed to ordinary ones ("1:" becomes ".Lnum1.k"). *)
