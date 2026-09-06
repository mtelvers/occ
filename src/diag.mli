(** Diagnostics.

    The compiler stops at the first error: constraint violations in the
    sense of C11 5.1.1.3 raise [Error].  Error recovery is a large amount
    of code that teaches nothing about C, so there is none.  Warnings are
    printed and compilation continues. *)

exception Error of Loc.t * string

val error : Loc.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** Format a message and raise [Error].  Never returns. *)

val warning : Loc.t -> ('a, Format.formatter, unit, unit) format4 -> 'a
(** Print [file:line:col: warning: message] on stderr. *)

val report : Loc.t -> string -> unit
(** Print an error in the same format.  Used by the driver when it
    catches [Error] at top level. *)
