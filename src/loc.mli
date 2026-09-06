(** Source positions.

    A [t] names a point in a file.  Positions are carried through
    preprocessing by line markers ([# 12 "file.c"], C11 6.10.4), so a
    diagnostic on a preprocessed unit still points at the original file. *)

type t = { file : string; line : int; col : int }

val none : t
(** A position for things with no source, such as compiler-generated names. *)

val pp : Format.formatter -> t -> unit
(** [file:line:col], the format editors and [make] understand. *)
