(** The ar archive format, GNU variant, as used for static libraries.
    See the header comment of ar.ml. *)

type member = {
  name : string;     (** the file name without directories *)
  body : string;
}

val read : string -> member list
(** The members of an archive's bytes, in order, without the index
    members.  Raises [Failure] on a malformed file. *)

val write : member list -> string
(** An archive of the members with a fresh symbol index, byte for byte
    what GNU ar produces in deterministic mode. *)
