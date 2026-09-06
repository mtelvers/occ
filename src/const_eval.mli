(** Constant expressions (6.6).

    An integer constant expression is folded to its value; an address
    constant folds to a symbol plus offset.  Anything else is not a
    constant expression and is reported as such. *)

type value =
  | Int of int64
  | Float of float
  | Addr of Typed.symbol * int64 (* &symbol + offset *)
  | Str of string * Ctype.t * int64 (* string literal, its array type, offset *)

val eval : Env.t -> Typed.expr -> value option
(** [None] if the expression is not constant. *)

val int_const : Env.t -> Typed.expr -> int64
(** Requires an integer constant expression (6.6p6). *)

val to_float : Ctype.t -> int64 -> float
(** The value of an integer of the given type as a float (6.3.1.4p2). *)

val normalise : Ctype.t -> int64 -> int64
(** Reduce a value to the range of an integer type (6.3.1.3), i.e. sign- or
    zero-extend from the type's width; [_Bool] becomes 0 or 1. *)
