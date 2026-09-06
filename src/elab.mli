(** Elaboration: [Syntax] to [Typed].

    Declarations become typed symbols (6.7), expressions are typed and
    every implicit conversion of 6.3 is made explicit, constant
    expressions are evaluated (6.6), and initializers are flattened to
    byte offsets (6.7.9).  Constraint violations are reported here, in
    the words of the standard where it has them. *)

val translation_unit : Syntax.translation_unit -> Env.t * Typed.translation_unit
(** The environment is returned for the sizes and layouts [Lower] needs. *)
