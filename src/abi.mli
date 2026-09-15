(** Classification of aggregates for parameter passing on x86-64 System V
    (ABI 3.2.3).

    Each eightbyte of a struct or union is INTEGER if it holds any integer
    or pointer, SSE if it holds only floating-point members, and the whole
    object is MEMORY if it is larger than two eightbytes, contains
    unaligned members, or holds a long double.  This is the part of a C
    compiler least covered by textbooks and most needed by the OCaml
    runtime, whose C calls pass structs. *)

val classify : Env.t -> Ctype.t -> Ir.cls list
(** One class per eightbyte, or [[Memory]]. *)
