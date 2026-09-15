(** How an aggregate is passed and returned, which the front end has to
    know because it decides the shape of the IR for a call.

    On x86-64 System V (ABI 3.2.3) each eightbyte of a struct or union
    is INTEGER if it holds any integer or pointer and SSE if it holds
    only floating-point members, and the whole object goes in memory if
    it is larger than two eightbytes or holds a long double.  On RISC-V
    the question is asked of the flattened members rather than of
    eightbytes, so the answer is a list of pieces either way: see the
    comment on [Ir.passing].

    This is the part of a C compiler least covered by textbooks and most
    needed by the OCaml runtime, whose C calls pass structs. *)

val classify : Env.t -> Ctype.t -> Ir.passing
(** How the machine now being compiled for carries this type. *)
