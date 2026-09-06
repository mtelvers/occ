(** The compiler driver: a gcc-compatible command line.

    A translation unit passes through four stages:

    {v .c --preprocess--> .i --compile--> .s --assemble--> .o --link--> a.out v}

    Each stage is either [Native] or [Delegate]d to gcc.  Configure, the
    OCaml Makefile and ocamlopt all drive [$CC] with gcc's flags, so the
    driver accepts those and the build never notices which stages are
    native.  The stage set is chosen with the [OCC_NATIVE] environment
    variable, a comma-separated subset of [pp,cc,as,ld]; the default is
    [pp,cc,as], leaving only linking to gcc's driver; [none] makes [occ] a
    pure wrapper.

    Delegated preprocessing produces gcc's dialect while gcc compiles, and
    this compiler's dialect (no [__GNUC__], our headers) once the compile
    stage is native, so a natively compiled unit always sees the same
    input the native preprocessor will eventually produce. *)

type stage = Preprocess | Compile | Assemble | Link

type mode = Native | Delegate

val mode : stage -> mode

val main : string array -> int
(** Run with [argv]; returns the process exit status. *)
