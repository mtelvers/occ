(** Translation phases 1 to 4 (C11 5.1.1.2): the preprocessor.

    Trigraphs and line splicing are applied as characters are read;
    comments become whitespace; the file is then broken into preprocessing
    tokens (6.4) and directives are executed (6.10).  Macro expansion
    follows the standard's rescanning rules, implemented with hide sets
    (the token carries the names it must not be expanded by again), which
    gives 6.10.3.4's behaviour directly.  The result is text with line
    markers, as [gcc -E] produces, for [Lexer] to tokenize. *)

type config = {
  include_dirs : string list; (** -I, searched for both forms of #include *)
  system_dirs : string list; (** -isystem and the defaults, searched after *)
  defines : (string * string option) list; (** -D, in order; a name may be "F(a,b)" *)
  undefines : string list; (** -U *)
  includes : string list; (** -include: files read before the main file *)
  line_markers : bool; (** false for -P: no # line "file" lines in the output *)
  assembler : bool; (** the input is assembly (.S): ## may yield two adjacent tokens, as in cpp's assembler mode *)
}

val run : config -> string -> string * string list
(** [run config file] returns the preprocessed text of [file] and the files
    it included that are not under a system directory, for -MMD. *)

val predefined_extras : string list
(** Macro definitions, as [-D] arguments, that glibc's headers look for in
    order to use assembler labels for symbol redirection instead of the
    [#define readdir readdir64] fallback, which changes declared types.
    glibc's <sys/cdefs.h> documents __REDIRECT as the hook for exactly
    this ("compilers that can do this some other way"). *)
