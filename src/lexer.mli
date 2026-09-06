(** Translation phases 3, 6 and 7 (C11 5.1.1.2): text to tokens.

    Phases 1 and 2, trigraphs and line splicing, belong to the
    preprocessor, which also removes comments and directives.  Because
    the compiler is developed on preprocessed input first, this lexer
    also tolerates what a preprocessor leaves behind: comments (6.4.9),
    line markers ([# 12 "file.c"], 6.10.4), and [#pragma] lines, which are
    skipped.  Adjacent string literals are concatenated here, in phase 6
    order, before pp-tokens become tokens (6.4).

    Whether an identifier names a type is the parser's business (6.7.8):
    the lexer never consults scope. *)

val tokenize : file:string -> string -> Token.loc_token list
(** Raises [Diag.Error] on the first ill-formed token. *)

val utf8_encode : Buffer.t -> int -> unit
(** Append the UTF-8 encoding of a code point (6.4.3). *)

val hex_value : char -> int
(** The value of a hexadecimal digit (6.4.4.1). *)
