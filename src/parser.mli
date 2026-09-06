(** Phase 7, syntax (C11 6.5 to 6.9): tokens to [Syntax.translation_unit].

    Recursive descent following the grammar in Annex A, one function per
    nonterminal, with the expression grammar folded into precedence
    climbing (6.5.5 to 6.5.14).  The one piece of context is the table of
    typedef names (6.7.8), kept here with block scoping so that the lexer
    can stay context-free.  Everything else the grammar needs to know, it
    learns from the next one or two tokens. *)

val parse : Token.loc_token list -> Syntax.translation_unit
