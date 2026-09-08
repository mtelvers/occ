# occsh

A POSIX shell, written in OCaml, sized by what the OCaml build asks of
one: every recipe in its Makefiles, the four scripts in its tree, and
its 28,000-line `configure`.

    ast        the syntax: words that remember their quoting, commands,
               redirections, pipelines
    lex        token recognition and quoting (2.2, 2.3), including
               here-document bodies read at the next newline
    word       taking a raw word apart into its parts, and the ${...}
               operators of 2.6.2
    parse      the grammar of 2.10, by recursive descent
    state      the execution environment of 2.12: variables, functions,
               options, traps, positional parameters
    arith      arithmetic expansion (2.6.4), which is C11 6.5 on integers
    expand     the word expansions of 2.6 in order, with field splitting
               (2.6.5) and pathname expansion (2.6.6)
    exec       running commands (2.9), redirection (2.7), traps (2.11)
    builtin    the built-in utilities (2.14)
    main       the sh utility: -c, a file, or the standard input

The clause numbers are those of IEEE Std 1003.1-2017, volume XCU,
chapter 2 (Shell Command Language).

## What it covers

Quoting in all three forms; parameter expansion with every operator of
2.6.2; command substitution in both spellings; arithmetic expansion with
the full C operator set including the assignments; field splitting on
IFS with the white-space rule; pathname expansion; the grammar of 2.10
in full (pipelines, AND-OR lists, `if`, `while`, `until`, `for`, `case`,
`{ }`, subshells, function definitions, `!`); every redirection operator
of 2.7 including here-documents and descriptor duplication; the special
and regular built-ins of 2.14; traps including EXIT; and the options
`-e -u -x -f -v -n -C -a -m -o`, with the `-e` exemptions of 2.8.1.

Two points that decide whether real scripts work:

- **A field is text plus a mark per character.** Whether a `*` is a
  pattern or a literal, and whether a space splits a field, depends on
  where the character came from: written plainly, out of an unquoted
  expansion, or quoted. `expand.ml` carries one mark per character for
  exactly that, so splitting looks only at unquoted-expansion characters
  and pattern matching escapes only quoted ones.

- **`$$` is fixed for the life of the shell**, subshells included.
  autoconf builds temporary file names from it in one process and reads
  them back in another; a `$$` that changed in a subshell would leave
  configure looking for a file that was never written.

## Testing

`tools/shcheck.sh` runs every script in `sh/test/` under `/bin/sh` and
under occsh in matching scratch directories and compares standard
output, standard error, the exit status and the files left behind. The
scripts are one per clause group: quoting, parameters, splitting,
arithmetic, redirection, control flow, functions, built-ins, `set -e`,
patterns, command substitution.

Three larger tests, in increasing order of demand:

1. The four scripts in the OCaml tree (`runtime/gen_primitives.sh`,
   `runtime/gen_primsc.sh`, `lambda/generate_runtimedef.sh`,
   `stdlib/Compflags`) give identical output under occsh and under
   `/bin/sh`.
2. `make world.opt SHELL=occsh` builds the OCaml tree.
3. `./configure` under occsh writes configuration files identical to the
   `/bin/sh` run, and never re-executes itself under another shell,
   which it does when the shell it was started in lacks something it
   needs.

## What it does not do

No job control, no command-line editing, no history: those are for an
interactive shell, and this one is only ever started to run a script.
`-m` is accepted and does nothing. `$'...'` and `[[ ]]` are bash
extensions and are not accepted, deliberately: the build's scripts use
neither, and accepting them would let a script that is not portable
appear to work.
