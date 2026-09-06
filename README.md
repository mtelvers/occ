# occ

A C11 compiler in OCaml, written to be read, and sized by one concrete
target: it must build the OCaml runtime and pass OCaml's testsuite.

The compiler follows the C11 standard's own structure. Modules are named
after translation phases (5.1.1.2), functions cite the clause they
implement, and every implicit conversion in 6.3 becomes an explicit node
before code generation sees it. There is one intermediate representation,
a one-page linear-scan register allocator, a few peepholes, and no
dependency outside the OCaml standard library. The occ-built OCaml
bytecode interpreter runs about 2.5 times slower than gcc's `-O2` build,
down from 6 times with everything in frame slots. x86-64 Linux, System V ABI. Assembly and linking are
left to binutils.

## Layout

    bin/main.ml          entry point
    src/
      loc, diag          positions, diagnostics (first error stops)
      token, lexer       6.4, phases 1–3 and 7
      preprocess         6.10
      syntax, parser     6.5–6.9, Annex A
      ctype              6.2.5 types
      typed, elab        elaboration to a fully explicit typed AST
      ir, lower          three-address IR
      driver             gcc-compatible command line, stage selection
      amd64/             asm AST, ABI classification, selection,
                         register allocation, emission
    include/             stdarg.h, stdatomic.h, stddef.h, ... (7.15–7.23)
    test/programs/       whole-program tests: // expect: N
    tools/mkcorpus.sh    preprocess the OCaml runtime into corpus/
    doc/phases.md        phase map, budgets, order of work
    doc/extensions.md    everything the runtime needs beyond C11

## Building and testing

    day10 build .                                # or: dune build
    day10 build --with-test . @runtest           # wrapper path, via gcc
    _build/default/test/run.exe $PWD/_build/default/bin/main.exe test/programs
    tools/corpus-check.sh obj                    # compile + assemble the runtime corpus

## Staging

`occ` is a gcc-compatible driver. Preprocessing, compilation and
assembly are native; linking is delegated to gcc's driver, which knows
where the C runtime files live. `OCC_NATIVE=pp,cc,as,ld` overrides the
set of native stages and `OCC_NATIVE=none` makes occ a pure gcc wrapper,
which is how the project was bootstrapped: `./configure CC=occ` on the
OCaml tree worked from day one and stages turned native one at a time.

`OCC_NATIVE=cc,as` preprocesses with `gcc -E -U__GNUC__ -nostdinc -I
include`, so glibc takes its portable paths and our headers replace
gcc's; `tools/ppcheck.sh` checks that the native preprocessor produces
the same token stream on every runtime unit.

    tools/mkcorpus.sh ~/ocaml     # ~330 preprocessed units into corpus/

## Definition of done

    ./configure CC=$PWD/_build/default/bin/main.exe   # in the OCaml tree
    make world.opt && make tests

Reached on 2026-09-03 against OCaml 5.6.0+dev (commit 76190d7736):
`make world.opt` builds, and `make tests` reports 1679 tests considered,
1 failed, 57 skipped. The one failure, `native-debugger`, compares gdb
backtraces against a reference recorded with gcc at `-O2`, whose inlining
gdb reports as an extra frame; occ's DWARF gives gdb file, line and
parameter information, but occ does not inline. Bytecode produced by the occ-built
compiler is byte-identical to the gcc-built compiler's. See
`doc/phases.md` for what remains and `doc/extensions.md` for everything
the runtime needed beyond C11.
