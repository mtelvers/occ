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
down from 6 times with everything in frame slots. x86-64 Linux, System V ABI. The assembler
(`occas`, byte-identical to GNU as on everything occ and ocamlopt
produce), the archiver (`occar`) and a static linker (`occld`) are ours
too, so a C program goes from source to executable without binutils.

## Layout

    bin/main.ml          entry point of occ, the compiler driver
    bin/occas.ml         entry point of occas, the assembler (GNU as command line)
    bin/occar.ml         entry point of occar, the archiver (ar command line)
    bin/occld.ml         entry point of occld, the static linker (ld command line)
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
      assembler/         gas syntax (lexer, parser), x86-64 encoding,
                         layout and relaxation, .eh_frame, .debug_line,
                         ELF relocatable output
      archiver/          ar archives with a symbol index; ELF symbol reading
      linker/            ELF object reading; loading, symbol resolution,
                         placement, relocation, executable output
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
    tools/ascheck.sh dir...                      # occas vs GNU as on every .s under dir
    tools/asdiff.sh file.s                       # where one file's object differs
    tools/archeck.sh dir                         # occar vs GNU ar on every .a under dir

## Staging

`occ` is a gcc-compatible driver. All four stages are native:
preprocessing, compilation, assembly (the `Assemble` module, the same
code as `occas`) and static linking (`Link`, the same code as `occld`),
which finds the C runtime's start files and static libraries where gcc
installs them and links them the way `gcc -static` does. Shared objects
(`-shared`) are still handed to gcc. `OCC_NATIVE=pp,cc,as,ld` selects
the native stages and `OCC_NATIVE=none` makes occ a pure gcc wrapper,
which is how the project was bootstrapped: `./configure CC=occ` on the
OCaml tree worked from day one and stages turned native one at a time.

`OCC_NATIVE=cc,as` preprocesses with `gcc -E -U__GNUC__ -nostdinc -I
include`, so glibc takes its portable paths and our headers replace
gcc's; `tools/ppcheck.sh` checks that the native preprocessor produces
the same token stream on every runtime unit.

    tools/mkcorpus.sh ~/ocaml     # ~330 preprocessed units into corpus/

## Assembler

`occas` reads the AT&T-syntax assembly that occ, ocamlopt and the
runtime's `amd64.S` produce and writes an ELF relocatable object. It is
four passes: collection (statements to encoded chunks with fixups),
layout (offsets, with jumps relaxed to their short form by iteration to a
fixed point), generation (`.cfi` directives to `.eh_frame`, `.file`/`.loc`
to `.debug_line`, plus a compile unit when the input has none), and
resolution (fixups to values or relocations, then the symbol table and
the file). Encoding choices follow GNU as, so `tools/ascheck.sh` can
demand identical section bytes and relocations: it does, on all 239
runtime units, 281 ocamlopt-compiled compiler modules and `amd64.S`.
OCaml's configure picks `occ -c` for `AS` and `ASPP` when `CC=occ`, so
`make world.opt` assembles every `.s` and `.S` with it.

## Archiver

`occar` is the `ar` command: `r`, `q`, `d`, `t`, `x` and `s` with the
`c` and `v` modifiers, which covers `ar rc` from the Makefiles and
ocamlopt and `ar rcs` from ocamlmklib. `Ar` writes the GNU archive
format with a symbol index built by reading each member's ELF symbol
table (`Elf_read`), in deterministic mode, so `tools/archeck.sh`
rebuilds every static library in the OCaml tree from its members and
gets the original file back byte for byte (34 of 34). Configure with
`AR=occar` to use it.

## Linker

`occld` links relocatable objects and archives into a statically linked
ELF executable in five steps: loading (archive members pulled in while
they define undefined symbols, COMDAT groups deduplicated), symbol
resolution (strong over weak over common), placement (input sections
grouped by name into output sections, output sections into read-only,
executable and writable segments; GOT, PLT and TLS sized from a scan of
the relocations), relocation (the ABI's formulas, general-dynamic TLS
rewritten to local-exec, IFUNC symbols given PLT entries with IRELATIVE
relocations for glibc's startup code), and output with a symbol table for
debuggers. It links glibc's `libc.a`, `libgcc_eh.a` and Ubuntu's
linker-script `libm.a`; statically linked `ocamlrun` and ocamlopt
programs run, and gdb finds their source lines.

## C library

occ builds musl (1.2.5) entirely on its own: `./configure --target=x86_64
CC=occ AR=occar` in the musl tree produces `libc.a` with no gcc runtime.
This needed the language and ABI features a real C library uses beyond
the OCaml runtime's subset:

  - GNU inline assembly with operands and constraints (`syntax`/`parser`
    to `select`): fixed registers, register variables, tied and memory
    operands, the x87 stack; enough for musl's syscalls, atomics and
    thread-pointer access.
  - variable length arrays (6.7.6.2): runtime `sizeof`, scaled pointer
    arithmetic, stack allocation.
  - `long double` as the 80-bit x87 format (class X87 in the ABI): kept in
    16-byte slots, computed on the FPU stack, passed in memory, returned in
    st(0); the assembler gained the x87 instruction set.
  - `va_arg` of aggregates.
  - linkage attributes: `weak`, `alias`, `visibility`, `constructor`,
    `destructor`, emitted as `.weak`, `.set`, `.hidden` and `.init_array`.

Programs link against the occ-built musl through a sysroot
(`occ --sysroot=DIR`, or `OCC_SYSROOT`): DIR/include for headers, DIR/lib
for crt1.o, crti.o, crtn.o and libc.a, with no gcc or glibc files. C
programs using stdio, malloc, pthreads with thread-local storage and libm
build and run this way.

OCaml itself then builds against the occ-built musl
(`./configure --disable-shared --without-zstd CC=occ AR=occar` with the
sysroot): `ocamlrun` and `ocamlopt.opt` come out as statically linked musl
executables produced with no GNU component anywhere in the toolchain, and
`make tests` reports 1563 passed, 115 skipped (shared-library and dynlink
tests under `--disable-shared`) and the one `native-debugger` failure the
glibc build also has.

## Definition of done

    ./configure CC=$PWD/_build/default/bin/main.exe   # in the OCaml tree
    make world.opt && make tests

Reached on 2026-09-03 against OCaml 5.6.0+dev (commit 76190d7736):
`make world.opt` builds, and `make tests` reports 1679 tests considered,
1 failed, 57 skipped. The one failure, `native-debugger`, compares gdb
backtraces against a reference recorded with gcc at `-O2`, whose inlining
gdb reports as an extra frame; occ's DWARF gives gdb file, line and
parameter information, but occ does not inline. Bytecode produced by the occ-built
compiler is byte-identical to the gcc-built compiler's. The same result,
1621 passed and the same one failure, holds on 2026-09-07 with every
object in the tree assembled by occas rather than GNU as. With all four
stages native (`./configure --disable-shared CC=occ AR=occar`, so that
occld links everything statically and no shared stubs are needed), the
clean build passes and the testsuite reports 1562 passed, 116 skipped
(the 59 extra skips are the shared-library and dynlink tests) and the
same one failure; `ocamlrun` and `ocamlopt.opt` are then static
executables that binutils never touched. See
`doc/phases.md` for what remains and `doc/extensions.md` for everything
the runtime needed beyond C11.
