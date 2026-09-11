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
down from 6 times with everything in frame slots. x86-64 Linux, System V ABI.

Everything the build runs is now ours as well: the assembler (`occas`,
byte-identical to GNU as on everything occ and ocamlopt produce), the
archiver (`occar`), a linker (`occld`), a make (`occmake`), a
POSIX shell (`occsh`) and the utilities the build calls (`occutils`:
sed, awk, grep, diff, sort, tr, cp, rm and twenty-odd more). With a
PATH holding only those, `./configure && make world.opt` builds the
OCaml compiler with no program written in C on it.

## Layout

    bin/main.ml          entry point of occ, the compiler driver
    bin/occas.ml         entry point of occas, the assembler (GNU as command line)
    bin/occar.ml         entry point of occar, the archiver (ar command line)
    bin/occld.ml         entry point of occld, the linker (ld command line)
    bin/occmake.ml       entry point of occmake, the make
    bin/occsh.ml         entry point of occsh, the shell
    bin/occutils.ml      entry point of occutils, every utility in one binary
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
    tools/toolbin.sh     stage the whole toolchain in one directory
    doc/phases.md        phase map, budgets, order of work
    doc/extensions.md    everything the runtime needs beyond C11
    doc/shell.md         what the OCaml build asks of a shell, measured

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

`occld` links relocatable objects and archives into an ELF executable in
five steps: loading (archive members pulled in while
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

### Shared objects, and the executables that load them

`occld -shared` produces a shared object instead, and an executable
linked against one carries what the loader needs too. That is a
different job in four ways, and `src/linker/dynamic.ml` is the part
that does it: the output is ET_DYN starting at address zero, so the
loader may put it anywhere; the places holding an address this link
cannot know go in `.rela.dyn` for the loader to fix; the names it offers
and the names it wants go in `.dynsym`, with `.hash` to find them by;
and `.dynamic` says where all of that is. Calls out of the object go
through stubs whose slots the loader fills before the object runs, and a
variable of another object that non-position-independent code refers to
by address gets space here and a copy relocation.

Two things GNU ld emits are left out deliberately, each measured first:
`.gnu.hash`, because a loader that finds only `DT_HASH` uses it, which
is a few lines against a few hundred; and the version tables on the
symbols an object *defines*, which nothing reads unless the object
itself declares versions.

The versions on its *references* are not optional, and the OCaml test
suite is what said so. glibc offers `realpath@@GLIBC_2.3`, which
accepts a null second argument, and `realpath@GLIBC_2.2.5`, which does
not; a reference naming no version is bound to whichever the loader
meets first, and it meets the old one. 334 tests failed on it. So
`occld` reads the version each name is offered under, binds to the
default, and writes `.gnu.version` and `.gnu.version_r` to say so.

With this, the OCaml tree builds with shared libraries enabled, which
is its own default: `runtime/libcamlrun_shared.so`, the `dll*.so` stubs
the bytecode runtime loads, and the `.cmxs` files `ocamlopt -shared`
produces. Its test suite then reports 1621 passed, 57 skipped and the
one `native-debugger` failure -- 59 tests more than the static
configuration, whose skips are mostly the dynamic-loading ones.

`occld -r` is the other job: a partial link, whose output is another
relocatable object. Sections of the same name are concatenated, the
symbol tables merged, and every relocation rewritten to its new place
and symbol; a relocation against a section symbol has its addend moved
by the offset its piece was placed at. OCaml needs it for
`ocamlopt -pack` and for `-output-complete-obj`.

    tools/ldrcheck.sh dir [group] [suffix]   # occld -r vs ld -r

compares the two on the same objects by what a later link reads: every
section's size, the bytes of each, the symbol table, and every
relocation's place, type, symbol and addend. Over the OCaml runtime's
objects, four builds' worth, they agree.

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

## Shell, utilities and make

`occsh` is a POSIX shell (IEEE Std 1003.1-2017, XCU chapter 2): quoting,
the word expansions in the order 2.6 sets out, the grammar of 2.10,
redirection including here-documents and descriptor duplication, traps,
the built-in utilities, and the `-e` exemptions of 2.8.1. `occutils`
holds the utilities in one binary, chosen by the name it is called
under, among them the whole of `sed` and of `awk` and a `diff` that
finds a shortest edit script by Myers' algorithm. `occmake` is a make in
the GNU dialect, sized by the OCaml build's own Makefiles. See
`sh/README.md`, `ut/README.md`, `mk/README.md`, and `doc/shell.md` for
what the build was measured to need.

Each is held to the same test as the assembler and the archiver: run the
reference implementation and ours on the same input and compare the
bytes. Two of the harnesses compare something else, because for a
utility not only what comes out matters but when: whether a line has
been passed on before its input ends, and how many recipes a recursive
build had running at once.

    tools/regexcheck.py                     # the regex engine vs GNU grep
    tools/shcheck.sh                        # occsh vs /bin/sh, per clause group
    tools/utcheck.py                        # occutils vs the GNU utilities
    tools/mkcheck.sh                        # occmake vs GNU make (-n plans)
    tools/jscheck.sh                        # occmake -jN vs GNU make -jN
    tools/streamcheck.sh                    # when the output goes out
    tools/ldrcheck.sh                       # occld -r vs GNU ld -r
    tools/dyncheck.sh                       # occld -shared vs GNU ld -shared

    7740 regex cases, identical
      22 shell scripts, identical output, error output, status and files
     256 utility cases, identical but for four named differences
      22 makefiles, identical plans, and identical output, status and
         files left behind for the four that are run for real
       8 parallel runs, the same peak number of recipes at once
      14 cases of when a utility passes its output on or stops
         reading, one named difference
       8 groups of the runtime's objects, partially linked, identical
         in sections, symbols and relocations
      13 shared objects and dynamic executables, identical in what a
         loader reads, and running the same

## A build with no C

`tools/toolbin.sh` stages the whole toolchain in one directory, with a
link per utility named as the scripts call it, and the compiler's
headers beside it in the layout an installation would have.

    tools/toolbin.sh                        # stages ./toolbin
    cd /path/to/ocaml
    env -i PATH=~/occ/toolbin HOME=$HOME TERM=dumb \
      CONFIG_SHELL=~/occ/toolbin/sh ~/occ/toolbin/sh \
      ./configure --disable-shared --without-zstd
    env -i PATH=~/occ/toolbin HOME=$HOME TERM=dumb \
      ~/occ/toolbin/make SHELL=~/occ/toolbin/sh world.opt

`configure` under that PATH finishes with status 0 and writes `m.h`,
`s.h`, `exec.h`, `config.common.ml` and `ld.conf` identical to a run
with dash and the GNU utilities; the rest of its output differs only in
the names of the tools it found, which is what should differ. It never
re-executes itself under another shell, which it does when the shell it
was started in lacks something it needs.

`make tests` under that PATH reports 1562 passed, 117 skipped and none
failed. A run of the same tree with dash and the GNU utilities passes
the same 1562 and fails one, `native-debugger`, which compares gdb
backtraces; the hermetic run skips it because gdb is not on that PATH.

`occmake -j8` builds the same tree in 291 seconds against 1075 serial,
and `-jN` is a limit for the whole build rather than for each make in
the recursion: the makes share a pool of job tokens, as GNU make's do.

The build produces the same tree whether GNU make or occmake drives
it. `tools/compare-trees.sh` on two trees built at paths of equal
length, one with each make, reports 1676 compiled units identical and
`ocamlc`, `ocaml`, `ocamllex`, `ocamldoc`, `ocamldep` and `ocamlyacc`
identical after their `#!` line. One `.cmt` differs, in sixteen bytes: a
digest of a generated source file that embeds the tree's own path, and
so cannot be the same in two trees.

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
