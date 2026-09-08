# occmake

A make in the GNU dialect, written in OCaml, sized (like the rest of occ)
by the OCaml build's own Makefiles.

    value      the variable database: flavours (= vs :=) and origins (6.7)
    expand     variable and function expansion (6, 8): ~30 built-in
               functions, $(call), $(foreach), $(eval), $(shell)
    func       pure helpers: pattern matching, file-name splitting
    rule       explicit, pattern, static-pattern and double-colon rules,
               .PHONY, target- and pattern-specific variables (6.11, 6.12)
    eval       reading a makefile: logical lines, conditionals, include,
               define/endef, assignments, rules, inline recipes
    build      the update algorithm (2, 10): prerequisites, timestamps,
               implicit-rule search, automatic variables, recipe execution
    make       the command line (9): -f -C -n -s -k -j, VAR=value, goals

`occmake` matches GNU make's `-n` command plan on the test makefiles
(`tools/mkcheck.sh`), covering variables and flavours, the function set,
conditionals, pattern and static-pattern rules, target-specific
variables, `$(eval $(call ...))` over `$(foreach)`, secondary expansion,
rule chaining, prerequisite merging and incremental rebuilds by
timestamp.

On the OCaml tree's own 4500-line Makefile, `occmake -n runtime` agrees
with GNU make on 1373 of 1379 command lines. The six that differ are
named in "Known differences" below.

## What it covers

    value      the variable database: flavours (= vs :=) and origins (6.7)
    expand     variable and function expansion (6, 8): ~30 built-in
               functions, $(call), $(foreach), $(eval), $(shell)
    func       pure helpers: pattern matching, file-name splitting, and
               the leading "./" a file name does not keep
    rule       explicit, pattern, static-pattern and double-colon rules,
               .PHONY, .PRECIOUS, .SECONDARY, target- and
               pattern-specific variables (6.11, 6.12), the merging of
               several rules for one target (4.11) and the replacing of
               one pattern rule by another (10.5.6)
    eval       reading a makefile: logical lines, conditionals, include,
               define/endef, assignments, rules, inline recipes, vpath
    build      the update algorithm (2, 10): prerequisites, timestamps,
               implicit-rule search and chaining (10.4), the directory
               search (4.5), automatic variables, recipe execution
    make       the command line (9), MAKEFLAGS and recursion (5.7)

The clause numbers are those of the GNU make manual, which is the
reference: make is not in POSIX in the dialect the OCaml build uses.

Six points where an approximate make would build the wrong thing, all
found by comparing plans on the tree itself rather than by reading:

- **Several rules may name one target.** Their prerequisites merge, and
  the rule that carries the recipe contributes its own first, so `$<`
  names it. The build gathers `runtime-all`'s prerequisites over three
  rules; keeping only the first silently builds less than was asked.
- **Implicit rules chain.** A pattern rule applies when a prerequisite
  can itself be made by another, which is how a header test goes from
  `.t` to `.c.o` to `.c`. Candidates are tried shortest stem first, so a
  specific rule beats a general one; and the files made only as links in
  a chain are removed at the end, unless `.PRECIOUS` or `.SECONDARY`
  names them.
- **A recipeless pattern rule cancels one already given**, and a later
  rule with the same patterns replaces an earlier one (10.5.6). The
  build opens with a recipeless `%.o: %.c` to cancel make's own built-in
  rule and gives a real one later.
- **Target-specific variables reach the prerequisites**, so they are
  bound on the way into a target rather than around its recipe (6.11).
  The build relies on it to give `runtime/sak` the runtime's own
  preprocessor flags, which are set on the objects that need them.
- **Whitespace is part of a value.** A variable keeps its trailing
  blanks (the build's `MKEXE_VIA_CC` ends in one on purpose); a
  continuation outside a recipe stands for exactly one space however
  much was written on either side of it; and a recipe keeps its
  continuations, because the shell reads them. Inside a `define` body
  the lines are a value, so they collapse -- and become recipe lines
  only later, when `$(eval)` reads the text back.
- **A variable given on the command line is recursively expanded**, so
  `$(MAKE) -C stdlib OCAMLRUN='$(ROOTDIR)/boot/ocamlrun'` is expanded by
  the sub-make, in the sub-make's directory.

`OCCMAKE_DEBUG=1` names, for each target, how it was chosen, with which
prerequisites, and what its automatic variables held: the quickest way
to see why a plan differs from make's.

## Known differences

`tools/mkcheck.sh` compares occmake's `-n` plan with GNU make's, except
where a `.expected` file beside the makefile says the two are meant to
differ. Three such differences:

- The intermediate files of a chain are reported in a settled order
  rather than make's internal one.
- An order-only prerequisite that names a directory may be created at a
  different point in the plan.
- `$(shell)` runs its command through the shell, so a command that does
  not exist is reported by the shell rather than by make.

## Not covered

Parallel jobs (`-j`) are accepted and ignored: a build is correct
serially, and the OCaml tree's only use of `-j` is to pass it to GNU
parallel in the test suite. There is no `--debug`, no `-p`, no jobserver
and no built-in rule set -- the build cancels the built-in rules
anyway.
