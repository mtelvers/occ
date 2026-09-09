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
to see why a plan differs from make's. A recipe that fails is reported
with the makefile and line its command was written on, since in a
recursive build over a 4500-line Makefile that is the only part of the
message that helps.

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

## Parallel jobs

`-jN` runs up to N recipes at once. The walk over the graph and the
running of recipes are separate passes for this: the walk decides what
has to be rebuilt and records a job for each, in the order it reached
them, with the jobs each one waits for; then a scheduler starts them,
never more than N at a time and never one whose jobs have not all
finished. Recording rather than running keeps the decisions in one
place, so `-j8` and `-j1` build the same tree.

`-jN` is a limit for the whole build and not for each make in it, which
a recursive build needs: the makes share one pool of tokens, made by the
make that was given `-j` and named in MAKEFLAGS
(`--jobserver-auth=fifo:PATH`, the spelling of GNU make 4.4 and later).
Every make may run one recipe for free, since it is itself occupying a
token of the make that started it, and takes a token from the pool
before starting a second. The make that made the pool removes it on the
way out; a make killed outright leaves the pipe behind, as GNU make
does, and it is an empty file in the temporary directory. `tools/jscheck.sh` checks this by measuring:
it runs a recursive tree under both makes and compares the largest
number of recipes either had running at once, with the pool and with the
pool taken away.

A make started by GNU make 4.3 or earlier is offered a pair of
descriptors instead of a named pipe. That pool cannot be joined here --
a descriptor passed down is one open file shared by every make, and
reading it without blocking would change how all of them read it -- so
such a make says so and runs one recipe at a time, rather than taking a
whole `-jN` for itself on top of what the rest of the tree is doing.

`.NOTPARALLEL` in a makefile, `-n`, and `-q` all keep the recipes where
the walk reaches them.

## Not covered

There is no `--debug`, no `-p` and no built-in rule set -- the build
cancels the built-in rules anyway. `-l` (load average) is accepted and
ignored.
