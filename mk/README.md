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

`occmake` matches GNU make's `-n` command plan on the test Makefiles
(`tools/mkcheck.sh`), covering variables and flavours, the function set,
conditionals, pattern and static-pattern rules, target-specific
variables, `$(eval $(call ...))` over `$(foreach)`, and incremental
rebuilds by timestamp.

It parses the OCaml runtime's 4500-line Makefile in full (about 2400
rules), handles `.SECONDEXPANSION` and the directory/file automatic
variables, and drives real build steps: it compiles every runtime C and
assembly file and archives `libcamlrun.a`, `libcamlrund.a` and
`libasmrun.a` (each with the same members GNU make produces). Its
command for an object matches GNU make's but for collapsed whitespace
where a variable expands empty. Not yet covered: parallel jobs (`-j`),
`vpath` search, `$(MAKE)` recursion for a whole `world.opt`, and a few
generated rules (e.g. the PIC runtime library).
