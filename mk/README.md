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
rules) and generates GNU make's exact command for a runtime object, but
does not yet drive the whole OCaml build: one `$(eval)`/`$(call)`
interaction with the verbose `$(info)` variables expands without
terminating, which an expansion-depth limit turns into an error rather
than a hang. That, parallel jobs (`-j`), `vpath`, and secondary
expansion are the remaining work.
