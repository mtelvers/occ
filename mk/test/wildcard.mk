# $(wildcard) is pathname expansion, and the notation is the shell's: a
# '*' or '?' or a bracket in any component, not only the last.  musl's
# build asks for $(wildcard src/*/$(ARCH)/*.[csS]), which is both at
# once, and a make that expanded only the last component built eleven of
# its 1350 objects and said it was done.
dir := $(dir $(lastword $(MAKEFILE_LIST)))files
all:
	@echo "last component  [$(notdir $(wildcard $(dir)/a/*.c))]"
	@echo "any component   [$(notdir $(wildcard $(dir)/*/*.c))]"
	@echo "two of them     [$(notdir $(wildcard $(dir)/*/*/*.c))]"
	@echo "a bracket       [$(notdir $(wildcard $(dir)/a/*.[ch]))]"
	@echo "a question mark [$(notdir $(wildcard $(dir)/a/on?.c))]"
	@echo "no such thing   [$(wildcard $(dir)/a/nothing.*)]"
	@echo "a plain name    [$(notdir $(wildcard $(dir)/a/one.c))]"
