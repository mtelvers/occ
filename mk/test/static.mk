# GNU make manual 4.12: a static pattern rule, whose recipe still sees
# the stem in $*
OBJS = a.out b.out
all: $(OBJS)
$(OBJS): %.out: %.in
	@echo "target=$@ stem=$* src=$< extra=dir/$*.mli"

# a static pattern rule may have order-only prerequisites too
GENS = one.gen two.gen
gens: $(GENS)
$(GENS): %.gen: %.in | outdir
	@echo "gen $@ stem=$* src=$< order=$|"
outdir:
	@echo "mkdir outdir"
