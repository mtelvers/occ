# GNU make manual 4.12: a static pattern rule, whose recipe still sees
# the stem in $*
OBJS = a.out b.out
all: $(OBJS)
$(OBJS): %.out: %.in
	@echo "target=$@ stem=$* src=$< extra=dir/$*.mli"
