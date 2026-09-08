define RULE
$(1): $(1).in
	@echo make $(1) from $$<
endef
NAMES = one two
$(foreach n,$(NAMES),$(eval $(call RULE,$(n))))
all: $(NAMES)
one.in two.in:
	@echo (touch $@)
.PHONY: all
