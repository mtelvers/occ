# GNU make manual 4.13: double-colon rules are independent
.PHONY: all first second
all: both
both:: first
	@echo "rule one of both"
both:: second
	@echo "rule two of both"
first: ; @echo "first"
second: ; @echo "second"
