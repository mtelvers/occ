# GNU make manual 10.4: a chain of pattern rules, and the intermediate
# files it leaves behind, which make then removes
O = o
all: gen/one.t gen/two.t
gen:
	@echo "mkdir gen"
gen/%.t: gen/%.c.$(O)
	@echo "touch $@ from $<"
gen/%.c.$(O): gen/%.c
	@echo "CC $@ from $< stem=$*"
gen/%.c: | gen
	@echo "GEN $@ stem=$*"
