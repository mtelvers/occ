all: t
t::
	@echo "rule 1 makes it" > t
t::
	@echo "rule 2 always runs"
t:: dep
	@echo "rule 3 has a prerequisite"
dep:
	@echo dep > dep
