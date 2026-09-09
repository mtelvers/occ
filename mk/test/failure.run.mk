# What happens when a recipe fails, which a plan cannot show: the
# message, the status, whether the rest of the build is attempted, and
# what is left behind.  -k is not passed here, so the first failure
# stops everything that depended on it.
# goals: both ignored dashk
both: first second
first:
	@echo one > made-first
	@false
second:
	@echo two > made-second

ignored:
	@echo three > made-third
	-@false
	@echo four > made-fourth

dashk: alpha beta
alpha:
	@false
beta:
	@echo five > made-fifth
