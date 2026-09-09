# -k: a failure stops what depended on it and nothing else, and make
# says at the end that the goal was not remade.  The goals here are run
# with -k.
# flags: -k
# goals: withk
withk: alpha beta
alpha:
	@false
beta:
	@echo made > made-beta
