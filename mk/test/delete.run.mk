# .DELETE_ON_ERROR (GNU make manual 5.4): a target whose recipe failed
# is removed, since what is left of it is not the file the rule
# promised and a later run would take it for finished.  A phony target
# and one named in .PRECIOUS are left alone.
# goals: out phony precious deep
.DELETE_ON_ERROR:

out:
	@echo half > out
	@false

.PHONY: phony
phony:
	@echo half > phony
	@false

.PRECIOUS: precious
precious:
	@echo half > precious
	@false

deep: middle
	@echo done > deep
middle:
	@echo half > middle
	@false
