# What make says when a goal needs nothing: a file it found and did not
# have to touch is up to date; one it could do nothing for at all has
# nothing to be done.
made: src
	@echo made > made
norecipe: src
empty:
.PHONY: phony
phony:
	@echo "phony ran"
