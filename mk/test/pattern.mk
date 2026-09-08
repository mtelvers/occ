OBJ = a.o b.o
prog: $(OBJ)
	@echo link $@ from $^
%.o: %.c
	@echo compile $< to $@ stem=$*
a.c b.c:
	@echo (source $@)
.PHONY: prog
