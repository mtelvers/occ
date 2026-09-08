# GNU make manual 4.11: several rules for one target merge their
# prerequisites, and the one with the recipe names $<
.PHONY: all
all: x y z
x: b
x: a
	@echo "x first=[$<] all=[$^]"
y: a
	@echo "y first=[$<] all=[$^]"
y: b
z.o: extra.h
%.o:
	@echo "z pattern first=[$<] all=[$^]"
z: z.o
	@echo "z done"
