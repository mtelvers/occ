# GNU make manual 4.5: the directory search
VPATH = sub other
all: found.txt pat.dat missing.txt
	@echo "all done"
found.txt:
	@echo "would build $@"
pat.dat:
	@echo "would build $@"
missing.txt:
	@echo "would build $@ from $^"
vpath %.cfg conf
use: a.cfg
	@echo "use $< and $^"
