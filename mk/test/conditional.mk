MODE = fast
X :=
ifeq ($(MODE),fast)
FLAGS = -O2
else
FLAGS = -O0
endif
ifdef X
D = yes
else
D = no
endif
all:
	@echo flags=$(FLAGS) d=$(D)
