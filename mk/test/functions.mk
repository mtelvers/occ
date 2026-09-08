L := a b c d e
call2 = [$(1):$(2)]
all:
	@echo words=$(words $(L)) rev=$(lastword $(L)) first=$(firstword $(L))
	@echo filter=$(filter-out b d,$(L)) sort=$(sort e a c b a)
	@echo patsubst=$(patsubst %.c,%.o,x.c y.h z.c) subst=$(subst o,0,foo)
	@echo call=$(call call2,X,Y) foreach=$(foreach x,$(L),<$(x)>)
	@echo if=$(if ,T,F) strip=[$(strip  a  b  )] dir=$(dir a/b/c.x) notdir=$(notdir a/b/c.x)
	@echo suffix=$(suffix a.b/c.x foo) basename=$(basename a/b.c.d) addprefix=$(addprefix p/,x y)
