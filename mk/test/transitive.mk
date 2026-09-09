all: user
.PHONY: tool
tool: bin/prog
bin:
	@mkdir -p bin
bin/prog: | bin
	@sleep 0.4; echo '#!/bin/sh' > bin/prog; echo 'echo TOOL' >> bin/prog; chmod +x bin/prog
user: tool
	@./bin/prog > out; cat out
