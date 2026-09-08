# XCU 2.5.2: $$ is the shell's process, in subshells too
outer=$$
( inner=$$; [ "$inner" = "$outer" ] && echo "subshell same" )
sub=$( echo $$ )
[ "$sub" = "$outer" ] && echo "substitution same"
f() { [ "$$" = "$outer" ] && echo "function same"; }
f
t="tmp$$"
: > "$t"
( [ -f "tmp$$" ] && echo "file found from subshell" )
rm -f "$t"
case $$ in [0-9]*) echo "numeric";; esac
