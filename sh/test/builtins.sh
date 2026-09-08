# XCU 2.14 and the utility descriptions
echo -n "no newline"; echo
printf '%s\n' one two three
printf '%d %i\n' 42 -7
printf '%5s|%-5s|\n' ab cd
printf '%05d|\n' 42
printf '%x %X %o\n' 255 255 8
printf '%c%c\n' abc def
printf 'a\tb\nc\n'
printf '%b\n' 'x\ty'
printf '%s\n' a b c
printf '[%s]'  a b c; echo
test -n "x"; echo "n=$?"
test -z ""; echo "z=$?"
test 1 -eq 1; echo "eq=$?"
test 1 -ne 2; echo "ne=$?"
test a = a; echo "streq=$?"
test a != a; echo "strne=$?"
[ -d . ]; echo "dir=$?"
[ -f /nonexistent ]; echo "file=$?"
[ ! -f /nonexistent ]; echo "notfile=$?"
[ 1 -lt 2 -a 3 -gt 2 ]; echo "and=$?"
[ 1 -gt 2 -o 3 -gt 2 ]; echo "or=$?"
[ "(" 1 = 1 ")" ]; echo "paren=$?"
:; echo "colon=$?"
true; echo "true=$?"
false; echo "false=$?"
eval 'echo evaluated'
eval 'x=5'; echo "x=$x"
set -- p q
echo "$#"
shift
echo "$# $1"
export FOO=bar
sh -c 'echo "child sees $FOO"'
readonly RO=1
unset x; echo "unset=[${x-gone}]"
IFS=' '
read a b <<EOF
hello world extra
EOF
echo "a=[$a] b=[$b]"
