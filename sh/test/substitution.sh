# XCU 2.6.3: command substitution
echo "$(echo inner)"
echo $(echo a b c)
echo "$(echo a b c)"
x=$(echo assigned)
echo "$x"
echo "`echo backquote`"
echo "$(echo one; echo two)"
echo "[$(echo trailing newlines)]"
echo "$(printf 'a\n\n\n')|"
f() { echo from-function; }
echo "$(f)"
echo "$(exit 3)"; echo "status=$?"
echo "nested: $(echo "$(echo deep)")"
n=$(( $(echo 3) + 4 ))
echo "$n"
d=$(pwd)
[ -d "$d" ] && echo "pwd ok"
echo "$(cat <<EOF
heredoc in subst
EOF
)"
