# XCU 2.5, 2.6.2: parameters and their expansions
set -- one two three
echo "$# [$1] [$2] [$3] [$4]"
echo "all=[$*]"
echo "at=[$@]"
for a in "$@"; do echo "arg:$a"; done
for a in $@; do echo "u:$a"; done
echo "${#}"
echo "len=${#1}"
unset e
echo "[${e-def}] [${e:-def}] [${e+alt}] [${e:+alt}]"
n=
echo "[${n-def}] [${n:-def}] [${n+alt}] [${n:+alt}]"
s=set
echo "[${s-def}] [${s:-def}] [${s+alt}] [${s:+alt}]"
echo "[${u=assigned}] [$u]"
p=/a/b/c.tar.gz
echo "${p#/a}" "${p##*/}" "${p%.gz}" "${p%%.*}"
echo "${p#*/}" "${p##/}"
q="  x  "
echo "[${q# }]"
echo "${#p}"
set -- a b c d e f g h i j k
echo "$# ${10} ${11}"
echo "${1}${2}"
