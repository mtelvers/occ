# XCU 2.6.5: field splitting
x="a b  c"
set -- $x; echo "n=$#"
set -- "$x"; echo "n=$#"
IFS=:
y="a:b::c"
set -- $y; echo "n=$# [$1][$2][$3][$4]"
y=":a:"
set -- $y; echo "n=$#"
y="a:"
set -- $y; echo "n=$#"
IFS=" "
z="  spaced   out  "
set -- $z; echo "n=$#"
IFS=
w="a b"
set -- $w; echo "n=$# [$1]"
unset IFS
set -- $w; echo "n=$#"
e=
set -- $e; echo "n=$#"
set -- "$e"; echo "n=$#"
