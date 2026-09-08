# XCU 2.8.1 and set -e: where the option does not act
set -e
false || echo "or protected"
if false; then echo no; fi
echo "if protected"
while false; do echo no; done
echo "while protected"
! false
echo "bang protected"
false && echo no
echo "and protected"
( false ) || echo "subshell status seen"
f() { false; return 0; }
f
echo "function ok"
set +e
false
echo "off=$?"
