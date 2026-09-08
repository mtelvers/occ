# XCU 2.9.5: function definitions
f() { echo "f: $# $1 $2"; }
f a b
f
g() { return 3; }
g; echo "g=$?"
h() { echo "h sees $x"; x=changed; }
x=before
h
echo "after=$x"
rec() {
  if [ "$1" -le 0 ]; then return 0; fi
  echo "rec $1"
  rec $(( $1 - 1 ))
}
rec 3
outer() { inner; }
inner() { echo nested-call; }
outer
loc() { local l=inside; echo "l=$l"; }
l=outside
loc
echo "l=$l"
args() { echo "$*"; }
args p q r
