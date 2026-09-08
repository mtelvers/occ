# XCU 2.14 eval, set and shift, as configure's own functions use them
eval 'x=1; y=2'
echo "$x $y"
v=name
eval "$v=value"
echo "$name"
eval 'echo "nested $(echo sub)"'
cmd='echo from-variable'
eval "$cmd"
set x one two three
shift
echo "$# $1 $2 $3"
set -- a b c
echo "$*"
set -- ${1+"$@"}
echo "$# $1"
as_fn_error () {
  status=$1; shift
  echo "error: $*" >&2
  exit $status
}
( as_fn_error 4 "something failed" ) 2>&1
echo "status=$?"
as_fn_ret_success () { return 0; }
as_fn_ret_failure () { return 1; }
as_fn_ret_success && echo "success fn"
as_fn_ret_failure || echo "failure fn"
if ( set x; as_fn_ret_success y && test x = "$1" ); then echo "positional trick"; fi
u=
: "${u:=filled}"
echo "$u"
unset novar
echo "[${novar-unset ok}]"
