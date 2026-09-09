#!/bin/sh
# Compare when the utilities pass their output on, not just what it is.
#
# The reference implementations differ from one another here, and the
# difference is what someone watching a pipeline sees: cat and tee write
# each block as they read it, while grep, sed, awk and the rest wait for
# a block of output to gather unless asked not to -- grep by
# --line-buffered and sed by -u.  The OCaml test suite watches its own
# progress through `tee', so a tee that could not be read until the
# input ended showed nothing for twenty minutes.
#
# The reference is named by its path rather than found on PATH: an
# interactive shell may have a function or an alias of the same name
# that answers differently, and one did.
#
# Each utility is given a line, then two seconds of silence, then
# another line.  What matters is whether the first line has come out
# before the input has ended: this looks after one second and compares
# the answer with the reference implementation's.
#
# usage: tools/streamcheck.sh
set -u
HERE=$(cd "$(dirname "$0")/.." && pwd)
OCCUTILS="$HERE/_build/default/bin/occutils.exe"
work=${TMPDIR:-/tmp}/streamcheck.$$
mkdir -p "$work"

ref() {                        # ref name: the reference implementation
  for d in /usr/bin /bin /usr/local/bin; do
    [ -x "$d/$1" ] && { echo "$d/$1"; return; }
  done
  echo "$1"
}

early() {                      # early command...: did anything come out?
  rm -f "$work/out"
  ( printf 'a\n'; sleep 2; printf 'b\n' ) | "$@" > "$work/out" 2>/dev/null &
  pid=$!
  sleep 1
  if [ -s "$work/out" ]; then answer=yes; else answer=no; fi
  wait $pid 2>/dev/null
  echo "$answer"
}

n=0; fail=0
known=0
check() {                      # check [-k reason] name arguments...
  reason=""
  if [ "$1" = -k ]; then reason=$2; shift 2; fi
  name=$1; shift
  n=$((n+1))
  binary=$(ref "$1"); shift
  a=$(early "$binary" "$@")
  b=$(early "$OCCUTILS" "$name" "$@")
  if [ "$a" != "$b" ]; then
    if [ -n "$reason" ]; then
      known=$((known+1))
      echo "known: $name $*: $reason"
    else
      fail=$((fail+1))
      echo "DIFF $name $*: reference passed it on early: $a, occutils: $b"
    fi
  fi
}

check cat  cat
check tee  tee /dev/stdout
check grep grep .
check grep grep --line-buffered .
check sed  sed -e s/x/x/
# sed gathers its output because -i has to write the file back and the
# last line's newline is decided at the end; -u is accepted and the
# output is the same, but it arrives when the input ends.  Nothing in
# the OCaml build passes -u.
check -k "occutils sed gathers its output; -i needs it" sed sed -u -e s/x/x/
check awk  awk '{print}'
check cut  cut -c1-3
check tr   tr a-z A-Z
check uniq uniq
check head head -n 100
# The other half of when: a utility that has its answer stops reading.
# `yes | grep -q y' has to end, and so does `yes | head -n 1'; a
# utility that read to the end of its input instead would never return.
ends() {                       # ends command...: did it end in time?
  if timeout 5 sh -c "yes 2>/dev/null | $* >/dev/null 2>&1"; then echo yes
  else [ $? = 124 ] && echo no || echo yes; fi
}

check_end() {                  # check_end name arguments...
  name=$1; shift
  n=$((n+1))
  binary=$(ref "$1"); shift
  a=$(ends "$binary" "$@")
  b=$(ends "$OCCUTILS" "$name" "$@")
  if [ "$a" != "$b" ]; then
    fail=$((fail+1))
    echo "DIFF $name $*: reference ended: $a, occutils: $b"
  fi
}

check_end grep grep -q y
check_end head head -n 1
check_end cmp  cmp -s /dev/null -

rm -rf "$work"
echo "$n cases, $fail differ, $known known differences"
[ "$fail" = 0 ]
