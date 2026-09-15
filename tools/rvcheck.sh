#!/bin/sh
# Compare what occ's code computes with what gcc's does, on the machine
# itself.  Run this on a RISC-V host, where occ is a native compiler.
#
# Each program in test/rv prints what it computed; the two compilers'
# builds of it must print the same bytes and leave the same status.  That
# is the only test that matters for a back end: the assembler and the
# linker are the reference toolchain's here, so what is being compared is
# the code and nothing else.
#
# usage: tools/rvcheck.sh [program...]
set -u
HERE=$(cd "$(dirname "$0")/.." && pwd)
OCC="$HERE/_build/default/bin/main.exe"
work=${TMPDIR:-/tmp}/rvcheck.$$
mkdir -p "$work"
# Three ways: the twenty-high/twelve-low addressing pair; the
# position-independent form with its global offset table and its
# initial-exec thread-local sequence; and with debugging information,
# which the assembler has to accept and which must not change what the
# program computes.
n=0; fail=0
for src in ${@:-$HERE/test/rv/*.c}; do
  [ -f "$src" ] || continue
  base=$(basename "$src" .c)
  for mode in fixed pic debug; do
    case $mode in
      fixed) gflags="-no-pie"; oflags="-no-pie";;
      pic)   gflags="-fPIE -pie"; oflags="-fPIE";;
      debug) gflags="-g -no-pie"; oflags="-g -no-pie";;
    esac
    name=$base.$mode
    n=$((n+1))
    # shellcheck disable=SC2086
    gcc -O0 $gflags -o "$work/$name.gcc" "$src" 2>"$work/$name.gcc.err" || {
      fail=$((fail+1)); echo "DIFF $name: gcc could not build it"; continue; }
    # shellcheck disable=SC2086
    if ! "$OCC" $oflags -o "$work/$name.occ" "$src" > "$work/$name.occ.err" 2>&1; then
      fail=$((fail+1))
      echo "DIFF $name: occ could not build it: $(head -1 "$work/$name.occ.err")"
      continue
    fi
    "$work/$name.gcc" > "$work/$name.gcc.out" 2>&1; echo "status $?" >> "$work/$name.gcc.out"
    "$work/$name.occ" > "$work/$name.occ.out" 2>&1; echo "status $?" >> "$work/$name.occ.out"
    if ! cmp -s "$work/$name.gcc.out" "$work/$name.occ.out"; then
      fail=$((fail+1))
      echo "DIFF $name: what it printed"
      [ -n "${VERBOSE:-}" ] && diff "$work/$name.gcc.out" "$work/$name.occ.out" | head -10
    fi
  done
done
rm -rf "$work"
echo "$n builds, $fail differ"
[ "$fail" = 0 ]
