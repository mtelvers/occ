#!/bin/sh
# Compare occmake with GNU make on a set of Makefiles: the sequence of
# commands each would run (make -n) must agree.
# usage: tools/mkcheck.sh [dir-of-makefiles]
set -u
HERE=$(cd "$(dirname "$0")/.." && pwd)
OCM="$HERE/_build/default/bin/occmake.exe"
dir=${1:-$HERE/mk/test}
n=0; fail=0
for mf in "$dir"/*.mk; do
  [ -f "$mf" ] || continue
  n=$((n+1))
  g=$(cd "$(dirname "$mf")" && make -n -f "$(basename "$mf")" 2>/dev/null)
  o=$(cd "$(dirname "$mf")" && "$OCM" -n -f "$(basename "$mf")" 2>/dev/null)
  if [ "$g" = "$o" ]; then :; else fail=$((fail+1)); echo "DIFF $(basename "$mf")"; fi
done
echo "$n makefiles, $fail differ"
