#!/bin/sh
# Rebuild every static library under a directory tree with occar, from the
# member objects GNU ar recorded (found next to the library), and compare
# the result byte for byte with the original.
# usage: tools/archeck.sh dir
set -u
HERE=$(cd "$(dirname "$0")/.." && pwd)
OCCAR="$HERE/_build/default/bin/occar.exe"
tmp=$(mktemp -d)
n=0; fail=0
for lib in $(find "$1" -name '*.a' | sort); do
  dir=$(dirname "$lib")
  members=""
  for m in $(ar t "$lib"); do
    f=$(find "$dir" -name "$m" | head -1); [ -z "$f" ] && f=$(find "$1" -name "$m" | head -1)
    [ -z "$f" ] && { echo "SKIP $lib: member $m not found"; members=""; break; }
    members="$members $f"
  done
  [ -z "$members" ] && continue
  n=$((n+1))
  rm -f "$tmp/x.a"
  "$OCCAR" rc "$tmp/x.a" $members || { fail=$((fail+1)); echo "FAIL (occar error) $lib"; continue; }
  cmp -s "$lib" "$tmp/x.a" || { fail=$((fail+1)); echo "DIFF $lib: $(cmp "$lib" "$tmp/x.a" 2>&1 | head -1)"; }
done
rm -rf "$tmp"
echo "$n archives, $fail differ"
