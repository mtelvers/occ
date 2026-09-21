#!/bin/sh
# Check that the tables occas writes mean what gas's mean.
#
# In code, data, symbols and code relocations the two assemblers agree
# byte for byte (tools/ascheck.sh).  The frame and line tables are the
# one place they do not: gas leaves every difference of two labels in
# them to the linker, as an ADD/SUB relocation pair, because a relaxing
# linker may change it, where occas -- which never relaxes, and emits no
# R_RISCV_RELAX to invite it -- works the difference out and writes the
# value.  So the bytes differ while the meaning must not.
#
# This asks a reader what it makes of each table and compares that: the
# unwind tables, the tree of debugging information entries, and the line
# table, whose "view" column is readelf's own bookkeeping about rows it
# cannot prove are at different addresses and is therefore dropped.
#
# usage: tools/rvtabcheck.sh dir...
set -u
HERE=$(cd "$(dirname "$0")/.." && pwd)
OCCAS="$HERE/_build/default/bin/occas.exe"
tmp=$(mktemp -d)
n=0; fail=0
lines() { readelf --debug-dump=decodedline "$1" | sed 's/  *[0-9]*  *x *$/ x/'; }
for d in "$@"; do
  for f in $(find "$d" -name '*.s' | sort); do
    n=$((n+1))
    as -mno-relax "$f" -o "$tmp/ref.o" 2>/dev/null || { echo "SKIP (gas rejects) $f"; continue; }
    if ! "$OCCAS" "$f" -o "$tmp/occ.o" 2>"$tmp/err"; then
      fail=$((fail+1)); echo "FAIL (occas error) $f: $(head -1 "$tmp/err")"; continue
    fi
    bad=""
    # Where gas used a relocation readelf cannot apply, what readelf
    # prints for gas is a placeholder rather than the table's meaning, so
    # there is nothing to compare: say so instead of calling it a
    # difference.  (The bytes of .text still match, which is what says
    # the distances are in fact the same.)
    readelf -wf "$tmp/ref.o" > "$tmp/ref.f" 2>"$tmp/w"
    if grep -q "unable to apply" "$tmp/w"; then
      echo "SKIP (readelf cannot evaluate gas's relocations) $f"; continue
    fi
    readelf -wf "$tmp/occ.o" > "$tmp/occ.f"
    cmp -s "$tmp/ref.f" "$tmp/occ.f" || bad="$bad frames"
    readelf --debug-dump=info "$tmp/ref.o" > "$tmp/ref.i"; readelf --debug-dump=info "$tmp/occ.o" > "$tmp/occ.i"
    cmp -s "$tmp/ref.i" "$tmp/occ.i" || bad="$bad info"
    lines "$tmp/ref.o" > "$tmp/ref.l"; lines "$tmp/occ.o" > "$tmp/occ.l"
    cmp -s "$tmp/ref.l" "$tmp/occ.l" || bad="$bad lines"
    if [ -n "$bad" ]; then fail=$((fail+1)); echo "DIFF $f:$bad"; fi
  done
done
rm -rf "$tmp"
echo "$n files, $fail differ"
[ "$fail" = 0 ]
