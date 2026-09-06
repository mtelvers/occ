#!/bin/sh
# Compile a test program with gcc and with occ, run both, and diff their
# output.  A difference points at a miscompiled construct.
# usage: tools/difftest.sh test/diff/expr.c
set -u
HERE=$(cd "$(dirname "$0")/.." && pwd)
OCC="$HERE/_build/default/bin/main.exe"
src=$1; base=$(basename "$src" .c); tmp=$(mktemp -d)
gcc -O0 -w -o "$tmp/$base.gcc" "$src" && "$tmp/$base.gcc" > "$tmp/gcc.out" 2>&1; echo "exit $?" >> "$tmp/gcc.out"
"$OCC" -o "$tmp/$base.occ" "$src" && "$tmp/$base.occ" > "$tmp/occ.out" 2>&1; echo "exit $?" >> "$tmp/occ.out"
if diff "$tmp/gcc.out" "$tmp/occ.out" > "$tmp/diff"; then echo "$src: identical ($(wc -l < "$tmp/gcc.out") lines)"; rm -rf "$tmp"; exit 0
else echo "$src: DIFFERENT (< gcc, > occ)"; grep '^[<>]' "$tmp/diff" | head -40; rm -rf "$tmp"; exit 1; fi
