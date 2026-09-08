#!/bin/sh
# Stage the OCaml-built toolchain in one directory, with a link per
# utility named as the scripts call it.  A PATH holding only this
# directory is a build with no program written in C.
# usage: tools/toolbin.sh [directory]
set -eu
HERE=$(cd "$(dirname "$0")/.." && pwd)
B="$HERE/_build/default/bin"
out=${1:-$HERE/toolbin}
mkdir -p "$out"
for t in main occas occar occld occmake occsh occutils; do
  [ -f "$B/$t.exe" ] || { echo "toolbin: $B/$t.exe is missing; run day10 build ." >&2; exit 1; }
  if [ "$t" = main ]; then cp -f "$B/$t.exe" "$out/occ"; else cp -f "$B/$t.exe" "$out/$t"; fi
done
# the compiler and the binutils replacements, under the names a build uses
for pair in cc:occ gcc:occ as:occas ar:occar ld:occld make:occmake sh:occsh; do
  name=${pair%%:*}; target=${pair#*:}
  ln -sf "$target" "$out/$name"
done
# every utility occutils holds, as a link named after it
for u in $("$out/occutils" 2>&1 | sed -n 's/^utilities://p'); do
  ln -sf occutils "$out/$u"
done
echo "staged in $out: $(ls "$out" | wc -l) entries"
