#!/bin/bash
# Show how occas's object for one .s file differs from GNU as's:
# disassembly, relocations and frame information.
# usage: tools/asdiff.sh file.s
set -u
HERE=$(cd "$(dirname "$0")/.." && pwd)
OCCAS="$HERE/_build/default/bin/occas.exe"
tmp=$(mktemp -d)
as --64 "$1" -o "$tmp/ref.o" || exit 1
"$OCCAS" "$1" -o "$tmp/occ.o" || exit 1
echo "== disassembly (gas < > occas) =="
objdump -d --no-show-raw-insn "$tmp/ref.o" | sed 1,3d > "$tmp/ref.dis"
objdump -d --no-show-raw-insn "$tmp/occ.o" | sed 1,3d > "$tmp/occ.dis"
diff "$tmp/ref.dis" "$tmp/occ.dis" | head -${2:-20}
echo "== other sections =="
for s in $(readelf -SW "$tmp/ref.o" | awk '/^ *\[ *[0-9]+\]/ { sub(/^ *\[ *[0-9]+\] */, ""); if ($1 ~ /^\./ && $2 != "NOBITS" && $2 != "RELA" && $2 != "SYMTAB" && $2 != "STRTAB" && $2 != "NOTE" && $1 != ".text" && strtonum("0x" $5) > 0) print $1 }'); do
  rm -f "$tmp/ref.bin"; objcopy --dump-section "$s=$tmp/ref.bin" "$tmp/ref.o" "$tmp/junk.o" 2>/dev/null
  rm -f "$tmp/occ.bin"; objcopy --dump-section "$s=$tmp/occ.bin" "$tmp/occ.o" "$tmp/junk.o" 2>/dev/null
  cmp "$tmp/ref.bin" "$tmp/occ.bin" >/dev/null 2>&1 || { echo "$s differs:"; cmp "$tmp/ref.bin" "$tmp/occ.bin" 2>&1 | head -2; }
done
echo "== debug info (decoded, gas < > occas) =="
diff <(readelf --debug-dump=info "$tmp/ref.o" | grep -v 'DW_AT_producer\|DW_AT_comp_dir' | sed 's/(indirect string, offset: 0x[0-9a-f]*)//') \
     <(readelf --debug-dump=info "$tmp/occ.o" | grep -v 'DW_AT_producer\|DW_AT_comp_dir' | sed 's/(indirect string, offset: 0x[0-9a-f]*)//') | head -${2:-20}
echo "== relocations (gas < > occas) =="
readelf -rW "$tmp/ref.o" | grep -E "^[0-9a-f]+ " | awk "{ print \$1, \$3, \$5, \$6, \$7 }" | sort > "$tmp/ref.rel"
readelf -rW "$tmp/occ.o" | grep -E '^[0-9a-f]+ |^Reloc' | awk '{ if ($1=="Relocation") print; else print $1, $3, $5, $6, $7 }' > "$tmp/occ.rel"
diff "$tmp/ref.rel" "$tmp/occ.rel" | head -${2:-20}
echo "== frames (gas < > occas) =="
readelf --debug-dump=frames "$tmp/ref.o" | grep -v '^$' | sed 's/ pc=[0-9a-f.]*//' > "$tmp/ref.fr"
readelf --debug-dump=frames "$tmp/occ.o" | grep -v '^$' | sed 's/ pc=[0-9a-f.]*//' > "$tmp/occ.fr"
diff "$tmp/ref.fr" "$tmp/occ.fr" | head -${2:-20}
rm -rf "$tmp"
