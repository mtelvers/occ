#!/bin/sh
# Compare occas with GNU as on every .s file under the given directories:
# the bytes of each allocated section must be identical and the relocations
# (type, symbol, addend, offset) must agree.  Prints one line per mismatch.
# usage: tools/ascheck.sh dir...
set -u
HERE=$(cd "$(dirname "$0")/.." && pwd)
OCCAS="$HERE/_build/default/bin/occas.exe"
tmp=$(mktemp -d)
n=0; fail=0
# named sections with file content (not NOBITS, not empty), except the tables
sections() { readelf -SW "$1" | awk '/^ *\[ *[0-9]+\]/ { sub(/^ *\[ *[0-9]+\] */, ""); if ($1 ~ /^\./ && $2 != "NOBITS" && $2 != "RELA" && $2 != "SYMTAB" && $2 != "STRTAB" && $2 != "NOTE" && strtonum("0x" $5) > 0) print $1 }' | sort -u; }
relocs() {
  # "section offset type symbol+addend", with local symbols reduced to their section
  readelf -rW "$1" | awk '
    /^Relocation section/ { sec=$3; gsub(/\x27/, "", sec); next }
    /^[0-9a-f]+ / { printf "%s %s %s %s\n", sec, $1, $3, ($5 ~ /^\./ ? $5 : $5) " " $6 " " $7 }' | sort
}
# the symbol table without section symbols and indices: type, binding,
# visibility, defined/undefined/common/absolute, value, size, name
symbols() {
  readelf -sW "$1" | awk '/^ *[0-9]+:/ && $1 != "0:" && $4 != "SECTION" && $4 != "FILE" {
    ndx = ($7 == "UND" || $7 == "COM" || $7 == "ABS") ? $7 : "DEF";
    print $4, $5, $6, ndx, ($7 == "UND" ? "0" : $2), $3, $8 }' | sort
}
for d in "$@"; do
  for f in $(find "$d" -name '*.s' | sort); do
    n=$((n+1))
    as --64 "$f" -o "$tmp/ref.o" 2>/dev/null || { echo "SKIP (gas rejects) $f"; continue; }
    if ! "$OCCAS" "$f" -o "$tmp/occ.o" 2>"$tmp/err"; then
      fail=$((fail+1)); echo "FAIL (occas error) $f: $(head -1 "$tmp/err")"; continue
    fi
    bad=""
    # gas synthesises a compile unit when .loc is used without .debug_info; ours
    # differs in producer string, so those sections are checked by asdiff instead
    synth=""; grep -q '^[[:space:]]*\.section[[:space:]]*\.debug_info' "$f" || synth="debug_info debug_abbrev debug_aranges debug_str"
    for s in $(sections "$tmp/ref.o"); do
      case " $synth " in *" ${s#.} "*) continue;; esac
      rm -f "$tmp/ref.bin"; objcopy --dump-section "$s=$tmp/ref.bin" "$tmp/ref.o" "$tmp/junk.o" 2>/dev/null
      rm -f "$tmp/occ.bin"; objcopy --dump-section "$s=$tmp/occ.bin" "$tmp/occ.o" "$tmp/junk.o" 2>/dev/null
      cmp -s "$tmp/ref.bin" "$tmp/occ.bin" || bad="$bad $s"
    done
    symbols "$tmp/ref.o" > "$tmp/ref.sym"; symbols "$tmp/occ.o" > "$tmp/occ.sym"
    cmp -s "$tmp/ref.sym" "$tmp/occ.sym" || bad="$bad symbols"
    relocs "$tmp/ref.o" | grep -v "^.rela.debug_\(info\|aranges\)" > "$tmp/ref.rel"; relocs "$tmp/occ.o" | grep -v "^.rela.debug_\(info\|aranges\)" > "$tmp/occ.rel"
    if [ -z "$synth" ]; then relocs "$tmp/ref.o" > "$tmp/ref.rel"; relocs "$tmp/occ.o" > "$tmp/occ.rel"; fi
    cmp -s "$tmp/ref.rel" "$tmp/occ.rel" || bad="$bad relocs"
    if [ -n "$bad" ]; then fail=$((fail+1)); echo "DIFF $f:$bad"; fi
  done
done
rm -rf "$tmp"
echo "$n files, $fail differ"
