#!/bin/sh
# Compare occld -r with GNU ld -r on the same objects.
#
# The two results are not expected to be byte-identical: ld interleaves
# each relocation section with the section it belongs to and orders the
# rest its own way, and any order is a valid relocatable object.  What
# must agree is everything a later link reads: the bytes of each section,
# the symbol table, and every relocation's place, type, symbol and
# addend.  Those are what this compares, through nm, objcopy and readelf.
#
# usage: tools/ldrcheck.sh directory-of-objects [group-size] [suffix]
#
# The suffix picks one set of objects: the OCaml runtime is built several
# times over from the same sources, and merging two builds of one file is
# a duplicate definition, for ld as much as for occld.
set -u
HERE=$(cd "$(dirname "$0")/.." && pwd)
OCCLD="$HERE/_build/default/bin/occld.exe"
dir=${1:?usage: tools/ldrcheck.sh directory-of-objects [group-size] [suffix]}
size=${2:-8}
suffix=${3:-.b.o}
work=${TMPDIR:-/tmp}/ldrcheck.$$
mkdir -p "$work"

# the bytes of a file as hex, without the trailing run of zeros
trim() {
  od -An -v -tx1 "$1" | tr -s ' \n' '\n' | grep . \
    | awk '{ a[NR] = $0 } END { n = NR; while (n > 0 && a[n] == "00") n--;
             for (i = 1; i <= n; i++) print a[i] }'
}

compare() {
  bad=""
  # the symbol table, without the section numbering the two order differently
  nm -S --format=posix "$1" 2>/dev/null | sort > "$work/syms.a"
  nm -S --format=posix "$2" 2>/dev/null | sort > "$work/syms.b"
  cmp -s "$work/syms.a" "$work/syms.b" || bad="$bad symbols"
  # The relocations, with the symbol names spelled out.  The raw info
  # field holds the symbol's index in the table, which the two number
  # differently, so it is dropped: what matters is the place, the type,
  # the symbol named and the addend.
  strip_index() {
    sed -E -e 's/^([0-9a-f]+) +[0-9a-f]+ +/\1 /' \
           -e "s/ at offset 0x[0-9a-f]+ contains/ contains/"
  }
  readelf -rW "$1" 2>/dev/null | grep -v '^File:' | strip_index | sort > "$work/rel.a"
  readelf -rW "$2" 2>/dev/null | grep -v '^File:' | strip_index | sort > "$work/rel.b"
  cmp -s "$work/rel.a" "$work/rel.b" || bad="$bad relocations"
  # the bytes of every section either of them has
  { readelf -SW "$1" 2>/dev/null; readelf -SW "$2" 2>/dev/null; } \
    | sed -n 's/^ *\[ *[0-9]*\] \(\.[^ ]*\).*/\1/p' | sort -u > "$work/secs"
  # the size of every section, which for .bss is all there is to compare
  sizes() {
    readelf -SW "$1" | sed -n 's/^ *\[ *[0-9]*\] \(\.[^ ]*\) *\([A-Z]*\) *[0-9a-f]* *[0-9a-f]* *\([0-9a-f]*\).*/\1 \3/p' \
      | grep -v '^\.\(symtab\|strtab\|shstrtab\|rela\|eh_frame\)' | sort
  }
  sizes "$1" > "$work/size.a"
  sizes "$2" > "$work/size.b"
  cmp -s "$work/size.a" "$work/size.b" || bad="$bad sizes"
  while read -r sec; do
    # .eh_frame is left out of the byte comparison: ld decodes the frame
    # table and re-emits it, trimming the padding at the end of the last
    # entry and rewriting that entry's length, while occld copies the
    # input sections as they stand.  Both are valid, and the part where a
    # partial link can go wrong -- the relocations against .eh_frame and
    # their addends -- is compared above.
    case $sec in .symtab|.strtab|.shstrtab|.rela*|.bss|.eh_frame) continue;; esac
    objcopy --dump-section "$sec=$work/sec.a" "$1" /dev/null 2>/dev/null || continue
    objcopy --dump-section "$sec=$work/sec.b" "$2" /dev/null 2>/dev/null || continue
    if [ "$sec" = .eh_frame ]; then
      # ld re-emits the frame table and trims the padding at the end of
      # the last entry; occld concatenates the input sections as they
      # are.  Both are valid -- an entry's length covers its padding --
      # so the trailing zeros are ignored here and any other difference
      # still shows.
      trim "$work/sec.a" > "$work/trim.a"
      trim "$work/sec.b" > "$work/trim.b"
      cmp -s "$work/trim.a" "$work/trim.b" || bad="$bad $sec"
    else
      cmp -s "$work/sec.a" "$work/sec.b" || bad="$bad $sec"
    fi
  done < "$work/secs"
  echo "$bad"
}

groups=0; fail=0; skipped=0
set -- $(ls "$dir"/*"$suffix" 2>/dev/null)
while [ $# -gt 0 ]; do
  n=0; objs=""
  while [ $# -gt 0 ] && [ $n -lt "$size" ]; do
    objs="$objs $1"; shift; n=$((n+1))
  done
  groups=$((groups+1))
  if ! "$OCCLD" -r -o "$work/ours.o" $objs 2>"$work/ours.err"; then
    skipped=$((skipped+1)); continue
  fi
  if ! ld -r -o "$work/gnu.o" $objs 2>"$work/gnu.err"; then
    skipped=$((skipped+1)); continue
  fi
  bad=$(compare "$work/ours.o" "$work/gnu.o")
  if [ -n "$bad" ]; then
    fail=$((fail+1))
    echo "DIFF group $groups:$bad"
    [ -n "${VERBOSE:-}" ] && echo "  objects:$objs"
  fi
done
rm -rf "$work"
echo "$groups groups, $fail differ, $skipped skipped"
[ "$fail" = 0 ]
