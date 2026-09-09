#!/bin/sh
# Compare how many recipes occmake and GNU make run at once in a
# recursive build.
#
# -jN is a limit for the whole build, not for each make in it: the makes
# share a pool of tokens (GNU make manual 5.7.1).  A make that kept N
# for itself would run N recipes per directory, so the number to check
# is the peak over the whole tree, and it must be the same for both
# makes.  Each recipe here leaves a file for as long as it runs and
# writes down how many were there, so the largest number written is that
# peak.
#
# usage: tools/jscheck.sh [j-values...]
set -u
HERE=$(cd "$(dirname "$0")/.." && pwd)
OCCMAKE="$HERE/_build/default/bin/occmake.exe"
GNU=${GNUMAKE:-make}
work=${TMPDIR:-/tmp}/jscheck.$$
mkdir -p "$work"
cat > "$work/Makefile" <<'MK'
LEAVES = a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4
all: a b c
a: ; @$(MAKE) -f Makefile a1 a2 a3 a4
b: ; @$(MAKE) -f Makefile b1 b2 b3 b4
c: ; @$(MAKE) -f Makefile c1 c2 c3 c4
$(LEAVES):
	@mkdir -p run; touch run/$@; ls run | wc -l >> counts; sleep 0.4; rm -f run/$@
MK
# the same tree with the pool taken away, which is what tells a working
# pool from a make that simply never runs much at once
sed 's/@\$(MAKE) -f/@MAKEFLAGS= $(MAKE) -j2 -f/' "$work/Makefile" > "$work/Makefile.nopool"

# TMPDIR is set to the work directory so that the pool these runs make
# is there and not among any another build's
peak() {                       # peak make-binary makefile -jN
  rm -rf "$work/run" "$work/counts"
  ( cd "$work" && TMPDIR="$work" "$1" -f "$2" "$3" -s >/dev/null 2>&1 )
  sort -n "$work/counts" | tail -1
}

fail=0; n=0
for j in ${@:-1 2 4 8}; do
  for mf in Makefile Makefile.nopool; do
    n=$((n+1))
    a=$(peak "$GNU" "$mf" "-j$j")
    b=$(peak "$OCCMAKE" "$mf" "-j$j")
    if [ "$a" != "$b" ]; then
      fail=$((fail+1))
      echo "DIFF $mf -j$j: $GNU peaked at $a, occmake at $b"
    fi
  done
done
# a pool is a named pipe, and the make that made it removes it
left=$(ls "$work"/occmake-jobs.* 2>/dev/null | wc -l)
if [ "$left" != 0 ]; then
  fail=$((fail+1))
  echo "DIFF $left job pool(s) left behind"
fi
rm -rf "$work"
echo "$n runs, $fail differ"
[ "$fail" = 0 ]
