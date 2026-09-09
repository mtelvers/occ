#!/bin/sh
# Compare occmake with GNU make on a set of Makefiles: the sequence of
# commands each would run (make -n) must agree.
#
# A makefile whose name ends in .run.mk is run for real instead, in a
# scratch directory of its own, and what the two makes did is compared:
# the output, the exit status and the files left behind.  That is for the
# behaviour a plan cannot show -- what happens when a recipe fails.
#
# usage: tools/mkcheck.sh [dir-of-makefiles]
set -u
HERE=$(cd "$(dirname "$0")/.." && pwd)
OCM="$HERE/_build/default/bin/occmake.exe"
dir=${1:-$HERE/mk/test}
n=0; fail=0
for mf in "$dir"/*.mk; do
  [ -f "$mf" ] || continue
  case $mf in *.run.mk) continue;; esac
  n=$((n+1))
  # A makefile with a .expected file beside it is one where occmake is
  # meant to differ from GNU make: it is compared with that file.
  if [ -f "$mf.expected" ]; then
    g=$(cat "$mf.expected")
  else
    g=$(cd "$(dirname "$mf")" && make -n -f "$(basename "$mf")" 2>/dev/null)
  fi
  o=$(cd "$(dirname "$mf")" && "$OCM" -n -f "$(basename "$mf")" 2>/dev/null)
  if [ "$g" = "$o" ]; then :; else fail=$((fail+1)); echo "DIFF $(basename "$mf")"; fi
done
# the ones that have to be run rather than planned
work=${TMPDIR:-/tmp}/mkcheck.$$
for mf in "$dir"/*.run.mk; do
  [ -f "$mf" ] || continue
  base=$(basename "$mf")
  # extra arguments this makefile is to be run with, if it names any
  flags=$(sed -n 's/^# *flags: *//p' "$mf")
  for goal in $(sed -n 's/^# *goals: *//p' "$mf"); do
    n=$((n+1))
    bad=""
    for which in a b; do
      rm -rf "$work/$which"; mkdir -p "$work/$which"
      cp "$mf" "$work/$which/Makefile"
      if [ "$which" = a ]; then m=make; else m="$OCM"; fi
      # the capture files are named so that no target collides with them
      ( cd "$work/$which" && "$m" $flags "$goal" >.out 2>.err; echo $? > .status )
      # the make's own name is in its messages, so it is taken out
      for f in .out .err; do
        sed -e 's/^[a-z]*make\(\.exe\)\{0,1\}\(\[[0-9]*\]\)*:/make:/' \
            -e "s#$work/$which#DIR#g" "$work/$which/$f" > "$work/$which/$f.norm"
      done
      ( cd "$work/$which" && ls | grep -v '^Makefile$' | sort > .files )
    done
    for f in .out.norm .err.norm .status .files; do
      cmp -s "$work/a/$f" "$work/b/$f" || bad="$bad $f"
    done
    if [ -n "$bad" ]; then
      fail=$((fail+1))
      echo "DIFF $base${flags:+ $flags} $goal:$bad"
      if [ -n "${VERBOSE:-}" ]; then
        for f in .out.norm .err.norm .status .files; do
          diff "$work/a/$f" "$work/b/$f"
        done
      fi
    fi
  done
done
rm -rf "$work"
echo "$n makefiles, $fail differ"
