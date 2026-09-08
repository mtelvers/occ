#!/bin/sh
# Compare occsh with the reference shell on every test script: standard
# output, standard error and exit status must all agree.
# usage: tools/shcheck.sh [dir-of-scripts] [reference-shell]
set -u
HERE=$(cd "$(dirname "$0")/.." && pwd)
OCSH="$HERE/_build/default/bin/occsh.exe"
dir=${1:-$HERE/sh/test}
ref=${2:-/bin/sh}
work=${TMPDIR:-/tmp}/shcheck.$$
n=0; fail=0
for s in "$dir"/*.sh; do
  [ -f "$s" ] || continue
  n=$((n+1))
  base=$(basename "$s")
  rm -rf "$work"; mkdir -p "$work/a" "$work/b"
  # A script with a .expected file beside it is one where occsh is meant
  # to differ from the reference shell: it is compared with that file
  # instead.  $LINENO is the case: dash does not have it, and a shell
  # that does saves configure from rewriting itself.
  if [ -f "$s.expected" ]; then
    cp "$s.expected" "$work/a/out"
    : > "$work/a/err"
    echo 0 > "$work/a/status"
  else
    ( cd "$work/a" && "$ref" "$s" >out 2>err; echo $? > status )
  fi
  ( cd "$work/b" && "$OCSH" "$s" >out 2>err; echo $? > status )
  bad=""
  for f in out err status; do
    cmp -s "$work/a/$f" "$work/b/$f" || bad="$bad $f"
  done
  # a script may leave files behind; those must match too
  ( cd "$work/a" && ls -a | grep -v '^\.$\|^\.\.$' | sort > /tmp/shcheck.la.$$ )
  ( cd "$work/b" && ls -a | grep -v '^\.$\|^\.\.$' | sort > /tmp/shcheck.lb.$$ )
  cmp -s /tmp/shcheck.la.$$ /tmp/shcheck.lb.$$ || bad="$bad files"
  rm -f /tmp/shcheck.la.$$ /tmp/shcheck.lb.$$
  if [ -n "$bad" ]; then
    fail=$((fail+1))
    echo "DIFF $base:$bad"
    if [ -n "${VERBOSE:-}" ]; then
      for f in $bad; do
        [ "$f" = files ] && continue
        echo "--- $f (reference vs occsh)"
        diff "$work/a/$f" "$work/b/$f" | head -30
      done
    fi
  fi
done
rm -rf "$work"
echo "$n scripts, $fail differ"
[ "$fail" = 0 ]
