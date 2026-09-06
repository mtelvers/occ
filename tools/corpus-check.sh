#!/bin/sh
# Run one native phase over every corpus unit and summarise the failures
# by message, most common first, with one example location each.
# usage: tools/corpus-check.sh tokens|ast|typed|ir|asm|obj  [max-messages-shown]
# "obj" compiles and assembles natively, so the assembler checks the output.
set -u
HERE=$(cd "$(dirname "$0")/.." && pwd)
OCC="$HERE/_build/default/bin/main.exe"
stage=$1; show=${2:-15}
log=$(mktemp)
n=0; fail=0
for f in $(find "$HERE/corpus" -name '*.i' | sort); do
  n=$((n+1))
  if [ "$stage" = obj ]; then
    ok=true; err=$("$OCC" -c -g "$f" -o /dev/null 2>&1) || ok=false
  else
    ok=true; err=$(OCC_NATIVE=cc "$OCC" "--dump=$stage" "$f" 2>&1 >/dev/null) || ok=false
  fi
  if ! $ok; then
    fail=$((fail+1))
    first=$(echo "$err" | grep -m1 -i 'error' | sed "s|$HERE/||; s|$HOME/ocaml/||; s|^/tmp/occ[0-9a-f]*\.s:|asm:|")
    [ -z "$first" ] && first=$(echo "$err" | head -1)
    # message without the location, for grouping
    msg=$(echo "$first" | sed 's/^[^ ]*:[0-9]*:[0-9]*: //; s/^asm:[0-9]*: //')
    printf '%s\t%s\n' "$msg" "$first" >> "$log"
  fi
done
if [ -s "$log" ]; then
  awk -F'\t' -v show="$show" '
    { c[$1]++; if (!($1 in ex)) ex[$1] = $2 }
    END { for (m in c) printf "%d\t%s\t%s\n", c[m], m, ex[m] }' "$log" \
  | sort -t "$(printf '\t')" -k1,1nr | head -"$show" \
  | awk -F'\t' '{ printf "%4d  %s\n      e.g. %s\n", $1, $2, substr($3, 1, 150) }'
fi
rm -f "$log"
echo "$stage: $n units, $fail failed"
