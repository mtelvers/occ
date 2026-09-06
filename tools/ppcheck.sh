#!/bin/bash
# Compare occ's preprocessor with gcc -E on every corpus unit: both outputs
# are tokenized by occ's lexer and the token sequences (without positions)
# must match.  The corpus .i files were made by gcc -E from the sources
# named in their first line marker, with the flags mkcorpus.sh used.
# usage: tools/ppcheck.sh [max-shown]
set -u
HERE=$(cd "$(dirname "$0")/.." && pwd)
OCC="$HERE/_build/default/bin/main.exe"
OCAML=${OCAML:-"$HOME/ocaml"}
show=${1:-10}
n=0; fail=0
tokens() { OCC_NATIVE=cc "$OCC" --dump=tokens "$1" 2>/dev/null | sed 's/^[^ ]*:[0-9]*:[0-9]*: //'; }
for i in $(find "$HERE/corpus" -name '*.i' | sort); do
  src=$(sed -n '1s/^# [0-9]* "\(.*\)".*/\1/p' "$i")
  case "$i" in
    */runtime/native/*) flags="-DNATIVE_CODE -DTARGET_amd64 -DMODEL_default -DSYS_linux";;
    *) flags="";;
  esac
  dir=$(dirname "$src")
  n=$((n+1))
  out=$(mktemp --suffix=.i)
  if ! OCC_NATIVE=pp "$OCC" -E -I "$HERE/corpus/shim/runtime" -I "$dir" -D_FILE_OFFSET_BITS=64 -DCAMLDLLIMPORT= -DIN_CAML_RUNTIME $flags "$src" -o "$out" 2>"$out.err"; then
    fail=$((fail+1)); [ $fail -le "$show" ] && echo "${i#$HERE/corpus/}: $(head -1 "$out.err" | cut -c1-150)"
  elif ! cmp -s <(tokens "$i") <(tokens "$out"); then
    fail=$((fail+1)); [ $fail -le "$show" ] && { echo "${i#$HERE/corpus/}: token streams differ:"; diff <(tokens "$i") <(tokens "$out") | head -4 | cut -c1-120; }
  fi
  rm -f "$out" "$out.err"
done
echo "ppcheck: $n units, $fail differ from gcc -E"
