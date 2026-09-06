#!/bin/bash
# Compare the bytecode artefacts of two OCaml build trees built from the same
# sources with different C compilers.  ocamlc is run with -absname, so every
# .cmo/.cmt embeds the tree's absolute path; the two paths must have the same
# length and are rewritten before comparing.  Bytecode executables are
# compared after their first line, the "#!/path/to/ocamlrun" header.
# usage: tools/compare-trees.sh <tree-a> <tree-b>
A=$(cd "$1" && pwd); B=$(cd "$2" && pwd)
[ ${#A} -eq ${#B} ] || { echo "tree paths must have equal length for path normalisation"; exit 1; }
same=0; diff=0; missing=0
norm() { sed "s|$A|$B|g" "$1"; }
while read -r f; do
  if [ ! -f "$B/$f" ]; then missing=$((missing+1)); continue; fi
  if cmp -s <(norm "$A/$f") "$B/$f"; then same=$((same+1)); else diff=$((diff+1)); echo "DIFFERS: $f"; fi
done < <(cd "$A" && find . \( -name '*.cmo' -o -name '*.cmi' -o -name '*.cma' -o -name '*.cmt' \) -not -path './_build/*' | sort)
echo "compiled units: $same identical, $diff differ, $missing missing in $B"
for exe in ocamlc ocaml lex/ocamllex ocamldoc/ocamldoc tools/ocamldep yacc/ocamlyacc; do
  [ -f "$A/$exe" ] && [ -f "$B/$exe" ] || continue
  if cmp -s <(tail -n +2 "$A/$exe" | sed "s|$A|$B|g") <(tail -n +2 "$B/$exe"); then echo "$exe: identical after the #! line"
  else echo "$exe: DIFFERS ($(stat -c %s "$A/$exe") vs $(stat -c %s "$B/$exe") bytes)"; fi
done
