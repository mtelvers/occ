# XCU 2.9: compound commands and lists
true && echo and-ok
false || echo or-ok
false && echo not-shown
true; echo "status=$?"
false; echo "status=$?"
! false; echo "neg=$?"
if true; then echo t; fi
if false; then echo f; else echo e; fi
if false; then echo a; elif true; then echo b; else echo c; fi
i=0
while [ $i -lt 3 ]; do echo "w$i"; i=$((i+1)); done
i=0
until [ $i -ge 2 ]; do echo "u$i"; i=$((i+1)); done
for x in 1 2 3; do
  if [ $x = 2 ]; then continue; fi
  echo "f$x"
done
for x in 1 2 3; do
  if [ $x = 2 ]; then break; fi
  echo "b$x"
done
for i in 1 2; do for j in a b; do echo "$i$j"; done; done
case abc in a*) echo starts-a;; esac
case xyz in a*) echo no;; *) echo default;; esac
case b in a|b|c) echo alt;; esac
case "x y" in "x y") echo quoted;; esac
{ echo group; }
( echo subshell )
v=outer
( v=inner; echo "in=$v" )
echo "out=$v"
echo one | cat | cat
echo pipe | wc -l | tr -d ' '
