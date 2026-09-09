all: a b c d
a: ; echo a > a; echo "made a"
b: ; echo b > b; echo "made b"
c: a ; echo "made c from $<"
d: b c ; @echo "made d"
