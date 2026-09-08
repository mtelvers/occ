# XCU 2.2.1: a backslash-newline disappears
x="one"\
"two"
echo "[$x]"
y=abc\
def
echo "[$y]"
cmd="printf"\
' %s\n'
echo "[$cmd]"
echo one \
  two \
  three
z="a
b"
echo "[$z]"
w='c\
d'
echo "[$w]"
if true && \
   true; then echo "joined condition"; fi
