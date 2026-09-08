# $LINENO, which autoconf uses in every error message
echo "at $LINENO"
echo "at $LINENO"
f() {
  echo "in function at $LINENO"
}
f
if true; then
  echo "in if at $LINENO"
fi
for i in 1; do
  echo "in loop at $LINENO"
done
