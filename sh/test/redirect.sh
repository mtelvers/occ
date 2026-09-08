# XCU 2.7: redirection
rm -f t1 t2
echo one > t1
echo two >> t1
cat t1
cat < t1
echo err 1>&2 2>/dev/null
{ echo a; echo b; } > t2
cat t2
exec 5> t1
echo through5 >&5
exec 5>&-
cat t1
cat nonexistent 2>/dev/null || echo "failed as expected"
echo x > t1 2>&1
cat t1
: > t1
cat t1
echo "count=$(wc -l < t2 | tr -d ' ')"
cat <<EOF
here $((1+1)) doc
EOF
cat <<'EOF'
literal $x doc
EOF
cat <<-EOF
	stripped
	  kept
EOF
a=1
cat <<E1; cat <<E2
first $a
E1
second
E2
rm -f t1 t2
