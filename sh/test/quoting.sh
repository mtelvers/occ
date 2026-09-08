# XCU 2.2: quoting
echo 'single $x `cmd` "dq" \n'
x=val
echo "double $x end"
echo "escaped \$x \\ \" \` done"
echo \a\ b\\c
echo 'a'"b"c\ d
echo "" '' end
echo "$x"'$x'\$x
printf '%s|' a "b c" 'd  e'; echo
echo "line1
line2"
y="has  two"
echo $y
echo "$y"
echo 'it'\''s'
echo "a\qb"
