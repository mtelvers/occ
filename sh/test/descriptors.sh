# XCU 2.7: descriptors a script opens for itself, as configure does
rm -f d.log
exec 5>d.log
echo "line one" >&5
echo "line two" >&5
( echo "from subshell" >&5 )
sh -c 'echo "from child" >&5'
exec 5>&-
cat d.log
exec 6>>d.log
echo appended >&6
exec 6>&-
cat d.log
exec 7<d.log
read a <&7
echo "read=[$a]"
exec 7<&-
{ echo out; echo err >&2; } 2>&1 | cat
echo both 2>&1 1>/dev/null
exec 3>&1
echo "to saved stdout" >&3
exec 1>d2.log
echo "redirected"
exec 1>&3
cat d2.log
echo "back on stdout"
rm -f d.log d2.log
