# XCU 2.7.4: here-documents, including several on one line
cat <<E
plain $HOME-free body
E
cat <<'E'
literal $notexpanded `nor this`
E
cat <<-E
	tab-stripped
		deeper kept
E
a=A; b=B
cat <<E1; cat <<E2
first $a
E1
second $b
E2
cat <<E | tr a-z A-Z
piped body
E
f() {
  cat <<E
in a function
E
}
f
while read line <<E
one line
E
do echo "read [$line]"; break; done
cat <<E
a backslash: \\ and a dollar: \$ and a quote: "
E
cat <<"E"
quoted delimiter: $x stays
E
: <<E
discarded
E
echo done
