# A shell reads a line, runs it, and only then reads the next (2.10.2),
# so a definition on one line is there for the next and the lines before
# one that will not parse have already run.
echo first
f() { echo from a function; }
f
alias a='echo from an alias'
a
# a command substitution that spans lines is still one line's worth
x=$(echo one
echo two)
echo "[$x]"
# a here-document body is read as the line is crossed
cat <<END
body
END
echo last
# A syntax error ends a shell that is not interactive (2.8.1), and only
# that shell: the lines it had already read have run, and the shell that
# started it carries on.  The message differs from one shell to another,
# so it is not compared here.
echo before
( echo inner; eval 'if' ) 2>/dev/null
echo "status $?"
echo after
# Text with no command in it has the status of a command that
# succeeded, and not the status of whatever ran last.
false; eval ""; echo "eval empty: $?"
false; eval "# nothing but a comment"; echo "eval comment: $?"
: > nothing
false; . ./nothing; echo "dot empty: $?"
false; { :; }; echo "group: $?"
