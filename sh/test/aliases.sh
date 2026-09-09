# Aliases (XCU 2.3.1 and the alias utility).  A definition applies to
# the lines read after it, not to the rest of its own line, because the
# line was read before the alias existed.
alias hello='echo hello'
hello
hello world

# the value is lexed where the name stood, so it may hold anything a
# command could
alias two='echo one; echo two'
two
alias redir='echo out >file'
redir
cat file

# a name that stands for itself is replaced once, so this is not a regress
alias echo='echo seen'
echo done
unalias echo

# a value ending in a blank leaves the following word to be looked at too
alias e='echo '
alias subject=world
e subject
alias plain=world
echo plain

# one alias may name another
alias inner='echo deep'
alias outer=inner
outer

# what a definition looks like written out again
alias q="echo 'a b'"
alias q
q
alias empty=
alias empty

# and reported by type and command -v
type e
command -v e

# unalias removes one, and -a removes them all
unalias q
alias q
echo "status $?"
unalias -a
alias
echo "none left"

# an alias is not looked for where a command name cannot go
alias arg=replaced
echo arg
