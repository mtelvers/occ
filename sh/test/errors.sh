# The shell errors of 2.8.1.  An error in a special built-in, and a
# redirection a special built-in cannot make, end a shell that is not
# interactive; the same error in anything else fails that command and
# leaves the shell running.  The messages differ from one shell to
# another -- they carry the shell's name -- so they are not compared
# here, only what the shell does next.
#
# Each case runs in a subshell, so that the shell this script is
# running in survives to try the next one.

try() {                        # try what: run it, then say what happened
  ( eval "$1"; echo "  ran on, status $?" ) 2>/dev/null
  echo "$1 -> subshell status $?"
}

# a redirection error: the command fails, the shell carries on
try 'echo hi > /nosuchdir/f'
try '/bin/echo hi > /nosuchdir/f'
try 'read x < /nosuchfile'
try '{ echo hi; } > /nosuchdir/f'
try 'while false; do :; done > /nosuchdir/f'
try 'g() { echo hi; }; g > /nosuchdir/f'
try 'x=1 > /nosuchdir/f'
try 'echo hi >&9'

# the same with a special built-in: the shell ends
try ': > /nosuchdir/f'
try 'export x=1 > /nosuchdir/f'

# an error in a special built-in ends the shell.  `local' is tried
# through sh -c rather than through the helper, since inside the helper
# it would be in a function and no error at all.
( sh -c 'local x=1; echo "  ran on"' ) 2>/dev/null
echo "local outside a function -> status $?"
try 'shift 5'
try 'shift abc'
try 'export 1bad=2'
try 'readonly 1bad=2'
try 'unset 1bad'
try 'unset -q'
try 'set -o nosuchopt'
try 'set -Z'
try '. /nosuchfile'
try 'readonly r=1; r=2'

# and one in a regular built-in does not
try 'cd /nosuchdir'
try 'umask nonsense'
try 'read x < /dev/null'

# A redirection that fails undoes the ones before it: the command does
# not run, and what the script prints afterwards must still go where it
# was going.
( echo A > redirected 2> /nosuchdir/f; echo "after a failed redirection" ) 2>/dev/null
echo "the file holds: [$(cat redirected 2>/dev/null)]"

# The options of the special built-ins: `--' ends them, an unknown one
# is an error, and so is an operand that could not be a variable name.
try 'x=1; unset -- x; echo "x is [${x-gone}]"'
try 'export -- one=1; echo "one is $one"'
try 'readonly -- two=2; echo "two is $two"'
try 'unset -q'
try 'export -q x'
try 'export 1bad'
try 'readonly 1bad'
try 'unset 1bad'
