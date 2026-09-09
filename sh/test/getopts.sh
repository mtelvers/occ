# getopts (XCU getopts): the options of a script, one call at a time
set -- -a -bval -c -- rest1 rest2
while getopts ab:c opt; do
  echo "opt=[$opt] optarg=[${OPTARG-unset}] optind=$OPTIND"
done
echo "after opt=[$opt] optind=$OPTIND"
shift $((OPTIND-1))
echo "rest=[$*]"

# clustered options share one argument word, and the last of them may
# still take an argument of its own
set -- -abc arg
OPTIND=1
while getopts abc: opt; do echo "[$opt] arg=[${OPTARG-unset}] ind=$OPTIND"; done
echo "end [$opt] ind=$OPTIND"

# an unknown option and a missing argument, reported by a message
set -- -x -a
OPTIND=1
getopts ab opt; echo "1: [$opt] arg=[${OPTARG-unset}] ind=$OPTIND st=$?"
getopts ab opt; echo "2: [$opt] arg=[${OPTARG-unset}] ind=$OPTIND st=$?"
set -- -b
OPTIND=1
getopts ab: opt; echo "3: [$opt] arg=[${OPTARG-unset}] ind=$OPTIND st=$?"

# and the same two reported through the variables, which is what a
# leading colon in the optstring asks for
set -- -q -b
OPTIND=1
getopts :ab: opt; echo "4: [$opt] arg=[${OPTARG-unset}] ind=$OPTIND st=$?"
getopts :ab: opt; echo "5: [$opt] arg=[${OPTARG-unset}] ind=$OPTIND st=$?"

# arguments given to getopts itself are scanned instead of the
# positional parameters
OPTIND=1
getopts abc: opt x -a -b; echo "explicit [$opt] ind=$OPTIND"

# a word that is not an option ends the scan and leaves OPTIND on it
set -- one -a
OPTIND=1
getopts a opt; echo "6: [$opt] ind=$OPTIND st=$?"

# a function may use getopts on its own arguments
f() {
  OPTIND=1
  while getopts n: o; do echo "f saw $o=$OPTARG"; done
}
f -n one
f -n two
