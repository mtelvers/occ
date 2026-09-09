# occutils

The utilities the OCaml build calls, in one binary. It picks which one
to be from the name it was invoked under, so `toolbin/sed` is a link to
it and behaves as `sed`; `occutils sed ...` works too.

    util       what they share: diagnostics, reading lines, buffered
               output, the '-' operand that means standard input
    table      the list of utilities, each with the options it accepts
    textio     cat tee head tail wc cut tr sort uniq cmp
    files      rm cp mv mkdir rmdir ln touch chmod mktemp install
    paths      basename dirname realpath pwd, and echo printf test
               true false
    misc       env which uname hostname ls expr sleep
    grep       grep
    sed        sed
    awk        awk
    diff       diff
    find       find
    xargs      xargs

The regular expressions, the pattern-matching notation and the option
parsing come from `posix/`, which the shell shares; so do the formatting
of `echo` and `printf` and the expression grammar of `test`, so that the
shell's built-in and the utility of the same name cannot drift apart.

The reference is IEEE Std 1003.1-2017 volume XCU for the behaviour, and
the GNU utilities for the details the standard leaves open, since those
are the ones the build's scripts were written against. Everything
compares by byte value: the reference runs are made under `LC_ALL=C`,
and nothing here consults a locale.

## The three that are not small

**sed** is the whole stream editor. The script compiles to a flat array
rather than a tree, because `b` and `t` branch to a label and a block is
only an address with a jump over it: as an array a branch is an index.
Addresses, ranges, negation, `s` with its flags and the GNU case
conversions in a replacement, the hold space, `a i c`, `y`, `n N P D`,
`q Q`, `r w`, `=`, `l`, and `-i` for editing in place.

**awk** is the whole language: the expression grammar with its
precedence and its concatenation-without-an-operator, arrays indexed by
strings, user-defined functions with arrays passed by reference,
`getline` in all its forms, output redirection to files and pipes, and
the built-in functions. The rule that has to be right is the one XCU
calls a numeric string: a value that came from the input and looks like
a number compares as a number, and one that came from the program as a
string compares as a string, which is why `$1 == 10` works without the
program converting anything.

**diff** finds a shortest edit script by Myers' algorithm (1986): the
edit graph is searched by increasing edit distance, keeping for each
diagonal the furthest point reached, which is O(ND) rather than the
O(NM) a full table would need. Output is plain or unified.

## Testing

`tools/utcheck.py` runs 228 cases against the GNU utilities. Each case
is an argument list and a standard input; the two run in identical
scratch directories, and standard output, standard error, the exit
status and the files left behind must all agree. They do, with four
deliberate differences the harness names:

- `awk`'s `for (k in a)` visits keys in a settled order rather than the
  reference's hash order. The standard leaves the order unspecified, and
  a settled one makes a build that uses awk reproducible.
- `find` walks directory entries in sorted order rather than the order
  `readdir` returns them, for the same reason.
- `diff -u` writes a header timestamp with only the resolution a double
  holds, which is under a microsecond at present dates rather than the
  nanosecond the reference prints.

Two bugs this harness caught are worth naming, because both would have
been hard to find from a build failure:

- `wc` aligns its counts in a field whose width comes from the total
  size of its inputs, and drops the alignment when there is only one
  count and one input. A fixed width differs from the reference on
  almost every call.
- `cp` must create a new copy with the source's permission bits, not
  with 0666. Getting that wrong copies a program to somewhere it cannot
  be run from, which the OCaml build hits at once: it copies
  `runtime/ocamlrun` into `boot/` and then runs it.
