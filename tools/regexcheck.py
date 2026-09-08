#!/usr/bin/env python3
"""Compare posix/regex.ml with GNU grep over a table of patterns.

Every pattern is run against every subject with `grep -o', which prints the
text each match covered, so a disagreement about which match was chosen
(POSIX asks for the leftmost longest) shows up and not just whether one
exists.  Both basic and extended syntax are tried, and the exit status is
compared too.

usage: tools/regexcheck.py [-v]
"""

import subprocess
import sys

OURS = "_build/default/bin/occutils.exe"

# Patterns exercising the constructs of XBD 9.3 and 9.4, plus every distinct
# pattern the OCaml build's own scripts and configure hand to grep and sed.
BRE = [
    "a", "abc", ".", "a.c", "^a", "a$", "^$", "^abc$", "a*", "ab*c", ".*",
    "a\\{2\\}", "a\\{2,\\}", "a\\{1,3\\}", "\\(ab\\)*", "\\(a\\|b\\)c",
    "a\\|ab", "ab\\|a", "\\(a\\)\\1", "[abc]", "[^abc]", "[a-c]", "[]a]",
    "[a-]", "[[:digit:]]", "[[:alpha:]][[:alnum:]]*", "[^[:space:]]",
    "*a", "a**", "\\.", "\\*", "x\\{0,1\\}y", "^[0-9]*\\.[0-9]*\\.[0-9]*$",
    "^[12]", "internal\\|obj\\|stdLabels", "\\.o$", "coreutils", "busybox",
    "^CAMLprim value .*)", "^CAMLprim value [^)]*$",
    "^\\([a-zA-Z_][a-zA-Z0-9_]*\\)=.*", "^[	 ]*datarootdir[	 ]*:*=",
    "\\${datarootdir}", "^#", "^-", "conftest", "[\\\\&|]",
    "\\(caml[a-zA-Z_0-9]*\\)", "a\\+", "a\\?", "\\<ab", "ab\\>", "\\bab\\b",
    "\\w*", "\\s",
]

ERE = [
    "a|ab", "ab|a", "(a|b)+", "a{2}", "a{2,}", "a{1,3}", "(ab)*c",
    "^(a|b)*$", "[[:digit:]]+", "[[:xdigit:]]+", "x?y", "a+b*c?",
    "(caml[a-zA-Z_0-9]+)[$.]([a-zA-Z_0-9]+)_[[:digit:]]+",
    "^(stdlib|camlinternal)(\\.[^i]*)(i?) :",
    "of size ([[:digit:]]+) at 0x[[:xdigit:]]+",
    "data race \\(.*/.+\\+0x[[:xdigit:]]+\\) in ",
    "pid=[[:digit:]]+", "M([0-9]+) \\(0x[[:xdigit:]]+\\)",
    "^[[:space:]]+- (caml_start_program|caml_startup)",
    "(a*)*b", "(a|)b", "()", "a()b", "[-a-z]+", "[a-]", "[]]",
    "\\.[0-9]+", "^$", ".*", "x{0,1}", "(x)(y)(z)",
]

SUBJECTS = [
    "", "a", "b", "ab", "abc", "aab", "aaa", "abcdef", "xyz",
    "a.c", "a*c", "*a", "hello world", "  spaced  ", "\ttab",
    "CAMLprim value caml_hash(value obj)", "CAMLprim value caml_foo(",
    "internal.cmi", "stdLabels.cmi", "obj", "foo.o", "5.6.0",
    "1.2.3", "12.3.4", "GNU coreutils 8.32", "BusyBox v1.30",
    "prefix=/usr/local", "datarootdir := /x", "${datarootdir}/doc",
    "camlStdlib__List.map_1234", "pid=4321 tid=99",
    "of size 8 at 0x7fff1234", "M12 (0xdeadbeef)",
    "    - caml_start_program", "stdlib.cmo :", "stdlib.cmi :",
    "a1b2c3", "___", "A_b-C", "aaaaaaaaaaaaaaaaaaaaaaaa",
    "]x", "-x", "x-", "0x1f", "no digits here",
]


def run(argv, data):
    p = subprocess.run(argv, input=data.encode(), stdout=subprocess.PIPE,
                       stderr=subprocess.PIPE)
    return p.returncode, p.stdout


def main():
    verbose = "-v" in sys.argv
    cases = 0
    bad = 0
    for flags, patterns in (([], BRE), (["-E"], ERE)):
        for pat in patterns:
            for subject in SUBJECTS:
                for extra in ([], ["-i"]):
                    args = flags + extra + ["-o", "-e", pat]
                    g = run(["grep"] + args, subject + "\n")
                    o = run([OURS, "grep"] + args, subject + "\n")
                    cases += 1
                    # GNU grep rejects a few patterns we accept and the other
                    # way round; only compare where GNU did not fail outright
                    if g[0] == 2 or o[0] == 2:
                        if g[0] != o[0]:
                            bad += 1
                            print("STATUS %r %r flags=%s: gnu=%d ours=%d"
                                  % (pat, subject, args, g[0], o[0]))
                        continue
                    if g != o:
                        bad += 1
                        print("DIFF pattern=%r subject=%r flags=%s"
                              % (pat, subject, " ".join(args)))
                        print("  gnu : status=%d %r" % (g[0], g[1]))
                        print("  ours: status=%d %r" % (o[0], o[1]))
                    elif verbose:
                        print("ok %r %r" % (pat, subject))
    print("%d cases, %d differ" % (cases, bad))
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
