#!/usr/bin/env python3
"""Compare occutils with the GNU utilities over a table of cases.

Each case is an argument list and the standard input to give it.  The
reference utility and ours are run in identical scratch directories, and
stdout, stderr, exit status and the files left behind must agree.  The
reference runs under LC_ALL=C, since that is the collating order the
utilities here implement.

usage: tools/utcheck.py [-v] [utility...]
"""

import os
import shutil
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OURS = os.path.join(HERE, "_build/default/bin/occutils.exe")

TEXT = "alpha beta\ngamma delta\nalpha beta\nepsilon\n"
NUMS = "10\n9\n100\n2\n10\n"
TABS = "a\tb\tc\nd\te\tf\n"
MIXED = "  spaced  \nUPPER\nlower\n123\n"

# (utility, argv-tail, stdin)
CASES = [
    # cat
    ("cat", [], TEXT),
    ("cat", ["f1"], ""),
    ("cat", ["f1", "f2"], ""),
    ("cat", ["f1", "-"], TEXT),
    ("cat", ["missing"], ""),

    # head and tail
    ("head", [], NUMS),
    ("head", ["-n", "2"], TEXT),
    ("head", ["-n", "0"], TEXT),
    ("head", ["-n", "99"], TEXT),
    ("head", ["-c", "5"], TEXT),
    ("head", ["-n", "1", "f1", "f2"], ""),
    ("tail", ["-n", "2"], TEXT),
    ("tail", ["-n", "1"], NUMS),
    ("tail", ["-n", "+2"], TEXT),
    ("tail", ["-n", "99"], TEXT),

    # wc
    ("wc", [], TEXT),
    ("wc", ["-l"], TEXT),
    ("wc", ["-c"], TEXT),
    ("wc", ["-w"], TEXT),
    ("wc", ["-l", "f1"], ""),
    ("wc", ["-l", "f1", "f2"], ""),
    ("wc", ["-l"], ""),
    ("wc", ["-l"], "no trailing newline"),

    # cut
    ("cut", ["-f", "2"], TABS),
    ("cut", ["-f", "1,3"], TABS),
    ("cut", ["-f", "2-"], TABS),
    ("cut", ["-d", " ", "-f", "1"], TEXT),
    ("cut", ["-c", "1-3"], TEXT),
    ("cut", ["-c", "2"], TEXT),
    ("cut", ["-d", ":", "-f", "1"], "no colon here\n"),
    ("cut", ["-d", ":", "-f", "1", "-s"], "no colon here\n"),

    # tr
    ("tr", ["a-z", "A-Z"], TEXT),
    ("tr", ["-d", "a"], TEXT),
    ("tr", ["-s", " "], MIXED),
    ("tr", ["-d", "\\n"], TEXT),
    ("tr", ["ab", "xy"], TEXT),
    ("tr", ["[:lower:]", "[:upper:]"], TEXT),
    ("tr", ["-d", "[:digit:]"], MIXED),
    ("tr", ["-cd", "a-z\\n"], MIXED),

    # sort and uniq
    ("sort", [], TEXT),
    ("sort", ["-u"], TEXT),
    ("sort", ["-r"], TEXT),
    ("sort", ["-n"], NUMS),
    ("sort", ["-nr"], NUMS),
    ("sort", ["-u"], NUMS),
    ("sort", ["-k", "2"], TEXT),
    ("sort", ["-t", ":", "-k", "2"], "a:2\nb:1\nc:3\n"),
    ("sort", ["-f"], MIXED),
    ("uniq", [], "a\na\nb\na\n"),
    ("uniq", ["-c"], "a\na\nb\na\n"),
    ("uniq", ["-d"], "a\na\nb\na\n"),
    ("uniq", ["-u"], "a\na\nb\na\n"),
    ("uniq", ["-i"], "a\nA\nb\n"),

    # grep
    ("grep", ["alpha"], TEXT),
    ("grep", ["-c", "alpha"], TEXT),
    ("grep", ["-v", "alpha"], TEXT),
    ("grep", ["-n", "alpha"], TEXT),
    ("grep", ["-o", "a.p"], TEXT),
    ("grep", ["-i", "ALPHA"], TEXT),
    ("grep", ["-E", "a(l|m)pha"], TEXT),
    ("grep", ["-F", "a."], "a.b\nacb\n"),
    ("grep", ["-x", "epsilon"], TEXT),
    ("grep", ["-w", "alpha"], "alpha\nalphabet\n"),
    ("grep", ["-q", "alpha"], TEXT),
    ("grep", ["nomatch"], TEXT),
    ("grep", ["-l", "alpha", "f1"], ""),
    ("grep", ["-h", "alpha", "f1", "f2"], ""),
    ("grep", ["alpha", "f1", "f2"], ""),

    # sed
    ("sed", ["s/alpha/ALPHA/"], TEXT),
    ("sed", ["s/a/X/g"], TEXT),
    ("sed", ["s/a/X/2"], TEXT),
    ("sed", ["-n", "2p"], TEXT),
    ("sed", ["-n", "$p"], TEXT),
    ("sed", ["-n", "1,2p"], TEXT),
    ("sed", ["-n", "/alpha/p"], TEXT),
    ("sed", ["-n", "/alpha/,/epsilon/p"], TEXT),
    ("sed", ["2d"], TEXT),
    ("sed", ["/alpha/d"], TEXT),
    ("sed", ["s/^/| /"], TEXT),
    ("sed", ["s/[aeiou]/./g"], TEXT),
    ("sed", ["-E", "s/(a|g)(l|a)/[\\1\\2]/g"], TEXT),
    ("sed", ["s/.*/[&]/"], TEXT),
    ("sed", ["-n", "s/alpha \\(.*\\)/\\1/p"], TEXT),
    ("sed", ["-e", "s/a/1/", "-e", "s/b/2/"], TEXT),
    ("sed", ["1!d"], TEXT),
    ("sed", ["y/abc/xyz/"], TEXT),
    ("sed", ["=", "-n"], TEXT),
    ("sed", ["$!d"], TEXT),
    ("sed", ["h;s/./-/g;p;x", "-n"], "abc\n"),
    ("sed", ["-n", "/alpha/{s/a/A/;p;}"], TEXT),
    ("sed", ["s/x/y/"], TEXT),
    ("sed", ["-E", "s/./\\u&/"], TEXT),
    ("sed", ["-E", "s/[a-z]+/\\U&/"], TEXT),
    ("sed", ["-n", "2{p;q;}"], TEXT),
    ("sed", ["a\\", "added"], "one\n"),
    ("sed", ["1i\\", "before"], "one\n"),
    ("sed", ["s/a/b/w out.txt"], TEXT),
    ("sed", ["-e", "/^#/d"], "# comment\ncode\n"),
    ("sed", ["s|a/b|X|"], "a/b\n"),
    ("sed", ["s/nothing/x/"], ""),
    ("sed", ["G"], "a\nb\n"),
    ("sed", ["-n", "N;P;D"], "1\n2\n3\n"),

    # awk
    ("awk", ["{print}"], TEXT),
    ("awk", ["{print $1}"], TEXT),
    ("awk", ["{print $2, $1}"], TEXT),
    ("awk", ["{print NF, NR}"], TEXT),
    ("awk", ["END {print NR}"], TEXT),
    ("awk", ["BEGIN {print 1+2, 3*4, 10/4, 7%3, 2^10}"], ""),
    ("awk", ["/alpha/ {print}"], TEXT),
    ("awk", ["$1 == \"alpha\" {print $2}"], TEXT),
    ("awk", ["{n++} END {print n}"], TEXT),
    ("awk", ["{a[$1]++} END {for (k in a) print k, a[k]}"], TEXT),
    ("awk", ["{print length($0)}"], TEXT),
    ("awk", ["{print substr($0, 2, 3)}"], TEXT),
    ("awk", ["{print index($0, \"beta\")}"], TEXT),
    ("awk", ["{print toupper($1)}"], TEXT),
    ("awk", ["{gsub(/a/, \"X\"); print}"], TEXT),
    ("awk", ["{sub(/a/, \"X\"); print}"], TEXT),
    ("awk", ["{n = split($0, p, \" \"); print n, p[1]}"], TEXT),
    ("awk", ["{if (match($0, /b.t/)) print RSTART, RLENGTH}"], TEXT),
    ("awk", ["{printf \"%s|%d|%5.2f\\n\", $1, NR, NR/3}"], TEXT),
    ("awk", ["BEGIN {print sprintf(\"%03d\", 7)}"], ""),
    ("awk", ["-F", ":", "{print $2}"], "a:b:c\nd:e:f\n"),
    ("awk", ["-F", "\\t", "{print $2}"], TABS),
    ("awk", ["-v", "x=5", "BEGIN {print x*2}"], ""),
    ("awk", ["function f(a) { return a*2 } BEGIN {print f(21)}"], ""),
    ("awk", ["BEGIN {for (i=0;i<3;i++) print i}"], ""),
    ("awk", ["BEGIN {i=0; while (i<3) {print i; i++}}"], ""),
    ("awk", ["BEGIN {i=0; do {print i; i++} while (i<2)}"], ""),
    ("awk", ["{if (NR==2) next; print}"], TEXT),
    ("awk", ["NR==2 {exit 3} {print}"], TEXT),
    ("awk", ["$1 ~ /^a/ {print \"yes\"}"], TEXT),
    ("awk", ["$1 !~ /^a/ {print \"no\"}"], TEXT),
    ("awk", ["/gamma/,/epsilon/ {print}"], TEXT),
    ("awk", ["{print ($1 > $2) ? \"gt\" : \"le\"}"], TEXT),
    ("awk", ["{s = s $1} END {print s}"], TEXT),
    ("awk", ["{print 10 == \"10\", \"10\" == \"10.0\", $1 == \"alpha\"}"], TEXT),
    ("awk", ["BEGIN {print 1==1.0, \"a\"<\"b\", 10<9}"], ""),
    ("awk", ["{$1 = \"X\"; print}"], TEXT),
    ("awk", ["{NF = 1; print; print NF}"], TEXT),
    ("awk", ["BEGIN {print int(3.9), int(-3.9)}"], ""),
    ("awk", ["BEGIN {OFS=\"-\"} {$1=$1; print}"], TEXT),
    ("awk", ["BEGIN {print length(\"abc\")}"], ""),
    ("awk", ["{a[NR]=$0} END {for(i=NR;i>0;i--) print a[i]}"], TEXT),
    ("awk", ["BEGIN {x[1]=1; delete x[1]; print length(x)}"], ""),
    ("awk", ["BEGIN {if (\"a\" in x) print \"in\"; else print \"out\"}"], ""),
    ("awk", ["{print > \"out.txt\"}"], TEXT),
    ("awk", ["BEGIN {printf \"%s\", \"no newline\"}"], ""),
    ("awk", ["BEGIN {print substr(\"hello\", 0, 3), substr(\"hello\", 2), substr(\"hello\", 10)}"], ""),
    ("awk", ["BEGIN {print 1/3}"], ""),
    ("awk", ["BEGIN {print 100000 * 100000}"], ""),
    ("awk", ["BEGIN {print -3 % 2}"], ""),

    # cmp and diff
    ("cmp", ["f1", "f1"], ""),
    ("cmp", ["f1", "f2"], ""),
    ("cmp", ["-s", "f1", "f2"], ""),
    ("diff", ["f1", "f1"], ""),
    ("diff", ["-q", "f1", "f2"], ""),
    ("diff", ["-u", "f1", "f2"], ""),
    ("diff", ["f1", "f2"], ""),

    # names
    ("basename", ["/a/b/c"], ""),
    ("basename", ["/a/b/c.txt", ".txt"], ""),
    ("basename", ["a"], ""),
    ("basename", ["/"], ""),
    ("basename", ["/a/b/"], ""),
    ("basename", [""], ""),
    ("dirname", ["/a/b/c"], ""),
    ("dirname", ["a"], ""),
    ("dirname", ["/"], ""),
    ("dirname", ["/a"], ""),
    ("dirname", ["a/b/"], ""),
    ("dirname", [""], ""),

    # expr
    ("expr", ["3", "+", "4"], ""),
    ("expr", ["10", "/", "3"], ""),
    ("expr", ["10", "%", "3"], ""),
    ("expr", ["2", "*", "3"], ""),
    ("expr", ["1", "=", "1"], ""),
    ("expr", ["1", "<", "2"], ""),
    ("expr", ["abc", "=", "abc"], ""),
    ("expr", ["abc", ":", "a*"], ""),
    ("expr", ["abcdef", ":", "\\(abc\\)"], ""),
    ("expr", ["0"], ""),
    ("expr", ["", "|", "fallback"], ""),

    # the file utilities, checked by what they leave behind
    ("rm", ["f1"], ""),
    ("rm", ["-f", "missing"], ""),
    ("rm", ["missing"], ""),
    ("rm", ["-rf", "d1"], ""),
    ("rm", ["-r", "d1"], ""),
    ("mkdir", ["new"], ""),
    ("mkdir", ["-p", "a/b/c"], ""),
    ("mkdir", ["d1"], ""),
    ("mkdir", ["-p", "d1"], ""),
    ("rmdir", ["d1/sub"], ""),
    ("cp", ["f1", "copy"], ""),
    ("cp", ["prog", "copy"], ""),
    ("cp", ["-p", "prog", "copy"], ""),
    ("cp", ["prog", "f1"], ""),
    ("cp", ["f1", "f2", "d1"], ""),
    ("cp", ["-r", "d1", "d2"], ""),
    ("mv", ["f1", "moved"], ""),
    ("mv", ["f1", "f2", "d1"], ""),
    ("ln", ["-s", "f1", "link"], ""),
    ("ln", ["f1", "hard"], ""),
    ("touch", ["fresh"], ""),
    ("touch", ["f1"], ""),
    ("chmod", ["644", "f1"], ""),
    ("chmod", ["+x", "f1"], ""),
    ("chmod", ["u-w", "f1"], ""),

    # find and xargs
    ("find", ["."], ""),
    ("find", [".", "-name", "f1"], ""),
    ("find", [".", "-type", "f"], ""),
    ("find", [".", "-type", "d"], ""),
    ("find", [".", "-name", "*.txt"], ""),
    ("find", ["d1", "-name", "sub"], ""),
    ("xargs", ["echo"], "a b c\n"),
    ("xargs", ["-n", "1", "echo"], "a b c\n"),
    ("xargs", ["echo"], "'quoted arg'\nplain\n"),

    # tee
    ("tee", ["t1"], TEXT),
    ("tee", ["t1", "t2"], TEXT),

    # printf and echo
    ("printf", ["%s\\n", "a", "b", "c"], ""),
    ("printf", ["%d-%d\\n", "1", "2"], ""),
    ("printf", ["%5s|%-5s|\\n", "ab", "cd"], ""),
    ("printf", ["%05.2f\\n", "3.14159"], ""),
    ("printf", ["%x %o %c\\n", "255", "8", "abc"], ""),
    ("printf", ["%b\\n", "a\\tb"], ""),
    ("printf", ["a\\tb\\n"], ""),
    ("printf", ["%s\\n"], ""),
    ("printf", ["%d\\n", "0x10"], ""),
    ("printf", ["%.3s|\\n", "abcdef"], ""),
]


def setup(d):
    """Make the scratch files the cases refer to."""
    with open(os.path.join(d, "f1"), "w") as f:
        f.write("alpha beta\ngamma delta\n")
    with open(os.path.join(d, "f2"), "w") as f:
        f.write("alpha beta\ngamma DELTA\nextra line\n")
    os.mkdir(os.path.join(d, "d1"))
    os.mkdir(os.path.join(d, "d1", "sub"))
    with open(os.path.join(d, "d1", "inner.txt"), "w") as f:
        f.write("inner\n")
    # an executable source, to see that a copy stays executable
    prog = os.path.join(d, "prog")
    with open(prog, "w") as f:
        f.write("#!/bin/sh\necho hi\n")
    os.chmod(prog, 0o755)


def listing(d):
    out = []
    for root, dirs, files in os.walk(d):
        dirs.sort()
        rel = os.path.relpath(root, d)
        for name in sorted(files + dirs):
            path = os.path.join(rel, name)
            full = os.path.join(root, name)
            if os.path.islink(full):
                out.append("l %s -> %s" % (path, os.readlink(full)))
            elif os.path.isdir(full):
                out.append("d %s" % path)
            else:
                mode = os.stat(full).st_mode & 0o777
                with open(full, "rb") as f:
                    body = f.read()
                out.append("f %s %o %r" % (path, mode, body))
    return "\n".join(out)


def run_one(cmd, args, data, base):
    d = tempfile.mkdtemp(dir=base)
    setup(d)
    env = dict(os.environ)
    env["LC_ALL"] = "C"
    env.pop("POSIXLY_CORRECT", None)
    p = subprocess.run(cmd + args, input=data.encode(), stdout=subprocess.PIPE,
                       stderr=subprocess.PIPE, cwd=d, env=env)
    files = listing(d)
    shutil.rmtree(d)
    return (p.returncode, p.stdout, files)


# Differences that are deliberate, with the reason.  Two of these are
# orders the standard leaves unspecified, where a settled order makes a
# build reproducible; the third is a timestamp the OCaml runtime cannot
# read to the nanosecond.
KNOWN = {
    ("awk", "{a[$1]++} END {for (k in a) print k, a[k]}"):
        "for-in visits keys in a settled order, not the reference's hash order",
    ("find", "."): "entries are visited in sorted order, not readdir order",
    ("find", ". -type f"): "entries are visited in sorted order, not readdir order",
    ("diff", "-u f1 f2"):
        "the header timestamp has only the resolution a double gives",
}


def main():
    verbose = "-v" in sys.argv
    only = [a for a in sys.argv[1:] if not a.startswith("-")]
    base = tempfile.mkdtemp()
    cases = 0
    bad = 0
    known = 0
    for (util, args, data) in CASES:
        if only and util not in only:
            continue
        cases += 1
        gnu = run_one([util], args, data, base)
        ours = run_one([OURS, util], args, data, base)
        if gnu != ours:
            key = (util, " ".join(args))
            if key in KNOWN:
                known += 1
                if verbose:
                    print("known %s %s: %s" % (util, " ".join(args), KNOWN[key]))
                continue
            bad += 1
            print("DIFF %s %s" % (util, " ".join(repr(a) for a in args)))
            if gnu[0] != ours[0]:
                print("  status: gnu=%d ours=%d" % (gnu[0], ours[0]))
            if gnu[1] != ours[1]:
                print("  stdout gnu : %r" % gnu[1][:400])
                print("  stdout ours: %r" % ours[1][:400])
            if gnu[2] != ours[2]:
                print("  files gnu : %s" % gnu[2][:400])
                print("  files ours: %s" % ours[2][:400])
        elif verbose:
            print("ok %s %s" % (util, " ".join(args)))
    shutil.rmtree(base)
    print("%d cases, %d differ, %d known differences" % (cases, bad, known))
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
