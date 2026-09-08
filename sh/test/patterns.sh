# XCU 2.13, 2.6.6: patterns and pathname expansion
mkdir -p pd/sub
: > pd/a.c
: > pd/b.c
: > pd/c.h
: > pd/.hidden
echo pd/*.c
echo pd/*
echo pd/?.c
echo pd/[ab].c
echo pd/[!a].c
echo pd/nomatch*
echo "pd/*.c"
case a.c in *.c) echo m1;; esac
case a.c in "*.c") echo no;; *) echo m2;; esac
v='*'
case '*' in $v) echo m3;; esac
case 'x' in $v) echo m4;; esac
case 'x' in "$v") echo no;; *) echo m5;; esac
p="a.c"
echo "${p%.c}.o"
set -f
echo pd/*.c
set +f
echo pd/*.c
rm -rf pd
