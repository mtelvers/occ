#!/bin/sh
# Compare occ's dynamic linking with gcc's, and then run both.
#
# A shared object is the one kind of output whose correctness a
# comparison cannot settle: what matters is what the system's loader
# makes of it.  So each case here is built twice, once with gcc and GNU
# ld and once with occ and occld, and then *run*, and the outputs
# compared.  What is compared besides that is what a loader reads: the
# objects to load, the names offered and wanted, and the relocations by
# type and symbol.
#
# Not compared, deliberately, because occld does not emit them and a
# loader does not need them: .gnu.hash (DT_HASH serves), the version
# tables on definitions (occld versions only its references, which is
# what keeps glibc from binding to a compatibility symbol), and the
# order and addresses of everything, which no two linkers agree on.
#
# usage: tools/dyncheck.sh
set -u
HERE=$(cd "$(dirname "$0")/.." && pwd)
OCC="$HERE/_build/default/bin/main.exe"
SRC="$HERE/test/dyn"
work=${TMPDIR:-/tmp}/dyncheck.$$
mkdir -p "$work"
n=0; fail=0

note() { n=$((n+1)); }
bad() { fail=$((fail+1)); echo "DIFF $1"; }

# the part of a dynamic table a loader acts on, in a settled order
tables() {
  {
    readelf -dW "$1" | sed -n 's/.*(\(NEEDED\|SONAME\|RUNPATH\)).*\[\(.*\)\]/\1 \2/p' | sort
    echo "--- symbols offered"
    readelf --dyn-syms -W "$1" | awk '$7 != "UND" && $8 != "" {print $8}' | sed 's/@.*//' | sort -u
    echo "--- symbols wanted"
    readelf --dyn-syms -W "$1" | awk '$7 == "UND" && $8 != "" {print $8}' | sed 's/@.*//' | sort -u
    echo "--- relocations"
    readelf -rW "$1" | awk '/^[0-9a-f]/ {name = $5; sub(/@.*/, "", name); print $3, name}' | sort | uniq -c | sort -k2
  } 2>/dev/null
}

compare_tables() {   # compare_tables name gnu occ
  tables "$2" > "$work/$1.gnu"
  tables "$3" > "$work/$1.occ"
  cmp -s "$work/$1.gnu" "$work/$1.occ" || {
    bad "$1: what a loader reads"
    [ -n "${VERBOSE:-}" ] && diff "$work/$1.gnu" "$work/$1.occ"
  }
}

compare_run() {      # compare_run name command...
  shift_name=$1; shift
  "$@" > "$work/run.out" 2>&1; echo "status $?" >> "$work/run.out"
  if [ -f "$work/$shift_name.expected" ]; then
    cmp -s "$work/$shift_name.expected" "$work/run.out" || {
      bad "$shift_name: what it does"
      [ -n "${VERBOSE:-}" ] && diff "$work/$shift_name.expected" "$work/run.out"
    }
  else
    cp "$work/run.out" "$work/$shift_name.expected"
  fi
}

# The structural comparison gives both linkers the *same* objects, so
# that what differs is the linking and not the compiling: occ and gcc
# make different (equally valid) choices about which addressing to use,
# and comparing their objects would compare those instead.
gcclib=$(ls -d /usr/lib/gcc/x86_64-linux-gnu/* 2>/dev/null | tail -1)
syslib=/usr/lib/x86_64-linux-gnu
# --as-needed, because that is what gcc's driver passes on this
# platform and so what the output of `occ -shared' is compared against:
# a library named and not used is not one the object needs.  Bare ld
# without it records every library it was given.
link_gnu_so() {   # link_gnu_so out object...
  out=$1; shift
  ld -shared --hash-style=sysv --as-needed -o "$out" "$syslib/crti.o" "$gcclib/crtbeginS.o" "$@" \
    -L"$gcclib" -L"$syslib" -lgcc -lgcc_s -lc -lgcc -lgcc_s "$gcclib/crtendS.o" "$syslib/crtn.o"
}
link_occ_so() {   # link_occ_so out object...
  out=$1; shift
  "$HERE/_build/default/bin/occld.exe" -shared -o "$out" "$syslib/crti.o" "$gcclib/crtbeginS.o" "$@" \
    -L"$gcclib" -L"$syslib" -lgcc -lgcc_s -lc -lgcc -lgcc_s "$gcclib/crtendS.o" "$syslib/crtn.o"
}

# ---- a shared object that needs nothing but itself, and one that does ----
for case in plain libcalls tls; do
  gcc -fPIC -ftls-model=initial-exec -c -o "$work/$case.o" "$SRC/$case.c" 2>/dev/null
  link_gnu_so "$work/$case.gnu.so" "$work/$case.o" 2>/dev/null \
    || bad "$case: GNU ld could not link it"
  link_occ_so "$work/$case.occ.so" "$work/$case.o" || bad "$case: occld could not link it"
  note
  compare_tables "$case" "$work/$case.gnu.so" "$work/$case.occ.so"
done

# ---- and what they do when loaded ----
cat > "$work/load.c" <<'EOF'
#include <dlfcn.h>
#include <stdio.h>
int main(int argc, char **argv) {
  void *h = dlopen(argv[1], RTLD_NOW);
  if (!h) { printf("dlopen: %s\n", dlerror()); return 1; }
  for (int i = 2; i < argc; i++) {
    long (*f)(void) = dlsym(h, argv[i]);
    if (!f) { printf("dlsym %s: %s\n", argv[i], dlerror()); return 1; }
    printf("%s -> %ld, again %ld\n", argv[i], f(), f());
  }
  return 0;
}
EOF
gcc "$work/load.c" -o "$work/load" -ldl 2>/dev/null
for pair in "plain answer deref" "tls bump"; do
  set -- $pair
  case=$1; shift
  note
  rm -f "$work/$case.run.expected"
  compare_run "$case.run" "$work/load" "$work/$case.gnu.so" "$@"
  compare_run "$case.run" "$work/load" "$work/$case.occ.so" "$@"
done

# libcalls takes an argument, so it gets its own loader
cat > "$work/greet.c" <<'EOF'
#include <dlfcn.h>
#include <stdio.h>
int main(int argc, char **argv) {
  void *h = dlopen(argv[1], RTLD_NOW);
  if (!h) { printf("dlopen: %s\n", dlerror()); return 1; }
  int (*greet)(const char *) = dlsym(h, "greet");
  printf("returned %d and %d\n", greet("world"), greet("again"));
  return 0;
}
EOF
gcc "$work/greet.c" -o "$work/greet" -ldl 2>/dev/null
note
rm -f "$work/libcalls.run.expected"
compare_run "libcalls.run" "$work/greet" "$work/libcalls.gnu.so"
compare_run "libcalls.run" "$work/greet" "$work/libcalls.occ.so"

# ---- an executable that a loaded object binds back into ----
gcc -Wl,-E -no-pie -o "$work/host.gnu" "$SRC/host.c" -ldl 2>/dev/null
"$OCC" -Wl,-E -o "$work/host.occ" "$SRC/host.c" -ldl || bad "host: occ could not link it"
gcc -fPIC -c -o "$work/plugin.o" "$SRC/plugin.c" 2>/dev/null
link_gnu_so "$work/plugin.gnu.so" "$work/plugin.o" 2>/dev/null || bad "plugin: GNU ld could not link it"
link_occ_so "$work/plugin.occ.so" "$work/plugin.o" || bad "plugin: occld could not link it"
note
compare_tables "plugin" "$work/plugin.gnu.so" "$work/plugin.occ.so"
# every combination: each host must be able to load either plugin
for h in gnu occ; do
  for p in gnu occ; do
    note
    rm -f "$work/host.run.expected"
    compare_run "host.run" "$work/host.gnu" "$work/plugin.$p.so"
    compare_run "host.run" "$work/host.$h" "$work/plugin.$p.so"
  done
done

# ---- a variable of the C library, referred to by address ----
gcc -no-pie -o "$work/copies.gnu" "$SRC/copies.c" 2>/dev/null
"$OCC" -Wl,-E -o "$work/copies.occ" "$SRC/copies.c" || bad "copies: occ could not link it"
note
rm -f "$work/copies.run.expected"
compare_run "copies.run" "$work/copies.gnu"
compare_run "copies.run" "$work/copies.occ"

# ---- a name the C library offers under two versions ----
gcc -no-pie -o "$work/versioned.gnu" "$SRC/versioned.c" 2>/dev/null
"$OCC" -Wl,-E -o "$work/versioned.occ" "$SRC/versioned.c" || bad "versioned: occ could not link it"
note
rm -f "$work/versioned.run.expected"
compare_run "versioned.run" "$work/versioned.gnu"
compare_run "versioned.run" "$work/versioned.occ"

rm -rf "$work"
echo "$n cases, $fail differ"
[ "$fail" = 0 ]
