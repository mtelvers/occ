#!/bin/sh
# Preprocess the OCaml runtime's C into corpus/ so the compiler proper can
# be developed against real input before the preprocessor exists.
#
# The flags reproduce what the OCaml Makefile passes (Makefile.build_config:
# OC_BYTECODE_CPPFLAGS and OC_NATIVE_CPPFLAGS) with two changes:
#   -U__GNUC__ -U__SIZEOF_INT128__   glibc and the runtime take their portable, C11 paths;
#   -nostdinc    gcc's private headers are replaced by ours in include/.
# The result is the dialect occ itself will see once its preprocessor is
# native, and it depends on the installed glibc, so keep corpus/ out of
# version control and regenerate it rather than editing it.
#
# usage: tools/mkcorpus.sh [path-to-configured-ocaml-tree]   (default ~/ocaml)
set -eu
OCAML=${1:-"$HOME/ocaml"}
HERE=$(cd "$(dirname "$0")/.." && pwd)
OUT="$HERE/corpus"
CC=${CC:-gcc}

[ -f "$OCAML/runtime/caml/m.h" ] || { echo "$OCAML is not a configured OCaml tree (no runtime/caml/m.h)"; exit 1; }

mkdir -p "$OUT/shim"
cp -rs "$OCAML/runtime" "$OUT/shim/runtime"
rm "$OUT/shim/runtime/caml/s.h"
sed '/HAVE_LABELS_AS_VALUES/d' "$OCAML/runtime/caml/s.h" > "$OUT/shim/runtime/caml/s.h"
# A tree configured before configure.ac learned STRTOD_L (2026) but whose
# floats.c already uses it would not build with gcc either; supply it.
if grep -q 'define HAS_STRTOD_L' "$OUT/shim/runtime/caml/s.h" && ! grep -q 'define STRTOD_L' "$OUT/shim/runtime/caml/s.h"; then
  echo '#define STRTOD_L strtod_l' >> "$OUT/shim/runtime/caml/s.h"
  echo "note: s.h in $OCAML predates its configure.ac (no STRTOD_L); added to the shim"
fi
RUNTIME="$OUT/shim/runtime"

COMMON="-E -U__GNUC__ -U__SIZEOF_INT128__ -nostdinc -I $HERE/include \
  -isystem /usr/include/x86_64-linux-gnu -isystem /usr/include \
  -I $RUNTIME -D_FILE_OFFSET_BITS=64 -DCAMLDLLIMPORT= -DIN_CAML_RUNTIME"
NATIVE="-DNATIVE_CODE -DTARGET_amd64 -DMODEL_default -DSYS_linux"

n=0; failed=0
pp() { # pp <outdir> <flags> <file>...
  dir=$1; flags=$2; shift 2
  mkdir -p "$dir"
  for f in "$@"; do
    b=$(basename "$f" .c)
    if $CC $COMMON "-D__REDIRECT(name,proto,alias)=name proto __asm__(#alias)" \
          "-D__REDIRECT_NTH(name,proto,alias)=name proto __asm__(#alias)" \
          "-D__REDIRECT_NTHNL(name,proto,alias)=name proto __asm__(#alias)" $flags "$f" -o "$dir/$b.i" 2>"$dir/$b.err"; then rm -f "$dir/$b.err"; n=$((n+1))
    else failed=$((failed+1)); echo "FAILED: $f (see $dir/$b.err)"; fi
  done
}

# File lists follow the OCaml Makefile (runtime_*_C_SOURCES) and
# otherlibs/unix/Makefile (OS_C_SOURCES, UNIX_OR_WIN32), so the corpus is
# exactly what a Linux build compiles: no Windows units, no tsan.
byte_only="backtrace_byt fail_byt fix_code interp startup_byt zstd"
native_only="backtrace_nat clambda_checks dynlink_nat fail_nat frame_descriptors startup_nat signals_nat"
common=$(cd "$OCAML/runtime" && ls *.c | sed 's/\.c$//' | grep -v -x -E "$(echo $byte_only $native_only tsan win32 | tr ' ' '|')")
files() { for b in $1; do echo "$OCAML/$2/$b.c"; done; }

pp "$OUT/runtime/byte"   ""        $(for b in $common $byte_only; do echo "$RUNTIME/$b.c"; done)
pp "$OUT/runtime/native" "$NATIVE" $(for b in $common $native_only; do echo "$RUNTIME/$b.c"; done)
pp "$OUT/otherlibs/unix" "-I $OCAML/otherlibs/unix" \
   $(ls "$OCAML"/otherlibs/unix/*.c | grep -v -E '_win32\.c$|/(close_on|createprocess|nonblock|startup|system|windbug|windir|winlist|winwait|winworker)\.c$')
pp "$OUT/otherlibs/systhreads"     "-I $OCAML/otherlibs/systhreads"     "$OCAML"/otherlibs/systhreads/*.c
pp "$OUT/otherlibs/runtime_events" "-I $OCAML/otherlibs/runtime_events" "$OCAML"/otherlibs/runtime_events/*.c
pp "$OUT/otherlibs/str"            ""                                   "$OCAML"/otherlibs/str/*.c
pp "$OUT/yacc"                     "-I $OCAML/yacc"                     $(ls "$OCAML"/yacc/*.c | grep -v wstr)

echo "$n units preprocessed into $OUT, $failed failed"
