#!/bin/sh
# Use gcc as a reference front end for the corpus dialect.
#
# gcc accepts everything tools/mkcorpus.sh produces except glibc's
# non-GNU-mode typedefs of _Float32 and friends, which gcc's parser
# reserves as keywords; those four lines are dropped first.  The
# __occ_atomic_* builtins from our <stdatomic.h> are unknown to gcc, so
# unprototyped declarations are prepended: enough for a syntax check, not
# for type checking their results.
#
# usage: tools/refcheck.sh corpus/runtime/native/*.i
set -u
fail=0
decls=$(for b in load store exchange compare_exchange_strong compare_exchange_weak \
                 fetch_add fetch_sub fetch_or fetch_xor fetch_and thread_fence signal_fence; do
          printf 'long __occ_atomic_%s();\n' "$b"; done)
for f in "$@"; do
  if ! { echo "$decls"; sed '/^typedef .* _Float[0-9]*x\{0,1\};$/d' "$f"; } \
       | gcc -std=c11 -fsyntax-only -Wno-int-conversion -x cpp-output - 2>/dev/null
  then echo "REJECTED: $f"; fail=$((fail+1)); fi
done
echo "$# units, $fail rejected by gcc"
