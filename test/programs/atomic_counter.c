// expect: 3
// requires: cc
// Exercises our <stdatomic.h>: the generic macros must expand to the
// __occ_atomic_* builtins and type-check from the pointer argument.
#include <stdatomic.h>

static _Atomic int counter;

int main(void) {
  atomic_store_explicit(&counter, 1, memory_order_relaxed);
  atomic_fetch_add(&counter, 2);
  return atomic_load_explicit(&counter, memory_order_acquire);
}
