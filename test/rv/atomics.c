/* 7.17 on the A extension */
#include <stdatomic.h>
int puts(const char *);
static void yes(const char *w, int ok) { puts(ok ? w : "FAILED"); }

static _Atomic long counter;
static _Atomic int narrow;
static _Atomic(void *) pointer;

int main(void) {
  atomic_store(&counter, 10);
  yes("load and store", atomic_load(&counter) == 10);
  yes("fetch add", atomic_fetch_add(&counter, 5) == 10 && atomic_load(&counter) == 15);
  yes("fetch sub", atomic_fetch_sub(&counter, 3) == 15 && atomic_load(&counter) == 12);
  yes("fetch and", (atomic_store(&counter, 0xff), atomic_fetch_and(&counter, 0x0f) == 0xff && atomic_load(&counter) == 0x0f));
  yes("fetch or", atomic_fetch_or(&counter, 0xf0) == 0x0f && atomic_load(&counter) == 0xff);
  yes("fetch xor", atomic_fetch_xor(&counter, 0xff) == 0xff && atomic_load(&counter) == 0);
  yes("exchange", (atomic_store(&counter, 7), atomic_exchange(&counter, 9) == 7 && atomic_load(&counter) == 9));
  long expected = 9;
  yes("compare exchange succeeds", atomic_compare_exchange_strong(&counter, &expected, 42) && atomic_load(&counter) == 42);
  expected = 1;
  yes("compare exchange fails", !atomic_compare_exchange_strong(&counter, &expected, 0) && expected == 42);
  atomic_store(&narrow, 100);
  yes("four bytes", atomic_fetch_add(&narrow, 1) == 100 && atomic_load(&narrow) == 101);
  atomic_store(&pointer, &counter);
  yes("a pointer", atomic_load(&pointer) == (void *)&counter);
  atomic_thread_fence(memory_order_seq_cst);
  yes("a fence", 1);
  yes("relaxed", (atomic_store_explicit(&counter, 3, memory_order_relaxed),
                  atomic_load_explicit(&counter, memory_order_relaxed) == 3));
  return 0;
}
