/* 7.17 on the A extension */
#include <stdatomic.h>
int puts(const char *);
static void yes(const char *w, int ok) { puts(ok ? w : "FAILED"); }

/* narrower than a word, and packed so that a neighbour would notice a
   mask that was wrong */
static _Atomic unsigned char bytes[4] = { 10, 20, 30, 40 };
static _Atomic short halves[2] = { 1000, 2000 };
static _Atomic signed char sbyte;

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

  /* one byte at a time, each at a different position in its word */
  yes("a byte loads and stores",
      (atomic_store(&bytes[0], 11), atomic_load(&bytes[0]) == 11));
  yes("a byte adds", atomic_fetch_add(&bytes[1], 5) == 20 && atomic_load(&bytes[1]) == 25);
  yes("its neighbours are untouched",
      atomic_load(&bytes[0]) == 11 && atomic_load(&bytes[2]) == 30 && atomic_load(&bytes[3]) == 40);
  yes("a byte subtracts", atomic_fetch_sub(&bytes[2], 40) == 30 && atomic_load(&bytes[2]) == 246);
  yes("a byte ands", (atomic_store(&bytes[3], 0xff),
                      atomic_fetch_and(&bytes[3], 0x0f) == 0xff && atomic_load(&bytes[3]) == 0x0f));
  yes("a byte ors", atomic_fetch_or(&bytes[3], 0xf0) == 0x0f && atomic_load(&bytes[3]) == 0xff);
  yes("a byte xors", atomic_fetch_xor(&bytes[3], 0xff) == 0xff && atomic_load(&bytes[3]) == 0);
  yes("a byte exchanges", atomic_exchange(&bytes[0], 7) == 11 && atomic_load(&bytes[0]) == 7);
  unsigned char bexp = 7;
  yes("a byte compares and exchanges",
      atomic_compare_exchange_strong(&bytes[0], &bexp, 99) && atomic_load(&bytes[0]) == 99);
  bexp = 7;
  yes("and reports what was there instead",
      !atomic_compare_exchange_strong(&bytes[0], &bexp, 0) && bexp == 99 && atomic_load(&bytes[0]) == 99);
  yes("still no neighbour disturbed", atomic_load(&bytes[1]) == 25 && atomic_load(&bytes[2]) == 246);

  /* a signed byte, which goes below zero */
  atomic_store(&sbyte, 1);
  yes("a signed byte below zero", atomic_fetch_sub(&sbyte, 3) == 1 && atomic_load(&sbyte) == -2);

  /* two bytes at a time, at both halves of a word */
  yes("a halfword adds", atomic_fetch_add(&halves[0], 24) == 1000 && atomic_load(&halves[0]) == 1024);
  yes("the other half is untouched", atomic_load(&halves[1]) == 2000);
  yes("a halfword exchanges", atomic_exchange(&halves[1], -1) == 2000 && atomic_load(&halves[1]) == -1);
  short hexp = -1;
  yes("a halfword compares and exchanges",
      atomic_compare_exchange_strong(&halves[1], &hexp, 300) && atomic_load(&halves[1]) == 300);
  yes("and the first half still stands", atomic_load(&halves[0]) == 1024);
  return 0;
}
