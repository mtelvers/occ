/* long double is IEEE binary128 here, with no hardware: the arithmetic
   is a call into the compiler's support library and a value travels in
   two integer registers */
#include <stdio.h>
int puts(const char *);
static void yes(const char *w, int ok) { puts(ok ? w : "FAILED"); }

static long double add(long double a, long double b) { return a + b; }
static long double mul(long double a, long double b) { return a * b; }
static long double divide(long double a, long double b) { return a / b; }
static int compare(long double a, long double b) { return (a < b) + 2 * (a == b) + 4 * (a > b); }
static long double widen(double v) { return (long double)v; }
static double narrow(long double v) { return (double)v; }
static long double from_long(long v) { return (long double)v; }
static long to_long(long double v) { return (long)v; }
static long double negate(long double v) { return -v; }

int main(void) {
  yes("add", add(1.5L, 2.25L) == 3.75L);
  yes("multiply", mul(1.5L, 4.0L) == 6.0L);
  yes("divide", divide(10.0L, 4.0L) == 2.5L);
  yes("compare less", compare(1.0L, 2.0L) == 1);
  yes("compare equal", compare(2.0L, 2.0L) == 2);
  yes("compare greater", compare(3.0L, 2.0L) == 4);
  yes("widen", widen(1.5) == 1.5L);
  yes("narrow", narrow(1.5L) == 1.5);
  yes("from long", from_long(-42) == -42.0L);
  yes("to long", to_long(1e10L) == 10000000000L);
  yes("negate", negate(2.5L) == -2.5L);
  /* The arithmetic has to happen at run time to test this machine:
     occ folds a constant long double expression at double precision,
     which loses a bit this format keeps.  That is a limitation of the
     constant folder rather than of the code generated, and it is the
     same on both machines -- see doc/riscv.md. */
  volatile long double one = 1.0L, tiny = 1e-20L;
  yes("more precision than a double", one + tiny != one);
  printf("printed %.5Lf %.20Lg\n", 3.5L, 1.0L / 3.0L);
  return 0;
}
