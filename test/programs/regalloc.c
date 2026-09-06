// expect: 0
// stdout: 30 60 4 10
// Register allocation: more live values than the five callee-saved
// registers, parameters used late after many temporaries (which once evicted
// a parameter into a spill slot already in use), loop-carried variables,
// and a post-increment on a register variable.
#include <stdio.h>
typedef long (*fn)(long);
static long twice(long x) { return 2 * x; }
static long thrice(long x) { return 3 * x; }
static long many(long a, long b, long c, fn f, fn g, long h) {
  long s = 0;
  for (long i = 0; i < 5; i++) {           /* loop-carried: i, s */
    long t1 = a + i, t2 = b + i, t3 = c + i, t4 = h + i;
    long t5 = t1 * t2, t6 = t3 * t4, t7 = t5 - t6, t8 = t7 + a;
    s += t8 - t7 + t1 - t1;                 /* s += a */
  }
  return f(s) + g(h);                       /* f and g used after everything above */
}
int main(void) {
  long k = 3, sum = 0;
  int n = 0;
  while (n++ < 3) sum += k++;               /* 3 + 4 + 5 */
  printf("%ld %ld %d %ld\n", many(3, 0, 0, twice, thrice, 10) - 30, many(1, 2, 3, twice, twice, 2) + 46, n, sum - 2);
  return 0;
}
