// expect: 0
// long double is the 80-bit x87 format: arithmetic, comparison, conversion,
// argument passing in memory, return in st(0), and the bit layout musl relies on
typedef unsigned long long u64;
union ldshape { long double f; struct { u64 m; unsigned short se; } i; };
static long double mac(long double a, long double b, long double c) { return a + b * c; }
int main(void) {
  long double a = 1.5L, b = 2.25L;
  union ldshape one = { .f = 1.0L };
  u64 big = 12345678901234567890ULL;
  long double fb = big;
  long double eps = 1.0L;
  while (1.0L + eps / 2 != 1.0L) eps /= 2;          /* LDBL_EPSILON = 2^-63 */
  return (a + b == 3.75L && a - b == -0.75L && a * b == 3.375L && (a / b) * b == a
          && a < b && !(a > b) && mac(1.0L, 2.0L, 3.5L) == 8.0L
          && one.i.m == 0x8000000000000000ULL && one.i.se == 0x3fff && sizeof(long double) == 16
          && (u64)fb == big && (long long)(fb / 4) == (long long)(big / 4)
          && eps == 1.0L / 9223372036854775808.0L) ? 0 : 1;
}
