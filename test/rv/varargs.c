/* variable arguments, which here are one pointer walking up through the
   registers the callee saved and into what the caller pushed */
#include <stdarg.h>
int puts(const char *);
static void yes(const char *w, int ok) { puts(ok ? w : "FAILED"); }

static long sum_ints(int n, ...) {
  va_list ap; va_start(ap, n);
  long s = 0;
  for (int i = 0; i < n; i++) s += va_arg(ap, int);
  va_end(ap);
  return s;
}
static double sum_doubles(int n, ...) {
  va_list ap; va_start(ap, n);
  double s = 0;
  for (int i = 0; i < n; i++) s += va_arg(ap, double);
  va_end(ap);
  return s;
}
static long mixed(int n, ...) {
  va_list ap; va_start(ap, n);
  long a = va_arg(ap, long);
  double b = va_arg(ap, double);
  int c = va_arg(ap, int);
  const char *s = va_arg(ap, const char *);
  va_end(ap);
  return a + (long)b + c + (s[0] == 'x' ? 1000 : 0);
}
/* more than the eight argument registers, so some arrive on the stack */
static long many(int n, ...) {
  va_list ap; va_start(ap, n);
  long s = 0;
  for (int i = 0; i < n; i++) s += va_arg(ap, long);
  va_end(ap);
  return s;
}

int main(void) {
  yes("three ints", sum_ints(3, 1, 2, 3) == 6);
  yes("eight ints", sum_ints(8, 1, 2, 3, 4, 5, 6, 7, 8) == 36);
  yes("doubles", sum_doubles(3, 1.5, 2.25, 3.25) == 7.0);
  yes("mixed", mixed(4, 10L, 2.5, 5, "x") == 1017);
  yes("onto the stack", many(12, 1L,2L,3L,4L,5L,6L,7L,8L,9L,10L,11L,12L) == 78);
  return 0;
}
