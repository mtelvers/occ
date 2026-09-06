// expect: 66
#include <stdarg.h>
static int sum(int n, ...) {
  va_list ap;
  va_start(ap, n);
  int s = 0;
  for (int i = 0; i < n; i++) s += va_arg(ap, int);
  va_end(ap);
  return s;
}
static double dsum(int n, ...) {
  va_list ap; va_start(ap, n);
  double s = 0;
  for (int i = 0; i < n; i++) s += va_arg(ap, double);
  va_end(ap);
  return s;
}
int main(void) {
  /* 8 int args exceed the 6 registers; 9 doubles exceed the 8 */
  return sum(8, 1, 2, 3, 4, 5, 6, 7, 8) + (int)dsum(9, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0) - 15;
}
