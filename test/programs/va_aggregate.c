// expect: 0
// va_arg of aggregates (ABI 3.5.7): from the register save area and the overflow area
#include <stdarg.h>
union semun { int val; void *buf; };
struct big { long a, b, c; };
struct two { double d; long n; };
static long f(int n, ...) {
  va_list ap; va_start(ap, n);
  long s = 0;
  for (int i = 0; i < n; i++) { union semun u = va_arg(ap, union semun); s += u.val; }
  struct big b = va_arg(ap, struct big);
  struct two t = va_arg(ap, struct two);
  va_end(ap);
  return s + b.a + b.b + b.c + (long)t.d + t.n;
}
int main(void) {
  union semun u1 = { .val = 1 }, u2 = { .val = 2 }, u3 = { .val = 3 }, u4 = { .val = 4 }, u5 = { .val = 5 }, u6 = { .val = 6 };
  struct big b = { 100, 200, 300 }; struct two t = { 2.5, 1000 };
  return f(6, u1, u2, u3, u4, u5, u6, b, t) == 1623 ? 0 : 1;
}
