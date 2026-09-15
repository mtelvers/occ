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

/* A va_list handed to the C library, which is the case that matters most
   and the one a compiler can get wrong invisibly: vsnprintf was compiled
   by the system from the ABI's declaration of va_list, so ours has to be
   the same type and not merely work among our own functions. */
int vsnprintf(char *, unsigned long, const char *, va_list);
unsigned long strlen(const char *);
int strcmp(const char *, const char *);

static int say(char *out, unsigned long n, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int k = vsnprintf(out, n, fmt, ap);
  va_end(ap);
  return k;
}

/* one of ours taking a va_list, and a copy walked a second time */
static long twice(int n, va_list ap) {
  va_list copy;
  va_copy(copy, ap);
  long s = 0;
  for (int i = 0; i < n; i++) s += va_arg(ap, int);
  for (int i = 0; i < n; i++) s += va_arg(copy, int);
  va_end(copy);
  return s;
}
static long handed_on(int n, ...) {
  va_list ap;
  va_start(ap, n);
  long s = twice(n, ap);
  va_end(ap);
  return s;
}

int main(void) {
  yes("three ints", sum_ints(3, 1, 2, 3) == 6);
  yes("eight ints", sum_ints(8, 1, 2, 3, 4, 5, 6, 7, 8) == 36);
  yes("doubles", sum_doubles(3, 1.5, 2.25, 3.25) == 7.0);
  yes("mixed", mixed(4, 10L, 2.5, 5, "x") == 1017);
  yes("onto the stack", many(12, 1L,2L,3L,4L,5L,6L,7L,8L,9L,10L,11L,12L) == 78);
  char buf[64];
  const char *want = "42 text 1.50 z abcd";
  int k = say(buf, sizeof buf, "%d %s %.2f %c %lx", 42, "text", 1.5, 'z', 0xabcdUL);
  yes("a va_list handed to the C library", k == (int)strlen(want) && strcmp(buf, want) == 0);
  yes("one handed to another function, and copied", handed_on(4, 1, 2, 3, 4) == 20);
  return 0;
}
