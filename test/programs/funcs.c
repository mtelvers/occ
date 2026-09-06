// expect: 21
static int fib(int n) { return n < 2 ? n : fib(n - 1) + fib(n - 2); }
int add8(int a, int b, int c, int d, int e, int f, int g, int h) { return a + b + c + d + e + f + g + h; }
int main(void) {
  int (*fp)(int) = fib;
  return fp(8) + add8(1, 1, 1, 1, 1, 1, 1, 1) - 8; /* 21 + 8 - 8 */
}
