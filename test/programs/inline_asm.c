// expect: 0
// GNU inline assembly with constraints, as musl writes it: fixed registers,
// register variables, tied operands, memory operands, the FPU stack.
static long sys3(long n, long a1, long a2, long a3) {
  unsigned long ret;
  __asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a1), "S"(a2), "d"(a3) : "rcx", "r11", "memory");
  return ret;
}
static long sys6(long n, long a1, long a2, long a3, long a4, long a5, long a6) {
  unsigned long ret;
  register long r10 __asm__("r10") = a4;
  register long r8 __asm__("r8") = a5;
  register long r9 __asm__("r9") = a6;
  __asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a1), "S"(a2), "d"(a3), "r"(r10), "r"(r8), "r"(r9) : "rcx", "r11", "memory");
  return ret;
}
static int cas(volatile int *p, int t, int s) {
  __asm__ __volatile__ ("lock ; cmpxchg %3, %1" : "=a"(t), "=m"(*p) : "a"(t), "r"(s) : "memory");
  return t;
}
static int swap(volatile int *p, int v) {
  __asm__ __volatile__ ("xchg %0, %1" : "=r"(v), "=m"(*p) : "0"(v) : "memory");
  return v;
}
static void inc(volatile int *p) { __asm__ __volatile__ ("lock ; incl %0" : "=m"(*p) : "m"(*p) : "memory"); }
static long double my_fabsl(long double x) { __asm__ ("fabs" : "+t"(x)); return x; }
static long my_lrintl(long double x) { long r; __asm__ ("fistpll %0" : "=m"(r) : "t"(x) : "st"); return r; }
int main(void) {
  char msg[] = "x";
  long w = sys3(1, 1, (long)msg, 0);           /* write of 0 bytes */
  long w6 = sys6(1, 1, (long)msg, 0, 0, 0, 0);
  volatile int x = 5;
  int old = cas(&x, 5, 9), sw = swap(&x, 11);
  inc(&x);
  return (w == 0 && w6 == 0 && old == 5 && sw == 9 && x == 12 && my_fabsl(-2.5L) == 2.5L && my_lrintl(6.5L) == 6) ? 0 : 1;
}
