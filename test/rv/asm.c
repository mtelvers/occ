/* Inline assembly (doc/extensions.md).  The templates are this
   machine's, so the test says nothing anywhere else; what it checks is
   the substitution of operands and the discipline around them. */
int puts(const char *);
static void yes(const char *w, int ok) { puts(ok ? w : "FAILED"); }

#if !defined(__riscv)
int main(void) { puts("a different machine"); return 0; }
#else
int main(void) {
  /* templates with no operands at all: a busy-wait hint, which is what
     the OCaml runtime's cpu_relax is here, and a compiler barrier */
  __asm__ volatile (".4byte 0x100000F");
  __asm__ volatile ("" ::: "memory");
  yes("a template with no operands", 1);

  long x = 40, y = 2, z = 0;
  __asm__ ("add %0, %1, %2" : "=r" (z) : "r" (x), "r" (y));
  yes("three registers", z == 42);

  __asm__ ("addi %0, %1, %2" : "=r" (z) : "r" (x), "i" (7));
  yes("an immediate", z == 47);

  long w = 41;
  __asm__ ("addi %0, %0, 1" : "+r" (w));
  yes("one register in and out", w == 42);

  z = 0;
  __asm__ ("add %0, %0, %2" : "=r" (z) : "0" (x), "r" (y));
  yes("an input tied to an output", z == 42);

  long arr[2] = { 7, 99 };
  __asm__ ("ld %0, %1" : "=r" (z) : "m" (arr[1]));
  yes("a memory operand", z == 99);

  double a = 1.5, b = 2.25, d = 0;
  __asm__ ("fadd.d %0, %1, %2" : "=f" (d) : "f" (a), "f" (b));
  yes("floating-point registers", d == 3.75);

  /* s2 is callee-saved, so the template borrowing it must not be
     noticed by anything outside */
  long keep = 1234;
  __asm__ volatile ("li s2, 0" ::: "s2");
  yes("a callee-saved register restored", keep == 1234);

  register long bound __asm__ ("a3") = 5;
  z = 0;
  __asm__ ("mv %0, %1" : "=r" (z) : "r" (bound));
  yes("a variable bound to a register", z == 5);
  return 0;
}
#endif
