/* __builtin_add_overflow and its relatives, which the OCaml runtime uses
   to check its own arithmetic (runtime/caml/misc.h).  The machine has no
   condition flags, so every answer here is computed in arithmetic; the
   operands are volatile so that neither compiler can fold the question
   away and answer it at compile time. */
int puts(const char *);
static void yes(const char *w, int ok) { puts(ok ? w : "FAILED"); }

int main(void) {
  volatile int a, b; int i;
  a = 2; b = 3;
  yes("int add", !__builtin_add_overflow(a, b, &i) && i == 5);
  a = 2147483647; b = 1;
  yes("int add overflows", __builtin_add_overflow(a, b, &i) && i == -2147483647 - 1);
  a = -2147483647 - 1; b = 1;
  yes("int sub overflows", __builtin_sub_overflow(a, b, &i) && i == 2147483647);
  a = 65536; b = 65536;
  yes("int mul overflows", __builtin_mul_overflow(a, b, &i) && i == 0);
  a = 46340; b = 46340;
  yes("int mul just fits", !__builtin_mul_overflow(a, b, &i) && i == 2147395600);

  volatile unsigned ua, ub; unsigned u;
  ua = 4294967295u; ub = 1;
  yes("unsigned add wraps", __builtin_add_overflow(ua, ub, &u) && u == 0);
  ua = 0; ub = 1;
  yes("unsigned sub wraps", __builtin_sub_overflow(ua, ub, &u) && u == 4294967295u);
  ua = 65536; ub = 65536;
  yes("unsigned mul wraps", __builtin_mul_overflow(ua, ub, &u) && u == 0);
  ua = 3; ub = 5;
  yes("unsigned mul", !__builtin_mul_overflow(ua, ub, &u) && u == 15);

  volatile signed char ca, cb; signed char c;
  ca = 127; cb = 1;
  yes("signed char add", __builtin_add_overflow(ca, cb, &c) && c == -128);
  ca = -128; cb = -1;
  yes("signed char mul", __builtin_mul_overflow(ca, cb, &c) && c == -128);
  volatile unsigned char da, db; unsigned char d;
  da = 255; db = 1;
  yes("unsigned char add", __builtin_add_overflow(da, db, &d) && d == 0);
  volatile short sa, sb; short sh;
  sa = 32767; sb = 1;
  yes("short add", __builtin_add_overflow(sa, sb, &sh) && sh == -32768);
  sa = 300; sb = 100;
  yes("short mul", !__builtin_mul_overflow(sa, sb, &sh) && sh == 30000);

  volatile long la, lb; long l;
  la = 9223372036854775807L; lb = 1;
  yes("long add overflows", __builtin_add_overflow(la, lb, &l) && l == -9223372036854775807L - 1);
  la = -9223372036854775807L - 1; lb = 1;
  yes("long sub overflows", __builtin_sub_overflow(la, lb, &l) && l == 9223372036854775807L);
  la = 4294967296L; lb = 4294967296L;
  yes("long mul overflows", __builtin_mul_overflow(la, lb, &l) && l == 0);
  la = 3037000499L; lb = 3037000499L;
  yes("long mul overflows just", __builtin_mul_overflow(la, lb, &l));
  la = -1; lb = 9223372036854775807L;
  yes("long mul by minus one", !__builtin_mul_overflow(la, lb, &l) && l == -9223372036854775807L);
  volatile unsigned long ka, kb; unsigned long k;
  ka = 18446744073709551615UL; kb = 1;
  yes("unsigned long add wraps", __builtin_add_overflow(ka, kb, &k) && k == 0);
  ka = 1; kb = 2;
  yes("unsigned long sub wraps", __builtin_sub_overflow(ka, kb, &k) && k == 18446744073709551615UL);
  ka = 4294967296UL; kb = 4294967296UL;
  yes("unsigned long mul wraps", __builtin_mul_overflow(ka, kb, &k) && k == 0);
  ka = 1000000007UL; kb = 1000000009UL;
  yes("unsigned long mul", !__builtin_mul_overflow(ka, kb, &k) && k == 1000000016000000063UL);
  return 0;
}
