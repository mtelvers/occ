/* integer arithmetic at every width, and the sign-extension rule RV64
   keeps: a 32-bit result must look the same to a comparison as it does
   to a store */
int puts(const char *);
void say(const char *s) { puts(s); }
static void yes(const char *what, int ok) { say(ok ? what : "FAILED"); }

int add(int a, int b) { return a + b; }
long mul(long a, long b) { return a * b; }
unsigned udiv(unsigned a, unsigned b) { return a / b; }
int srem(int a, int b) { return a % b; }
long shifts(long a, int n) { return (a << n) | (a >> n); }
int narrow(long v) { return (int)v; }
short shorten(int v) { return (short)v; }
unsigned char byte(int v) { return (unsigned char)v; }

int main(void) {
  yes("add", add(40, 2) == 42);
  yes("add wraps", add(2147483647, 1) == -2147483648);
  yes("mul", mul(6, 7) == 42);
  yes("mul wide", mul(4294967296L, 3) == 12884901888L);
  yes("udiv", udiv(100, 7) == 14);
  yes("srem", srem(-100, 7) == -2);
  yes("srem positive", srem(100, 7) == 2);
  yes("shifts", shifts(1024, 3) == (1024 << 3 | 1024 >> 3));
  yes("narrow", narrow(0x1234567890ABL) == (int)0x567890ABL);
  yes("shorten", shorten(0x12345) == 0x2345);
  yes("byte", byte(0x1234) == 0x34);
  yes("compare", (add(1, 1) < add(1, 2)) == 1);
  yes("unsigned compare", (udiv(10, 1) > udiv(5, 1)) == 1);
  /* A comparison against a constant with its top bit set.  The machine
     keeps a 32-bit value sign-extended in a 64-bit register, so the
     constant has to be made the same way or the two disagree. */
  volatile unsigned top = 4294967295u;
  yes("unsigned equal to a large constant", top == 4294967295u);
  yes("unsigned above one", top > 2147483648u);
  yes("unsigned below the largest", (top - 1) < 4294967295u);
  volatile unsigned short half = 65535;
  yes("unsigned short against a constant", half == 65535);
  volatile unsigned char one_byte = 255;
  yes("unsigned char against a constant", one_byte == 255);
  volatile unsigned long whole = 18446744073709551615UL;
  yes("unsigned long against a constant", whole == 18446744073709551615UL);
  yes("and above half of it", whole > 9223372036854775808UL);
  return 0;
}
