/* floating point: the arithmetic, the comparisons and the conversions */
int puts(const char *);
static void yes(const char *w, int ok) { puts(ok ? w : "FAILED"); }
static int close(double a, double b) { double d = a - b; return d < 1e-9 && d > -1e-9; }

static double fadd(double a, double b) { return a + b; }
static float fsubf(float a, float b) { return a - b; }
static double fdiv(double a, double b) { return a / b; }
static int fcmp(double a, double b) { return (a < b) + 2 * (a == b) + 4 * (a > b); }
static double from_int(int v) { return (double)v; }
static double from_unsigned(unsigned v) { return (double)v; }
static int to_int(double v) { return (int)v; }
static long to_long(double v) { return (long)v; }
static double widen(float v) { return (double)v; }
static float narrow(double v) { return (float)v; }

int main(void) {
  yes("add", close(fadd(1.5, 2.25), 3.75));
  yes("subtract float", fsubf(2.5f, 1.25f) == 1.25f);
  yes("divide", close(fdiv(10.0, 4.0), 2.5));
  yes("multiply", close(1.5 * 4.0, 6.0));
  yes("compare less", fcmp(1.0, 2.0) == 1);
  yes("compare equal", fcmp(2.0, 2.0) == 2);
  yes("compare greater", fcmp(3.0, 2.0) == 4);
  yes("from int", close(from_int(-42), -42.0));
  yes("from unsigned", close(from_unsigned(4000000000u), 4000000000.0));
  yes("to int", to_int(-3.75) == -3);
  yes("to long", to_long(1e10) == 10000000000L);
  yes("widen", close(widen(1.5f), 1.5));
  yes("narrow", narrow(1.5) == 1.5f);
  yes("negate", close(-fadd(1.0, 2.0), -3.0));
  return 0;
}
