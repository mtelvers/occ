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

/* More floating-point arguments than there are floating-point registers.
   The psABI then puts them in the integer registers -- their bits, not
   their values -- and only when those are gone on the stack, so the
   ninth of sixteen doubles travels in a0.  A test in OCaml's own suite
   found this missing. */
static double sixteen(double a, double b, double c, double d,
                      double e, double f, double g, double h,
                      double i, double j, double k, double l,
                      double m, double n, double o, double p) {
  return a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p;
}

static double twenty(double a, double b, double c, double d,
                     double e, double f, double g, double h,
                     double i, double j, double k, double l,
                     double m, double n, double o, double p,
                     double q, double r, double s, double t) {
  return a + b + c + d + e + f + g + h + i + j + k + l
       + m + n + o + p + q + r + s + t;
}

static float sixteen_floats(float a, float b, float c, float d,
                            float e, float f, float g, float h,
                            float i, float j, float k, float l,
                            float m, float n, float o, float p) {
  return a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p;
}

static double mixed_up(long a, double b, long c, double d, long e, double f,
                       long g, double h, long i, double j, long k, double l,
                       long m, double n, long o, double p, long q, double r) {
  return (double)(a + c + e + g + i + k + m + o + q) + b + d + f + h + j + l + n + p + r;
}

int main(void) {
  yes("sixteen doubles, eight of them in integer registers",
      close(sixteen(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768),
            65535.0));
  yes("twenty, so four of them on the stack",
      close(twenty(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048,
                   4096, 8192, 16384, 32768, 65536, 131072, 262144, 524288),
            1048575.0));
  yes("sixteen floats", sixteen_floats(1, 2, 4, 8, 16, 32, 64, 128,
                                       256, 512, 1024, 2048, 4096, 8192, 16384, 32768) == 65535.0f);
  yes("integers and doubles together, both kinds running out",
      close(mixed_up(1, 2, 4, 8, 16, 32, 64, 128, 256, 512,
                     1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072),
            262143.0));
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
