/* aggregates by value: the rows measured from the machine's own gcc */
int puts(const char *);
static void yes(const char *w, int ok) { puts(ok ? w : "FAILED"); }

struct i8  { long a; };
struct i16 { long a, b; };
struct i24 { long a, b, c; };
struct f8  { double a; };
struct ff  { float a, b; };
struct f16 { double a, b; };
struct mix { double a; long b; };
struct s12 { int a, b, c; };

static long t_i8(struct i8 v) { return v.a; }
static long t_i16(struct i16 v) { return v.a * 10 + v.b; }
static long t_i24(struct i24 v) { return v.a * 100 + v.b * 10 + v.c; }
static double t_f8(struct f8 v) { return v.a; }
static double t_ff(struct ff v) { return v.a * 10 + v.b; }
static double t_f16(struct f16 v) { return v.a * 10 + v.b; }
static double t_mix(struct mix v) { return v.a * 10 + (double)v.b; }
static long t_s12(struct s12 v) { return v.a * 100 + v.b * 10 + v.c; }

static struct i16 make_i16(long a, long b) { struct i16 v = { a, b }; return v; }
static struct f16 make_f16(double a, double b) { struct f16 v = { a, b }; return v; }
static struct i24 make_i24(long a, long b, long c) { struct i24 v = { a, b, c }; return v; }

int main(void) {
  struct i8 a = { 7 }; struct i16 b = { 1, 2 }; struct i24 c = { 1, 2, 3 };
  struct f8 d = { 1.5 }; struct ff e = { 1.5f, 2.5f }; struct f16 f = { 1.5, 2.5 };
  struct mix g = { 1.5, 2 }; struct s12 h = { 1, 2, 3 };
  yes("one integer", t_i8(a) == 7);
  yes("two integers", t_i16(b) == 12);
  yes("by reference", t_i24(c) == 123);
  yes("one double", t_f8(d) == 1.5);
  yes("two floats", t_ff(e) == 17.5);
  yes("two doubles", t_f16(f) == 17.5);
  yes("one of each", t_mix(g) == 17.0);
  yes("twelve bytes", t_s12(h) == 123);
  struct i16 r1 = make_i16(4, 5);
  yes("returned in two", r1.a == 4 && r1.b == 5);
  struct f16 r2 = make_f16(1.25, 2.5);
  yes("returned in floats", r2.a == 1.25 && r2.b == 2.5);
  struct i24 r3 = make_i24(9, 8, 7);
  yes("returned by reference", r3.a == 9 && r3.b == 8 && r3.c == 7);
  return 0;
}
