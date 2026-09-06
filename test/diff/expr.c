/* Differential test: compile with gcc and with occ, run both, diff the
   output.  Every line prints one observable value; a difference points at a
   miscompiled construct.  Kept free of undefined behaviour so that gcc's
   answer is the standard's. */
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>
#include <math.h>

#define P(fmt, e) printf("%-40s " fmt "\n", #e, (e))

struct s { char c; short sh; int i; long l; double d; float f; unsigned char uc; };
struct bits { unsigned a : 3; unsigned b : 5; int c : 4; unsigned long d : 33; signed char e : 2; };
union u { long l; double d; unsigned char b[8]; };
struct small { int x; int y; };
struct pair_d { double a, b; };
struct mixed { int i; double d; };
struct big { long a[3]; };

static struct small mk_small(int x, int y) { struct small s = { x, y }; return s; }
static struct pair_d mk_pd(double a, double b) { struct pair_d p = { a, b }; return p; }
static struct mixed mk_mixed(int i, double d) { struct mixed m = { i, d }; return m; }
static struct big mk_big(long a) { struct big b = { { a, a + 1, a + 2 } }; return b; }
static long sum_big(struct big b) { return b.a[0] + b.a[1] + b.a[2]; }
static double sum_mixed(struct mixed m, struct pair_d p, struct small s) { return m.i + m.d + p.a + p.b + s.x + s.y; }
static int many(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j) { return a - b + c - d + e - f + g - h + i - j; }
static double manyd(double a, double b, double c, double d, double e, double f, double g, double h, double i, double j) { return a - b + c - d + e - f + g - h + i - j; }
static long va(int n, ...) {
  va_list ap; va_start(ap, n); long s = 0;
  for (int k = 0; k < n; k++) {
    switch (k % 4) {
      case 0: s += va_arg(ap, int); break;
      case 1: s += va_arg(ap, long); break;
      case 2: s += (long)va_arg(ap, double); break;
      default: s += *va_arg(ap, int *); break;
    }
  }
  va_end(ap); return s;
}
struct odd3 { char a, b, c; }; struct odd5 { int a; char b; }; struct odd12 { int a, b, c; }; struct odd13 { long a; int b; char c; };
static int odd_sum(struct odd3 a, struct odd5 b, struct odd12 c, struct odd13 d) { return a.a + a.b + a.c + b.a + b.b + c.a + c.b + c.c + (int)d.a + d.b + d.c; }
static struct odd3 mk_odd3(int k) { struct odd3 r = { k, k + 1, k + 2 }; return r; }
static struct odd12 mk_odd12(int k) { struct odd12 r = { k, k + 1, k + 2 }; return r; }
static struct odd13 mk_odd13(int k) { struct odd13 r = { k, k + 1, k + 2 }; return r; }
static int fact(int n) { return n <= 1 ? 1 : n * fact(n - 1); }
static int cmp_int(const void *a, const void *b) { return *(const int *)a - *(const int *)b; }

int main(void) {
  unsigned char buf[8] = { 0x80, 0x01, 0xff, 0xfe, 0x12, 0x34, 0x56, 0x78 };
  unsigned char *p = buf;
  signed char sc = -5; unsigned char uc = 200; short sh = -300; unsigned short us = 60000;
  int i = -7; unsigned ui = 3000000000u; long l = -123456789012L; unsigned long ul = 18000000000000000000UL;
  long long ll = -1; float f = 1.5f; double d = -2.75;

  /* promotions and byte assembly */
  P("%u", (unsigned)(uint16_t)((p[0] << 8) + p[1]));
  P("%d", (int16_t)((p[0] << 8) + p[1]));
  P("%u", (unsigned)(((uint32_t)p[0] << 24) + (p[1] << 16) + (p[2] << 8) + p[3]));
  P("%d", (int32_t)(((uint32_t)p[0] << 24) + (p[1] << 16) + (p[2] << 8) + p[3]));
  P("%lu", ((uintptr_t)p[0] << 56) + ((uintptr_t)p[7]));
  P("%d", sc >> 1); P("%d", uc >> 1); P("%d", sh >> 2); P("%d", us >> 3);
  P("%d", sc + uc); P("%d", (signed char)uc); P("%u", (unsigned char)sc); P("%d", (short)us);
  P("%u", (unsigned)(uint8_t)i); P("%u", (unsigned)(uint16_t)i);
  P("%ld", (long)ui); P("%d", (int)ul); P("%u", (unsigned)l); P("%lld", ll >> 3);
  P("%lu", ul >> 60); P("%ld", l >> 5); P("%lu", ul / 7); P("%lu", ul % 7); P("%ld", l / 7); P("%ld", l % 7);
  P("%u", ui / 7); P("%u", ui % 7); P("%d", i / 2); P("%d", i % 2); P("%d", -i / 2);
  P("%d", (int)(ui > (unsigned)i)); P("%d", (int)(i < ui)); P("%d", (int)(l < ul)); P("%d", (int)(-1 < 0u));
  P("%d", (int)(sc < uc)); P("%d", (int)(sh < us)); P("%d", (int)((unsigned char)-1 == 255));
  P("%d", 1 << 31 == INT32_MIN); P("%u", 1u << 31); P("%lu", 1ul << 63); P("%d", (int)(0x7fffffff + 1u));
  P("%d", !i); P("%d", !!i); P("%d", ~i); P("%d", -i); P("%d", i & 0xf); P("%d", i | 8); P("%d", i ^ 5);
  P("%d", i && 0); P("%d", i || 0); P("%d", 0 && (i = 99)); P("%d", i);
  P("%d", i ? 1 : 2); P("%d", 0 ? 1 : 2); P("%d", (i, 5)); P("%d", (i += 3, i *= 2, i -= 1)); P("%d", i);
  P("%d", i <<= 2); P("%d", i >>= 1); P("%d", i %= 5); P("%d", i /= 2); P("%d", i |= 6); P("%d", i &= 3); P("%d", i ^= 1);
  P("%u", ui <<= 1); P("%u", ui >>= 3); P("%d", uc += 100); P("%d", sc -= 200); P("%d", sh *= 300); P("%d", us /= 7);

  /* floating point */
  P("%.6f", f); P("%.6f", d); P("%.6f", f + d); P("%.6f", d * 3); P("%.6f", d / 4); P("%.6f", -d);
  P("%d", (int)d); P("%d", (int)-d); P("%d", (int)f); P("%u", (unsigned)2.5); P("%ld", (long)-1e10);
  P("%.1f", (double)ul); P("%.1f", (double)l); P("%.1f", (double)ui); P("%.1f", (double)uc); P("%.1f", (double)sc);
  P("%lu", (unsigned long)1e19); P("%lu", (unsigned long)9.2233720368547758e18); P("%ld", (long)-9.0e18);
  P("%.3f", (float)d); P("%d", d < f); P("%d", d > f); P("%d", d == d); P("%d", d != f); P("%d", d <= -2.75); P("%d", f >= 1.5);
  P("%d", !d); P("%d", d && 1); P("%d", (int)(0.0 || 0));
  P("%.2f", d += 1); P("%.2f", d *= 2); P("%.2f", d -= 0.5); P("%.2f", d /= 2); P("%.2f", f++); P("%.2f", ++f); P("%.2f", f--);
  { double nan = 0.0 / 0.0; P("%d", nan == nan); P("%d", nan != nan); P("%d", nan < 1.0); P("%d", nan >= 1.0); }
  P("%d", (int)(1e30 > 0)); P("%.6e", 1e30 / 3); P("%.6e", 1.0 / 3.0f);

  /* pointers and arrays */
  { int arr[5] = { 1, 2, 3, 4, 5 }; int *q = arr + 4; int *r = &arr[1];
    P("%ld", q - r); P("%d", *q - *r); P("%d", q > r); P("%d", r[2]); P("%d", 2[r]); P("%d", *(arr + 3));
    P("%d", *q--); P("%d", *--q); P("%d", *++r); P("%d", *r++); P("%d", (int)(q - arr)); P("%d", (int)(r - arr));
    q -= 2; P("%d", *q); r += 1; P("%d", *r); P("%d", (int)sizeof arr); P("%d", (int)(sizeof arr / sizeof *arr));
    P("%d", arr[4] = 9); P("%d", arr[0] += arr[4]); P("%d", (arr[1]++, arr[1])); }
  { char s[16] = "hello"; const char *t = "world"; char *e;
    strcat(s, t); P("%s", s); P("%d", (int)strlen(s)); P("%c", s[4]); P("%d", s[0] == 'h'); P("%d", s[10]);
    e = strchr(s, 'w'); P("%ld", e - s); P("%d", *e); P("%d", (int)(uintptr_t)(e - s)); P("%d", (int)sizeof("abc")); P("%d", (int)sizeof(s)); }
  { int m[2][3] = { { 1, 2, 3 }, { 4, 5, 6 } }; int (*row)[3] = m + 1; P("%d", row[0][2]); P("%d", (*row)[1]); P("%d", **m); P("%d", (int)sizeof m[0]); P("%d", m[1][2] * m[0][1]); }
  { int x = 5; int *px = &x; int **ppx = &px; **ppx = 8; P("%d", x); P("%d", *px == x); (*ppx)++; P("%d", px == &x + 1); }

  /* structs, unions, bit-fields */
  { struct s a = { 'a', -2, 3, -4L, 5.5, 6.5f, 250 }; struct s b = a; b.i = 30; a.c++;
    P("%d", a.c); P("%d", b.c); P("%d", a.sh); P("%d", b.i); P("%ld", a.l); P("%.1f", a.d); P("%.1f", b.f); P("%u", a.uc);
    P("%d", (int)sizeof a); P("%d", (int)offsetof(struct s, d)); P("%d", (int)offsetof(struct s, uc));
    struct s *ps = &b; ps->l = 99; ps->d += 1; P("%ld", b.l); P("%.1f", b.d); P("%d", (&b)->i); }
  { struct bits bf = { 5, 17, -3, 8000000000UL, -1 }; P("%u", bf.a); P("%u", bf.b); P("%d", bf.c); P("%lu", bf.d); P("%d", bf.e);
    bf.a += 4; bf.c = 7; bf.b--; bf.e = 1; P("%u", bf.a); P("%d", bf.c); P("%u", bf.b); P("%d", bf.e); P("%d", (int)sizeof bf);
    struct bits z = { 0 }; z.d = 1; P("%lu", z.d); P("%u", z.a); P("%d", bf.a > bf.c); P("%d", bf.c++); P("%d", bf.c); }
  { union u un; un.l = 0x0102030405060708L; P("%d", un.b[0]); P("%d", un.b[7]); un.d = 1.0; P("%lx", (unsigned long)un.l); P("%d", (int)sizeof un); }
  { struct small sm = mk_small(3, 4); P("%d", sm.x + sm.y); P("%d", mk_small(7, 8).y);
    struct pair_d pd = mk_pd(1.25, 2.5); P("%.2f", pd.a + pd.b); struct mixed mx = mk_mixed(2, 0.5); P("%.2f", mx.i + mx.d);
    struct big bg = mk_big(10); P("%ld", sum_big(bg)); P("%ld", sum_big(mk_big(1)));
    P("%.2f", sum_mixed(mx, pd, sm)); P("%.2f", sum_mixed(mk_mixed(1, 1), mk_pd(1, 1), mk_small(1, 1))); }
  { struct small arr[3] = { { 1, 2 }, [2] = { .y = 9, .x = 8 } }; P("%d", arr[1].x); P("%d", arr[2].y); P("%d", arr[2].x);
    struct small *ps = arr; ps++; ps->x = 5; P("%d", arr[1].x); P("%d", (ps + 1)->y); P("%d", (int)((char *)&arr[2] - (char *)arr)); }
  { static struct { int n; const char *name; struct small s; } table[] = { { 1, "one", { 1, 1 } }, { 2, "two", { 2, 2 } } };
    P("%s", table[1].name); P("%d", table[0].s.y + table[1].s.x); P("%d", (int)(sizeof table / sizeof table[0])); }

  /* calls */
  P("%d", many(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)); P("%.1f", manyd(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
  { int k = 42; P("%ld", va(8, 1, 2L, 3.0, &k, 5, 6L, 7.0, &k)); P("%ld", va(0)); }
  P("%d", fact(6)); { int (*fp)(int) = fact; P("%d", fp(5)); P("%d", (*fp)(4)); }
  { int arr[] = { 5, 3, 9, 1 }; qsort(arr, 4, sizeof arr[0], cmp_int); printf("sorted %d %d %d %d\n", arr[0], arr[1], arr[2], arr[3]); }
  P("%d", printf("%s", "") == 0);

  /* control flow */
  { int n = 0; for (int k = 0; k < 10; k++) { if (k == 3) continue; if (k == 8) break; n += k; } P("%d", n);
    n = 0; while (n < 5) n++; do n += 10; while (n < 40); P("%d", n);
    int sw = 0; for (int k = 0; k < 6; k++) switch (k) { case 0: sw += 1; case 1: sw += 10; break; case 4: case 5: sw += 100; break; default: sw += 1000; } P("%d", sw);
    unsigned char code = 0x85; switch (code) { case 0x85: P("%s", "case 0x85"); break; default: P("%s", "default"); }
    /* dense switches become jump tables; sparse ones stay compare chains */
    { int acc = 0; for (int k = -2; k < 14; k++) switch (k) { case 0: acc += 1; break; case 1: acc += 2; break; case 2: acc += 4; break; case 3: acc += 8; break; case 5: acc += 16; break; case 7: acc += 32; break; case 8: acc += 64; break; case 12: acc += 128; break; default: acc += 1000; } P("%d", acc);
      unsigned char op = 9; switch (op) { case 3: P("%s", "three"); break; case 9: P("%s", "nine"); break; case 10: P("%s", "ten"); break; case 200: P("%s", "big"); break; default: P("%s", "other"); }
      long big = 1L << 40; switch (big) { case 1L << 40: P("%s", "forty"); break; case 1: case 2: case 3: case 4: P("%s", "small"); break; default: P("%s", "none"); }
      int neg2 = -3; switch (neg2) { case -5: P("%s", "m5"); break; case -4: P("%s", "m4"); break; case -3: P("%s", "m3"); break; case -2: P("%s", "m2"); break; case -1: P("%s", "m1"); break; default: P("%s", "d"); } }
    uint32_t magic = 0x8495A6BEu; switch (magic) { case 0x8495A6BE: P("%s", "magic small"); break; case 0x8495A6BF: P("%s", "magic big"); break; default: P("%s", "bad object"); }
    unsigned long umagic = 0x8495A6BE8495A6BEul; switch (umagic) { case 0x8495A6BE8495A6BEul: P("%s", "big magic"); break; default: P("%s", "no"); }
    long lsw = -5; switch (lsw) { case -5: P("%s", "neg case"); break; default: P("%s", "default"); }
    int g = 0; goto skip; g = 100; skip: g += 1; P("%d", g);
    int labelled = 0; loop: if (++labelled < 3) goto loop; P("%d", labelled); }
  { int t = 0; unsigned char c8 = 0x40; if (c8 >= 0x40) t |= 1; if (c8 >= 0x80) t |= 2; if ((c8 & 0xF) == 0) t |= 4; if ((c8 >> 4) & 0x7) t |= 8; P("%d", t); }

  /* odd-sized aggregates in registers: partial eightbytes */
  { struct s3 { char a, b, c; }; struct s5 { int a; char b; }; struct s12 { int a, b, c; }; struct s13 { long a; int b; char c; };
    struct s3 v3 = { 1, 2, 3 }; struct s5 v5 = { 5, 6 }; struct s12 v12 = { 7, 8, 9 }; struct s13 v13 = { 10, 11, 12 };
    int (*f3)(struct s3) = 0; (void)f3;
    struct s3 r3 = ((struct s3 (*)(struct s3, struct s5, struct s12, struct s13))0 ? v3 : v3); (void)r3;
    P("%d", v3.a + v3.b + v3.c); P("%d", v5.a + v5.b); P("%d", v12.a + v12.b + v12.c); P("%ld", v13.a + v13.b + v13.c); }
  P("%d", odd_sum(( struct odd3){ 1, 2, 3 }, (struct odd5){ 4, 5 }, (struct odd12){ 6, 7, 8 }, (struct odd13){ 9, 10, 11 }));
  { struct odd3 r = mk_odd3(4); P("%d", r.a + r.b + r.c); struct odd12 q = mk_odd12(2); P("%d", q.a * q.b * q.c); struct odd13 t = mk_odd13(3); P("%ld", t.a + t.b + t.c); }
  /* narrow register variables: the upper bits of the machine register are
     garbage after a 64-bit store, so widening must extend from the low part */
  { int neg = (int)-1L; unsigned un = 0xfffffff0u; short sh2 = (short)0x8001; unsigned char c2 = (unsigned char)0x1ff;
    unsigned long z = (unsigned)neg; long w = neg; unsigned long z2 = un; long w2 = (int)un; long w3 = sh2; unsigned long z3 = (unsigned short)sh2; long w4 = c2;
    P("%lu", z); P("%ld", w); P("%lu", z2); P("%ld", w2); P("%ld", w3); P("%lu", z3); P("%ld", w4);
    for (int k = 0; k < 2; k++) { neg -= 1; z = (unsigned)neg; } P("%lu", z); P("%d", neg < 0); P("%u", un >> 4); }
  /* _Bool conditions in every branching position, and libm intrinsics */
  { _Bool flag = (i == i); char pad[7] = { 1, 2, 3, 4, 5, 6, 7 }; (void)pad;
    flag ? (void)P("%s", "flag true") : (void)P("%s", "flag false");
    struct small s1 = { 1, 1 }, s2 = { 2, 2 }; struct small pick = flag ? s1 : s2; P("%d", pick.x);
    _Bool nf = !flag; nf ? (void)P("%s", "wrong") : (void)P("%s", "nf false"); P("%d", flag && !nf); P("%d", nf || flag);
    while (nf) nf = 0; for (_Bool g = 1; g; g = 0) P("%s", "loop once"); if (nf) P("%s", "wrong"); else P("%s", "else"); }
  P("%.3f", fabs(-2.5)); P("%.3f", fabs(3.25)); P("%.3f", sqrt(16.0)); P("%.3f", sqrt(2.0) * sqrt(2.0)); P("%.3f", (double)fabsf(-1.5f)); P("%.3f", (double)sqrtf(9.0f));
  /* enums, sizeof, alignment, constants */
  { enum e { A, B = 5, C, D = -1, E }; P("%d", A); P("%d", C); P("%d", E); P("%d", (int)sizeof(enum e)); P("%d", (int)sizeof(enum e) == sizeof(int)); }
  P("%d", (int)sizeof(long double)); P("%d", (int)_Alignof(long double)); P("%d", (int)sizeof(struct mixed)); P("%d", (int)_Alignof(struct s));
  P("%d", (int)sizeof(1 ? (void *)0 : 0)); P("%d", (int)sizeof 'a'); P("%d", (int)sizeof(char)); P("%d", 'a'); P("%d", '\377'); P("%d", '\n');
  P("%d", (int)sizeof(L"ab")); P("%d", (int)sizeof(u8"ab")); P("%d", (int)sizeof(U"ab")); P("%d", (int)sizeof(u"ab")); P("%d", L'x'); P("%d", (int)L"abc"[1]);
  P("%lld", 0x7fffffffffffffffLL); P("%lu", 0xffffffffffffffffUL); P("%d", 0x7fffffff); P("%u", 0xffffffff); P("%d", 017);
  P("%d", (int)(4294967295 > 0)); P("%d", (int)sizeof 4294967295); P("%d", (int)sizeof 2147483648); P("%d", (int)sizeof 2147483647);
  P("%.3e", 1e-300 * 1e-300); P("%.3f", 0x1.8p1); P("%.3f", 1e2f); P("%d", (int)sizeof 1.0f); P("%d", (int)sizeof 1.0);
  { _Bool b1 = 5, b2 = 0.5, b3 = (void *)0 != 0; P("%d", b1); P("%d", b2); P("%d", b3); P("%d", b1 + b2); P("%d", (int)sizeof b1); b1++; P("%d", b1); b1 = 256; P("%d", b1); }
  { const char *sel = _Generic(i, int: "int", long: "long", default: "other"); P("%s", sel); P("%s", _Generic(d, double: "double", default: "other")); }
  { int a2[3] = { [1] = 4 }; printf("a2 %d %d %d\n", a2[0], a2[1], a2[2]); char s2[8] = "ab"; P("%d", s2[3]); P("%d", (int)sizeof s2);
    struct s zs = { .i = 4 }; P("%d", zs.c + zs.i); P("%.1f", zs.d); }
  { int x = 3; int y = x++ + ++x; P("%d", y); P("%d", x); int z = (x = 2) + x; P("%d", z > 0); }
  { unsigned char c = 250; c += 10; P("%d", c); unsigned short w = 65530; w += 10; P("%d", w); signed char s8 = 120; s8 += 7; P("%d", s8); }
  { long shifty = 1; P("%ld", shifty << 40); P("%d", 1 << 20); P("%ld", (long)1 << 33); P("%lu", 0xFFul << 56); P("%lx", (unsigned long)-1 >> 4); P("%x", -1 >> 4); }
  return 0;
}
