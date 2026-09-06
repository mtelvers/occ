// expect: 76
struct point { int x, y; };
struct big { long a, b, c; double d; };
struct mixed { double d; int i; };
static struct point mk(int x, int y) { struct point p = { x, y }; return p; }
static int area(struct point a, struct point b) { return (b.x - a.x) * (b.y - a.y); }
static struct big bigger(struct big b) { b.a += 1; b.d *= 2; return b; }
static double mixed_sum(struct mixed m) { return m.d + m.i; }
int main(void) {
  struct point a = mk(1, 2), b = mk(5, 8);
  struct big g = { 1, 2, 3, 4.5 };
  g = bigger(g);
  struct mixed m = { .d = 1.5, .i = 2 };
  struct point *pp = &b;
  pp->x += 1;                                  /* b.x = 6 */
  return area(a, b) + (int)g.a + (int)g.d + (int)(mixed_sum(m) * 10); /* 30 + 2 + 9 + 35 */
}
