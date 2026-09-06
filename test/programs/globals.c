// expect: 36
struct s { int a; char name[8]; int *p; };
int x = 5;
static int y;                          /* zero */
int *px = &x;
struct s table[2] = { { 1, "one", &x }, [1] = { .a = 2, .name = "two" } };
static const char *names[] = { "a", "bb", "ccc" };
enum { RED, GREEN = 5, BLUE } colour = BLUE;
union u { int i; char c[4]; } un = { .c = { 1, 0, 0, 0 } };
struct bits { unsigned a : 3, b : 5; int c : 4; } bf = { 5, 17, -3 };
int main(void) {
  y = *px + table[1].a + table[0].name[1] - 'n';   /* 5 + 2 + 0 = 7 */
  int n = names[2][2] - 'a';                         /* 2 */
  bf.b += 1;                                          /* 18 */
  return y + n + colour + un.i + bf.a + bf.b + bf.c;  /* 7 + 2 + 6 + 1 + 5 + 18 - 3 = 36 */
}
