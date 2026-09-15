/* pointers, arrays and the address arithmetic that goes with them */
int puts(const char *);
static void yes(const char *w, int ok) { puts(ok ? w : "FAILED"); }

static int arr[8] = { 0, 1, 2, 3, 4, 5, 6, 7 };
static char text[] = "riscv";
struct point { int x, y; };
static struct point origin = { 3, 4 };

static int sum(const int *p, int n) { int s = 0; while (n--) s += *p++; return s; }
static void swap(int *a, int *b) { int t = *a; *a = *b; *b = t; }
static int strlen_(const char *s) { const char *p = s; while (*p) p++; return (int)(p - s); }
static int *third(int *p) { return p + 3; }

int main(void) {
  yes("array sum", sum(arr, 8) == 28);
  yes("pointer arithmetic", *third(arr) == 3);
  yes("difference", third(arr) - arr == 3);
  int a = 1, b = 2; swap(&a, &b);
  yes("swap", a == 2 && b == 1);
  yes("string length", strlen_(text) == 5);
  yes("string bytes", text[0] == 'r' && text[4] == 'v');
  yes("struct fields", origin.x == 3 && origin.y == 4);
  struct point *p = &origin;
  yes("through a pointer", p->x + p->y == 7);
  int two[2][3] = { { 1, 2, 3 }, { 4, 5, 6 } };
  yes("two dimensions", two[1][2] == 6 && two[0][0] == 1);
  yes("cast to char", *(char *)arr == 0);
  return 0;
}
