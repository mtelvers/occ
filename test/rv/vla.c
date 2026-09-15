/* Variable length arrays (6.7.6.2): the size is not known until the
   function runs, so the space comes from the stack itself.  The last two
   checks are the reason this is interesting on a machine whose
   arguments go below the stack pointer: a call with more arguments than
   there are registers has to find its own room even though a variable
   length array has moved sp since the frame was made. */
int puts(const char *);
static void yes(const char *w, int ok) { puts(ok ? w : "FAILED"); }

static int squares(int n) {
  int a[n];
  for (int i = 0; i < n; i++) a[i] = i * i;
  int t = 0;
  for (int i = 0; i < n; i++) t += a[i];
  return t;
}

static int grid(int rows, int cols) {
  int m[rows][cols];
  for (int i = 0; i < rows; i++)
    for (int j = 0; j < cols; j++) m[i][j] = i * cols + j;
  int t = 0;
  for (int i = 0; i < rows; i++)
    for (int j = 0; j < cols; j++) t += m[i][j];
  return t;
}

static long ten(long a, long b, long c, long d, long e,
                long f, long g, long h, long i, long j) {
  return a + b + c + d + e + f + g + h + i + j;
}

static long array_then_call(int n) {
  char buf[n];
  for (int k = 0; k < n; k++) buf[k] = (char)(k + 1);
  long t = ten(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  for (int k = 0; k < n; k++) t += buf[k];   /* the array must have survived */
  return t;
}

static int size_of(int n) { int a[n]; a[0] = 0; return (int)sizeof a; }

static int depth(int n) {
  if (n == 0) return 0;
  int a[n];
  a[0] = n;
  a[n - 1] = n;
  return a[0] + a[n - 1] + depth(n - 1);
}

int main(void) {
  yes("a one-dimensional array", squares(5) == 30);
  yes("sizeof is computed when it runs",
      size_of(3) == 3 * (int)sizeof(int) && size_of(9) == 9 * (int)sizeof(int));
  yes("two dimensions", grid(3, 4) == 66);
  yes("one row", grid(1, 9) == 36);
  yes("each call gets its own", squares(3) == 5 && squares(10) == 285);
  yes("a call that needs the stack", ten(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) == 55);
  yes("and one after an array", array_then_call(6) == 55 + 21);
  yes("recursion", depth(4) == 2 * (4 + 3 + 2 + 1));
  int n = 5;
  int v[n];
  yes("sizeof a variable length array", sizeof v == 5 * sizeof(int));
  return 0;
}
