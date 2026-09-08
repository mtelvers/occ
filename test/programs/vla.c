// expect: 0
// variable length arrays (6.7.6.2): declarations, sizeof, pointers to VLAs
static long sum(int n, int m) {
  int a[n][m];
  for (int i = 0; i < n; i++) for (int j = 0; j < m; j++) a[i][j] = i * 10 + j;
  long s = 0;
  for (int i = 0; i < n; i++) for (int j = 0; j < m; j++) s += a[i][j];
  return s + sizeof a + sizeof a[0];
}
static const void *find(const void *key, const void *base, unsigned long nel, unsigned long width) {
  const char (*p)[width] = base;
  for (unsigned long i = 0; i < nel; i++) {
    const char *a = p[i], *b = key;
    unsigned long k = 0;
    while (k < width && a[k] == b[k]) k++;
    if (k == width) return p + i;
  }
  return 0;
}
int main(void) {
  int n = 3;
  char b[n * 3 + 1];
  b[sizeof b - 1] = 7;
  long arr[5] = { 10, 20, 30, 40, 50 }, key = 40;
  const long *f = find(&key, arr, 5, sizeof(long));
  return (sum(3, 4) == 202 && sizeof b == 10 && b[9] == 7 && f - arr == 3 && sizeof(struct { int q; }[n + 1]) == 16) ? 0 : 1;
}
