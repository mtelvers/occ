// expect: 42
int main(void) {
  int a = 6, b = 7;
  int c = a * b;            /* 42 */
  int d = (c + 3) / 5;      /* 9 */
  int e = d % 4;            /* 1 */
  unsigned u = 1u << 31;
  long l = -1L;
  return c + e - 1 + (u >> 31) - 1 + (int)(l + 1) + (~0 & 0);
}
