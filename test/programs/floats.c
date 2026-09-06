// expect: 7
int main(void) {
  double d = 3.5;
  float f = 2.25f;
  double s = d + f;            /* 5.75 */
  unsigned long big = 1UL << 63;
  double bd = (double)big;     /* 2^63 */
  unsigned long back = (unsigned long)bd;
  int neg = (int)-d;           /* -3 */
  if (back != big) return 100;
  if (!(d > f) || d == f || (f < 0)) return 101;
  return (int)s + 1 + neg + (int)(bd / bd) + 3; /* 5 + 1 - 3 + 1 + 3 */
}
