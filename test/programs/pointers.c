// expect: 42
int arr[5] = { 1, 2, 3, 4, 5 };
static int sum(const int *p, int n) { int s = 0; while (n-- > 0) s += *p++; return s; }
int main(void) {
  int *p = arr + 1;
  p[1] = 30;                  /* arr[2] = 30 */
  int *q = &arr[4];
  if (q - p != 3 || !(q > p)) return 1;
  return sum(arr, 5);         /* 1+2+30+4+5 */
}
