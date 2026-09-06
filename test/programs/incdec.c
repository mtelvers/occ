// expect: 9
int main(void) {
  int i = 5;
  int a = i++;        /* a=5, i=6 */
  int b = ++i;        /* b=7, i=7 */
  int c = i--;        /* c=7, i=6 */
  char ch = 127; ch++; /* wraps to -128 */
  _Bool flag = 5;     /* 1 */
  flag++;             /* stays 1 */
  short sh = -1; unsigned short us = sh; /* 65535 */
  return a + b + c - i - (ch == -128 ? 10 : 0) - flag + (us == 65535) + 6; /* 5+7+7-6-10-1+1+6 */
}
