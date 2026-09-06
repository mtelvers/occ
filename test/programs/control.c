// expect: 55
int main(void) {
  int sum = 0;
  for (int i = 1; i <= 10; i++) {
    if (i % 2 == 0) continue;
    sum += i;                 /* 1+3+5+7+9 = 25 */
  }
  int n = 0;
  while (n < 10) n++;
  do { n += 5; } while (n < 20); /* 20 */
  switch (n) {
    case 10: return 1;
    case 20: sum += 30; break;
    default: return 2;
  }
  return sum;                 /* 55 */
}
