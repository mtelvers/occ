// expect: 0
// stdout: hello, world 42 3.50 x
#include <stdio.h>
#include <string.h>
int main(void) {
  char buf[32] = "hello";
  const char *w = ", world";
  strcat(buf, w);
  printf("%s %d %.2f %c\n", buf, 42, 3.5, 'x');
  return (int)strlen(buf) - 12;
}
