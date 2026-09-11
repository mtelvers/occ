/* calls out and reads a variable of another object */
#include <stdio.h>
#include <string.h>
static int calls = 0;
int greet(const char *who) {
  calls++;
  fprintf(stdout, "hello, %s (call %d, len %zu)\n", who, calls, strlen(who));
  fflush(stdout);
  return calls;
}
