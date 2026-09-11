/* stderr and stdout are variables of the C library, and code that is
   not position-independent refers to them by address */
#include <stdio.h>
int main(void) {
  fprintf(stderr, "to stderr\n");
  fprintf(stdout, "and to stdout\n");
  return 0;
}
