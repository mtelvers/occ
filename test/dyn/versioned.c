/* realpath is offered by glibc under two versions, and only the
   default one accepts a null second argument */
#include <stdio.h>
#include <stdlib.h>
int main(void) {
  char *r = realpath("/dev/null", NULL);
  printf("realpath -> %s\n", r ? r : "NULL");
  free(r);
  return r == NULL;
}
