/* the C library as real code uses it: formatted output, allocation,
   strings, sorting and a function pointer */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct person { char name[8]; int age; };

static int by_age(const void *a, const void *b) {
  const struct person *x = a, *y = b;
  return x->age - y->age;
}

static long fib(int n) { return n < 2 ? n : fib(n - 1) + fib(n - 2); }

int main(void) {
  printf("ints %d %ld %u %#x\n", -42, 1234567890123L, 4000000000u, 255);
  printf("strings %s %c %s\n", "one", 'x', "three");
  printf("floats %.3f %e %g\n", 3.14159, 12345.6789, 0.0001);
  printf("widths %5d|%-5d|%05d|%+d\n", 42, 42, 42, 42);
  char *buf = malloc(64);
  snprintf(buf, 64, "%s-%d", "made", 7);
  printf("snprintf %s len %zu\n", buf, strlen(buf));
  strcpy(buf, "hello");
  strcat(buf, " world");
  printf("strings %s %d %d\n", buf, strcmp(buf, "hello world"), (int)strlen(buf));
  memset(buf, 'z', 4); buf[4] = 0;
  printf("memset %s\n", buf);
  free(buf);
  struct person people[4] = { { "dee", 40 }, { "al", 20 }, { "cy", 35 }, { "bo", 25 } };
  qsort(people, 4, sizeof people[0], by_age);
  for (int i = 0; i < 4; i++) printf("sorted %s %d\n", people[i].name, people[i].age);
  long (*f)(int) = fib;
  printf("fib %ld %ld\n", f(10), fib(20));
  int *heap = calloc(10, sizeof(int));
  for (int i = 0; i < 10; i++) heap[i] = i * i;
  printf("calloc %d %d\n", heap[3], heap[9]);
  free(heap);
  printf("limits %d %ld\n", 2147483647, 9223372036854775807L);
  return 0;
}
