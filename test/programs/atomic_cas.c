// expect: 3
// A lock-free push with compare-exchange on a pointer, as the runtime's
// custom operations table does; the width must be the pointee's, not _Bool's.
#include <stdatomic.h>
#include <stdlib.h>
struct node { int v; struct node *next; };
static _Atomic(struct node *) head;
static void push(int v) {
  struct node *n = malloc(sizeof *n);
  n->v = v;
  struct node *prev = atomic_load(&head);
  do { n->next = prev; } while (!atomic_compare_exchange_weak(&head, &prev, n));
}
int main(void) {
  push(1); push(2); push(3);
  int count = 0, sum = 0;
  for (struct node *l = atomic_load(&head); l != NULL; l = l->next) { count++; sum += l->v; }
  _Atomic long big = 5;
  long expected = 4;
  if (atomic_compare_exchange_strong(&big, &expected, 9) || expected != 5) return 100;
  expected = 5;
  if (!atomic_compare_exchange_strong(&big, &expected, 9) || atomic_load(&big) != 9) return 101;
  return sum - count * 1;  /* 6 - 3 */
}
