// expect: 12
// _Thread_local with extern: the declaration must not become a definition,
// and the variable must be addressable through the TLS base.
#include <pthread.h>
extern _Thread_local int counter;
_Thread_local int counter = 5;
static _Thread_local long other;
static void *worker(void *arg) {
  counter += 100;               /* this thread's copy only */
  other = counter;
  *(long *)arg = other;
  return 0;
}
int main(void) {
  pthread_t t;
  long seen = 0;
  counter += 7;                 /* 12 in the main thread */
  pthread_create(&t, 0, worker, &seen);
  pthread_join(t, 0);
  if (seen != 105 || other != 0) return 1;
  return counter;
}
