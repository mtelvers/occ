/* The data side, which is ELF's rather than the machine's: thread-local
   storage, weak and hidden bindings, an alias, and the constructors the
   loader runs before main.  A second thread proves that a thread-local
   is per-thread and not merely a global that happens to work. */
/* The thread functions are declared here rather than included: glibc's
   <sys/cdefs.h> defines __attribute__ away when the compiler does not
   claim to be GNU C, and occ deliberately does not (doc/extensions.md),
   so a header in the way would take the attributes below with it. */
typedef unsigned long thread_id;
int pthread_create(thread_id *, const void *, void *(*)(void *), void *);
int pthread_join(thread_id, void **);
int puts(const char *);
static void yes(const char *w, int ok) { puts(ok ? w : "FAILED"); }

__thread int slot = 7;                  /* initialised: .tdata */
__thread long zeroed;                   /* not: .tbss */
static __thread int private_slot = 3;   /* a local one */

static void *other(void *unused) {
  (void)unused;
  int ok = slot == 7 && zeroed == 0 && private_slot == 3;
  slot = 99; zeroed = 1; private_slot = 4;
  return ok ? (void *)1 : (void *)0;
}

int the_target(void) { return 11; }
__attribute__((weak)) int weakly(void) { return 12; }
int aliased(void) __attribute__((alias("the_target")));
__attribute__((visibility("hidden"))) int unseen = 13;

static int order[4], n_order;
__attribute__((constructor)) static void first(void) { order[n_order++] = 1; }
__attribute__((constructor)) static void second(void) { order[n_order++] = 2; }
__attribute__((destructor)) static void last(void) { puts("the destructor ran"); }

int main(void) {
  yes("a thread-local, initialised", slot == 7);
  yes("one with no initialiser", zeroed == 0);
  yes("a static thread-local", private_slot == 3);
  slot = 8; zeroed = 5; private_slot = 6;
  thread_id t; void *r;
  if (pthread_create(&t, 0, other, 0) != 0) return 1;
  pthread_join(t, &r);
  yes("a second thread has its own", r == (void *)1);
  yes("and did not disturb ours", slot == 8 && zeroed == 5 && private_slot == 6);
  yes("a weak definition", weakly() == 12);
  yes("an alias", aliased() == 11);
  yes("a hidden symbol", unseen == 13);
  yes("the constructors ran, in order", n_order == 2 && order[0] == 1 && order[1] == 2);
  return 0;
}
