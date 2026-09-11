/* an executable a loaded object binds back into */
#include <dlfcn.h>
#include <stdio.h>
int host_counter = 100;
void host_says(const char *what) { printf("host says: %s (counter %d)\n", what, host_counter++); }
int main(int argc, char **argv) {
  void *h = dlopen(argv[1], RTLD_NOW);
  if (!h) { printf("dlopen: %s\n", dlerror()); return 1; }
  void (*run)(void) = dlsym(h, "run");
  if (!run) { printf("dlsym: %s\n", dlerror()); return 1; }
  run(); run();
  printf("counter is now %d\n", host_counter);
  return 0;
}
