/* a shared object that needs nothing but itself: the pointer below is
   an address inside it, which the loader has to rebase */
static int counter = 7;
int *const self = &counter;
int answer(void) { return counter * 6; }
int deref(void) { return *self; }
