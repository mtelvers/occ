/* A frame, and a block move, too big for the twelve bits an offset or an
   immediate has.  Real code has both -- the OCaml runtime's own files
   are where these turned up -- and the small tests had neither. */
int puts(const char *);
static void yes(const char *w, int ok) { puts(ok ? w : "FAILED"); }

struct big { int n; char pad[4093]; };

static struct big empty;                  /* in .bss */

static struct big make(int n) {
  struct big b;
  b.n = n;
  for (int i = 0; i < 4093; i++) b.pad[i] = (char)(i & 0x3f);
  return b;
}

static int by_value(struct big b) {
  int t = b.n;
  for (int i = 0; i < 4093; i += 97) t += b.pad[i];
  return t;
}

static int by_pointer(const struct big *b) {
  int t = b->n;
  for (int i = 0; i < 4093; i += 97) t += b->pad[i];
  return t;
}

static int deep(void) {
  char buf[5000];                         /* the frame is past 2047 bytes */
  buf[0] = 1;
  buf[2048] = 3;
  buf[4999] = 2;
  return buf[0] + buf[2048] + buf[4999];
}

int main(void) {
  yes("a frame past twelve bits", deep() == 6);
  struct big b = make(7);
  yes("an aggregate returned in memory", b.n == 7 && b.pad[4092] == (char)(4092 & 0x3f));
  yes("one passed by value", by_value(b) == by_pointer(&b));
  struct big c = b;
  yes("a large copy", c.n == 7 && c.pad[1000] == b.pad[1000] && c.pad[4092] == b.pad[4092]);
  struct big z = { 0 };
  yes("a large clear", z.n == 0 && z.pad[0] == 0 && z.pad[4000] == 0);
  yes("and one in .bss", empty.n == 0 && empty.pad[4000] == 0);
  return 0;
}
