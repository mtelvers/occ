/* control flow: every statement Lower turns into branches */
int puts(const char *);
static void yes(const char *w, int ok) { puts(ok ? w : "FAILED"); }

static int loops(int n) { int s = 0; for (int i = 0; i < n; i++) s += i; return s; }
static int whileloop(int n) { int s = 0; while (n > 0) { s += n; n--; } return s; }
static int dowhile(int n) { int s = 0; do { s++; n--; } while (n > 0); return s; }
static int switcher(int n) {
  switch (n) { case 1: return 10; case 2: return 20; case 7: return 70; default: return -1; }
}
static int gotos(int n) { int s = 0; again: if (n <= 0) goto done; s += n; n--; goto again; done: return s; }
static int nested(int a, int b) { if (a > b) { if (a > 10) return 1; return 2; } else if (b > 10) return 3; return 4; }
static int shortcircuit(int a, int b) { return (a && b) + 2 * (a || b); }
static int ternary(int a) { return a > 0 ? a * 2 : -a; }
static int breaker(void) { int s = 0; for (int i = 0; i < 10; i++) { if (i == 5) break; if (i % 2) continue; s += i; } return s; }

int main(void) {
  yes("for", loops(5) == 10);
  yes("while", whileloop(4) == 10);
  yes("do-while", dowhile(3) == 3);
  yes("switch 1", switcher(1) == 10);
  yes("switch 7", switcher(7) == 70);
  yes("switch default", switcher(99) == -1);
  yes("goto", gotos(4) == 10);
  yes("nested if", nested(20, 1) == 1 && nested(5, 1) == 2 && nested(1, 20) == 3 && nested(1, 2) == 4);
  yes("short circuit", shortcircuit(1, 0) == 2 && shortcircuit(1, 1) == 3 && shortcircuit(0, 0) == 0);
  yes("ternary", ternary(3) == 6 && ternary(-3) == 3);
  yes("break and continue", breaker() == 6);
  return 0;
}
