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

/* A switch whose cases have their top bit set, which is what a
   marshalling magic number looks like: the value and the constants must
   be made the same way or none of them ever matches. */
static int magic(unsigned m) {
  switch (m) {
  case 0x8495A6BEu: return 1;
  case 0x8495A6BFu: return 2;
  case 0x8495A6BDu: return 3;
  case 0x7fffffffu: return 4;
  default: return -1;
  }
}

static int wide_magic(unsigned long m) {
  switch (m) {
  case 0xffffffffffffffffUL: return 1;
  case 0x8000000000000000UL: return 2;
  default: return -1;
  }
}

/* A switch over a run of nearby values, which becomes a jump table: the
   shape of an interpreter's dispatch, and the reason the table is worth
   having.  The holes and the ends are what a table gets wrong if the
   bounds check or the index is off by one. */
static int dispatch(int op, int a, int b) {
  switch (op) {
  case 0: return a + b;
  case 1: return a - b;
  case 2: return a * b;
  case 3: return a & b;
  case 4: return a | b;
  case 5: return a ^ b;
  case 6: return a << (b & 7);
  case 8: return a >> (b & 7);          /* 7 is a hole */
  case 9: return -a;
  case 10: return ~b;
  case 11: return a < b;
  case 12: return a == b;
  default: return 12345;
  }
}

/* one whose cases do not start at zero, so the index is the value less
   the smallest case */
static int shifted(int op) {
  switch (op) {
  case 100: return 1; case 101: return 2; case 102: return 3;
  case 103: return 4; case 104: return 5; case 106: return 7;
  default: return -1;
  }
}

/* and one on a narrower type, where the index has to be taken
   zero-extended */
static int narrow_switch(unsigned char c) {
  switch (c) {
  case 250: return 1; case 251: return 2; case 252: return 3;
  case 253: return 4; case 255: return 6;
  default: return 0;
  }
}

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
  yes("switch on a value with its top bit set",
      magic(0x8495A6BEu) == 1 && magic(0x8495A6BFu) == 2 && magic(0x8495A6BDu) == 3
      && magic(0x7fffffffu) == 4 && magic(0) == -1);
  yes("a jump table", dispatch(0, 7, 3) == 10 && dispatch(1, 7, 3) == 4
      && dispatch(2, 7, 3) == 21 && dispatch(5, 7, 3) == 4
      && dispatch(6, 1, 3) == 8 && dispatch(8, 64, 3) == 8
      && dispatch(9, 5, 0) == -5 && dispatch(10, 0, 0) == -1
      && dispatch(11, 1, 2) == 1 && dispatch(12, 2, 2) == 1);
  yes("its holes and its edges",
      dispatch(7, 1, 1) == 12345 && dispatch(13, 1, 1) == 12345
      && dispatch(-1, 1, 1) == 12345 && dispatch(1000000, 1, 1) == 12345);
  yes("a table that does not start at zero",
      shifted(100) == 1 && shifted(104) == 5 && shifted(106) == 7
      && shifted(105) == -1 && shifted(99) == -1 && shifted(107) == -1);
  yes("a table on a narrower type",
      narrow_switch(250) == 1 && narrow_switch(253) == 4 && narrow_switch(255) == 6
      && narrow_switch(254) == 0 && narrow_switch(0) == 0 && narrow_switch(100) == 0);
  yes("switch at the register's own width",
      wide_magic(0xffffffffffffffffUL) == 1 && wide_magic(0x8000000000000000UL) == 2
      && wide_magic(1) == -1);
  return 0;
}
