// A threaded-code dispatcher, which is what the extension exists for:
// a static table of label addresses, indexed by an opcode, entered with
// "goto *".  interp.c stores the entries as offsets from a base label
// and adds the base back, so that form is here too.
// expect: 0
// stdout: 70 offsets b

#include <stdio.h>

static int run(const int *code, int n)
{
  static const void *const table[] = { &&op_add, &&op_mul, &&op_done };
  int acc = 0, i = 0;
#define next() do { if (i >= n) return acc; goto *table[code[i]]; } while (0)
  next();
op_add:
  acc += 10; i++; next();
op_mul:
  acc *= 3; i++; next();
op_done:
  return acc;
#undef next
}

/* the entries held as offsets from one label, as a threaded interpreter
   keeps them so that an opcode is a displacement */
static int offsets(int which)
{
  static const void *const table[] = { &&one, &&two };
  const char *base = (const char *) &&one;
  long off = (const char *) table[which] - base;
  goto *(void *) (base + off);
one:
  return 1;
two:
  return 2;
}

int main(void)
{
  printf("%d", run((int[]){ 0, 0, 1, 0, 2 }, 5));
  printf(" %s", offsets(0) == 1 && offsets(1) == 2 ? "offsets" : "wrong");
  /* a label's address is void *, so it converts and compares like one */
  void *p = &&b;
  if (p == &&b) goto *p;
  printf(" unreached\n");
  return 1;
b:
  printf(" b\n");
  return 0;
}
