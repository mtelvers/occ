// expect: 0
// stdout: sum=6 str=(a + b) id=CAT(x_, y) count=0,1 v=7 empty=1 one=0
// The preprocessor: stringizing, pasting, variadic macros including the
// GNU ", ## __VA_ARGS__" idiom, __COUNTER__, #if arithmetic, and rescanning.
#include <stdio.h>
#define STR(x) #x
#define XSTR(x) STR(x)
#define CAT(a, b) a ## b
#define SUM(...) sum(__VA_ARGS__, 0)
#define LOG(fmt, ...) printf(fmt, ## __VA_ARGS__)
#define GENSYM_(n, c) CAT(n, c)
#define GENSYM(n) GENSYM_(n, __COUNTER__) /* two levels, so __COUNTER__ expands before ## */
#define SEVEN 7
#define EXPR (a + b)
#if (1 << 4) - 6 == 10 && defined(SEVEN) && SEVEN * 2 > 13 && !defined(NOT_DEFINED)
#define IF_OK 1
#else
#define IF_OK 0
#endif
#if 0xffffffffffffffff > 0 && -1 < 0u
#define UNSIGNED_OK 1 /* not reached: -1 converts to UINTMAX_MAX */
#else
#define UNSIGNED_OK 0
#endif
static int sum(int a, ...) { return a + 5; }
int main(void) {
  int x_y = 1; (void)x_y;
  int GENSYM(v_) = 0; int GENSYM(v_) = 1;
  LOG("sum=%d str=%s id=%s count=%d,%d v=%d empty=%d one=%d\n",
      SUM(1), XSTR(EXPR), STR(CAT(x_, y)), v_0, v_1, SEVEN, IF_OK, UNSIGNED_OK);
  LOG("");
  return 0;
}
