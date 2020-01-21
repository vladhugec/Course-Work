/* /tmp/book/src/uscheme/overflow.c:1 */
#line 1 "/tmp/book/src/uscheme/overflow.c"
#include "all.h"
/* cinterps.nw:1638 */
#line 1638 "cinterps.nw"
static volatile char *low_water_mark = NULL;

#define N 600 /* fuel in units of 10,000 */

static int default_eval_fuel = N * 10000;
static int eval_fuel         = N * 10000;
static bool throttled = 1;
static bool env_checked = 0;

int checkoverflow(int limit) {
  volatile char c;
  if (!env_checked) {
      env_checked = 1;
      const char *options = getenv("BPCOPTIONS");
      if (options == NULL)
          options = "";
      throttled = strstr(options, "nothrottle") == NULL;
  }
  if (low_water_mark == NULL) {
    low_water_mark = &c;
    return 0;
  } else if (low_water_mark - &c >= limit) {
    runerror("recursion too deep");
    return -1; /* not reachable, but the compiler can't tell */
  } else if (throttled && eval_fuel-- <= 0) {
    eval_fuel = default_eval_fuel;
    runerror("CPU time exhausted");
    return -1;
  } else {
    return (low_water_mark - &c);
  }
}

extern void reset_overflow_check(void) {
  eval_fuel = default_eval_fuel;
}
