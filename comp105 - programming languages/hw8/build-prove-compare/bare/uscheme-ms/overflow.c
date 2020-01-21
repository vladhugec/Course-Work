#include "all.h"
/* overflow.c 1070b */
static volatile char *low_water_mark = NULL;

#define N 600 /* fuel in units of 10,000 */

static int default_eval_fuel = N * 10000;
static int eval_fuel         = N * 10000;

int checkoverflow(int limit) {
  volatile char c;
  if (low_water_mark == NULL) {
    low_water_mark = &c;
    return 0;
  } else if (low_water_mark - &c >= limit) {
    runerror("recursion too deep");
    return -1; /* not reachable, but the compiler can't tell */
  } else if (eval_fuel-- <= 0) {
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
