#include "all.h"
/*
 * Options and diagnostic code
 * 
 * <options.c>=
 */
Value getoption(Name name, Env env, Value defaultval) {
    Value *p = find(name, env);
    if (p)
        return *p;
    else
        return defaultval;
}
