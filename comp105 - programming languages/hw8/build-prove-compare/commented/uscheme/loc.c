#include "all.h"
/*
 * Implementation of memory allocation
 * 
 * To allocate a new location, we call [[malloc]].
 * Chapter [->] describes a much more interesting and
 * efficient implementation of [[allocate]]. [*]
 * <loc.c>=
 */
Value* allocate(Value v) {
    Value *loc = malloc(sizeof(*loc));
    assert(loc != NULL);
    *loc = v;
    return loc;
}
/*
 * To use [[malloc]] requires no special initialization
 * or resetting.
 * <loc.c>=
 */
void initallocate(Env *globals) {
    (void)globals;
}
