#include "all.h"
/* loc.c 181a */
Value* allocate(Value v) {
    Value *loc = malloc(sizeof(*loc));
    assert(loc != NULL);
    *loc = v;
    return loc;
}
/* loc.c 181b */
void initallocate(Env *globals) {
    (void)globals;
}
