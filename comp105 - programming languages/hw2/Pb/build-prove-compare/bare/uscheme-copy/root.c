#include "all.h"
/* root.c 1225c */
struct Roots roots = { { NULL, { NULL } }, NULL, NULL };
/* root.c 1225d */
#ifndef DEBUG_GC_REGISTERS    /*OMIT*/
void pushreg(Value *reg) {
    roots.registers = mkRL(reg, roots.registers);
}
/* root.c 1225e */
void popreg(Value *reg) {
    Registerlist regs = roots.registers;
    assert(regs != NULL);
    assert(reg == regs->hd);
    roots.registers = regs->tl;
    free(regs);
}
#endif /*OMIT*/
/* root.c 1225f */
void pushregs(Valuelist regs) {
    for (; regs; regs = regs->tl)
        pushreg(&regs->hd);
}

void popregs (Valuelist regs) {
    if (regs != NULL) {
        popregs(regs->tl);
        popreg(&regs->hd);
    }
}
