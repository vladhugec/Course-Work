#include "all.h"
/*
 * <root.c>=
 */
struct Roots roots = { { NULL, { NULL } }, NULL, NULL };
/*
 * Here are implementations of [[pushreg]] and
 * [[popreg]].
 * <root.c>=
 */
#ifndef DEBUG_GC_REGISTERS    /*OMIT*/
void pushreg(Value *reg) {
    roots.registers = mkRL(reg, roots.registers);
}
/*
 * Popping a register requires a check that the roots
 * match.
 * <root.c>=
 */
void popreg(Value *reg) {
    Registerlist regs = roots.registers;
    assert(regs != NULL);
    assert(reg == regs->hd);
    roots.registers = regs->tl;
    free(regs);
}
#endif /*OMIT*/
/*
 * When pushing and popping a list of registers, we push
 * left to right and pop right to left.
 * <root.c>=
 */
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
