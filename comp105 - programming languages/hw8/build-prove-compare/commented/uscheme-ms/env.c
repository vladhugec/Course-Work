#include "all.h"
/*
 * We look up a name by following [[tl]] pointers. [*]
 * <env.c>=
 */
Value* find(Name name, Env env) {
    for (; env; env = env->tl)
        if (env->name == name)
            return env->loc;
    return NULL;
}
/*
 * <env.c>=
 */
void printenv(Printbuf output, va_list_box *box) {
    char *prefix = " ";

    bprint(output, "{");
    for (Env env = va_arg(box->ap, Env); env; env = env->tl) {
        bprint(output, "%s%n -> %v", prefix, env->name, *env->loc);
        prefix = ", ";
    }
    bprint(output, " }");
}
/*
 * To help support static analysis of micro-Scheme
 * programs, we can dump all the names in an
 * environment.
 * <env.c>=
 */
void dump_env_names(Env env) {
    for ( ; env; env = env->tl)
        fprint(stdout, "%n\n", env->name);
}
/*
 * <env.c>=
 */
Env bindalloc(Name name, Value val, Env env) {
    Env newenv = malloc(sizeof(*newenv));
    assert(newenv != NULL);

    newenv->name = name;
    pushcontext(mkLetxenvStruct(env), roots.stack);
    newenv->loc  = allocate(val);
    popframe(roots.stack);
    newenv->tl   = env;
    return newenv;
}
/*
 * Please also observe that [[val]] is a parameter
 * passed by value, so we have a fresh copy of it. It
 * contains [[Value*]] pointers, so you might think it
 * needs to be on the root stack for the copying
 * collector (so that the pointers can be updated if
 * necessary). But by the time we get to [[allocate]],
 * our copy of [[val]] is dead—only [[allocate]]'s
 * private copy matters.
 */

/*
 * In [[bindalloclist]], by contrast, when we call
 * [[bindalloc]] with [[vs->hd]], our copy of [[vs->hd]]
 * is dead, as is everything that precedes it. But
 * values reachable from [[vs->tl]] are still live.
 * To make them visible to the garbage collector,
 * we treat them as ``machine registers.''
 * <env.c>=
 */
Env bindalloclist(Namelist xs, Valuelist vs, Env env) {
    Valuelist oldvals = vs;
    pushregs(oldvals);
    for (; xs && vs; xs = xs->tl, vs = vs->tl)
        env = bindalloc(xs->hd, vs->hd, env);
    popregs(oldvals);
    return env;
}
