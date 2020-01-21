#include "all.h"
/*
 * I choose a representation of environments that makes
 * it easy to share and extend them: an environment
 * contains a single binding and a pointer to the rest
 * of the bindings in the environment. [*]
 * <env.c>=
 */
struct Env {
    Name name;
    Value *loc;
    Env tl;
};
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
 * Function [[bindalloc]] always creates a new
 * environment with a new binding. There is never any
 * mutation.
 * <env.c>=
 */
Env bindalloc(Name name, Value val, Env env) {
    Env newenv = malloc(sizeof(*newenv));
    assert(newenv != NULL);

    newenv->name = name;
    newenv->loc  = allocate(val);
    newenv->tl   = env;
    return newenv;
}
/*
 * Function [[bindalloclist]] binds names to values in
 * sequence.
 * <env.c>=
 */
Env bindalloclist(Namelist xs, Valuelist vs, Env env) {
    for (; xs && vs; xs = xs->tl, vs = vs->tl)
        env = bindalloc(xs->hd, vs->hd, env);
    assert(xs == NULL && vs == NULL);
    return env;
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
