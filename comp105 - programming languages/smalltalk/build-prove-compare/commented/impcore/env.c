#include "all.h"
/*
 * I represent an environment as two parallel lists: one
 * holding names and one holding values. The
 * representation's key invariant is that the lists have
 * the same length. \iimplabelValenv
 * <env.c>=
 */
struct Valenv {
    Namelist  xs;
    Valuelist vs;
    // invariant: lists have the same length
};
/*
 * Given the representation, creating an environment is
 * simple. To prevent the invariant from being violated,
 * I assert that [[xs]] and [[vs]] have equal length.
 * <env.c>=
 */
Valenv mkValenv(Namelist xs, Valuelist vs) {
    Valenv e = malloc(sizeof(*e));
    assert(e != NULL);
    assert(lengthNL(xs) == lengthVL(vs));
    e->xs = xs;
    e->vs = vs;
    return e;
}
/*
 * <env.c>=
 */
static Value* findval(Name x, Valenv env) {
    Namelist  xs;
    Valuelist vs;

    for (xs=env->xs, vs=env->vs; xs && vs; xs=xs->tl, vs=vs->tl)
        if (x == xs->hd)
            return &vs->hd;
    return NULL;
}
/*
 * <env.c>=
 */
bool isvalbound(Name name, Valenv env) {
    return findval(name, env) != NULL;
}
/*
 * We fetch a value through the pointer returned by
 * [[findval]], if any.
 * <env.c>=
 */
Value fetchval(Name name, Valenv env) {
    Value *vp = findval(name, env);
    assert(vp != NULL);
    return *vp;
}
/*
 * You might think that to add a new binding to an
 * environment, we would have to insert a new name and
 * value at the beginning of the lists. But I can get
 * away with an optimization. If x in dom rho, instead
 * of extending rho by making rho{x |->v}, I overwrite
 * the old binding of x. This optimization is safe only
 * because no program written in Impcore can tell that
 * it is there. Proving that the optimization is safe
 * requires reasoning about the rules of the operational
 * semantics, which show that in any context where rho{x
 * |->v} appears, there is no way to get to the old rho
 * (x). (See \impexpagesafe-overwrite.) [*]
 * <env.c>=
 */
void bindval(Name name, Value val, Valenv env) {
    Value *vp = findval(name, env);
    if (vp != NULL)
        *vp = val;              // safe optimization
    else {
        env->xs = mkNL(name, env->xs);
        env->vs = mkVL(val,  env->vs);
    }
}
/*
 * Implementation of function environments
 * 
 * This code is continued from Chapter [->], which gives
 * the implementation of value environments. Except for
 * types, the code is identical to code in \crefpage
 * impcore.Valenv.imp. \iimplabelFunenv
 * <env.c>=
 */
struct Funenv {
    Namelist xs;
    Funlist funs;
    // invariant: both lists are the same length
};
/*
 * <env.c>=
 */
Funenv mkFunenv(Namelist xs, Funlist funs) {
    Funenv env = malloc(sizeof *env);
    assert(env != NULL);
    assert(lengthNL(xs) == lengthFL(funs));
    env->xs = xs;
    env->funs = funs;
    return env;
}
/*
 * <env.c>=
 */
static Fun* findfun(Name name, Funenv env) {
    Namelist xs  = env->xs;
    Funlist funs = env->funs;

    for ( ; xs && funs; xs = xs->tl, funs = funs->tl)
        if (name == xs->hd)
            return &funs->hd;
    return NULL;
}
/*
 * <env.c>=
 */
bool isfunbound(Name name, Funenv env) {
    return findfun(name, env) != NULL;
}
/*
 * <env.c>=
 */
Fun fetchfun(Name name, Funenv env) {
    Fun *fp = findfun(name, env);
    assert(fp != NULL);
    return *fp;
}
/*
 * <env.c>=
 */
void bindfun(Name name, Fun fun, Funenv env) {
    Fun *fp = findfun(name, env);
    if (fp != NULL)
        *fp = fun;              // safe optimization
    else {
        env->xs   = mkNL(name, env->xs);
        env->funs = mkFL(fun,  env->funs);
    }
}
/*
 * <env.c>=
 */
void dump_fenv_names(Funenv env) {
    Namelist xs;
    if (env)
        for (xs = env->xs; xs; xs = xs->tl)
            print("%n\n", xs->hd);
}
