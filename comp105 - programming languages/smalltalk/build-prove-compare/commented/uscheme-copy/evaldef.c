#include "all.h"
/*
 * Evaluating definitions
 * 
 * The function [[evaldef]] evaluates a definition,
 * updates the store, and returns a new environment. If
 * [[echo]] is nonzero, [[evaldef]] also prints.\
 * scmflabelevaldef
 * <evaldef.c>=
 */
Env evaldef(Def d, Env env, Echo echo) {
    switch (d->alt) {
    case VAL:
        /*
         * <evaluate [[val]] binding and return new environment>=
         */
        {
            pushcontext(*d->u.val.exp, roots.stack);
            if (find(d->u.val.name, env) == NULL)
                env = bindalloc(d->u.val.name, unspecified(), env);
            *d->u.val.exp = topframe(roots.stack)->context;
            popframe(roots.stack);
            Value v = eval(d->u.val.exp, env);
            *find(d->u.val.name, env) = v;
            if (echo == ECHOES) {
                if (d->u.val.exp->alt == LAMBDAX)
                    print("%n\n", d->u.val.name);
                else
                    print("%v\n", v);
            }
            return env;
        }
    case EXP:
        /*
         * As in Impcore, evaluating a top-level expression has
         * the same effect on the environment as evaluating a
         * definition of [[it]], except that the interpreter
         * always prints the value, never the name ``it.''
         * <evaluate expression, store the result in [[it]], and return new
                                                                   environment>=
         */
        {
            Value v = eval(d->u.exp, env);
            Value *itloc = find(strtoname("it"), env);
            if (echo == ECHOES)
                print("%v\n", v);
            if (itloc == NULL) {
                return bindalloc(strtoname("it"), v, env);
            } else {
                *itloc = v;
                return env;
            }
        }
    case DEFINE:
        /*
         * We rewrite \xdefine to \xval.
         * <evaluate function definition and return new environment>=
         */
        return evaldef(mkVal(d->u.define.name, mkLambdax(d->u.define.lambda)),
                       env, echo);
    case DEFS:                                                     /*OMIT*/
        for (Deflist ds = d->u.defs; ds != NULL; ds = ds->tl)      /*OMIT*/
            env = evaldef(ds->hd, env, echo);                      /*OMIT*/
        return env;                                                /*OMIT*/
    }
    assert(0);
    return NULL;
}
/*
 * <evaldef.c>=
 */
void readevalprint(XDefstream xdefs, Env *envp, Echo echo) {
    roots.globals.internal.pending_tests =
                              mkULL(NULL, roots.globals.internal.pending_tests);
    roots.registers = NULL;  // clean up after syntax error

    for (XDef d = getxdef(xdefs); d; d = getxdef(xdefs))
        switch (d->alt) {
        case DEF:
            *envp = evaldef(d->u.def, *envp, echo);
            break;
        case USE:
            /*
             * Reading a file is as in Impcore, except that again
             * we cannot mutate an environment, so we mutate
             * [[*envp]] instead. When [[readevalprint]] calls
             * itself recursively to read a file, it passes the same
             * [[envp]] it was given.
             * <read in a file and update [[*envp]]>=
             */
            {
                const char *filename = nametostr(d->u.use);
                FILE *fin = fopen(filename, "r");
                if (fin == NULL)
                    runerror("cannot open file \"%s\"", filename);
                readevalprint(filexdefs(filename, fin, NO_PROMPTS), envp, echo);
                fclose(fin);
            }
            break;
        case TEST:
            roots.globals.internal.pending_tests->hd =
                  mkUL(d->u.test, roots.globals.internal.pending_tests->hd);
            break;
        default:
            assert(0);
        }

    process_tests(roots.globals.internal.pending_tests->hd, *envp);
    roots.globals.internal.pending_tests = popULL(
                                          roots.globals.internal.pending_tests);
}
/*
 * In the [[DEF]] case, as alluded to on \cpageref
 * scheme.repl.envp, the assignment to [[*envp]] ensures
 * that after a successful call to [[evaldef]], the new
 * environment is remembered, even if a later call to
 * [[evaldef]] exits the loop by calling [[runerror]].
 * This code is more complicated than the analogous code
 * in Impcore: Impcore's [[readevalprint]] simply
 * mutates the global environment. In micro-Scheme,
 * environments are not mutable, so we mutate
 * a C location instead.
 */

