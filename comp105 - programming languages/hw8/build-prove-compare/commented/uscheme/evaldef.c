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
         * According to the operational semantics, the
         * right-hand side of a [[val]] binding must be
         * evaluated in an environment in which the name [[d->
         * u.val.name]] is bound. If the binding is not already
         * present, we bind the name to an unspecified value.
         * <evaluate [[val]] binding and return new environment>=
         */
        {
            if (find(d->u.val.name, env) == NULL)
                env = bindalloc(d->u.val.name, unspecified(), env);
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
 * Function [[readevalprint]] evaluates definitions,
 * updates the environment [[*envp]], and remembers unit
 * tests. After all definitions have been read, it runs
 * the remembered unit tests. The last test added to
 * [[unit_tests]] is the one at the front of the list,
 * but we want to run tests in the order in which they
 * appear, so the tests are run back to front. \
 * scmflabelreadevalprint [*]
 * <evaldef.c>=
 */
void readevalprint(XDefstream xdefs, Env *envp, Echo echo) {
    UnitTestlist pending_unit_tests = NULL;

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
            pending_unit_tests = mkUL(d->u.test, pending_unit_tests);
            break;
        default:
            assert(0);
        }

    process_tests(pending_unit_tests, *envp);
}
