#include "all.h"
/*
 * Implementation of the evaluator
 * 
 * [*]
 * <eval.c>=
 */
/*
 * As in Impcore's interpreter, [[evallist]] evaluates a
 * list of arguments in turn, returning a list of
 * values.
 * <eval.c declarations>=
 */
static Valuelist evallist(Explist es, Env env);
/*
 * As in Impcore, the evaluator is still a [[switch]]:\
 * scmflabeleval
 * <eval.c>=
 */
Value eval(Exp e, Env env) {
    checkoverflow(1000000 * sizeof(char *)); // OMIT
    switch (e->alt) {
    case LITERAL:
        /*
         * <evaluate [[e->u.literal]] and return the result>=
         */
        return e->u.literal;
    case VAR:   
        /*
         * Variables
         * 
         * Variable lookup and assignment are simpler than in
         * Impcore, because we have only one rule each. We
         * implement rho(x) by find(x, rho), we implement sigma(
         * \aloc) by [[*]]\aloc, and we update sigma(\aloc) by
         * assigning to [[*]]\aloc. [*]
         * <evaluate [[e->u.var]] and return the result>=
         */
        if (find(e->u.var, env) == NULL)
            runerror("name %n not found", e->u.var);
        return *find(e->u.var, env);
    case SET:
        /*
         * [*] [*]
         * <evaluate [[e->u.set]] and return the result>=
         */
        if (find(e->u.set.name, env) == NULL)
            runerror("set unbound variable %n in %e", e->u.set.name, e);
        return *find(e->u.set.name, env) = eval(e->u.set.exp, env);
    case IFX:
        /*
         * Conditional, iteration, and sequence
         * 
         * The implementations of the control-flow operations
         * are very much as in Impcore. We don't bother
         * repeating the operational semantics.
         * <evaluate [[e->u.ifx]] and return the result>=
         */
        if (istrue(eval(e->u.ifx.cond, env)))
            return eval(e->u.ifx.truex, env);
        else
            return eval(e->u.ifx.falsex, env);
    case WHILEX:
        /*
         * <evaluate [[e->u.whilex]] and return the result>=
         */
        while (istrue(eval(e->u.whilex.cond, env)))
            eval(e->u.whilex.body, env);
        return falsev;
    case BEGIN:
        /*
         * <evaluate [[e->u.begin]] and return the result>=
         */
        {
            Value lastval = falsev;
            for (Explist es = e->u.begin; es; es = es->tl)
                lastval = eval(es->hd, env);
            return lastval;
        }
    case APPLY:
        /*
         * We handle application of primitives separately from
         * application of closures.
         * 
         * <evaluate [[e->u.apply]] and return the result>=
         */
        {
            Value     f  = eval    (e->u.apply.fn,      env);
            Valuelist vs = evallist(e->u.apply.actuals, env);

            switch (f.alt) {
            case PRIMITIVE:
                /*
                 * Applying a primitive is simpler than in our Impcore
                 * interpreter because we represent a primitive by a
                 * pair containing a function pointer and a tag. The tag
                 * is passed to the function, along with the arguments 
                 * ([[vs]]), plus the abstract syntax [[e]], which is
                 * used in error messages.
                 * <apply [[f.u.primitive]] to [[vs]] and return the result>=
                 */
                return f.u.primitive.function(e, f.u.primitive.tag, vs);
            case CLOSURE:
                /*
                 * To apply a closure, we extend the closure's
                 * environment (rho_c in the operational semantics) with
                 * the bindings for the formal variables and then
                 * evaluate the body in that environment. [*]
                 * <apply [[f.u.closure]] to [[vs]] and return the result>=
                 */
                {
                    Namelist xs = f.u.closure.lambda.formals;
                    checkargc(e, lengthNL(xs), lengthVL(vs));
                    return eval(f.u.closure.lambda.body,
                                bindalloclist(xs, vs, f.u.closure.env));
                }
            default:
                runerror("%e evaluates to non-function %v in %e", e->u.apply.fn,
                                                                          f, e);
            }
        }
    case LETX:
        /*
         * Let, let*, and letrec
         * 
         * Each expression in the [[let]] family uses its
         * internal names and expressions to create a new
         * environment, then evaluates the body in that
         * environment. The rules for creating the environment
         * depend on the keyword.
         * <evaluate [[e->u.letx]] and return the result>=
         */
        switch (e->u.letx.let) {
        case LET:
            /*
             * A \xlet expression evaluates the expressions to be
             * bound, then binds them all at once. The functions
             * [[evallist]] and [[bindalloclist]] do all the work.
             * <extend [[env]] by simultaneously binding [[es]] to [[xs]]>=
             */
            env = bindalloclist(e->u.letx.xs, evallist(e->u.letx.es, env), env);
            break;
        case LETSTAR:
            /*
             * A \xletstar expression binds a new name as each
             * expression is evaluated.
             * 
             * <extend [[env]] by sequentially binding [[es]] to [[xs]]>=
             */
            {
                Namelist xs;
                Explist es;

                for (xs = e->u.letx.xs, es = e->u.letx.es;
                     xs && es;
                     xs = xs->tl, es = es->tl)
                    env = bindalloc(xs->hd, eval(es->hd, env), env);
                assert(xs == NULL && es == NULL);
            }
            break;
        case LETREC:
            /*
             * Finally, \xletrec must bind each name to a location
             * before evaluating any of the expressions. The initial
             * contents of the new locations are unspecified. To be
             * faithful to the semantics, we compute all the values
             * before storing any of them.
             * <extend [[env]] by recursively binding [[es]] to [[xs]]>=
             */
            {
                Namelist xs;

                for (xs = e->u.letx.xs; xs; xs = xs->tl)    
                    env = bindalloc(xs->hd, unspecified(), env);
                Valuelist vs = evallist(e->u.letx.es, env);
                for (xs = e->u.letx.xs;
                     xs && vs;
                     xs = xs->tl, vs = vs->tl)
                    *find(xs->hd, env) = vs->hd;
                assert(xs == NULL && vs == NULL);
            }
            break;
        default:
            assert(0);
        }
        return eval(e->u.letx.body, env);
        /*
         * As with [[lambda]], all checking for distinct names
         * is done at parse time.
         */

    case LAMBDAX:
        /*
         * Closures and function application
         * 
         * Wrapping a closure is simple:
         * <evaluate [[e->u.lambdax]] and return the result>=
         */
        return mkClosure(e->u.lambdax, env);
        /*
         * Because function [[check_exp_duplicates]] in \
         * chunkrefschemea.chunk.check_exp_duplicates ensures
         * that the \ldotsnx are distinct at the time [[e]] is
         * parsed, that check needn't be repeated here.
         * (Checking at parse time is an advantage because you
         * find out about the problem right away, without having
         * to wait for your bad code to be evaluated.)
         */

    }
    assert(0);
}
/*
 * <eval.c>=
 */
static Valuelist evallist(Explist es, Env env) {
    if (es == NULL) {
        return NULL;
    } else {
        Value v = eval(es->hd, env);   // enforce uScheme's order of evaluation
        return mkVL(v, evallist(es->tl, env));
    }
}
