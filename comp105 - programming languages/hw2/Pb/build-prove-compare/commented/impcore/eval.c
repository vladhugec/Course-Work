#include "all.h"
/*
 * Function [[eval]] is mutually recursive with a
 * private helper function, [[evallist]]: [*]
 * <eval.c>=
 */
static Valuelist evallist(Explist es, Valenv globals, Funenv functions, Valenv
                                                                       formals);
/*
 * Like any other recursive interpreter, [[eval]]
 * implements the operational semantics by examining the
 * inference rules from the bottom up. The first step in
 * evaluating [[e]] is to discover what the syntactic
 * form of [[e]] is, by looking at the tag (the [[alt]]
 * field) in the discriminated union.\iiflabeleval [*]\
 * iimplabeleval
 * <eval.c>=
 */
Value eval(Exp e, Valenv globals, Funenv functions, Valenv formals) {
    checkoverflow(1000000 * sizeof(char *));
                                        // see last section of Appendix A (OMIT)
    switch (e->alt) {
    case LITERAL:
        /*
         * Only one rule has \xliteral in its conclusion. The
         * implementation simply returns the literal value.
         * <evaluate [[e->u.literal]] and return the result>=
         */
        return e->u.literal;
    case VAR:
        /*
         * Two rules have variables in their conclusions. We
         * know which rule to use by checking x in dom rho,
         * which in C is [[isvalbound(e->u.var, formals)]]. If x
         * \notindom rho and x \notindom xi, the operational
         * semantics gets stuck—so the interpreter issues an
         * error message. Less formally, we look up the variable
         * by checking first the local environment and then the
         * global environment.
         * <evaluate [[e->u.var]] and return the result>=
         */
        if (isvalbound(e->u.var, formals))
            return fetchval(e->u.var, formals);
        else if (isvalbound(e->u.var, globals))
            return fetchval(e->u.var, globals);
        else
            runerror("unbound variable %n", e->u.var);
        assert(0);   // not reached, but the compiler can't know
        /*
         * The call to [[runerror]] illustrates the convenience
         * of the extensible printer; I use [[ needing to
         * convert it to a string.
         */

    case SET:
        /*
         * Setting a variable is very similar. Again there are
         * two rules, and again we distinguish by looking at the
         * domain of rho ([[formals]]). Because both rules
         * require the premise \evale ==>\eval[']v, we evaluate
         * the right-hand side first and put the result in 
         * [[v]].
         * <evaluate [[e->u.set]] and return the result>=
         */
        {
            Value v = eval(e->u.set.exp, globals, functions, formals);

            if (isvalbound(e->u.set.name, formals))
                bindval(e->u.set.name, v, formals);
            else if (isvalbound(e->u.set.name, globals))
                bindval(e->u.set.name, v, globals);
            else
                runerror("tried to set unbound variable %n in %e", e->u.set.name
                                                                           , e);
            return v;
        }
    case IFX:
        /*
         * To evaluate [[ifx]], we again have two rules. Both
         * rules have the same first premise: \evale_1 ==>\eval
         * [']v_1. We can find v_1, xi', and rho' by making the
         * recursive call [[eval(e->u.ifx.cond, globals,
         * functions, formals)]]. This call is safe only because
         * the new environments xi' and rho' are used in the
         * third premises of both rules. If xi' and rho' were
         * not always used, we would have had to make copies and
         * pass the copies to the recursive call.
         * 
         * Once we have v_1, testing it against zero tells us
         * which rule to use, and the third premises of both
         * rules require recursive calls to [[eval]]. The
         * expression e_2 is [[e->u.ifx.truex]]; e_3 is [[e->
         * u.ifx.falsex]]. [*]
         * <evaluate [[e->u.ifx]] and return the result>=
         */
        if (eval(e->u.ifx.cond, globals, functions, formals) != 0)
            return eval(e->u.ifx.truex, globals, functions, formals);
        else
            return eval(e->u.ifx.falsex, globals, functions, formals);
    case WHILEX:
        /*
         * <evaluate [[e->u.whilex]] and return the result>=
         */
        while (eval(e->u.whilex.cond, globals, functions, formals) != 0)
            eval(e->u.whilex.exp, globals, functions, formals);
        return 0;
    case BEGIN:
        /*
         * For the [[begin]] expression, I use a [[for]] loop to
         * evaluate all the premises in turn. Local variable
         * [[lastval]] remembers the value of the last
         * expression in the [[begin]]. In the pointless case
         * where the [[begin]] doesn't contain any expressions,
         * I have crafted the operational semantics to match the
         * implementation. [*]
         * <evaluate [[e->u.begin]] and return the result>=
         */
        {
            Value lastval = 0;
            for (Explist es = e->u.begin; es; es = es->tl)
                lastval = eval(es->hd, globals, functions, formals);
            return lastval;
        }
    case APPLY:
        /*
         * There are many rules for applying functions, but I
         * divide them into two classes. One class contains only
         * the rule for user-defined functions; the other class
         * contains the rules for primitives. To apply the
         * function named f, the interpreter looks at the form
         * of phi(f). In the code, f is [[e->u.apply.name]].
         * <evaluate [[e->u.apply]] and return the result>=
         */
        {
            Fun f;
            /*
             * The last two lines of [[eval]] might seem
             * superfluous, but it isn't; it helps protect me,
             * and you, from mistakes. Calling [[assert(0)]] ensures
             * that if I forget a case in the [[switch]],
             * the evaluator will halt with an error message, rather
             * than silently do something unpredictable. Another way
             * to protect yourself from mistakes is to turn on all
             * compiler warnings, which I do; calling [[assert(0)]],
             * which the compiler knows can never return, keeps the
             * C compiler from issuing a warning that [[eval]] might
             * not return.
             * <make [[f]] the function denoted by [[e->u.apply.name]], or call
                                                                  [[runerror]]>=
             */
            if (!isfunbound(e->u.apply.name, functions))
                runerror("call to undefined function %n in %e", e->u.apply.name,
                                                                             e);
            f = fetchfun(e->u.apply.name, functions);
            switch (f.alt) {
            case USERDEF:
                /*
                 * The premises of the \rulenameApplyUser rule require
                 * that the list of formal parameters to f be the same
                 * length as the list of actual parameters in the call.
                 * I let [[xs]] represent the formals \ldotsnx and
                 * [[vs]] represent the actuals v_1, ..., v_m. If the
                 * formals and actuals are the same length, so m=n,
                 * I use [[mkValenv(xs, vs)]] to create a fresh
                 * environment \nomathbreak{x_1 |->v_1, ..., x_n |->v_n
                 * } in which to evaluate the body.
                 * <apply [[f.u.userdef]] and return the result>=
                 */
                {
                    Namelist  xs = f.u.userdef.formals;
                    Valuelist vs = evallist(e->u.apply.actuals, globals,
                                                            functions, formals);
                    checkargc(e, lengthNL(xs), lengthVL(vs));
                    return eval(f.u.userdef.body, globals, functions, mkValenv(
                                                                       xs, vs));
                }
            case PRIMITIVE:
                /*
                 * Impcore has few primitive operators, and they are
                 * simple. We handle [[print]] separately from the
                 * arithmetic primitives. More general techniques for
                 * implementing primitives, which are appropriate for
                 * larger languages, are shown as part of the
                 * implementation of micro-Scheme in \crefpage
                 * scheme.defn-of-Primitive.
                 * <apply [[f.u.primitive]] and return the result>=
                 */
                {
                    Valuelist vs = evallist(e->u.apply.actuals, globals,
                                                            functions, formals);
                    if (f.u.primitive == strtoname("print"))
                        /*
                         * Primitive [[print]] is shown here; [[println]] and
                         * [[printu]] appear in \crefapp:impcore.
                         * <apply \impcore\ primitive [[print]] to [[vs]] and
                                                                        return>=
                         */
                        {
                            checkargc(e, 1, lengthVL(vs));
                            Value v = nthVL(vs, 0);
                            print("%v", v);
                            return v;
                        }
                    else if (f.u.primitive == strtoname("println"))
                        /*
                         * Printing primitives
                         * 
                         * <apply \impcore\ primitive [[println]] to [[vs]] and
                                                                        return>=
                         */
                        {
                            checkargc(e, 1, lengthVL(vs));
                            Value v = nthVL(vs, 0);
                            print("%v\n", v);
                            return v;
                        }
                    else if (f.u.primitive == strtoname("printu"))
                        /*
                         * <apply \impcore\ primitive [[printu]] to [[vs]] and
                                                                        return>=
                         */
                        {
                            checkargc(e, 1, lengthVL(vs));
                            Value v = nthVL(vs, 0);
                            print_utf8(v);
                            return v;
                        }
                    else
                        /*
                         * <apply arithmetic primitive to [[vs]] and return>=
                         */
                        {
                            const char *s = nametostr(f.u.primitive);
                            Value v, w;
                            /*
                             * <check that [[vs]] has exactly two values, and
                                                assign them to [[v]] and [[w]]>=
                             */
                            checkargc(e, 2, lengthVL(vs));
                            v = nthVL(vs, 0);
                            w = nthVL(vs, 1);
                            /*
                             * <if operation [[s]] would overflow on [[v]] and
                                                      [[w]], call [[runerror]]>=
                             */
                            checkarith(s[0], v, w, 32);
                            assert(strlen(s) == 1);

                            switch (s[0]) {
                            case '<':
                                return v < w;
                            case '>':
                                return v > w;
                            case '=':
                                return v == w;
                            case '+':
                                return v + w;
                            case '-':
                                return v - w;
                            case '*':
                                return v * w;
                            case '/':
                                if (w == 0)
                                    runerror("division by zero in %e", e);
                                return v / w;
                            default:
                                assert(0);
                            }
                        }
                        /*
                         * The code also depends on the fact that Impcore shares
                         * C's rules for the values of comparison expressions.
                         * Impcore does not share C's other rules of arithmetic:
                         * in Impcore, if the result of an arithmetic operation
                         * does not fit in the range -2^31 to 2^31, the
                         * operation causes a checked run-time error. Function
                         * [[checkarith]] is defined in \crefapp:cinterps.
                         */

                }
            default:
                assert(0);
            }
        }
    }
    assert(0);
}
/*
 * During unit testing, [[runerror]] operates in testing
 * mode, and it behaves a little differently. The
 * details are in \crefpagecinterps.error.
 */

/*
 * Applying a user-defined function has something in
 * common with [[begin]], because arguments e_1, ...,
 * e_n have to be evaluated. The difference is that
 * [[begin]] keeps only result v_n (in variable [[v]] in
 * chunk [->]), where function evaluation keeps all the
 * result values, to bind into a new environment.
 * To produce values \ldotsnv, I define the auxiliary
 * function [[evallist]], which is given e_1, ..., e_n
 * along with xi_0, phi, and rho_0. It evaluates e_1,
 * ..., e_n in order, and it mutates the environments so
 * that when it is finished, xi= xi_n and rho= rho_n.
 * Finally, [[evallist]] returns the list v_1, ..., v_n.
 * \iimplabelevallist
 * <eval.c>=
 */
static Valuelist evallist(Explist es, Valenv globals, Funenv functions, Valenv
                                                                      formals) {
    if (es == NULL) {
        return NULL;
    } else {
        Value v = eval(es->hd, globals, functions, formals);
        return mkVL(v, evallist(es->tl, globals, functions, formals));
    }
}
/*
 * The rules of Impcore require that [[es->hd]] be
 * evaluated before [[es->tl]]. To ensure the correct
 * order of evaluation, we must call [[eval(es->hd,]]
 *  ...[[)]] and [[evallist(es->tl,]] ...[[)]] in
 * separate C statements. Writing both calls as
 * parameters to [[mkVL]] would be a mistake because
 * C makes no guarantees about the order in which the
 * actual parameters of a function are evaluated.
 */

/*
 * Responsibilty for evaluating definitions is shared
 * between two functions. Function [[readevalprint]]
 * takes as input a stream of definitions. The extended
 * definitions are handled directly in
 * [[readevalprint]]:
 * 
 *   • Each unit test is remembered and later run.
 *   • A file mentioned in [[use]] is converted to a
 *  stream of extended definitions, then passed
 *  recursively to [[readevalprint]].
 * 
 * The true definitions are passed on to [[evaldef]]. 
 * [*]\iiflabelreadevalprint \iimplabelreadevalprint
 * <eval.c>=
 */
void readevalprint(XDefstream xdefs, Valenv globals, Funenv functions, Echo echo
                                                                             ) {
    UnitTestlist pending_unit_tests = NULL;
                                            // to be run when xdefs is exhausted

    for (XDef d = getxdef(xdefs); d; d = getxdef(xdefs))
        switch (d->alt) {
        case TEST:
            pending_unit_tests = mkUL(d->u.test, pending_unit_tests);
            break;
        case USE:
            /*
             * On seeing [[use]], we open the file named by [[use]],
             * build a stream of definitions, and through
             * [[readevalprint]], recursively call [[evaldef]] on
             * all the definitions in that file.[*] When reading
             * definitions via [[use]], the interpreter neither
             * prompts nor echoes.
             * <evaluate [[d->u.use]], possibly mutating [[globals]] and
                                                                 [[functions]]>=
             */
            {
                const char *filename = nametostr(d->u.use);
                FILE *fin = fopen(filename, "r");
                if (fin == NULL)
                    runerror("cannot open file \"%s\"", filename);
                readevalprint(filexdefs(filename, fin, NO_PROMPTS), globals,
                                                               functions, echo);
                fclose(fin);
            }
            /*
             * As noted in Exercise [->], this code can leak open
             * file descriptors.
             */

            break;
        case DEF:
            evaldef(d->u.def, globals, functions, echo);
            break;
        default:
            assert(0);
        }
    reset_overflow_check();     /* OMIT */

    process_tests(pending_unit_tests, globals, functions);
}
/*
 * Function [[process_tests]], defined in \crefpage
 * impcorea.testing, runs the [[pending_unit_tests]] in
 * the order in which they appear in the source code.
 */

/*
 * Just like [[eval]], [[evaldef]] looks at the
 * conclusions of rules, and it discriminates on the
 * syntactic form of [[d]]. [*]\iiflabelevaldef\
 * iimplabelevaldef
 * <eval.c>=
 */
void evaldef(Def d, Valenv globals, Funenv functions, Echo echo) {
    switch (d->alt) {
    case VAL:
        /*
         * A variable definition updates xi. The premise shows
         * we must call [[eval]] to get value v and environment 
         * xi'. When we call [[eval]], we must use an empty
         * environment as rho. The rule says the new
         * environment xi' is retained, and the value of the
         * expression, v, is bound to x in it. The
         * implementation may also print v.
         * <evaluate [[d->u.val]], mutating [[globals]]>=
         */
        {
            Value v = eval(d->u.val.exp, globals, functions, mkValenv(NULL, NULL
                                                                             ));
            bindval(d->u.val.name, v, globals);
            if (echo == ECHOES)
                print("%v\n", v);
        }
        return;
    case EXP:
        /*
         * Evaluating a top-level expression is just like
         * evaluating a definition of [[it]].
         * <evaluate [[d->u.exp]] and possibly print the result>=
         */
        {
            Value v = eval(d->u.exp, globals, functions, mkValenv(NULL, NULL));
            bindval(strtoname("it"), v, globals);
            if (echo == ECHOES)
                print("%v\n", v);
        }
        return;
    case DEFINE:
        /*
         * A function definition updates phi. Our implementation
         * also prints the name of the function.
         * <evaluate [[d->u.define]], mutating [[functions]]>=
         */
        bindfun(d->u.define.name, mkUserdef(d->u.define.userfun), functions);
        if (echo == ECHOES)
            print("%n\n", d->u.define.name);
        /*
         * The evaluator does not check to see that the \ldotsnx
         * are all distinct—the x_i's are checked when the
         * definition is parsed, by function
         * [[check_def_duplicates]] in \chunkref
         * cparse.chunk.check-def-duplicates.
         */

        return;
    }
    assert(0);
}
