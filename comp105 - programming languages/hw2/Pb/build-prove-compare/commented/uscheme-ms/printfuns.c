#include "all.h"
/*
 * The printer for names prints a name's string. A 
 * [[Name]] should never be [[NULL]], but if something
 * goes drastically wrong and a [[NULL]] pointer is
 * printed as a name, the code won't crash.
 * <printfuns.c>=
 */
void printname(Printbuf output, va_list_box *box) {
    Name np = va_arg(box->ap, Name);
    bufputs(output, np == NULL ? "<null>" : nametostr(np));
}
/*
 * <printfuns.c>=
 */
void printchar(Printbuf output, va_list_box *box) {
    int c = va_arg(box->ap, int);
    bufput(output, c);
}
/*
 * <printfuns.c>=
 */
void printpar(Printbuf output, va_list_box *box) {
    Par p = va_arg(box->ap, Par);
    if (p == NULL) {
        bprint(output, "<null>");
        return;
    }

    switch (p->alt){
    case ATOM:
        bprint(output, "%n", p->u.atom);
        break;
    case LIST:
        bprint(output, "(%P)", p->u.list);
        break;
    }
}
/*
 * The rest of the code deals with printing—a complex
 * and unpleasant task.
 * 
 * Printing and values
 * 
 * The printing code is lengthy and tedious. The length
 * and tedium are all about printing closures. When
 * printing a closure nicely, you don't want to see the
 * entire environment that is captured in the closure.
 * You want to see only the parts of the environment
 * that the closure actually depends on—the free
 * variables of the [[lambda]] expression.
 * 
 * Finding free variables in an expression
 * 
 * Finding free variables is hard work. I start with a
 * bunch of utility functions on names. Function
 * [[nameinlist]] says whether a particular [[Name]] is
 * on a [[Namelist]].
 * <printfuns.c>=
 */
static bool nameinlist(Name n, Namelist xs) {
    for (; xs; xs=xs->tl)
        if (n == xs->hd)
            return true;
    return false;
}
/*
 * Function [[addname]] adds a name to a list, unless
 * it's already there.
 * <printfuns.c>=
 */
static Namelist addname(Name n, Namelist xs) {
    if (nameinlist(n, xs))
        return xs;
    else
        return mkNL(n, xs);
}
/*
 * Function [[freevars]] is passed an expression, a list
 * of variables known to be bound, and a list of
 * variables known to be free. If the expression
 * contains free variables not on either list,
 * [[freevars]] adds them to the free list and returns
 * the new free list. Function [[freevars]] works by
 * traversing an abstract-syntax tree; when it finds a
 * name, it calls [[addfree]] to calculate the new list
 * of free variables
 * <printfuns.c>=
 */
static Namelist addfree(Name n, Namelist bound, Namelist free) {
    if (nameinlist(n, bound))
        return free;
    else
        return addname(n, free);
}
/*
 * Here's the tree traversal. Computing the free
 * variables of an expression is as much work as
 * evaluating the expression. We have to know all the
 * rules for environments.
 * <printfuns.c>=
 */
Namelist freevars(Exp e, Namelist bound, Namelist free) {
    switch (e->alt) {
    case LITERAL:
        break;
    case VAR:
        free = addfree(e->u.var, bound, free);
        break;
    case IFX:
        free = freevars(e->u.ifx.cond, bound, free);
        free = freevars(e->u.ifx.truex, bound, free);
        free = freevars(e->u.ifx.falsex, bound, free);
        break;
    case WHILEX:
        free = freevars(e->u.whilex.cond, bound, free);
        free = freevars(e->u.whilex.body, bound, free);
        break;
    case BEGIN:
        for (Explist es = e->u.begin; es; es = es->tl)
            free = freevars(es->hd, bound, free);
        break;
    case SET:
        free = addfree(e->u.set.name, bound, free);
        free = freevars(e->u.set.exp, bound, free);
        break;
    case APPLY:
        free = freevars(e->u.apply.fn, bound, free);
        for (Explist es = e->u.apply.actuals; es; es = es->tl)
            free = freevars(es->hd, bound, free);
        break;
    case LAMBDAX:
        /*
         * The case for lambda expressions is the interesting
         * one. Any variables that are bound by the [[lambda]]
         * are added to the ``known bound'' list for the
         * recursive examination of the [[lambda]]'s body.
         * <let [[free]] be the free variables for [[e->u.lambdax]]>=
         */
        for (Namelist xs = e->u.lambdax.formals; xs; xs = xs->tl)
            bound = addname(xs->hd, bound);
        free = freevars(e->u.lambdax.body, bound, free);
        break;
    case LETX:
        /*
         * The let expressions are a bit tricky; we have to
         * follow the rules exactly.
         * <let [[free]] be the free variables for [[e->u.letx]]>=
         */
        switch (e->u.letx.let) {
            Namelist xs;   // used to visit every bound name
            Explist  es;   // used to visit every expression that is bound
        case LET:
            for (es = e->u.letx.es; es; es = es->tl)
                free = freevars(es->hd, bound, free);
            for (xs = e->u.letx.xs; xs; xs = xs->tl)
                bound = addname(xs->hd, bound);
            free = freevars(e->u.letx.body, bound, free);
            break;
        case LETSTAR:
            for (xs = e->u.letx.xs, es = e->u.letx.es
               ; xs && es
               ; xs = xs->tl, es = es->tl
               ) 
            {
                free  = freevars(es->hd, bound, free);
                bound = addname(xs->hd, bound);
            }
            free = freevars(e->u.letx.body, bound, free);
            break;
        case LETREC:
            for (xs = e->u.letx.xs; xs; xs = xs->tl)
                bound = addname(xs->hd, bound);
            for (es = e->u.letx.es; es; es = es->tl)
                free = freevars(es->hd, bound, free);
            free = freevars(e->u.letx.body, bound, free);
            break;
        }
        break;
    /*
     * <extra cases for finding free variables in {\uscheme} expressions>=
     */
    /*
     * Supporting code for \titleuschemeplus
     * 
     * [*] [*] \invisiblelocaltableofcontents[*]
     * 
     * The stack of evaluation contexts
     * 
     * This section shows the implementation of the
     * [[Stack]] of evaluation contexts and its
     * instrumentation.
     * 
     * Implementing the stack
     * 
     * In \chaprefschemes, the representation of a [[Stack]]
     * is private to this module. In \chaprefgcs, the
     * representation is exposed to the garbage collector.
     */

    /*
     * Finding free variables
     * 
     * Here are extra cases for the [[freevars]] function,
     * which is used to do a good job printing closures.
     * <extra cases for finding free variables in {\uscheme} expressions>=
     */
    case BREAKX:
        break;
    case CONTINUEX:
        break;
    case RETURNX:
        free = freevars(e->u.returnx, bound, free);
        break;
    case THROW:
        free = freevars(e->u.throw, bound, free);
        break;
    case TRY_CATCH:
        free = freevars(e->u.try_catch.body, bound, free);
        free = freevars(e->u.try_catch.handler, bound, free);
        break;
    /*
     * These forms appear only in contexts, and we have no
     * business looking for a free variable.
     * <extra cases for finding free variables in {\uscheme} expressions>=
     */
    case HOLE:
    case CALLENV:
    case LETXENV:
    case WHILE_RUNNING_BODY:
        assert(0);
        break;
    }
    return free;
}
/*
 * A recursive function is represented by a closure
 * whose environment includes a pointer back to the
 * recursive function itself. If we print such a closure
 * by printing the values of the free variables, the
 * printer could loop forever. The [[depth]] parameter
 * cuts off this loop, so when [[depth]] reaches 0, the
 * printing functions print closures simply as
 * [[<procedure>]].
 * <printfuns.c>=
 */
static void printnonglobals(Printbuf output, Namelist xs, Env env, int depth);

static void printclosureat(Printbuf output, Lambda lambda, Env env, int depth) {
    if (depth > 0) {
        Namelist vars = freevars(lambda.body, lambda.formals, NULL);
        bprint(output, "<%\\, {", lambda);
        printnonglobals(output, vars, env, depth - 1);
        bprint(output, "}>");
    } else {
        bprint(output, "<procedure>");
    }
}
/*
 * The value-printing functions also need a [[depth]]
 * parameter.
 * <printfuns.c>=
 */
static void printvalueat(Printbuf output, Value v, int depth);
/*
 * Function [[printtail]] handles the correct printing
 * of lists. If a cons cell doesn't have another cons
 * cell or [[NIL]] in its [[cdr]] field, the [[car]] and
 * [[cdr]] are separated by a dot. [*]
 * <helper functions for [[printvalue]]>=
 */
static void printtail(Printbuf output, Value v, int depth) {
    switch (v.alt) {
    case NIL:
        bprint(output, ")");
        break;
    case PAIR:
        bprint(output, " ");
        printvalueat(output, *v.u.pair.car, depth);
        printtail(output, *v.u.pair.cdr, depth);
        break;
    default:
        bprint(output, " . ");
        printvalueat(output, v, depth);
        bprint(output, ")");
        break;
    }
}
static void printvalueat(Printbuf output, Value v, int depth) {
    switch (v.alt){
    case NIL:
        bprint(output, "()");
        return;
    case BOOLV:
        bprint(output, v.u.boolv ? "#t" : "#f");
        return;
    case NUM:
        bprint(output, "%d", v.u.num);
        return;
    case SYM:
        bprint(output, "%n", v.u.sym);
        return;
    case PRIMITIVE:
        bprint(output, "<procedure>");
        return;
    case PAIR:
        bprint(output, "(");
        if (v.u.pair.car == NULL) bprint(output, "<NULL>"); else  // OMIT
        printvalueat(output, *v.u.pair.car, depth);
        if (v.u.pair.cdr == NULL) bprint(output, " <NULL>)"); else // OMIT
        printtail(output, *v.u.pair.cdr, depth);
        return;
    case CLOSURE:
        printclosureat(output, v.u.closure.lambda, v.u.closure.env, depth);
        return;
    default:
        bprint(output, "<unknown v.alt=%d>", v.alt);
        return;
    }
}
/*
 * If you ask just to print a value, the default depth
 * is 0. That is, by default the interpreter doesn't
 * print closures. If you need to debug, increase the
 * default depth.
 * <printfuns.c>=
 */
void printvalue(Printbuf output, va_list_box *box) {
    printvalueat(output, va_arg(box->ap, Value), 0);
}
/*
 * Finally, the implementation of [[printnonglobals]].
 * <printfuns.c>=
 */
Env *globalenv;
static void printnonglobals(Printbuf output, Namelist xs, Env env, int depth) {
    char *prefix = "";
    for (; xs; xs = xs->tl) {
        Value *loc = find(xs->hd, env);
        if (loc && (globalenv == NULL || find(xs->hd, *globalenv) != loc)) {
            bprint(output, "%s%n -> ", prefix, xs->hd);
            prefix = ", ";
            printvalueat(output, *loc, depth);
        }
    }
}
/*
 * Print functions for expressions
 * 
 * Here is the (boring) code that prints abstract-syntax
 * trees.
 * <printfuns.c>=
 */
void printdef(Printbuf output, va_list_box *box) {
    Def d = va_arg(box->ap, Def);
    if (d == NULL) {
        bprint(output, "<null>");
        return;
    }

    switch (d->alt) {
    case VAL:
        bprint(output, "(val %n %e)", d->u.val.name, d->u.val.exp);
        return;
    case EXP:
        bprint(output, "%e", d->u.exp);
        return;
    case DEFINE:
        bprint(output, "(define %n %\\)", d->u.define.name, d->u.define.lambda);
        return;
    case DEFS:
                                                                        /*OMIT*/
        for (Deflist ds = d->u.defs; ds; ds = ds->tl)
                                                                        /*OMIT*/
            bprint(output, "%t%s", ds->hd, ds->tl != NULL ? "\n" : "");
                                                                        /*OMIT*/
        return;
                                                                        /*OMIT*/
    }
    assert(0);
}
/*
 * <printfuns.c>=
 */
void printxdef(Printbuf output, va_list_box *box) {
    XDef d = va_arg(box->ap, XDef);
    if (d == NULL) {
        bprint(output, "<null>");
        return;
    }

    switch (d->alt) {
    case USE:
        bprint(output, "(use %n)", d->u.use);
        return;
    case TEST:
        bprint(output, "CANNOT PRINT UNIT TEST XXX\n");
        return;
    case DEF:
        bprint(output, "%t", d->u.def);
        return;
    }
    assert(0);
}
/*
 * <printfuns.c>=
 */
static void printlet(Printbuf output, Exp let) {
    switch (let->u.letx.let) {
    case LET:
        bprint(output, "(let (");
        break;
    case LETSTAR:
        bprint(output, "(let* (");
        break;
    case LETREC:
        bprint(output, "(letrec (");
        break;
    default:
        assert(0);
    }
    Namelist xs;  // visits every let-bound name
    Explist es;   // visits every bound expression
    for (xs = let->u.letx.xs, es = let->u.letx.es; 
         xs && es;
         xs = xs->tl, es = es->tl)
        bprint(output, "(%n %e)%s", xs->hd, es->hd, xs->tl?" ":"");
    bprint(output, ") %e)", let->u.letx.body);
}   
/*
 * <printfuns.c>=
 */
void printexp(Printbuf output, va_list_box *box) {
    Exp e = va_arg(box->ap, Exp);
    if (e == NULL) {
        bprint(output, "<null>");
        return;
    }

    switch (e->alt) {
    case LITERAL:
        if (e->u.literal.alt == NUM || e->u.literal.alt == BOOLV)
            bprint(output, "%v", e->u.literal);
        else
            bprint(output, "'%v", e->u.literal);
        break;
    case VAR:
        bprint(output, "%n", e->u.var);
        break;
    case IFX:
        bprint(output, "(if %e %e %e)", e->u.ifx.cond, e->u.ifx.truex, e->
                                                                  u.ifx.falsex);
        break;
    case WHILEX:
        bprint(output, "(while %e %e)", e->u.whilex.cond, e->u.whilex.body);
        break;
    case BEGIN:
        bprint(output, "(begin%s%E)", e->u.begin ? " " : "", e->u.begin);
        break;
    case SET:
        bprint(output, "(set %n %e)", e->u.set.name, e->u.set.exp);
        break;
    case LETX:
        printlet(output, e);
        break;
    case LAMBDAX:
        bprint(output, "%\\", e->u.lambdax);
        break;
    case APPLY:
        bprint(output, "(%e%s%E)", e->u.apply.fn,
              e->u.apply.actuals ? " " : "", e->u.apply.actuals);
        break;
    /*
     * Support for \titleuschemeplus
     * 
     * These empty definitions are placeholders for code
     * that implements parts of \uschemeplus, an extension
     * that adds control operators to micro-Scheme. \
     * uschemeplus is the topic of \crefschemes.chap.
     * <extra cases for printing {\uscheme} ASTs>=
     */
    /*
     * <extra cases for printing {\uscheme} ASTs>=
     */
    case BREAKX:
        bprint(output, "(break)");
        break;
    case CONTINUEX:
        bprint(output, "(continue)");
        break;
    case RETURNX:
        bprint(output, "(return %e)", e->u.returnx);
        break;
    case THROW:
        bprint(output, "(throw %e)", e->u.throw);
        break;
    case TRY_CATCH:
        bprint(output, "(try-catch %e %e)", e->u.try_catch.body, e->
                                                           u.try_catch.handler);
        break;
    case HOLE:
        bprint(output, "<*>");
        break;
    case LETXENV:
        fprintf(stderr, "Restore let environment %p", (void*)e->u.letxenv);
        break;
    case CALLENV:
        fprintf(stderr, "Restore caller's environment %p", (void*)e->u.callenv);
        break;
    case WHILE_RUNNING_BODY:
        bprint(output, "(while-running-body %e %e)", e->u.whilex.cond, e->
                                                                 u.whilex.body);
        break;
    default:
        assert(0);
    }
}
/*
 * <printfuns.c>=
 */
void printlambda(Printbuf output, va_list_box *box) {
    Lambda l = va_arg(box->ap, Lambda);
    bprint(output, "(lambda (%N) %e)", l.formals, l.body);
}
