#include "all.h"
/* printfuns.c 1065b */
void printname(Printbuf output, va_list_box *box) {
    Name np = va_arg(box->ap, Name);
    bufputs(output, np == NULL ? "<null>" : nametostr(np));
}
/* printfuns.c 1065c */
void printchar(Printbuf output, va_list_box *box) {
    int c = va_arg(box->ap, int);
    bufput(output, c);
}
/* printfuns.c 1065e */
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
/* printfuns.c 1192a */
static bool nameinlist(Name n, Namelist xs) {
    for (; xs; xs=xs->tl)
        if (n == xs->hd)
            return true;
    return false;
}
/* printfuns.c 1192b */
static Namelist addname(Name n, Namelist xs) {
    if (nameinlist(n, xs))
        return xs;
    else
        return mkNL(n, xs);
}
/* printfuns.c 1192c */
static Namelist addfree(Name n, Namelist bound, Namelist free) {
    if (nameinlist(n, bound))
        return free;
    else
        return addname(n, free);
}
/* printfuns.c 1193a */
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
        /* let [[free]] be the free variables for [[e->u.lambdax]] 1193b */
        for (Namelist xs = e->u.lambdax.formals; xs; xs = xs->tl)
            bound = addname(xs->hd, bound);
        free = freevars(e->u.lambdax.body, bound, free);
        break;
    case LETX:
        /* let [[free]] be the free variables for [[e->u.letx]] 1194 */
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
    /* extra cases for finding free variables in {\uscheme} expressions 1204b */
    }
    return free;
}
/* printfuns.c 1195a */
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
/* printfuns.c 1195b */
static void printvalueat(Printbuf output, Value v, int depth);
/* helper functions for [[printvalue]] 1196b */
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
/* printfuns.c 1196a */
void printvalue(Printbuf output, va_list_box *box) {
    printvalueat(output, va_arg(box->ap, Value), 0);
}
/* printfuns.c 1196c */
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
/* printfuns.c 1201c */
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
/* printfuns.c 1202a */
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
/* printfuns.c 1202b */
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
/* printfuns.c 1203a */
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
    /* extra cases for printing {\uscheme} ASTs 1204a */
    default:
        assert(0);
    }
}
/* printfuns.c 1203b */
void printlambda(Printbuf output, va_list_box *box) {
    Lambda l = va_arg(box->ap, Lambda);
    bprint(output, "(lambda (%N) %e)", l.formals, l.body);
}
