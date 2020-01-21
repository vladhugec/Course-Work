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
 * <printfuns.c>=
 */
void printexp(Printbuf output, va_list_box *box) {
    Exp e = va_arg(box->ap, Exp);
    if (e == NULL) {
        bprint(output, "<null>");
        return;
    }

    switch (e->alt){
    case LITERAL:
        bprint(output, "%v", e->u.literal);
        break;
    case VAR:
        bprint(output, "%n", e->u.var);
        break;
    case SET:
        bprint(output, "(set %n %e)", e->u.set.name, e->u.set.exp);
        break;
    case IFX:
        bprint(output, "(if %e %e %e)", e->u.ifx.cond, e->u.ifx.truex, e->
                                                                  u.ifx.falsex);
        break;
    case WHILEX:
        bprint(output, "(while %e %e)", e->u.whilex.cond, e->u.whilex.exp);
        break;
    case BEGIN:
        bprint(output, "(begin%s%E)", e->u.begin?" ":"", e->u.begin);
        break;
    case APPLY:
        bprint(output, "(%n%s%E)", e->u.apply.name,
                      e->u.apply.actuals?" ":"", e->u.apply.actuals);
        break;
    }
}
/*
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
        break;
    case EXP:
        bprint(output, "%e", d->u.exp);
        break;
    case DEFINE:
        bprint(output, "(define %n (%N) %e)", d->u.define.name,
                      d->u.define.userfun.formals,
              d->u.define.userfun.body);
        break;
    }
}
/*
 * Although it's not bound to any conversion specifier,
 * here is a function that prints extended definitions.
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
        break;
    case TEST:
        /*
         * <print unit test [[d->u.test]] to file [[output]]>=
         */
        {   UnitTest t = d->u.test;
            switch (t->alt) {
            case CHECK_EXPECT:
                bprint(output, "(check-expect %e %e)",
                       t->u.check_expect.check, t->u.check_expect.expect);
                break;
            case CHECK_ASSERT:
                bprint(output, "(check-assert %e)", t->u.check_assert);
                break;
            case CHECK_ERROR:
                bprint(output, "(check-error %e)", t->u.check_error);
                break;
            default:
                assert(0);
            }
        }
        break;
    case DEF:
        bprint(output, "%t", d->u.def);
        break;
    }
    assert(0);
}
/*
 * <printfuns.c>=
 */
void printvalue(Printbuf output, va_list_box *box) {
    Value v = va_arg(box->ap, Value);
    bprint(output, "%d", v);
}
/*
 * In Impcore, a function can't be rendered as concrete
 * syntax. But for debugging, it helps to see something,
 * so I put some information in angle brackets.
 * <printfuns.c>=
 */
void printfun(Printbuf output, va_list_box *box) {
    Fun f = va_arg(box->ap, Fun);
    switch (f.alt) {
    case PRIMITIVE:
        bprint(output, "<%n>", f.u.primitive);
        break;
    case USERDEF:
        bprint(output, "<userfun (%N) %e>", f.u.userdef.formals,
                                                              f.u.userdef.body);
        break;
    default:
        assert(0);
    }
}
