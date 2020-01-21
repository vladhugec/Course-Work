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
/* printfuns.c 1176 */
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
/* printfuns.c 1177a */
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
        bprint(output, "(define %n (%N) (locals %N) %e)", d->u.define.name,
                      d->u.define.userfun.formals,
           d->u.define.userfun.locals,
              d->u.define.userfun.body);
        break;
    }
}
/* printfuns.c 1177b */
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
        /* print unit test [[d->u.test]] to file [[output]] 1178a */
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
/* printfuns.c 1178b */
void printvalue(Printbuf output, va_list_box *box) {
    Value v = va_arg(box->ap, Value);
    bprint(output, "%d", v);
}
/* printfuns.c 1178c */
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
