#include "all.h"
/* prim.c 175b */
static int32_t projectint32(Exp e, Value v) {
    if (v.alt != NUM)
        runerror("in %e, expected an integer, but got %v", e, v);
    return v.u.num;
}
/* prim.c 175d */
static int32_t divide(int32_t n, int32_t m) {
    if (n >= 0)
        if (m >= 0)
            return n / m;
        else
            return -(( n - m - 1) / -m);
    else
        if (m >= 0)
            return -((-n + m - 1) /  m);
        else
            return -n / -m;
}
/* prim.c 176a */
Value arith(Exp e, int tag, Valuelist args) {
    checkargc(e, 2, lengthVL(args));
    int32_t n = projectint32(e, nthVL(args, 0));
    int32_t m = projectint32(e, nthVL(args, 1));

    switch (tag) {
    case PLUS:
        checkarith('+', n, m, 32); // OMIT
        return mkNum(n + m);
    case MINUS:
        checkarith('-', n, m, 32); // OMIT
        return mkNum(n - m);
    case TIMES:
        checkarith('*', n, m, 32); // OMIT
        return mkNum(n * m);
    case DIV:
        if (m==0)
            runerror("division by zero");
        checkarith('/', n, m, 32); // OMIT
        return mkNum(divide(n, m));
    case LT:
        return mkBoolv(n < m);
    case GT:
        return mkBoolv(n > m);
    default:
        assert(0);
    }
}
/* prim.c 177a */
Value binary(Exp e, int tag, Valuelist args) {
    checkargc(e, 2, lengthVL(args));
    Value v = nthVL(args, 0);
    Value w = nthVL(args, 1);

    switch (tag) {
    case CONS: 
        return cons(v, w);
    case EQ:   
        return equalatoms(v, w);
    default:
        assert(0);
    }
}
/* prim.c 177b */
Value cons(Value v, Value w) {
    return mkPair(allocate(v), allocate(w));
}
/* prim.c 177c */
Value equalatoms(Value v, Value w) {
    if (v.alt != w.alt)
        return falsev;

    switch (v.alt) {
    case NUM:
        return mkBoolv(v.u.num   == w.u.num);
    case BOOLV:
        return mkBoolv(v.u.boolv == w.u.boolv);
    case SYM:
        return mkBoolv(v.u.sym   == w.u.sym);
    case NIL:
        return truev;
    default:
        return falsev;
    }
}
/* prim.c 179a */
Value unary(Exp e, int tag, Valuelist args) {
    checkargc(e, 1, lengthVL(args));
    Value v = nthVL(args, 0);
    switch (tag) {
    case NULLP:
        return mkBoolv(v.alt == NIL);
    case BOOLEANP:
        return mkBoolv(v.alt == BOOLV);
    case NUMBERP:
        return mkBoolv(v.alt == NUM);
    case SYMBOLP:
        return mkBoolv(v.alt == SYM);
    case PAIRP:
        return mkBoolv(v.alt == PAIR);
    case PROCEDUREP:
        return mkBoolv(v.alt == CLOSURE || v.alt == PRIMITIVE);
    case CAR:
        if (v.alt != PAIR)
            runerror("car applied to non-pair %v in %e", v, e);
        return *v.u.pair.car;
    case CDR:
        if (v.alt != PAIR)
            runerror("cdr applied to non-pair %v in %e", v, e);
        return *v.u.pair.cdr;
    case PRINTLN:
        print("%v\n", v);
        return v;
    case PRINT:
        print("%v", v);
        return v;
    case PRINTU:
        if (v.alt != NUM)
            runerror("printu applied to non-number %v in %e", v, e);
        print_utf8(v.u.num);
        return v;
    case ERROR:
        runerror("%v", v);
        return v;
    default:
        assert(0);
    }
}
