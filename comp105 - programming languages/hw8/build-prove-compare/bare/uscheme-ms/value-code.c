#include "all.h"
Lambda mkLambda(Namelist formals, Exp body) {
    Lambda n;
    
    n.formals = formals;
    n.body = body;
    return n;
}

Value mkNil(void) {
    Value n;
    
    n.alt = NIL;
    
    return n;
}

Value mkBoolv(bool boolv) {
    Value n;
    
    n.alt = BOOLV;
    n.u.boolv = boolv;
    return n;
}

Value mkNum(int num) {
    Value n;
    
    n.alt = NUM;
    n.u.num = num;
    return n;
}

Value mkSym(Name sym) {
    Value n;
    
    n.alt = SYM;
    n.u.sym = sym;
    return n;
}

Value mkPair(Value *car, Value *cdr) {
    Value n;
    
    n.alt = PAIR;
    n.u.pair.car = car;
    n.u.pair.cdr = cdr;
    return n;
}

Value mkClosure(Lambda lambda, Env env) {
    Value n;
    
    n.alt = CLOSURE;
    n.u.closure.lambda = lambda;
    n.u.closure.env = env;
    return n;
}

Value mkPrimitive(int tag, Primitive *function) {
    Value n;
    
    n.alt = PRIMITIVE;
    n.u.primitive.tag = tag;
    n.u.primitive.function = function;
    return n;
}

Value mkForward(Value *forward) {
    Value n;
    
    n.alt = FORWARD;
    n.u.forward = forward;
    return n;
}

Value mkInvalid(const char *invalid) {
    Value n;
    
    n.alt = INVALID;
    n.u.invalid = invalid;
    return n;
}

