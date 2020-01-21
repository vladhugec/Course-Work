#include "all.h"
Def mkVal(Name name, Exp exp) {
    Def n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = VAL;
    n->u.val.name = name;
    n->u.val.exp = exp;
    return n;
}

Def mkExp(Exp exp) {
    Def n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = EXP;
    n->u.exp = exp;
    return n;
}

Def mkDefine(Name name, Lambda lambda) {
    Def n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = DEFINE;
    n->u.define.name = name;
    n->u.define.lambda = lambda;
    return n;
}

Def mkDefs(Deflist defs) {
    Def n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = DEFS;
    n->u.defs = defs;
    return n;
}

struct Def mkValStruct(Name name, Exp exp) {
    struct Def n;
    
    n.alt = VAL;
    n.u.val.name = name;
    n.u.val.exp = exp;
    return n;
}

struct Def mkExpStruct(Exp exp) {
    struct Def n;
    
    n.alt = EXP;
    n.u.exp = exp;
    return n;
}

struct Def mkDefineStruct(Name name, Lambda lambda) {
    struct Def n;
    
    n.alt = DEFINE;
    n.u.define.name = name;
    n.u.define.lambda = lambda;
    return n;
}

struct Def mkDefsStruct(Deflist defs) {
    struct Def n;
    
    n.alt = DEFS;
    n.u.defs = defs;
    return n;
}

XDef mkDef(Def def) {
    XDef n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = DEF;
    n->u.def = def;
    return n;
}

XDef mkUse(Name use) {
    XDef n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = USE;
    n->u.use = use;
    return n;
}

XDef mkTest(UnitTest test) {
    XDef n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = TEST;
    n->u.test = test;
    return n;
}

struct XDef mkDefStruct(Def def) {
    struct XDef n;
    
    n.alt = DEF;
    n.u.def = def;
    return n;
}

struct XDef mkUseStruct(Name use) {
    struct XDef n;
    
    n.alt = USE;
    n.u.use = use;
    return n;
}

struct XDef mkTestStruct(UnitTest test) {
    struct XDef n;
    
    n.alt = TEST;
    n.u.test = test;
    return n;
}

UnitTest mkCheckExpect(Exp check, Exp expect) {
    UnitTest n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = CHECK_EXPECT;
    n->u.check_expect.check = check;
    n->u.check_expect.expect = expect;
    return n;
}

UnitTest mkCheckAssert(Exp check_assert) {
    UnitTest n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = CHECK_ASSERT;
    n->u.check_assert = check_assert;
    return n;
}

UnitTest mkCheckError(Exp check_error) {
    UnitTest n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = CHECK_ERROR;
    n->u.check_error = check_error;
    return n;
}

struct UnitTest mkCheckExpectStruct(Exp check, Exp expect) {
    struct UnitTest n;
    
    n.alt = CHECK_EXPECT;
    n.u.check_expect.check = check;
    n.u.check_expect.expect = expect;
    return n;
}

struct UnitTest mkCheckAssertStruct(Exp check_assert) {
    struct UnitTest n;
    
    n.alt = CHECK_ASSERT;
    n.u.check_assert = check_assert;
    return n;
}

struct UnitTest mkCheckErrorStruct(Exp check_error) {
    struct UnitTest n;
    
    n.alt = CHECK_ERROR;
    n.u.check_error = check_error;
    return n;
}

Exp mkLiteral(Value literal) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = LITERAL;
    n->u.literal = literal;
    return n;
}

Exp mkVar(Name var) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = VAR;
    n->u.var = var;
    return n;
}

Exp mkSet(Name name, Exp exp) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = SET;
    n->u.set.name = name;
    n->u.set.exp = exp;
    return n;
}

Exp mkIfx(Exp cond, Exp truex, Exp falsex) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = IFX;
    n->u.ifx.cond = cond;
    n->u.ifx.truex = truex;
    n->u.ifx.falsex = falsex;
    return n;
}

Exp mkWhilex(Exp cond, Exp body) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = WHILEX;
    n->u.whilex.cond = cond;
    n->u.whilex.body = body;
    return n;
}

Exp mkBegin(Explist begin) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = BEGIN;
    n->u.begin = begin;
    return n;
}

Exp mkLetx(Letkeyword let, Namelist xs, Explist es, Exp body) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = LETX;
    n->u.letx.let = let;
    n->u.letx.xs = xs;
    n->u.letx.es = es;
    n->u.letx.body = body;
    return n;
}

Exp mkLambdax(Lambda lambdax) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = LAMBDAX;
    n->u.lambdax = lambdax;
    return n;
}

Exp mkApply(Exp fn, Explist actuals) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = APPLY;
    n->u.apply.fn = fn;
    n->u.apply.actuals = actuals;
    return n;
}

Exp mkBreakx(void) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = BREAKX;
    
    return n;
}

Exp mkContinuex(void) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = CONTINUEX;
    
    return n;
}

Exp mkReturnx(Exp returnx) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = RETURNX;
    n->u.returnx = returnx;
    return n;
}

Exp mkThrow(Exp throw) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = THROW;
    n->u.throw = throw;
    return n;
}

Exp mkTryCatch(Exp body, Exp handler) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = TRY_CATCH;
    n->u.try_catch.body = body;
    n->u.try_catch.handler = handler;
    return n;
}

Exp mkHole(void) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = HOLE;
    
    return n;
}

Exp mkWhileRunningBody(void) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = WHILE_RUNNING_BODY;
    
    return n;
}

Exp mkCallenv(Env callenv) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = CALLENV;
    n->u.callenv = callenv;
    return n;
}

Exp mkLetxenv(Env letxenv) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = LETXENV;
    n->u.letxenv = letxenv;
    return n;
}

struct Exp mkLiteralStruct(Value literal) {
    struct Exp n;
    
    n.alt = LITERAL;
    n.u.literal = literal;
    return n;
}

struct Exp mkVarStruct(Name var) {
    struct Exp n;
    
    n.alt = VAR;
    n.u.var = var;
    return n;
}

struct Exp mkSetStruct(Name name, Exp exp) {
    struct Exp n;
    
    n.alt = SET;
    n.u.set.name = name;
    n.u.set.exp = exp;
    return n;
}

struct Exp mkIfxStruct(Exp cond, Exp truex, Exp falsex) {
    struct Exp n;
    
    n.alt = IFX;
    n.u.ifx.cond = cond;
    n.u.ifx.truex = truex;
    n.u.ifx.falsex = falsex;
    return n;
}

struct Exp mkWhilexStruct(Exp cond, Exp body) {
    struct Exp n;
    
    n.alt = WHILEX;
    n.u.whilex.cond = cond;
    n.u.whilex.body = body;
    return n;
}

struct Exp mkBeginStruct(Explist begin) {
    struct Exp n;
    
    n.alt = BEGIN;
    n.u.begin = begin;
    return n;
}

struct Exp mkLetxStruct(Letkeyword let,
    Namelist xs,
    Explist es,
    Exp body) {
    struct Exp n;
    
    n.alt = LETX;
    n.u.letx.let = let;
    n.u.letx.xs = xs;
    n.u.letx.es = es;
    n.u.letx.body = body;
    return n;
}

struct Exp mkLambdaxStruct(Lambda lambdax) {
    struct Exp n;
    
    n.alt = LAMBDAX;
    n.u.lambdax = lambdax;
    return n;
}

struct Exp mkApplyStruct(Exp fn, Explist actuals) {
    struct Exp n;
    
    n.alt = APPLY;
    n.u.apply.fn = fn;
    n.u.apply.actuals = actuals;
    return n;
}

struct Exp mkBreakxStruct(void) {
    struct Exp n;
    
    n.alt = BREAKX;
    
    return n;
}

struct Exp mkContinuexStruct(void) {
    struct Exp n;
    
    n.alt = CONTINUEX;
    
    return n;
}

struct Exp mkReturnxStruct(Exp returnx) {
    struct Exp n;
    
    n.alt = RETURNX;
    n.u.returnx = returnx;
    return n;
}

struct Exp mkThrowStruct(Exp throw) {
    struct Exp n;
    
    n.alt = THROW;
    n.u.throw = throw;
    return n;
}

struct Exp mkTryCatchStruct(Exp body, Exp handler) {
    struct Exp n;
    
    n.alt = TRY_CATCH;
    n.u.try_catch.body = body;
    n.u.try_catch.handler = handler;
    return n;
}

struct Exp mkHoleStruct(void) {
    struct Exp n;
    
    n.alt = HOLE;
    
    return n;
}

struct Exp mkWhileRunningBodyStruct(void) {
    struct Exp n;
    
    n.alt = WHILE_RUNNING_BODY;
    
    return n;
}

struct Exp mkCallenvStruct(Env callenv) {
    struct Exp n;
    
    n.alt = CALLENV;
    n.u.callenv = callenv;
    return n;
}

struct Exp mkLetxenvStruct(Env letxenv) {
    struct Exp n;
    
    n.alt = LETXENV;
    n.u.letxenv = letxenv;
    return n;
}

