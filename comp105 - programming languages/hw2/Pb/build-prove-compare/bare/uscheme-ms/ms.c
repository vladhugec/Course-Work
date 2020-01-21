#include "all.h"
/* ms.c 325a */
/* private declarations for mark-and-sweep collection 325b */
typedef struct Mvalue Mvalue;
struct Mvalue {
    Value v;
    unsigned live:1;
};
/* private declarations for mark-and-sweep collection 326b */
#ifndef GCHYPERDEBUG /*OMIT*/
#define GROWTH_UNIT 24\
                    /* increment in which the heap grows, measured in objects */
#else /*OMIT*/
#define GROWTH_UNIT 3 /*OMIT*/
#endif /*OMIT*/
typedef struct Page Page;
struct Page {
    Mvalue pool[GROWTH_UNIT];
    Page *tl;
};
/* private declarations for mark-and-sweep collection 326c */
Page *pagelist, *curpage;
Mvalue *hp, *heaplimit;
/* private declarations for mark-and-sweep collection 327b */
static void visitloc          (Value *loc);
static void visitvalue        (Value v);
static void visitenv          (Env env);
static void visitexp          (Exp exp);
static void visitexplist      (Explist es);
static void visitframe        (Frame *fr);
static void visitstack        (Stack s);
static void visittest         (UnitTest t);
static void visittestlists    (UnitTestlistlist uss);
static void visitregister     (Register reg);
static void visitregisterlist (Registerlist regs);
static void visitroots        (void);
/* private declarations for mark-and-sweep collection 1511a */
static int nalloc;              /* total number of allocations */
static int ncollections;        /* total number of collections */
static int nmarks;              /* total number of cells marked */
/* ms.c 326a */
int gc_uses_mark_bits = 1;
/* ms.c 326d */
static void makecurrent(Page *page) {
    assert(page != NULL);
    curpage = page;
    hp = &page->pool[0];
    heaplimit = &page->pool[GROWTH_UNIT];
}
/* ms.c 326e */
static int heapsize;            /* OMIT */
static void addpage(void) {
    Page *page = calloc(1, sizeof(*page));
    assert(page != NULL);

/* tell the debugging interface that each object on [[page]] has been acquired 342a */
    {   unsigned i;
        for (i = 0; i < sizeof(page->pool)/sizeof(page->pool[0]); i++)
            gc_debug_post_acquire(&page->pool[i].v, 1);
    }

    if (pagelist == NULL) {
        pagelist = page;
    } else {
        assert(curpage != NULL && curpage->tl == NULL);
        curpage->tl = page;
    }
    makecurrent(page);
    heapsize += GROWTH_UNIT;   /* OMIT */
}
/* ms.c ((prototype)) 327a */
Value* allocloc(void) {
    if (hp == heaplimit)
        addpage();
    assert(hp < heaplimit);

/* tell the debugging interface that [[&hp->v]] is about to be allocated 342b */
    gc_debug_pre_allocate(&hp->v);
    return &(hp++)->v;
}
/* ms.c 328a */
static void visitenv(Env env) {
    for (; env; env = env->tl)
        visitloc(env->loc);
}
/* ms.c ((prototype)) 328b */
static void visitloc(Value *loc) {
    Mvalue *m = (Mvalue*) loc;
    if (!m->live) {
        m->live = 1;
        visitvalue(m->v);
    }
}
/* ms.c 328c */
static void visitregister(Value *reg) {
    visitvalue(*reg);
}
/* ms.c 328d */
static void visitvalue(Value v) {
    switch (v.alt) {
    case NIL:
    case BOOLV:
    case NUM:
    case SYM:
    case PRIMITIVE:
        return;
    case PAIR:
        visitloc(v.u.pair.car);
        visitloc(v.u.pair.cdr);
        return;
    case CLOSURE:
        visitexp(v.u.closure.lambda.body);
        visitenv(v.u.closure.env);
        return;
    default:
        assert(0);
        return;
    }
    assert(0);
}
/* ms.c 1219 */
static void visitexp(Exp e) {
    switch (e->alt) {
    /* cases for [[visitexp]] 1220a */
    case LITERAL:
        visitvalue(e->u.literal);
        return;
    case VAR:
        return;
    case IFX:
        visitexp(e->u.ifx.cond);
        visitexp(e->u.ifx.truex);
        visitexp(e->u.ifx.falsex);
        return;
    case WHILEX:
        visitexp(e->u.whilex.cond);
        visitexp(e->u.whilex.body);
        return;
    case BEGIN:
        visitexplist(e->u.begin);
        return;
    case SET:
        visitexp(e->u.set.exp);
        return;
    case LETX:
        visitexplist(e->u.letx.es);
        visitexp(e->u.letx.body);
        return;
    case LAMBDAX:
        visitexp(e->u.lambdax.body);
        return;
    case APPLY:
        visitexp(e->u.apply.fn);
        visitexplist(e->u.apply.actuals);
        return;
    /* cases for [[visitexp]] 1220b */
    case BREAKX:
        return;
    case CONTINUEX:
        return;
    case RETURNX:
        visitexp(e->u.returnx);
        return;
    case THROW:
        visitexp(e->u.throw);
        return;
    case TRY_CATCH:
        visitexp(e->u.try_catch.handler);
        visitexp(e->u.try_catch.body);
        return;
    /* cases for [[visitexp]] 1221a */
    case WHILE_RUNNING_BODY:
        visitexp(e->u.whilex.cond);
        visitexp(e->u.whilex.body);
        return;
    case LETXENV:
        visitenv(e->u.letxenv);
        return;
    case CALLENV:
        visitenv(e->u.callenv);
        return;
    case HOLE:
        return;
    }
    assert(0);
}
/* ms.c 1221b */
static void visitexplist(Explist es) {
    for (; es; es = es->tl)
        visitexp(es->hd);
}
/* ms.c 1221c */
static void visitregisterlist(Registerlist regs) {
    for ( ; regs != NULL; regs = regs->tl)
        visitregister(regs->hd);
}
/* ms.c 1221d */
/* representation of [[struct Stack]] 1207a */
struct Stack {
    int size;
    Frame *frames;  // memory for 'size' frames
    Frame *sp;      // points to first unused frame
};
static void visitstack(Stack s) {
    Frame *fr;
    for (fr = s->frames; fr < s->sp; fr++) {
        visitframe(fr);
    }
}
/* ms.c 1221e */
static void visitframe(Frame *fr) {
    visitexp(&fr->context);
    if (fr->syntax != NULL)
        visitexp(fr->syntax);
}
/* ms.c 1221f */
static void visittestlists(UnitTestlistlist uss) {
    UnitTestlist ul;

    for ( ; uss != NULL; uss = uss->tl)
        for (ul = uss->hd; ul; ul = ul->tl)
            visittest(ul->hd);
}
/* ms.c 1222a */
static void visittest(UnitTest t) {
    switch (t->alt) {
    case CHECK_EXPECT:
        visitexp(t->u.check_expect.check);
        visitexp(t->u.check_expect.expect);
        return;
    case CHECK_ASSERT:
        visitexp(t->u.check_assert);
        return;
    case CHECK_ERROR:
        visitexp(t->u.check_error);
        return;
    }
    assert(0);
}
/* ms.c 1222b */
static void visitroots(void) {
    visitenv(*roots.globals.user);
    visittestlists(roots.globals.internal.pending_tests);
    visitstack(roots.stack);
    visitregisterlist(roots.registers);
}
/* ms.c ((prototype)) 1233b */
/* you need to redefine these functions */
void printfinalstats(void) { 
  (void)nalloc; (void)ncollections; (void)nmarks;
  assert(0); 
}
/* ms.c ((prototype)) 1233c */
void avoid_unpleasant_compiler_warnings(void) {
    (void)visitroots;
}
