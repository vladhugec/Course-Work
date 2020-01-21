#include "all.h"
/*
 * <ms.c>=
 */
/*
 * A mark-and-sweep collector needs to associate a mark
 * bit with each heap location. To keep things simple,
 * we won't try to pack mark bits densely; we just wrap
 * each [[Value]] in another structure, which holds a
 * single mark bit, [[live]]. By placing the [[Value]]
 * at the beginning, we ensure that it is safe to cast
 * between values of type [[Value*]] and type
 * [[Mvalue*]].
 * <private declarations for mark-and-sweep collection>=
 */
typedef struct Mvalue Mvalue;
struct Mvalue {
    Value v;
    unsigned live:1;
};
/*
 * The debugging interface described in \secref
 * gcs.gc-debug below needs to know that each value is
 * wrapped with a mark bit.
 */

/*
 * The wrapped values are grouped into pages. A single
 * page holds a contiguous array of objects; the entire
 * heap is formed by chaining pages into linked lists.
 * The page is the unit of heap growth; when the heap is
 * too small, we call [[malloc]] to add one or more
 * pages to the heap.
 * <private declarations for mark-and-sweep collection>=
 */
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
/*
 * We use the [[tl]] field to chain pages on a linked
 * list; the head of that list is called [[pagelist]].
 * The ``heap pointer'' [[hp]] points to the next
 * [[Mvalue]] to be allocated. The pointer [[heaplimit]]
 * points to the first [[Mvalue]] after the current
 * page, [[curpage]].
 * <private declarations for mark-and-sweep collection>=
 */
Page *pagelist, *curpage;
Mvalue *hp, *heaplimit;
/*
 * The search itself is straightforward; there is one
 * procedure for each type of object to be visited.
 * Not every type in \secrefgcs.root-types is
 * represented, because not every type is reachable.
 * For example, there is no way to reach a [[Valuelist]]
 * from the roots.
 * <private declarations for mark-and-sweep collection>=
 */
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
/*
 * <private declarations for mark-and-sweep collection>=
 */
static int nalloc;              /* total number of allocations */
static int ncollections;        /* total number of collections */
static int nmarks;              /* total number of cells marked */
/*
 * <ms.c>=
 */
int gc_uses_mark_bits = 1;
/*
 * The function [[makecurrent]] makes [[curpage]],
 * [[hp]], and [[heaplimit]] point to an page from which
 * no locations have yet been allocated.
 * <ms.c>=
 */
static void makecurrent(Page *page) {
    assert(page != NULL);
    curpage = page;
    hp = &page->pool[0];
    heaplimit = &page->pool[GROWTH_UNIT];
}
/*
 * When the heap grows, it grows by one page at a time.
 * We allocate new pages with [[calloc]] so that the
 * mark bits are zeroed. It is a checked run-time error
 * to call [[addpage]] except when [[pagelist]] is
 * [[NULL]] or when [[curpage]] points to the last page
 * in the list.
 * <ms.c>=
 */
static int heapsize;            /* OMIT */
static void addpage(void) {
    Page *page = calloc(1, sizeof(*page));
    assert(page != NULL);
    /*
     * Some of the debugging functions are used in some of
     * the prototype code above:
     * <tell the debugging interface that each object on [[page]] has been
                                                                      acquired>=
     */
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
/*
 * <ms.c ((prototype))>=
 */
Value* allocloc(void) {
    if (hp == heaplimit)
        addpage();
    assert(hp < heaplimit);
    /*
     * <tell the debugging interface that [[&hp->v]] is about to be allocated>=
     */
    gc_debug_pre_allocate(&hp->v);
    return &(hp++)->v;
}
/*
 * Writing ``visit'' procedures is mostly
 * straightforward. Here, for example, is the code to
 * visit an [[Env]].
 * <ms.c>=
 */
static void visitenv(Env env) {
    for (; env; env = env->tl)
        visitloc(env->loc);
}
/*
 * The most important ``visit'' procedure is the one
 * that visits a location and sets the mark bit.
 * The value stored in the location is visited only if
 * the location hasn't been visited already.
 * <ms.c ((prototype))>=
 */
static void visitloc(Value *loc) {
    Mvalue *m = (Mvalue*) loc;
    if (!m->live) {
        m->live = 1;
        visitvalue(m->v);
    }
}
/*
 * In the tricolor-marking story, if [[m->live]] is not
 * set, then [[m]] is white. Setting [[m->live]] makes 
 * [[m]] gray, and after [[m->v]] is visited, [[m]] is
 * black.
 */

/*
 * Visiting a register is not the same as visiting a
 * heap location; a register has no mark bit.
 * <ms.c>=
 */
static void visitregister(Value *reg) {
    visitvalue(*reg);
}
/*
 * Function [[visitvalue]] is concerned with a value's
 * components of type [[Value *]], [[Exp]], or [[Env]].
 * <ms.c>=
 */
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
/*
 * To visit an expression, we visit its literal value,
 * if any, and of course its subexpressions.
 * <ms.c>=
 */
static void visitexp(Exp e) {
    switch (e->alt) {
    /*
     * <cases for [[visitexp]]>=
     */
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
    /*
     * Next, \uschemeplus expressions:
     * <cases for [[visitexp]]>=
     */
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
    /*
     * Last, \uschemeplus evaluation contexts:
     * <cases for [[visitexp]]>=
     */
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
/*
 * There are more cases than will fit on a page, so I
 * break them into three groups. First, micro-Scheme
 * expressions:
 */

/*
 * Function [[visitexplist]] visits a list of
 * expressions.
 * <ms.c>=
 */
static void visitexplist(Explist es) {
    for (; es; es = es->tl)
        visitexp(es->hd);
}
/*
 * Function [[visitregiserlist]] visits a list of
 * registers.
 * <ms.c>=
 */
static void visitregisterlist(Registerlist regs) {
    for ( ; regs != NULL; regs = regs->tl)
        visitregister(regs->hd);
}
/*
 * To visit a [[Stack]], we have to be able to see the
 * representation. Then we visit all the frames.
 * <ms.c>=
 */
/*
 * <representation of [[struct Stack]]>=
 */
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
/*
 * Visiting a frame means visiting both expressions.
 * <ms.c>=
 */
static void visitframe(Frame *fr) {
    visitexp(&fr->context);
    if (fr->syntax != NULL)
        visitexp(fr->syntax);
}
/*
 * Visiting lists of pending unit tests visits all lists
 * on the list.
 * <ms.c>=
 */
static void visittestlists(UnitTestlistlist uss) {
    UnitTestlist ul;

    for ( ; uss != NULL; uss = uss->tl)
        for (ul = uss->hd; ul; ul = ul->tl)
            visittest(ul->hd);
}
/*
 * Visiting a unit test means visiting its component
 * expressions.
 * <ms.c>=
 */
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
/*
 * Visiting roots means visiting the global variables,
 * the stack, and any machine registers.
 * <ms.c>=
 */
static void visitroots(void) {
    visitenv(*roots.globals.user);
    visittestlists(roots.globals.internal.pending_tests);
    visitstack(roots.stack);
    visitregisterlist(roots.registers);
}
/*
 * <ms.c ((prototype))>=
 */
/* you need to redefine these functions */
void printfinalstats(void) { 
  (void)nalloc; (void)ncollections; (void)nmarks;
  assert(0); 
}
/*
 * <ms.c ((prototype))>=
 */
void avoid_unpleasant_compiler_warnings(void) {
    (void)visitroots;
}
/*
 * Completed garbage collectors
 * 
 * [Table of contents]
 * 
 * [*]
 * 
 * Mark and sweep
 * 
 * These variables help us accumulate statistical
 * information; they are totals for the lifetime of the
 * program.
 */

