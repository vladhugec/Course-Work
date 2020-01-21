#include "all.h"
/*
 * Prototype of a copying system for micro-Scheme
 * 
 * Here we present more of the details of copying
 * collection for micro-Scheme. Further details appear
 * in Section [->], and to get a complete system, you
 * can work Exercises [->] through [->].
 * <copy.c>=
 */
/*
 * The semispaces [[fromspace]] and [[tospace]] each
 * have size [[semispacesize]].
 * <private declarations for copying collection>=
 */
static Value *fromspace, *tospace;    /* used only at GC time */
static int semispacesize;
                                     /* # of objects in fromspace and tospace */
/*
 * We always allocate from [[fromspace]]. The heap
 * pointer [[hp]] points to the next location available
 * to be allocated, and [[heaplimit]] points just past
 * the last location available to be allocated. The
 * number of locations that can be allocated before the
 * next collection is [[heaplimit - hp]].
 * <private declarations for copying collection>=
 */
static Value *hp, *heaplimit;                /* used for every allocation */
/*
 * <private declarations for copying collection>=
 */
static void scanenv      (Env env);
static void scanexp      (Exp exp);
static void scanexplist  (Explist es);
static void scanframe    (Frame *fr);
static void scantest     (UnitTest t);
static void scantests    (UnitTestlist ts);
static void scanloc      (Value *vp);
/*
 * <private declarations for copying collection>=
 */
#define isinspace(LOC, SPACE) ((SPACE) <= (LOC) && (LOC) < (SPACE) +\
                                                                  semispacesize)
static Value *forward(Value *p);
/*
 * A more aggressive collector would probably compute
 * [[isinspace]] with a single comparison (Exercise [->]
 * ).
 */

/*
 * Placeholders for exercises
 * 
 * <private declarations for copying collection>=
 */
static void collect(void);
/*
 * <copy.c>=
 */
/*
 * <representation of [[struct Stack]]>=
 */
struct Stack {
    int size;
    Frame *frames;  // memory for 'size' frames
    Frame *sp;      // points to first unused frame
};
/*
 * Allocation
 * 
 * In a copying system, the allocator is very simple. In
 * real systems, [[hp]] is in a register, and
 * [[allocloc]] is inlined. [*]
 * <copy.c>=
 */
int nalloc;   /* OMIT */
Value* allocloc(void) {
    if (hp == heaplimit)
        collect();
    assert(hp < heaplimit);
    assert(isinspace(hp, fromspace)); /*runs after spaces are swapped*/ /*OMIT*/
    nalloc++;   /* OMIT */
    /*
     * <tell the debugging interface that [[hp]] is about to be allocated>=
     */
    gc_debug_pre_allocate(hp);
    return hp++;
}
/*
 * The assertion can help detect bugs in a heap-growth
 * algorithm.
 */

/*
 * [*] The implementations of our scanning procedures
 * are more complicated than they would be in real life.
 * In a real system, these scanning procedures would
 * simply forward internal pointers. In our system,
 * because only [[Value]] objects are allocated on the
 * heap, we have a hybrid of forwarding and graph
 * traversal. As before, we use [[Env]] to illustrate;
 * we forward the [[loc]] pointer but traverse the
 * [[tl]] pointer.
 * <copy.c>=
 */
static void scanenv(Env env) {
    for (; env; env = env->tl)
      { /*OMIT*/
        env->loc = forward(env->loc);
        assert(isinspace(env->loc, tospace)); /*OMIT*/
      } /*OMIT*/
}
/*
 * <copy.c>=
 */
static void scanloc(Value *vp) {
    switch (vp->alt) {
    case NIL:
    case BOOLV:
    case NUM:
    case SYM:
        return;
    case PAIR:
        vp->u.pair.car = forward(vp->u.pair.car);
        vp->u.pair.cdr = forward(vp->u.pair.cdr);
        return;
    case CLOSURE:
        scanexp(vp->u.closure.lambda.body);
        scanenv(vp->u.closure.env);
        return;
    case PRIMITIVE:
        return;
    default:
        assert(0);
        return;
    }
}
/*
 * The complete implementation of [[forward]] has one
 * more subtlety; in our system, a root can appear on
 * the context stack more than once. [This doesn't
 * happen in real systems, primarily because compilers
 * engineer call stacks in a way that avoids duplicating
 * any part of any environment. As~a consequence, no
 * root appears on the stack more than once. Our
 * interpreter ensures only that each root appears on
 * the stack \emph{at least} once, not exactly once.]
 * For example, the evaluation state might be multiple \
 * astcallenv contexts whose environments share a
 * [[Value *]] pointer associated with the name 
 * [[foldr]]. If we scan such an environment for a
 * second time, the [[loc]] field associated with
 * [[foldr]] already points to to-space. A pointer that
 * already points to to-space must not be forwarded.
 * <copy.c>=
 */
static Value* forward(Value *p) {
    if (isinspace(p, tospace)) {
                          /* already in to space; must belong to scanned root */
        return p;
    } else {
        assert(isinspace(p, fromspace));
        /*
         * The basic operation used in a copying collector is
         * forwarding a pointer. This operation takes a pointer
         * to an object in from-space and returns a pointer to
         * the unique copy of that object in to-space. The code
         * is very simple; [[p]] is a pointer into from-space,
         * and we copy the object [[*p]] unless it has been
         * copied already. We copy the object to [[*hp]], which
         * is safe because while pointers are being forwarded,
         * [[hp]] points into to-space, at the boundary between
         * allocated and unallocated locations. Because to-space
         * is as big as from-space, and because no object is
         * copied more than once, there is guaranteed to be
         * enough room to hold all the objects.
         * <forward pointer [[p]] and return the result>=
         */
        if (p->alt == FORWARD) {            /* forwarding pointer */
            assert(isinspace(p->u.forward, tospace));   /* OMIT */
            return p->u.forward;
        } else {
            assert(isinspace(hp, tospace)); /* there is room */   /* OMIT */
            /*
             * <tell the debugging interface that [[hp]] is about to be
                                                                     allocated>=
             */
            gc_debug_pre_allocate(hp);
            *hp = *p;
            *p  = mkForward(hp);
                                /* overwrite *p with a new forwarding pointer */
            assert(isinspace(p->u.forward, tospace)); /*extra*/   /* OMIT */
            return hp++;
        }
    }
    return NULL; /* appease a stupid compiler */  /*OMIT*/
}
/*
 * Measurements show that the [[isinspace]] test
 * contributes significantly to garbage-collection time,
 * so I make it a macro. [*]
 */

/*
 * Scanning expressions means scanning internal values
 * or subexpressions.
 * <copy.c>=
 */
static void scanexp(Exp e) {
    switch (e->alt) {
    /*
     * First, micro-Scheme expressions:
     * <cases for [[scanexp]]>=
     */
    case LITERAL:
        scanloc(&e->u.literal);
        return;
    case VAR:
        return;
    case IFX:
        scanexp(e->u.ifx.cond);
        scanexp(e->u.ifx.truex);
        scanexp(e->u.ifx.falsex);
        return;
    case WHILEX:
        scanexp(e->u.whilex.cond);
        scanexp(e->u.whilex.body);
        return;
    case BEGIN:
        scanexplist(e->u.begin);
        return;
    case SET:
        scanexp(e->u.set.exp);
        return;
    case LETX:
        scanexplist(e->u.letx.es);
        scanexp(e->u.letx.body);
        return;
    case LAMBDAX:
        scanexp(e->u.lambdax.body);
        return;
    case APPLY:
        scanexp(e->u.apply.fn);
        scanexplist(e->u.apply.actuals);
        return;
    /*
     * Next, \uschemeplus expressions:
     * <cases for [[scanexp]]>=
     */
    case BREAKX:
        return;
    case CONTINUEX:
        return;
    case RETURNX:
        scanexp(e->u.returnx);
        return;
    case THROW:
        scanexp(e->u.throw);
        return;
    case TRY_CATCH:
        scanexp(e->u.try_catch.handler);
        scanexp(e->u.try_catch.body);
        return;
    /*
     * <cases for [[scanexp]]>=
     */
    case HOLE:
    case WHILE_RUNNING_BODY:
        return;
    case LETXENV:
        scanenv(e->u.letxenv);
        return;
    case CALLENV:
        scanenv(e->u.callenv);
        return;
    }
    assert(0);
}
/*
 * <copy.c>=
 */
static void scanframe(Frame *fr) {
    scanexp(&fr->context);
        if (fr->syntax != NULL)
            scanexp(fr->syntax);
}
/*
 * <copy.c>=
 */
static void scanexplist(Explist es) {
    for (; es; es = es->tl)
        scanexp(es->hd);
}
/*
 * Scanning a source means scanning its pending tests.
 * <copy.c>=
 */
static void scantests(UnitTestlist tests) {
    for (; tests; tests = tests->tl)
        scantest(tests->hd);
}
/*
 * Scanning a test means scanning its expressions.
 * <copy.c>=
 */
static void scantest(UnitTest t) {
    switch (t->alt) {
    case CHECK_EXPECT:
        scanexp(t->u.check_expect.check);
        scanexp(t->u.check_expect.expect);
        return;
    case CHECK_ASSERT:
        scanexp(t->u.check_assert);
        return;
    case CHECK_ERROR:
        scanexp(t->u.check_error);
        return;
    }
    assert(0);
}
/*
 * <copy.c ((prototype))>=
 */
/* you need to redefine these functions */
static void collect(void) { (void)scanframe; (void)scantests; assert(0); }
void printfinalstats(void) { assert(0); }
/* you need to initialize this variable */
int gc_uses_mark_bits;
