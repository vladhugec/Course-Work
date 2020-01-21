#include "all.h"
/*
 * GC debugging, with or without Valgrind
 * 
 * [*] This code implements the debugging interface
 * described in \secrefgcs.gc-debug. It finds bugs in
 * three ways:
 * 
 *   • When memory belongs to the collector and not the
 *  interpreter, the [[alt]] field is set to
 *  [[INVALID]]. If [[validate]] is called with an
 *  [[INVALID]] expression, it dies.
 *   • When memory belongs to the collector and not the
 *  interpreter, we tell Valgrind that nobody must
 *  read or write it. If your collector mistakenly
 *  reclaims memory that the interpreter still has
 *  access to, when the interpreter tries to read or
 *  write that memory, Valgrind will bleat. (Valgrind
 *  is discussed briefly in \crefpagegcs.valgrind.)
 *   • When memory is given from the collector to the
 *  interpreter, we tell Valgrind that it is OK to
 *  write but not OK to read until it has been
 *  initialized.
 * 
 * If you don't have Valgrind, you can [[#define
 * NOVALGRIND]], and you'll still have the [[INVALID]]
 * thing in the [[alt]] field to help you.
 * <gcdebug.c>=
 */
#ifndef NOVALGRIND
  #include <valgrind/memcheck.h>
#else
  /*
   * To prevent compiler warnings, the do-nothing macros
   * ``evaluate'' their arguments by casting them to
   * [[void]].
   * <define do-nothing replacements for Valgrind macros>=
   */
  #define VALGRIND_CREATE_BLOCK(p, n, s)     ((void)(p),(void)(n),(void)(s))
  #define VALGRIND_CREATE_MEMPOOL(p, n, z)   ((void)(p),(void)(n),(void)(z))
  #define VALGRIND_MAKE_MEM_DEFINED_IF_ADDRESSABLE(p, n) \
                                             ((void)(p),(void)(n))
  #define VALGRIND_MAKE_MEM_DEFINED(p, n)    ((void)(p),(void)(n))
  #define VALGRIND_MAKE_MEM_UNDEFINED(p, n)  ((void)(p),(void)(n))
  #define VALGRIND_MAKE_MEM_NOACCESS(p, n)   ((void)(p),(void)(n))
  #define VALGRIND_MEMPOOL_ALLOC(p1, p2, n)  ((void)(p1),(void)(p2),(void)(n))
  #define VALGRIND_MEMPOOL_FREE(p1, p2)      ((void)(p1),(void)(p2))
  /*
   * The Valgrind calls are described in Valgrind's
   * documentation for ``custom memory allocators.''
   */

#endif
/*
 * <gcdebug.c>=
 */
static int gc_pool_object;
static void *gc_pool = &gc_pool_object;  /* valgrind needs this */
static int gcverbose;  /* GCVERBOSE tells gcprintf & gcprint to make noise */

void gc_debug_init(void) {
    VALGRIND_CREATE_MEMPOOL(gc_pool, 0, gc_uses_mark_bits);
    gcverbose = getenv("GCVERBOSE") != NULL;
}
/*
 * When we acquire objects, we make each one invalid, we
 * tell Valgrind that each one exists, and we mark all
 * the memory as inaccessible (because it belongs to the
 * collector).
 * <gcdebug.c>=
 */
void gc_debug_post_acquire(Value *mem, unsigned nvalues) {
    unsigned i;
    for (i = 0; i < nvalues; i++) {
        gcprintf("ACQUIRE %p\n", (void*)&mem[i]);
        mem[i] = mkInvalid("memory acquired from OS");
        VALGRIND_CREATE_BLOCK(&mem[i], sizeof(*mem), "managed Value");
    }
    /*
     * <when using mark bits, barf unless [[nvalues]] is 1>=
     */
    if (gc_uses_mark_bits) /* mark and sweep */
        assert(nvalues == 1);
    VALGRIND_MAKE_MEM_NOACCESS(mem, nvalues * sizeof(*mem));
}
/*
 * Before we release memory, we check that the objects
 * are invalid. We have to tell Valgrind that it's
 * temporarily OK to look at the object.
 * <gcdebug.c>=
 */
void gc_debug_pre_release(Value *mem, unsigned nvalues) {
    unsigned i;
    for (i = 0; i < nvalues; i++) {
        gcprintf("RELEASE %p\n", (void*)&mem[i]);
        VALGRIND_MAKE_MEM_DEFINED(&mem[i].alt, sizeof(mem[i].alt));
        assert(mem[i].alt == INVALID);
    }
    VALGRIND_MAKE_MEM_NOACCESS(mem, nvalues * sizeof(*mem));
}
/*
 * Before handing an object to the interpreter, we tell
 * Valgrind it's been allocated, we make it invalid, and
 * finally tell Valgrind that it's writable but
 * uninitialized.
 * <gcdebug.c>=
 */
void gc_debug_pre_allocate(Value *mem) {
    gcprintf("ALLOC %p\n", (void*)mem);
    VALGRIND_MEMPOOL_ALLOC(gc_pool, mem, sizeof(*mem));
    VALGRIND_MAKE_MEM_DEFINED_IF_ADDRESSABLE(&mem->alt, sizeof(mem->alt));
    assert(mem->alt == INVALID);
    *mem = mkInvalid("allocated but uninitialized");
    VALGRIND_MAKE_MEM_UNDEFINED(mem, sizeof(*mem));    
}
/*
 * When we get an object back, we check that it's not
 * invalid (because it should have been initialized to a
 * valid value immediately after it was allocated). Then
 * we mark it invalid and tell Valgrind it's been freed.
 * <gcdebug.c>=
 */
void gc_debug_post_reclaim(Value *mem) {
    gcprintf("FREE %p\n", (void*)mem);
    assert(mem->alt != INVALID);
    *mem = mkInvalid("memory reclaimed by the collector");
    VALGRIND_MEMPOOL_FREE(gc_pool, mem);
}
/*
 * The loop to reclaim a block works only if the pointer
 * is a pointer to an array of [[Value]], not an array
 * of [[Mvalue]].
 * <gcdebug.c>=
 */
void gc_debug_post_reclaim_block(Value *mem, unsigned nvalues) {
    unsigned i;
    /*
     * <when using mark bits, barf unless [[nvalues]] is 1>=
     */
    if (gc_uses_mark_bits) /* mark and sweep */
        assert(nvalues == 1);
    for (i = 0; i < nvalues; i++)
        gc_debug_post_reclaim(&mem[i]);
}
/*
 * Here are the printing functions.
 * <gcdebug.c>=
 */
void gcprint(const char *fmt, ...) {
  if (gcverbose) {
    va_list_box box;
    Printbuf buf = printbuf();

    assert(fmt);
    va_start(box.ap, fmt);
    vbprint(buf, fmt, &box);
    va_end(box.ap);
    fwritebuf(buf, stderr);
    fflush(stderr);
    freebuf(&buf);
  }
}
/*
 * <gcdebug.c>=
 */
void gcprintf(const char *fmt, ...) {
  if (gcverbose) {
    va_list args;

    assert(fmt);
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fflush(stderr);
  }
}
/*
 * The code uses depth-first search to make sure no
 * value is ever its own ancestor.
 * <gcdebug.c>=
 */
struct va { /* value ancestors */
    Value *l;
    struct va *parent;
};
/*
 * <gcdebug.c>=
 */
static void check(Value *l, struct va *ancestors) {
    struct va *c;
    for (c = ancestors; c; c = c->parent)
        if (l == c->l) {
            fprintf(stderr, "%p is involved in a cycle\n", (void *)l);
            if (c == ancestors) {
                fprintf(stderr, "%p -> %p\n", (void *)l, (void *)l);
            } else {
                fprintf(stderr, "%p -> %p\n", (void *)l, (void *)ancestors->l);
                while (ancestors->l != l) {
                    fprintf(stderr, "%p -> %p\n",
                            (void *)ancestors->l, (void *)ancestors->parent->l);
                    ancestors = ancestors->parent;
                }
            }
            runerror("cycle of cons cells");
        }
}
/*
 * <gcdebug.c>=
 */
static void search(Value *v, struct va *ancestors) {
    if (v->alt == PAIR) {
        struct va na;  /* new ancestors */
        check(v->u.pair.car, ancestors);
        check(v->u.pair.cdr, ancestors);
        na.l = v;
        na.parent = ancestors;
        search(v->u.pair.car, &na);
        search(v->u.pair.cdr, &na);
    }
}

void cyclecheck(Value *l) {
    search(l, NULL);
}
