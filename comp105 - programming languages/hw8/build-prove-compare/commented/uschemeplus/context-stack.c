#include "all.h"
/*
 * Instrumentation is stored in three global variables.
 * Tail-call optimization is on by default; showing the
 * high stack mark is not.
 * <context-stack.c>=
 */
/*
 * Supporting code for \titleuschemeplus
 * 
 * [*] [*] \invisiblelocaltableofcontents[*]
 * 
 * The stack of evaluation contexts
 * 
 * This section shows the implementation of the
 * [[Stack]] of evaluation contexts and its
 * instrumentation.
 * 
 * Implementing the stack
 * 
 * In \chaprefschemes, the representation of a [[Stack]]
 * is private to this module. In \chaprefgcs, the
 * representation is exposed to the garbage collector.
 * <representation of [[struct Stack]]>=
 */
struct Stack {
    int size;
    Frame *frames;  // memory for 'size' frames
    Frame *sp;      // points to first unused frame
};

int optimize_tail_calls = 1;
int high_stack_mark;
                      // maximum number of frames used in the current evaluation
int show_high_stack_mark;
/*
 * A fresh, empty stack can hold 8 frames.
 * <context-stack.c>=
 */
Stack emptystack(void) {
    Stack s;
    s = malloc(sizeof *s);
    assert(s);
    s->size = 8;
    s->frames = malloc(s->size * sizeof(*s->frames));
    assert(s->frames);
    s->sp = s->frames;
    return s;
}
/*
 * A stack that has already been allocated can be
 * emptied by calling [[clearstack]]. This situation may
 * occur if a call to [[eval]] is terminated prematurely
 * (with a nonempty stack) by a call to [[error]].
 * <context-stack.c>=
 */
void clearstack (Stack s) {
    s->sp = s->frames;
}
/*
 * Unless the [[sp]] and [[frames]] fields point to the
 * same memory, there is a frame on top of the stack.
 * <context-stack.c>=
 */
Frame *topframe (Stack s) {
    assert(s);
    if (s->sp == s->frames)
        return NULL;
    else
        return s->sp - 1;
}
/*
 * Pushing, whether [[pushcontext]] or [[pushenv_opt]],
 * is implemented using the private function
 * [[pushframe]]. Function [[pushframe]] returns a
 * pointer to the frame just pushed.
 * <context-stack.c>=
 */
static Frame *pushframe (Frame f, Stack s) {
    assert(s);
    /*
     * Ten thousand stack frames ought to be enough for
     * anybody.
     * <if stack [[s]] is full, enlarge it>=
     */
    if (s->sp - s->frames == s->size) {
        unsigned newsize = 2 * s->size;
        if (newsize > 10000) {
            clearstack(s);
            runerror("recursion too deep");
        }
        s->frames = realloc(s->frames, newsize * sizeof(*s->frames));
        assert(s->frames);
        s->sp = s->frames + s->size;
        s->size = newsize;
    }
    *s->sp++ = f;
    /*
     * <set [[high_stack_mark]] from stack [[s]]>=
     */
    {   int n = s->sp - s->frames;
        if (n > high_stack_mark)
            high_stack_mark = n;
    }
    return s->sp - 1;
}
/*
 * A frame can be popped only if the stack is not empty.
 * But there is no need for memory management or
 * instrumentation.
 * <context-stack.c>=
 */
void popframe (Stack s) {
    assert(s->sp - s->frames > 0);
    s->sp--;
}
/*
 * Here's the specialized [[pushcontext]].
 * <context-stack.c>=
 */
static Frame mkExpFrame(struct Exp e) {
  Frame fr;
  fr.context = e;
  fr.syntax = NULL;
  return fr;
}

Exp pushcontext(struct Exp e, Stack s) {
  Frame *fr;
  assert(s);
  fr = pushframe(mkExpFrame(e), s);
  return &fr->context;
}
/*
 * <context-stack.c>=
 */
void printnoenv(FILE *output, va_list_box* box) {
    Env env = va_arg(box->ap, Env);
    fprintf(output, "@%p", (void *)env);
}
/*
 * <context-stack.c>=
 */
void printstack(FILE *output, va_list_box *box) {
    Stack s = va_arg(box->ap, Stack);
    Frame *fr;

    for (fr = s->sp-1; fr >= s->frames; fr--) {
        fprint(output, "  ");
        printframe(output, fr);
        fprint(output, ";\n");
    }
}
/*
 * <context-stack.c>=
 */
void printoneframe(FILE *output, va_list_box *box) {
    Frame *fr = va_arg(box->ap, Frame*);
    printframe(output, fr);
}
/*
 * <context-stack.c>=
 */
void printframe (FILE *output, Frame *fr) {
    fprintf(output, "%p: ", (void *) fr);
    fprint(output, "[%e]", &fr->context);
}
