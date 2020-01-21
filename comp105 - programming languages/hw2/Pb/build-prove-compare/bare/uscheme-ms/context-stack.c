#include "all.h"
/* context-stack.c 1207b */
/* representation of [[struct Stack]] 1207a */
struct Stack {
    int size;
    Frame *frames;  // memory for 'size' frames
    Frame *sp;      // points to first unused frame
};

int optimize_tail_calls = 1;
int high_stack_mark;
                      // maximum number of frames used in the current evaluation
int show_high_stack_mark;
/* context-stack.c 1207c */
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
/* context-stack.c 1208a */
void clearstack (Stack s) {
    s->sp = s->frames;
}
/* context-stack.c 1208c */
Frame *topframe (Stack s) {
    assert(s);
    if (s->sp == s->frames)
        return NULL;
    else
        return s->sp - 1;
}
/* context-stack.c 1208d */
static Frame *pushframe (Frame f, Stack s) {
    assert(s);
    /* if stack [[s]] is full, enlarge it 1208e */
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
    /* set [[high_stack_mark]] from stack [[s]] 1210f */
    {   int n = s->sp - s->frames;
        if (n > high_stack_mark)
            high_stack_mark = n;
    }
    return s->sp - 1;
}
/* context-stack.c 1209a */
void popframe (Stack s) {
    assert(s->sp - s->frames > 0);
    s->sp--;
}
/* context-stack.c 1209b */
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
/* context-stack.c 1209e */
void printnoenv(FILE *output, va_list_box* box) {
    Env env = va_arg(box->ap, Env);
    fprintf(output, "@%p", (void *)env);
}
/* context-stack.c 1210a */
void printstack(FILE *output, va_list_box *box) {
    Stack s = va_arg(box->ap, Stack);
    Frame *fr;

    for (fr = s->sp-1; fr >= s->frames; fr--) {
        fprint(output, "  ");
        printframe(output, fr);
        fprint(output, ";\n");
    }
}
/* context-stack.c 1210b */
void printoneframe(FILE *output, va_list_box *box) {
    Frame *fr = va_arg(box->ap, Frame*);
    printframe(output, fr);
}
/* context-stack.c 1210c */
void printframe (FILE *output, Frame *fr) {
    fprintf(output, "%p: ", (void *) fr);
    fprint(output, "[%e]", &fr->context);
}
