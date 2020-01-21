#include "all.h"
/* stack-debug.c 1210g */
static int etick, vtick;  // number of times saw a current expression or value
static int *trace_countp; // if not NULL, points to value of &trace-stack
/* stack-debug.c 1211a */
void stack_trace_init(int *countp) { 
    etick = vtick = 0; 
    trace_countp = countp;
}
/* stack-debug.c 1211c */
void stack_trace_current_expression(Exp e, Env rho, Stack s) {
    if (trace_countp && *trace_countp != 0) {
        (*trace_countp)--;
        etick++;
        fprint(stderr, "exp  %d = %e\n", etick, e);
        fprint(stderr, "env  %R\n", rho);
        fprint(stderr, "stack\n%S\n", s);
    }
}
/* stack-debug.c 1211d */
void stack_trace_current_value(Value v, Env rho, Stack s) {
    if (trace_countp && *trace_countp != 0) {
        (*trace_countp)--;
        vtick++;
        fprint(stderr, "val  %d = %v\n", vtick, v);
        fprint(stderr, "env  %R\n", rho);
        if (topframe(s)) 
            fprint(stderr, "stack\n%S\n", s);
        else 
            fprint(stderr, " (final answer from stack-based eval)\n");
    }
}
