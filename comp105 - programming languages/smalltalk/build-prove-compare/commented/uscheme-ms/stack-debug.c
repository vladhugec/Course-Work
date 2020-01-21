#include "all.h"
/*
 * Tracing machine state using the stack
 * 
 * Variables [[etick]] and [[vtick]] count the number of
 * state transitions involving an expression or a
 * variable as the current item, respectively. Pointer
 * [[trace_countp]] points to the value of a \
 * uschemeplus number. That way, [[set]] expressions in
 * the \uschemeplus code can turn tracing on and off
 * during a single call to [[eval]].
 * <stack-debug.c>=
 */
static int etick, vtick;  // number of times saw a current expression or value
static int *trace_countp; // if not NULL, points to value of &trace-stack
/*
 * Initalization sets the private variables.
 * <stack-debug.c>=
 */
void stack_trace_init(int *countp) { 
    etick = vtick = 0; 
    trace_countp = countp;
}
/*
 * Tracing a current expression shows the tick number,
 * the expression, a pointer to the environment, and the
 * stack. The trace count is decremented.
 * <stack-debug.c>=
 */
void stack_trace_current_expression(Exp e, Env rho, Stack s) {
    if (trace_countp && *trace_countp != 0) {
        (*trace_countp)--;
        etick++;
        fprint(stderr, "exp  %d = %e\n", etick, e);
        fprint(stderr, "env  %R\n", rho);
        fprint(stderr, "stack\n%S\n", s);
    }
}
/*
 * Tracing a current value works the same way, except
 * I use a special rendering for the empty stack.
 */

/*
 * <stack-debug.c>=
 */
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
