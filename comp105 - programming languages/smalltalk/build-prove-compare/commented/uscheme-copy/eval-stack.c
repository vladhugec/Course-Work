#include "all.h"
/*
 * Structure and invariants of the evaluator
 * 
 * Function [[eval]] repeats the state transition <e/v,
 * rho,sigma,S> \STEPS<e'/v', rho', sigma', S'> until
 * the current item e/v is a value and the stack S is
 * empty. At that point it returns S. Here are the
 * essential invariants:
 * 
 *   • The environment rho is always in [[env]] and the
 *  stack is always in [[evalstack]]. As part of the
 *  state transition, these variables are mutated in
 *  place to hold rho' and S' respectively.
 *   • When it is necessary to inspect the stack, local
 *  variable [[fr]] (for ``frame'') points to the top
 *  of the stack.
 *   • When the current item is an expression e, that
 *  expression is stored in argument [[e]], and the
 *  state transition begins at label [[exp]].
 *   • When the current item is a value v, that value is
 *  stored in local variable [[v]], and the state
 *  transition begins at label [[value]].
 *   • Each state transition ends with [[goto exp]] or
 *  [[goto value]]. Before the goto, either [[e]] or 
 *  [[v]] must be set to the current item for the
 *  next state. (And [[env]] and [[evalstack]] must
 *  also be set.)
 * 
 * [*]\scmpflabeleval For ease of garbage collection
 * later, the stack should be a global variable!
 * <eval-stack.c>=
 */
Value eval(Exp e, Env env) {
    Value v;
    Frame *fr;
    /*
     * <definition of static [[Exp hole]], which always has a hole>=
     */
    static struct Exp holeExp = { HOLE, { { NIL, { 0 } } } };
    static Exp hole = &holeExp;
    static Stack evalstack;

    /*
     * This initialization code runs in [[eval]] and sets
     * its local variable [[evalstack]].
     * <ensure that [[evalstack]] is initialized and empty>=
     */
    if (evalstack == NULL)
        evalstack = emptystack();
    else
        clearstack(evalstack);
    /*
     * Revisions to [[eval]]
     * 
     * \chaprefschemes's [[eval]] function needs just a
     * couple of changes to support garbage collection.
     * First, the evaluation stack is part of the root set:
     * <ensure that [[evalstack]] is initialized and empty>=
     */
    assert(topframe(roots.stack) == NULL);
    roots.stack = evalstack;
    /*
     * Instrumentation for the high stack mark
     * 
     * <use the options in [[env]] to initialize the instrumentation>=
     */
    high_stack_mark = 0;
    show_high_stack_mark = 
        istrue(getoption(strtoname("&show-high-stack-mark"), env, falsev));
    /*
     * The following code runs in [[eval]], which has access
     * to [[env]]. There's just a little sanity checking—if
     * someone changes \uschemeplus variable [[ ---
     * trace-stack]] from a number to a non-number, chaos
     * may ensue.
     * <use the options in [[env]] to initialize the instrumentation>=
     */
    {   Value *p = find(strtoname("&trace-stack"), env);
        if (p && p->alt == NUM)
            stack_trace_init(&p->u.num);
        else
            stack_trace_init(NULL);
    }
    /*
     * <use the options in [[env]] to initialize the instrumentation>=
     */
    optimize_tail_calls = 
        istrue(getoption(strtoname("&optimize-tail-calls"), env, truev));

    exp: 
        stack_trace_current_expression(e, env, evalstack);
        /*
         * When the current item is an expression e, 16 of the
         * 20 expression \makenwnotdef(left as exercise) forms
         * in \uschemeplus are legitimate.
         * <take a step from a state of the form $\seval e$>=
         */
        switch (e->alt) {
        case LITERAL:
            /*
             * Interpreting forms that don't change the stack
             * 
             * A literal evaluates to itself without changing the
             * stack. Environment rho and store sigma are unchanged,
             * but we must transition to a state of the form \sevalv
             * S. To implement that transition, we assign to [[v]]
             * and then [[goto value]].
             * <start evaluating expression [[e->u.literal]] and transition to
                                                                the next state>=
             */
            v = e->u.literal;
            goto value;
        case VAR:   
            /*
             * Variable lookup doesn't change the stack. [*]
             * <start evaluating expression [[e->u.var]] and transition to the
                                                                    next state>=
             */
            if (find(e->u.var, env) == NULL)
                runerror("variable %n not found", e->u.var);
            v = *find(e->u.var, env);
            goto value;
        case SET:
            /*
             * Interpreting forms that push a single evaluation
             * context
             * 
             * To implement \xset, push a context on the stack.
             * Environment and store are unchanged, but we we have
             * to push the context \xset(x, \hole) onto the stack,
             * and we have to transition to a state in which the
             * current item is e, not \xset(x,e). To implement that
             * transition, we assign to [[e]] and then [[goto exp]].
             * [*] [*]
             * <start evaluating expression [[e->u.set]] and transition to the
                                                                    next state>=
             */
            if (find(e->u.set.name, env) == NULL)
                runerror("set unbound variable %n", e->u.set.name);
            pushcontext(mkSetStruct(e->u.set.name, hole), evalstack);
            e = e->u.set.exp;
            goto exp;
            /*
             * The rest of the computation of \xset is in the \
             * rulenameFinish-Assign rule. When we see a context
             * pushed by \xset, we fill the hole by assigning to the
             * variable.
             */

        case IFX:
            /*
             * The implementation if \xif is very similar to the
             * implementation of \xset: push one frame onto the
             * stack, then do as it says.
             * <start evaluating expression [[e->u.ifx]] and transition to the
                                                                    next state>=
             */
            pushcontext(mkIfxStruct(hole, e->u.ifx.truex, e->u.ifx.falsex),
                                                                     evalstack);
            e = e->u.ifx.cond;
            goto exp;
        case WHILEX:
            /*
             * <start evaluating expression [[e->u.whilex]] and transition to
                                                                the next state>=
             */
            pushcontext(mkWhilexStruct(e->u.whilex.cond, e->u.whilex.body),
                                                                     evalstack);
            e = e->u.whilex.cond;
            goto exp;
        case BEGIN:
            /*
             * Interpreting \xbegin
             * 
             * Evaluating a [[begin]] expression pushes it as a
             * context. No values are remembered and no expressions
             * are overwritten, so we needn't copy the [[Explist]].
             * <start evaluating expression [[e->u.begin]] and transition to the
                                                                    next state>=
             */
            pushcontext(mkBeginStruct(e->u.begin), evalstack);
            v = falsev;
            goto value;
        case LETX:
            if (/*
                 * Interpreting \xlet, \xletrec, and \xletstar
                 * 
                 * If a [[let]], [[letrec]], or [[let*]] has no
                 * bindings, we just evaluate the body. \opsEmpty-Let \
                 * eval\xlet(<>, e) ==>\evale
                 * <[[e->u.letx]] contains no bindings>=
                 */
                e->u.letx.xs == NULL && e->u.letx.es == NULL) {
                 e = e->u.letx.body; // continue with the body
                 goto exp;
            } else {
                switch (e->u.letx.let) {
                   case LET:
                     /*
                      * Evaluating a \xlet expression pushes a context in
                      * which the first expression is replaced by a hole. We
                      * build the context using [[copyEL]] and insert the
                      * hole using [[head_replaced_with_hole]].
                      * <start evaluating nonempty [[let]] expression
                                [[e->u.letx]] and transition to the next state>=
                      */
                     pushcontext(mkLetxStruct(e->u.letx.let, e->u.letx.xs, 
                                              copyEL(e->u.letx.es), e->
                                                                   u.letx.body),
                                 evalstack);
                     fr = topframe(evalstack);
                     e  = head_replaced_with_hole(fr->context.u.letx.es);
                     assert(e);
                     goto exp;
                   case LETSTAR:
                      /*
                       * Implementing \xletstar is simpler; there are no
                       * holes. Because there are no holes and nothing is ever
                       * overwritten, we needn't copy the syntax.
                       * <start evaluating nonempty [[let*]] expression
                                [[e->u.letx]] and transition to the next state>=
                       */
                      pushenv_opt(env, LETXENV, evalstack);
                      pushcontext(mkLetxStruct(e->u.letx.let, e->u.letx.xs, e->
                                                     u.letx.es, e->u.letx.body),
                                  evalstack);
                      fr = topframe(evalstack);
                      assert(fr->context.u.letx.es);
                      e = fr->context.u.letx.es->hd;
                      assert(e);
                      goto exp;
                   case LETREC:
                      /*
                       * Evaluating a \xletrec expression is very similar.
                       * We first save the environment on the stack and extend
                       * it with locations containing unspecified values. Then
                       * we do what we did for \xlet: replace the first
                       * expression with a hole and evaluate that expression.
                       * <start evaluating nonempty [[letrec]] expression
                                [[e->u.letx]] and transition to the next state>=
                       */
                      pushenv_opt(env, LETXENV, evalstack);
                      {   Namelist xs;
                          for (xs = e->u.letx.xs; xs; xs = xs->tl)    
                              env = bindalloc(xs->hd, unspecified(), env);
                      }
                      pushcontext(mkLetxStruct(e->u.letx.let, e->u.letx.xs, 
                                               copyEL(e->u.letx.es), e->
                                                                   u.letx.body),
                                  evalstack);
                      fr = topframe(evalstack);
                      e  = head_replaced_with_hole(fr->context.u.letx.es);
                      assert(e);
                      goto exp;
                   default:
                     assert(0);
                }
            }
        case LAMBDAX:
            /*
             * A \xlambda evaluates to a closure; we need only to
             * check for duplicate names.
             * <start evaluating expression [[e->u.lambdax]] and transition to
                                                                the next state>=
             */
            v = mkClosure(e->u.lambdax, env);
            goto value;
        case APPLY:
            /*
             * Interpreting forms that evaluate expressions in
             * sequence
             * 
             * Function application
             * 
             * When starting to evaluate an application, we push a
             * frame in which the function position is replaced with
             * a hole, and we start evaluating the function.
             * As explained in the previous section, the list of
             * [[actuals]] in the evaluation context needs to be a
             * copy of the list in the syntax.
             * <start evaluating expression [[e->u.apply]] and transition to the
                                                                    next state>=
             */
            pushcontext(mkApplyStruct(mkHole(), copyEL(e->u.apply.actuals)),
                                                                     evalstack);
            topframe(evalstack)->syntax = e;
            e = e->u.apply.fn;
            goto exp;
        case BREAKX:
            /*
             * Interpreting control operators
             * 
             * The [[break]] operator either terminates a running
             * loop or unwinds one context from the stack.
             * 
             * \stdbreak
             * 
             * \stdbreak
             * <start evaluating [[(break)]] and transition to the next state>=
             */
            fr = topframe(evalstack);
            if (fr == NULL) 
                runerror("(break) occurred outside any loop");
            else
                switch (fr->context.alt) {
                    case WHILE_RUNNING_BODY:  // Break-Transfer
                        popframe(evalstack);
                        v = falsev;
                        goto value;
                    case LETXENV:             // Break-Unwind-Letenv
                        env = fr->context.u.letxenv;
                        popframe(evalstack);
                        goto exp;
                    case CALLENV:
                        runerror("(break) in function outside of any loop");
                    default:                  // Break-Unwind
                        popframe(evalstack);
                        goto exp;
                }
        case CONTINUEX:
            /*
             * The [[(continue)]] operator is very much like
             * [[(break)]]; its implementation is \exref
             * schemes.ex.continue.
             * <start evaluating [[(continue)]] and transition to the next state
                                                                 ((prototype))>=
             */
            runerror("The implementation of (continue) is left as an exercise");
        case RETURNX:
            /*
             * A [[return]] first evaluates its expression.
             * <start evaluating expression [[e->u.returnx]] and transition to
                                                                the next state>=
             */
            pushcontext(mkReturnxStruct(hole), evalstack);
            e = e->u.returnx;
            goto exp;
            /*
             * Once the expression has been evaluated, it unwinds
             * the stack until it finds a \ccallenv context. The
             * implementation is \exrefschemes.ex.return.
             */

        case THROW:
            /*
             * Similarly, \xthrow begins by evaluating its argument.
             * <start evaluating expression [[e->u.throw]] and transition to the
                                                                    next state>=
             */
            pushcontext(mkThrowStruct(hole), evalstack);
            e = e->u.throw;
            goto exp;
            /*
             * When \xthrow(\hole) is seen on the stack, the
             * semantics says to unwind the stack to the nearest
             * [[try-catch]] handler. The implementation is \exref
             * schemes.ex.throw.
             */

        case TRY_CATCH:
            /*
             * A [[try-catch]] begins by evaluating its handler.
             * <start evaluating expression [[e->u.try_catch]] and transition to
                                                                the next state>=
             */
            pushcontext(mkTryCatchStruct(e->u.try_catch.body, hole), evalstack);
            e = e->u.try_catch.handler;
            goto exp;
        /*
         * <cases where the current item is an expression form that should
                                                      appear only on the stack>=
         */
        case WHILE_RUNNING_BODY:
        case HOLE:
        case LETXENV:
        case CALLENV:
            assert(0);
        }
        assert(0);
    value: 
        stack_trace_current_value(v, env, evalstack);
        v = validate(v);
        /*
         * <if [[evalstack]] is empty, return [[v]]; otherwise step from a state
                                of the form $\sevalv {\mathtt{fr} \sconsop S}$>=
         */
        fr = topframe(evalstack);
        if (fr == NULL) {
            /*
             * <if [[show_high_stack_mark]] is set, show maximum stack size>=
             */
            if (show_high_stack_mark)
                fprintf(stderr, "High stack mark == %d\n", high_stack_mark);
            return v;
        } else {
            /*
             * When the current item is a value v, 14 of the 20
             * expression forms in \uschemeplus may legitimately
             * appear as the evaluation context [[fr]] on top of the
             * stack.
             * <take a step from a state of the form $\sevalv {\mathtt{fr} \
                                                                   sconsop S}$>=
             */
            switch (fr->context.alt) {
            case SET:
                /*
                 * <fill hole in context [[fr->context.u.set]] and transition to
                                                                the next state>=
                 */
                assert(fr->context.u.set.exp->alt == HOLE);
                assert(find(fr->context.u.set.name, env) != NULL);
                *find(fr->context.u.set.name, env) = validate(v);
                popframe(evalstack);
                goto value;
            case IFX:
                /*
                 * <fill hole in context [[fr->context.u.ifx]] and transition to
                                                                the next state>=
                 */
                assert(fr->context.u.ifx.cond->alt == HOLE);
                e = istrue(v) ? fr->context.u.ifx.truex : fr->
                                                           context.u.ifx.falsex;
                popframe(evalstack);
                goto exp;
            case WHILEX:
                /*
                 * When the condition has been evaluated, it sees the \
                 * xwhile context on top of the stack. A true condition
                 * converts the context to a \cwhileb context and starts
                 * evaluating the body. A false condition pops the
                 * context and produces \vfalse.
                 * <if [[v]] is true, continue with body in context
                                                      [[fr->context.u.whilex]]>=
                 */
                if (istrue(validate(v))) {   // Small-Step-While-Condition-True
                    fr->context.alt = WHILE_RUNNING_BODY;
                    e = fr->context.u.whilex.body;
                    goto exp;
                } else {                     // Small-Step-While-Condition-False
                    popframe(evalstack);
                    v = falsev;
                    goto value;
                }
            case WHILE_RUNNING_BODY:
                /*
                 * When the \cwhileb context is seen, the evaluation of
                 * the loop's body is complete. We transition back to a
                 * \cwhilea context and evaluate the condition again.
                 * <transition to [[WHILEX]] and continue with condition in
                                              context [[fr->context.u.whilex]]>=
                 */
                fr->context.alt = WHILEX;
                e = fr->context.u.whilex.cond;
                goto exp;
            case BEGIN:
                /*
                 * When evaluation of a subexpression is complete,
                 * we either continue with the next subexpression, or if
                 * there are no more subexpressions, produce v as the
                 * result of the \xbegin.
                 * <continue with the next expression in context
                                                       [[fr->context.u.begin]]>=
                 */
                if (fr->context.u.begin) {   // Small-Step-Begin-Next-Expression
                    e = fr->context.u.begin->hd;
                    fr->context.u.begin = fr->context.u.begin->tl;
                    goto exp;
                } else {                     // Small-Step-Begin-Exhausted
                    popframe(evalstack);
                    goto value;
                }    
            case APPLY:
                /*
                 * When we see an \xapply frame at the top of the stack,
                 * three rules might apply: If rule \rulename
                 * Small-Step-Apply-First-Arg applies, the expression
                 * [[fr->context.u.apply.fn]] in function position will
                 * be a hole. \stdbreak If rule \rulename
                 * Small-Step-Apply-Next-Arg applies, function
                 * [[transition_explist]] will return the next argument.
                 * \stdbreak If rule \rulenameSmall-Step-Apply-Last-Arg
                 * applies, function [[transition_explist]] will return
                 * NULL. There is also a case not given in the
                 * semantics, when the list of arguments is empty.
                 * <fill hole in context [[fr->context.u.apply]] and transition
                                                             to the next state>=
                 */
                if (fr->context.u.apply.fn->alt == HOLE) {
                                                   // Small-Step-Apply-First-Arg
                    *fr->context.u.apply.fn = mkLiteralStruct(v);
                    e = head_replaced_with_hole(fr->context.u.apply.actuals);
                    if (e)
                        goto exp;
                                                   // Small-Step-Apply-First-Arg
                    else
                        goto apply_last_arg;
                                                      // empty list of arguments
                } else {                                    
                    e = transition_explist(fr->context.u.apply.actuals, v); 
                    if (e)
                        goto exp;
                                                    // Small-Step-Apply-Next-Arg
                    else goto
                        apply_last_arg;
                                                    // Small-Step-Apply-Last-Arg
                }
                apply_last_arg:   // Small-Step-Apply-Last-Arg (or no arguments)
                    /*
                     * <apply [[fr->context]]'s [[fn]] to its [[actuals]]; free memory; transition to next state>=
                     */
                    {
                        Value     fn = asLiteral (fr->context.u.apply.fn);
                        Valuelist vs = asLiterals(fr->context.u.apply.actuals);
                        free  (fr->context.u.apply.fn);
                        freeEL(fr->context.u.apply.actuals);

                        popframe(evalstack);
                        
                        switch (fn.alt) {
                          case PRIMITIVE:
                              /*
                               * <apply [[fn.u.primitive]] to [[vs]] and
                                                  transition to the next state>=
                               */
                              e = fr->syntax;
                              pushcontext(mkLetxenvStruct(env), evalstack);
                              env = NULL;
                              v = fn.u.primitive.function(e, fn.u.primitive.tag,
                                                                            vs);
                              freeVL(vs);
                              goto value;
                          case CLOSURE:
                              /*
                               * Here's the closure rule again: Before
                                                                  evaluating the
                               * body of the closure, we save the environment on
                                                                             the
                               * stack.
                               * <save [[env]], bind [[vs]] to [[fn.u.closure]]
                    's formals, and transition to evaluation of closure's body>=
                               */
                              {
                                  Namelist xs = fn.u.closure.lambda.formals;

                                  checkargc(e, lengthNL(xs), lengthVL(vs));
                                  pushenv_opt(env, CALLENV, evalstack);
                                  env = bindalloclist(xs, vs, fn.u.closure.env);
                                  e   = fn.u.closure.lambda.body;
                                  freeVL(vs);
                                  goto exp;
                              }
                          default:
                              runerror("%e evaluates to non-function %v in %e",
                                    fr->syntax->u.apply.fn, fn, fr->syntax);
                        }
                    }
            case LETX:
                switch (fr->context.u.letx.let) {
                   case LET:     /*
                                  * Here are the rules for a \xlet expression in
                                  * progress:
                                  * 
                                  * \stdbreak
                                  * 
                                  * \stdbreak Function [[transition_explist]]
                                                                       puts v in
                                  * the hole and moves the hole. But if the hole
                                                                           is in
                                  * last position, implementing the \rulename
                                  * Small-Step-Let-Body rule is a bit tricky.
                                                                       Before we
                                  * pop the context \mathbox\xlet(<x_1, v_1, ...
                                                                        , x_n, \
                                  * hole>, e) off the stack, we have to use
                                                                   names \ldotsn
                                  * x to update the environment rho, but after
                                                                      we pop the
                                  * \xlet context, the original rho needs to be
                                                                        saved on
                                  * the stack. I implement this semantics in
                                                                          steps,
                                  * as shown in the numbered comments in the
                                                                           code.
                                  * <continue with [[let]] context
                                                        [[fr->context.u.letx]]>=
                                  */
                                 e = transition_explist(fr->context.u.letx.es, v
                                                                              );
                                 if (e) {         // Small-Step-Next-Let-Exp 
                                     goto exp;
                                 } else {        // Small-Step-Let-Body
                                     Namelist xs  = fr->context.u.letx.xs;
                                                  // 1. Remember x's and v's    
                                     Explist  es  = fr->context.u.letx.es;
                                     Valuelist vs = asLiterals(es);
                                     e = fr->context.u.letx.body;
                                                  // 2. Update e                
                                     popframe(evalstack);
                                                  // 3. Pop the LET context     
                                     pushenv_opt(env, LETXENV, evalstack);
                                                  // 4. Push env                
                                     env = bindalloclist(xs, vs, env);
                                                  // 5. Update env              
                                     freeEL(es);
                                                  // 6. Recover memory          
                                     freeVL(vs);
                                     goto exp;
                                                  // 7. Transition to next state
                                 }
                   case LETSTAR: /*
                                  * With a [[let*]], the hole is always in the
                                                                           first
                                  * position. We don't even have to look for it:
                                                                        Name x_i
                                  * is at the head of [[fr->context.u.letx.xs]].
                                                                         We bind
                                  * it and move on.
                                  * <continue with [[let*]] context
                                                        [[fr->context.u.letx]]>=
                                  */
                                 assert(fr->context.u.letx.xs != NULL && fr->
                                                     context.u.letx.es != NULL);
                                 env = bindalloc(fr->context.u.letx.xs->hd, v,
                                                                           env);
                                 fr->context.u.letx.xs = fr->context.u.letx.xs->
                                                                             tl;
                                 fr->context.u.letx.es = fr->context.u.letx.es->
                                                                             tl;
                                 if (fr->context.u.letx.es) {
                                                  // Small-Step-Next-Letstar-Exp
                                     e = fr->context.u.letx.es->hd;
                                     goto exp;
                                 } else {
                                                      // Small-Step-Letstar-Body
                                     e = fr->context.u.letx.body;
                                     popframe(evalstack);
                                     goto exp;
                                 }
                   case LETREC:  /*
                                  * The expressions in a \xletrec are already
                                                                       evaluated
                                  * in an extended environment, so when the last
                                  * expression is evaluated, the only step
                                                                   needed before
                                  * evaluating the body is to update the store.
                                  * <continue with [[letrec]] context
                                                        [[fr->context.u.letx]]>=
                                  */
                                 e = transition_explist(fr->context.u.letx.es, v
                                                                              );
                                 if (e) {  // Small-Step-Next-Letrec-Exp
                                     goto exp;
                                 } else {  // Small-Step-Letrec-Body
                                     /*
                                      * <store values in
     [[fr->context.u.letx.es]] in locations bound to [[fr->context.u.letx.xs]]>=
                                      */
                                     {
                                         Namelist xs = fr->context.u.letx.xs;
                                         Explist  es = fr->context.u.letx.es;
                                         while (es || xs) { 
                                             assert(es && xs);
                                             assert(find(xs->hd, env));
                                             *find(xs->hd, env) = asLiteral(es->
                                                                            hd);
                                             es = es->tl;
                                             xs = xs->tl;
                                         }
                                     };
                                     freeEL(fr->context.u.letx.es);
                                     e = fr->context.u.letx.body;
                                     popframe(evalstack);
                                     goto exp;
                                 }
                   default:      assert(0);
                }
            case LETXENV:
                /*
                 * The \xlet and \xapply forms both save environments on
                 * the stack. When we encounter a saved environment, we
                 * restore it.
                 * <restore [[env]] from [[fr->context.u.letxenv]], pop the
                                       stack, and transition to the next state>=
                 */
                env = fr->context.u.letxenv;
                popframe(evalstack);
                goto value;
            case CALLENV:
                /*
                 * <restore [[env]] from [[fr->context.u.callenv]], pop the
                                       stack, and transition to the next state>=
                 */
                env = fr->context.u.callenv;
                popframe(evalstack);
                goto value;
            case TRY_CATCH:
                /*
                 * When we see a \xcatch context, the next step depends
                 * on the position of the hole.
                 * <if awaiting handler, install [[v]] and evaluate body,
                          otherwise pop stack and transition to the next state>=
                 */
                if (fr->context.u.try_catch.handler->alt == HOLE) {
                                                            // Try-Catch-Handler
                    if (v.alt != CLOSURE && v.alt != PRIMITIVE) 
                        runerror(
             "Handler in try-catch is %v, but a handler must be a function", v);
                    e = fr->context.u.try_catch.body;
                    popframe(evalstack);
                    pushenv_opt(env, LETXENV, evalstack);
                    pushcontext(mkTryCatchStruct(hole, mkLiteral(v)), evalstack)
                                                                               ;
                    goto exp;
                } else {
                                                             // Try-Catch-Finish
                    assert(fr->context.u.try_catch.body->alt == HOLE);
                    popframe(evalstack);
                    goto value;
                }
            case RETURNX:
                /*
                 * <return [[v]] from the current function ((prototype))>=
                 */
                runerror("Implementation of (return e) is left as an exercise");
            case THROW:
                /*
                 * <throw [[v]] to the nearest [[try-catch]] that has an
                                               installed handler ((prototype))>=
                 */
                runerror("Implementation of (throw e) is left as an exercise");
            case LITERAL:  // syntactic values never appear as contexts
            case VAR:
            case LAMBDAX:
            case HOLE:     // and neither do bare holes
            case BREAKX:   // nor do break or continue
            case CONTINUEX:
                assert(0);
            }
            /*
             * The rest of this section fills in the pieces.
             * We start with the simplest forms, which don't even
             * look at the stack.
             */

        }

        assert(0);
}
/*
 * I've presented only cases involving two \ccallenv
 * frames in sequence, but to get true, proper tail
 * calls in all circumstances, we must also optimize
 * stacks involving any mix of \ccallenv and \cletenv
 * frames in sequence. \exrefschemes.ex.simplify-stack
 * asks you to work out the semantics; here's the code: 
 * [*] [*]
 * <eval-stack.c>=
 */
static int isenv(Exp e) {
    return e && (e->alt == CALLENV || e->alt == LETXENV);
}
                          
void pushenv_opt(Env env, Expalt context, Stack s) {
    if (optimize_tail_calls && topframe(s) && isenv(&topframe(s)->context)) {
        if (context == CALLENV && topframe(s)->context.alt == LETXENV) 
            topframe(s)->context = mkCallenvStruct(topframe(s)->
                                                             context.u.letxenv);
                           /* subtle and quick to anger */
    } else {
        struct Exp e = context == CALLENV ? mkCallenvStruct(env) :
                                                           mkLetxenvStruct(env);
        pushcontext(e, s);
    }
}
