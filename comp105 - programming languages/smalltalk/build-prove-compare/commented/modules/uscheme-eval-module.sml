(* <uscheme-eval-module.sml>=                   *)
functor MkUschemeEval (structure Env : ENV where type name = string
                                             and type 'a env = 'a
                                                               UschemeSyntax.env
                      ) :>
  EVAL where type exp = UschemeSyntax.exp
         and type def = UschemeSyntax.def
         and type value = UschemeSyntax.value
         and type basis = UschemeSyntax.value ref Env.env
  =
struct
  open Curry
  open DepthCheck
  open Env
  open Interactivity
  open RuntimeError
  open UschemeSyntax
  open UschemeValUtils
  type basis = value ref env
  

(*****************************************************************)
(*                                                               *)
(*   DEFINITIONS OF [[EVAL]], [[EVALDEF]], [[BASIS]], AND [[PROCESSDEF]] FOR \USCHEME *)
(*                                                               *)
(*****************************************************************)

(* Evaluation                                   *)
(*                                              *)
(* The machinery above is enough to write the evaluator, *)
(* which takes an expression and an environment and *)
(* produces a value. To make the evaluator easy to *)
(* write, I do most of the work of evaluation in the *)
(* nested function [[ev]], which inherits the   *)
(* environment [[rho]] from the outer function [[eval]]. *)
(* Most AST nodes are evaluated in the same environment *)
(* as their parents, and each such node is evaluated by *)
(* passing it to [[ev]], which lets [[rho]] be implicit. *)
(* The first case of [[ev]] is the evaluation of a *)
(* literal value [[v]], which evaluates to itself. \ *)
(* mlsflabeleval                                *)
(* <definitions of [[eval]], [[evaldef]], [[basis]], and [[processDef]] for \uscheme>= *)
fun eval (e, rho) =
  let val go = applyCheckingOverflow id in go end (* OMIT *)
  let fun ev (LITERAL v) = v
        (* To evaluate [[VAR]] and [[SET]], we need environment *)
        (* lookup. The environment [[rho]] binds each name to a *)
        (* mutable ref cell, which is the ML analogs of a *)
        (* pointer to a location allocated on the heap. In , *)
        (* locations are read and written using special pointer *)
        (* syntax (the [[*]] operator), but in ML, we read and *)
        (* write locations using the functions [[!]] and [[:=]], *)
        (* which are in the initial basis of Standard ML. *)
        (* <more alternatives for [[ev]] for \uscheme>= *)
        | ev (VAR x) = !(find (x, rho))
        | ev (SET (x, e)) = 
            let val v = ev e
            in  find (x, rho) := v;
                v
            end
        (* The right-hand side of [[SET]], here called [[e]], is *)
        (* evaluated in the same environment as the [[SET]], so *)
        (* I evaluate it using [[ev]].                  *)
        (*                                              *)

        (* To evaluate [[IF]] and [[WHILE]], we must interpret a *)
        (* micro-Scheme value as a Boolean. That is the job of *)
        (* the projection function [[bool]].            *)
        (* <more alternatives for [[ev]] for \uscheme>= *)
        | ev (IFX (e1, e2, e3)) = ev (if bool (ev e1) then e2 else e3)
        | ev (WHILEX (guard, body)) = 
            if bool (ev guard) then 
              (ev body; ev (WHILEX (guard, body)))
            else
              NIL
        (* To evaluate [[BEGIN]], I define an auxiliary function *)
        (* [[b]], which uses an accumulating parameter  *)
        (* [[lastval]] to remember the value of the last *)
        (* expression. By initializing [[lastval]] to [[false]], *)
        (* I handle the possibility of an empty [[begin]]. *)
        (* <more alternatives for [[ev]] for \uscheme>= *)
        | ev (BEGIN es) =
            let fun b (e::es, lastval) = b (es, ev e)
                  | b (   [], lastval) = lastval
            in  b (es, BOOLV false)
            end
        (* To evaluate [[LAMBDA]], I capture a closure, which is *)
        (* as simple as in C. [*]                       *)
        (* <more alternatives for [[ev]] for \uscheme>= *)
        | ev (LAMBDA (xs, e)) = ( errorIfDups ("formal parameter", xs, "lambda")
                                ; CLOSURE ((xs, e), rho)
                                )
        (* To evaluate an application, I begin by evaluating the *)
        (* expression [[f]] that is in function position. What's *)
        (* next depends on whether [[f]] evaluates to a *)
        (* primitive, a closure, or something else. To apply a *)
        (* primitive function, as in C, apply it to the syntax  *)
        (* [[e]] and to the values of the arguments.    *)
        (* <more alternatives for [[ev]] for \uscheme>= *)
        | ev (e as APPLY (f, args)) = 
               (case ev f
                  of PRIMITIVE prim => prim (e, map ev args)
                   | CLOSURE clo    =>
                        (* Applying a closure is more interesting. To apply a *)

                          (* micro-Scheme closure correctly, I have to create *)

                          (* fresh locations to hold the values of the actual *)

                       (* parameters. In C, we used the function [[allocate]] *)

                            (* for this purpose; in ML, the built-in function *)

                        (* [[ref]] does the same thing: create a new location *)

                             (* and initialize its contents with a value. The *)

                       (* ML expression \monoboxmap ref actuals does half the *)

                      (* work of C's [[bindalloclist]]; function [[bindList]] *)

                           (* does the other half. \mdbuseschemebindalloclist *)

                         (* <apply closure [[clo]] to [[args]] ((mlscheme))>= *)
                                       let val ((formals, body), savedrho) = clo
                                           val actuals = map ev args
                                       in  eval (body, bindList (formals, map
                                                         ref actuals, savedrho))
                                           handle BindListLength => 
                                               raise RuntimeError (
                                      "Wrong number of arguments to closure; " ^
                                                                   "expected ("
                                                       ^ spaceSep formals ^ ")")
                                       end
                   | v => raise RuntimeError ("Applied non-function " ^
                                                                  valueString v)
               )
        (* This code uses a nice feature of ML pattern matching: *)
        (* in a pattern match, the keyword [[as]] helps name the *)
        (* whole value being matched. The pattern \monoboxe as *)
        (* APPLY (f, args) means ``match this pattern if the *)
        (* given argument is an [[APPLY]] node, call the *)
        (* children of the node [[f]] and [[args]], and call the *)
        (* entire node [[e]].''                         *)

        (* To evaluate [[LET]], it is easiest to unzip the list *)
        (* of pairs [[bs]] into a pair of lists [[(names, *)
        (* values)]]. I then use [[map]] to apply both [[ref]] *)
        (* and [[ev]] to each value. To evaluate [[LETSTAR]], *)
        (* by contrast, it is easier to walk the bindings one *)
        (* pair at a time. The function [[ListPair.unzip]] is *)
        (* from the [[ListPair]] module in the Standard Basis *)
        (* Library.                                     *)
        (* <more alternatives for [[ev]] for \uscheme>= *)
        | ev (LETX (LET, bs, body)) =
            let val (names, values) = ListPair.unzip bs
                val _ = errorIfDups ("bound name", names, "let")
        (*unboxval*)
            in  eval (body, bindList (names, map (ref o ev) values, rho))
            end
        | ev (LETX (LETSTAR, bs, body)) =
            let fun step ((n, e), rho) = bind (n, ref (eval (e, rho)), rho)
            in  eval (body, foldl step rho bs)
            end
        (* To evaluate [[LETREC]], I first operate separately on *)
        (* the names and values to build the new environment *)
        (* [[rho']]. I then use [[map]] on [[bs]] to get a list *)
        (* of updates, use [[List.app]] to mutate one cell for *)
        (* each update, and finally evaluate the body. Function *)
        (* [[List.app]] applies a function for side effect. *)
        (* <more alternatives for [[ev]] for \uscheme>= *)
        | ev (LETX (LETREC, bs, body)) =
            let val (names, values) = ListPair.unzip bs
                val _ = errorIfDups ("bound name", names, "letrec")
                val rho' = bindList (names, map (fn _ => ref (unspecified()))
                                                                    values, rho)
                val updates = map (fn (n, e) => (n, eval (e, rho'))) bs
        (*unboxval*)
            in  List.app (fn (n, v) => find (n, rho') := v) updates; 
                eval (body, rho')
            end
(*unboxval*)
  in  ev e
  end
(* Functions [[eval]] and [[ev]] are mutually recursive. *)

(* When a definition introduces a new name, that *)
(* definition is evaluated in an environment that *)
(* already includes the name being defined. If the name *)
(* is not already bound, it is bound to a fresh location *)
(* that is initialized with an unspecified value. *)
(* <definitions of [[eval]], [[evaldef]], [[basis]], and [[processDef]] for \uscheme>= *)
fun withNameBound (x, rho) =
  (find (x, rho); rho)
  handle NotFound _ => bind (x, ref (unspecified ()), rho)
(*unboxval*)  
(* Given a [[val]] binding to name x, [[evaldef]] first *)
(* uses [[withNameBound]] to make sure x is bound to a *)
(* location in the environment. It then evaluates the *)
(* right-hand side and stores the new value in x's *)
(* location. The interpreter's response is usually the *)
(* value, but if the definition binds a [[lambda]] *)
(* expression, the interpreter instead responds with the *)
(* name x. As in \crefscheme.chap, [[define]] is *)
(* syntactic sugar for [[val]] with [[lambda]]. *)
(*                                              *)
(* The [[EXP]] form doesn't bind a name; [[evaldef]] *)
(* just evaluates the expression, binds the result to  *)
(* [[it]], and responds with the value. \mlsflabel *)
(* evaldef                                      *)
(* <definitions of [[eval]], [[evaldef]], [[basis]], and [[processDef]] for \uscheme>= *)
fun evaldef (VAL (x, e), rho) =
      let val rho = withNameBound (x, rho)
          val v   = eval (e, rho)
          val _   = find (x, rho) := v
          val response = case e of LAMBDA _ => x
                                 | _ => valueString v
      in  (rho, response)
      end
  | evaldef (EXP e, rho) =        
      let val v   = eval (e, rho)
          val rho = withNameBound ("it", rho)
          val _   = find ("it", rho) := v
      in  (rho, valueString v)
      end
  | evaldef (DEFINE (f, lambda), rho) =
      let val (xs, e) = lambda
          val _ = errorIfDups ("formal parameter", xs, "definition of function "
                                                                            ^ f)
      in  evaldef (VAL (f, LAMBDA lambda), rho)
      end
(*unboxval*)
(* The differences between [[VAL]] and [[EXP]] are *)
(* subtle: for [[VAL]], the rules of micro-Scheme *)
(* require that we add the name to environment [[rho]] *)
(* before evaluating expression [[e]]. For [[EXP]], we *)
(* don't bind the name [[it]] until after evaluating the *)
(* first top-level expression.                  *)

(* The language-dependent [[basis]] is, for     *)
(* micro-Scheme, the single environment rho, which maps *)
(* each name to a mutable location that holds a value. *)
(* Function [[processDef]] calls [[evaldef]], prints its *)
(* response, and returns its environment.       *)
(* <definitions of [[eval]], [[evaldef]], [[basis]], and [[processDef]] for \uscheme>= *)
type basis = value ref env
fun processDef (d, rho, interactivity) =
  let val (rho', response) = evaldef (d, rho)
      val _ = if prints interactivity then println response else ()
  in  rho'
  end
end
