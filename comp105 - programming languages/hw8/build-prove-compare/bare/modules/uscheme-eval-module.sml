(* uscheme-eval-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
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

(* definitions of [[eval]], [[evaldef]], [[basis]], and [[processDef]] for \uscheme 364a *)
fun eval (e, rho) =
  let val go = applyCheckingOverflow id in go end (* OMIT *)
  let fun ev (LITERAL v) = v
        (* more alternatives for [[ev]] for \uscheme 364b *)
        | ev (VAR x) = !(find (x, rho))
        | ev (SET (x, e)) = 
            let val v = ev e
            in  find (x, rho) := v;
                v
            end
        (* more alternatives for [[ev]] for \uscheme 364c *)
        | ev (IFX (e1, e2, e3)) = ev (if bool (ev e1) then e2 else e3)
        | ev (WHILEX (guard, body)) = 
            if bool (ev guard) then 
              (ev body; ev (WHILEX (guard, body)))
            else
              NIL
        (* more alternatives for [[ev]] for \uscheme 365a *)
        | ev (BEGIN es) =
            let fun b (e::es, lastval) = b (es, ev e)
                  | b (   [], lastval) = lastval
            in  b (es, BOOLV false)
            end
        (* more alternatives for [[ev]] for \uscheme 365b *)
        | ev (LAMBDA (xs, e)) = ( errorIfDups ("formal parameter", xs, "lambda")
                                ; CLOSURE ((xs, e), rho)
                                )
        (* more alternatives for [[ev]] for \uscheme 365c *)
        | ev (e as APPLY (f, args)) = 
               (case ev f
                  of PRIMITIVE prim => prim (e, map ev args)
                   | CLOSURE clo    =>
                       (* apply closure [[clo]] to [[args]] ((mlscheme)) 365d *)
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
        (* more alternatives for [[ev]] for \uscheme 366a *)
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
        (* more alternatives for [[ev]] for \uscheme 366b *)
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
(* definitions of [[eval]], [[evaldef]], [[basis]], and [[processDef]] for \uscheme 369c *)
fun withNameBound (x, rho) =
  (find (x, rho); rho)
  handle NotFound _ => bind (x, ref (unspecified ()), rho)
(*unboxval*)  
(* definitions of [[eval]], [[evaldef]], [[basis]], and [[processDef]] for \uscheme 369d *)
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
(* definitions of [[eval]], [[evaldef]], [[basis]], and [[processDef]] for \uscheme 373a *)
type basis = value ref env
fun processDef (d, rho, interactivity) =
  let val (rho', response) = evaldef (d, rho)
      val _ = if prints interactivity then println response else ()
  in  rho'
  end
end
