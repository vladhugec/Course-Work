(* <uscheme-unit-test-display-module.sml>=      *)
structure UschemeUnitTestDisplay :> sig 
  type exp
  type value
  val expString : exp -> string
end
  where type exp = UschemeSyntax.exp
    and type value = UschemeSyntax.value
      =
struct
  open Error
  open UschemeSyntax
  open UschemeValUtils

  

(*****************************************************************)
(*                                                               *)
(*   DEFINITION OF [[EXPSTRING]] FOR \USCHEME                    *)
(*                                                               *)
(*****************************************************************)

(* To print information about a failed test, we need *)
(* function [[expString]].                      *)
(* <definition of [[expString]] for \uscheme>=  *)
fun expString e =
  let fun bracket s = "(" ^ s ^ ")"
      val bracketSpace = bracket o spaceSep
      fun exps es = map expString es
      fun withBindings (keyword, bs, e) =
        bracket (spaceSep [keyword, bindings bs, expString e])
      and bindings bs = bracket (spaceSep (map binding bs))
      and binding (x, e) = bracket (x ^ " " ^ expString e)
      val letkind = fn LET => "let" | LETSTAR => "let*" | LETREC => "letrec"
  in  case e
        of LITERAL (v as NUM   _) => valueString v
         | LITERAL (v as BOOLV _) => valueString v
         | LITERAL v => "'" ^ valueString v
         | VAR name => name
         | SET (x, e) => bracketSpace ["set", x, expString e]
         | IFX (e1, e2, e3) => bracketSpace ("if" :: exps [e1, e2, e3])
         | WHILEX (cond, body) =>
                         bracketSpace ["while", expString cond, expString body]
         | BEGIN es => bracketSpace ("begin" :: exps es)
         | APPLY (e, es) => bracketSpace (exps (e::es))
         | LETX (lk, bs, e) => bracketSpace [letkind lk, bindings bs, expString
                                                                              e]
         | LAMBDA (xs, body) => bracketSpace ["lambda", bracketSpace xs,
                                                                 expString body]
  end
end
