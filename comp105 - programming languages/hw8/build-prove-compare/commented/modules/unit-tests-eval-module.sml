(* <unit-tests-eval-module.sml>=                *)
(* XXX this can't work because of nesting *)
functor MkUnitTestsEval(structure Eval : EVAL
                        val testEqual : Eval.value * Eval.value -> bool
                       )
  :> sig end (* XXX do not use me *)
  =
struct
  open Eval
  open Error
end                       
