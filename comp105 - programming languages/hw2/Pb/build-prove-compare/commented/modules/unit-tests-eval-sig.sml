(* <unit-tests-eval.sig>=                       *)
signature UNIT_TESTS_EVAL = sig
  (* unit tests based on evaluation *)
  (* XXX signature can't be instantiated because of nesting *)
  type exp
  type basis
  val checkExpectPasses : exp * exp * basis -> bool
                                                  (* failure prints to stderr *)
  val checkErrorPasses  : exp * basis -> bool
                                                  (* failure prints to stderr *)
end
