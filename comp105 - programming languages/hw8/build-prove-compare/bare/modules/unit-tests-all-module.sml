(* unit-tests-all-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
functor MkUnitTests(structure Test : UNIT_TEST_GOOD
                   ) :> UNIT_TESTS_PROCESS
  where type unit_test = Test.unit_test
    and type basis = Test.basis
      =
struct
  open Test
  open UnitTestReport
  

(*****************************************************************)
(*                                                               *)
(*   SHARED DEFINITION OF [[PROCESSTESTS]]                       *)
(*                                                               *)
(*****************************************************************)

(* shared definition of [[processTests]] 1013a *)
fun numberOfGoodTests (tests, rho) =
  foldr (fn (t, n) => if testIsGood (t, rho) then n + 1 else n) 0 tests
fun processTests (tests, rho) =
      reportTestResults (numberOfGoodTests (tests, rho), length tests)
(*unboxval*)
end
