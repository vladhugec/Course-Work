(* <unit-tests-all-module.sml>=                 *)
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

(* Function [[processTests]] is shared among all bridge *)
(* languages. For each test, it calls the       *)
(* language-dependent [[testIsGood]], adds up the number *)
(* of good tests, and reports the result. [*]   *)
(* <shared definition of [[processTests]]>=     *)
fun numberOfGoodTests (tests, rho) =
  foldr (fn (t, n) => if testIsGood (t, rho) then n + 1 else n) 0 tests
fun processTests (tests, rho) =
      reportTestResults (numberOfGoodTests (tests, rho), length tests)
(*unboxval*)
end
