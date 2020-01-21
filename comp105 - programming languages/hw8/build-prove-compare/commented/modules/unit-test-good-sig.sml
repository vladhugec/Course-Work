(* <unit-test-good.sig>=                        *)
signature UNIT_TEST_GOOD = sig
  type basis
  type unit_test
  val testIsGood : unit_test * basis -> bool (* failure prints to stderr *)
end
