(* unit-test-good.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
signature UNIT_TEST_GOOD = sig
  type basis
  type unit_test
  val testIsGood : unit_test * basis -> bool (* failure prints to stderr *)
end
