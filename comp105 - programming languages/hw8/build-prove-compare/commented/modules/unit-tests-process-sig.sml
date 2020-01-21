(* <unit-tests-process.sig>=                    *)
signature UNIT_TESTS_PROCESS = sig
  type unit_test
  type basis
  val processTests : unit_test list * basis -> unit
end    
