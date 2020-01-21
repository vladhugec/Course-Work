(* unit-tests-process.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
signature UNIT_TESTS_PROCESS = sig
  type unit_test
  type basis
  val processTests : unit_test list * basis -> unit
end    
