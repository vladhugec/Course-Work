(* unit-test-report.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
signature UNIT_TEST_REPORT = sig
  val failtest : string list -> bool
  val reportTestResultsOf : string -> int * int -> unit
  val reportTestResults   :           int * int -> unit
end
