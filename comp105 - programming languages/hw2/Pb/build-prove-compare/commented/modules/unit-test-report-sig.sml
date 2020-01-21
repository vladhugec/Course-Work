(* <unit-test-report.sig>=                      *)
signature UNIT_TEST_REPORT = sig
  val failtest : string list -> bool
  val reportTestResultsOf : string -> int * int -> unit
  val reportTestResults   :           int * int -> unit
end
