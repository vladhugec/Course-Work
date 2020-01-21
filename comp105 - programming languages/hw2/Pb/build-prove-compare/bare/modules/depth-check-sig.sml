(* depth-check.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
signature DEPTH_CHECK = sig
  val applyCheckingOverflow : ('a -> 'b) -> 'a -> 'b
  val resetOverflowCheck    : unit -> unit
end
