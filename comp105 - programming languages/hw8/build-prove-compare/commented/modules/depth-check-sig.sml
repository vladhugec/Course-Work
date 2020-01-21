(* <depth-check.sig>=                           *)
signature DEPTH_CHECK = sig
  val applyCheckingOverflow : ('a -> 'b) -> 'a -> 'b
  val resetOverflowCheck    : unit -> unit
end
