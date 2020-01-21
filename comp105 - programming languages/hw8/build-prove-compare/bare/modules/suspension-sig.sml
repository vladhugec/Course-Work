(* suspension.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
signature SUSPENSION = sig
  type 'a susp
  val delay : (unit -> 'a) -> 'a susp
  val demand : 'a susp -> 'a
end
