(* <suspension.sig>=                            *)
signature SUSPENSION = sig
  type 'a susp
  val delay : (unit -> 'a) -> 'a susp
  val demand : 'a susp -> 'a
end
