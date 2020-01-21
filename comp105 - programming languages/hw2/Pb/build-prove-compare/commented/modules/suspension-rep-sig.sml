(* <suspension-rep.sig>=                        *)
signature SUSPENSION_REP = sig
  datatype 'a action
    = PENDING  of unit -> 'a
    | PRODUCED of 'a

  type 'a susp = 'a action ref
  val delay : (unit -> 'a) -> 'a susp
  val demand : 'a susp -> 'a
end
