(* suspension-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
structure SuspensionRep :> SUSPENSION_REP = struct
  

(*****************************************************************)
(*                                                               *)
(*   SUSPENSIONS                                                 *)
(*                                                               *)
(*****************************************************************)

(* suspensions 1015a *)
datatype 'a action
  = PENDING  of unit -> 'a
  | PRODUCED of 'a

type 'a susp = 'a action ref
(*unboxval*)
(* suspensions 1015b *)
fun delay f = ref (PENDING f)
fun demand cell =
  case !cell
    of PENDING f =>  let val result = f ()
                     in  (cell := PRODUCED result; result)
                     end
     | PRODUCED v => v
(*unboxval*)
end

structure Suspension :> SUSPENSION where type 'a susp = 'a SuspensionRep.susp = 
struct
  open SuspensionRep
end
