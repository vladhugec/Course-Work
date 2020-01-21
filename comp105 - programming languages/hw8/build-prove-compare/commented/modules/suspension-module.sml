(* <suspension-module.sml>=                     *)
structure SuspensionRep :> SUSPENSION_REP = struct
  

(*****************************************************************)
(*                                                               *)
(*   SUSPENSIONS                                                 *)
(*                                                               *)
(*****************************************************************)

(* To implement suspensions, I use a standard   *)
(* combination of imperative and functional code. *)
(* A suspension is a reference to an [[action]], which *)
(* can be pending or can have produced a result. *)
(* <suspensions>=                               *)
datatype 'a action
  = PENDING  of unit -> 'a
  | PRODUCED of 'a

type 'a susp = 'a action ref
(*unboxval*)
(* Functions [[delay]] and [[demand]] convert to and *)
(* from suspensions.                            *)
(* <suspensions>=                               *)
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
