(* listutil-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
structure Listutil :> LISTUTIL = struct
  

(*****************************************************************)
(*                                                               *)
(*   LIST FUNCTIONS NOT PROVIDED BY \SML'S INITIAL BASIS         *)
(*                                                               *)
(*****************************************************************)

(* list functions not provided by \sml's initial basis 1007b *)
fun zip3 ([], [], []) = []
  | zip3 (x::xs, y::ys, z::zs) = (x, y, z) :: zip3 (xs, ys, zs)
  | zip3 _ = raise ListPair.UnequalLengths

fun unzip3 [] = ([], [], [])
  | unzip3 (trip::trips) =
      let val (x,  y,  z)  = trip
          val (xs, ys, zs) = unzip3 trips
      in  (x::xs, y::ys, z::zs)
      end
(* list functions not provided by \sml's initial basis 1007c *)
val reverse = rev
(* list functions not provided by \sml's initial basis 1007d *)
fun optionList [] = SOME []
  | optionList (NONE :: _) = NONE
  | optionList (SOME x :: rest) =
      (case optionList rest
         of SOME xs => SOME (x :: xs)
          | NONE    => NONE)
end
