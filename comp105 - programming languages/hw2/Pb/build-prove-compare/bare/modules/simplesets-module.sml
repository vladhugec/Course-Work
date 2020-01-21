(* simplesets-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
structure Listsets :> SIMPLESETS where type 'a set = 'a list = struct
  

(*****************************************************************)
(*                                                               *)
(*   SIMPLE IMPLEMENTATIONS OF SET OPERATIONS                    *)
(*                                                               *)
(*****************************************************************)

(* simple implementations of set operations 1006a *)
type 'a set = 'a list
val emptyset = []
fun member x = 
  List.exists (fn y => y = x)
fun insert (x, ys) = 
  if member x ys then ys else x::ys
fun union (xs, ys) = foldl insert ys xs
fun inter (xs, ys) =
  List.filter (fn x => member x ys) xs
fun diff  (xs, ys) = 
  List.filter (fn x => not (member x ys)) xs
(*unboxval*)
end
