(* collection-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
structure Collection :> COLLECTION = struct
  open Listsets
  

(*****************************************************************)
(*                                                               *)
(*   COLLECTIONS WITH MAPPING AND COMBINING FUNCTIONS            *)
(*                                                               *)
(*****************************************************************)

(* collections with mapping and combining functions 1006b *)
datatype 'a collection = C of 'a set
fun elemsC (C xs) = xs
fun singleC x     = C [x]
val emptyC        = C []
(*unboxval*)
(* collections with mapping and combining functions 1007a *)
fun joinC     (C xs) = C (List.concat (map elemsC xs))
fun mapC  f   (C xs) = C (map f xs)
fun filterC p (C xs) = C (List.filter p xs)
fun mapC2 f (xc, yc) = joinC (mapC (fn x => mapC (fn y => f (x, y)) yc) xc)
(*unboxval*)
end
