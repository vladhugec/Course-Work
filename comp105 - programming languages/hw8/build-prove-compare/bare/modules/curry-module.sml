(* curry-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
structure Curry :> CURRY = struct
  

(*****************************************************************)
(*                                                               *)
(*   FOR WORKING WITH CURRIED FUNCTIONS: [[ID]], [[FST]], [[SND]], [[PAIR]], [[CURRY]], AND [[CURRY3]] *)
(*                                                               *)
(*****************************************************************)

(* for working with curried functions: [[id]], [[fst]], [[snd]], [[pair]], [[curry]], and [[curry3]] 1029c *)
fun id x = x
fun fst (x, y) = x
fun snd (x, y) = y
fun pair x y = (x, y)
fun curry  f x y   = f (x, y)
fun curry3 f x y z = f (x, y, z)
(*unboxval*)
end
