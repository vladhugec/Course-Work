(* <curry-module.sml>=                          *)
structure Curry :> CURRY = struct
  

(*****************************************************************)
(*                                                               *)
(*   FOR WORKING WITH CURRIED FUNCTIONS: [[ID]], [[FST]], [[SND]], [[PAIR]], [[CURRY]], AND [[CURRY3]] *)
(*                                                               *)
(*****************************************************************)

(* There are a variety of ways to create useful *)
(* functions in the [[f]] position. Many such functions *)
(* are Curried. Here are some of them.          *)
(* <for working with curried functions: [[id]], [[fst]], [[snd]], [[pair]], [[curry]], and [[curry3]]>= *)
fun id x = x
fun fst (x, y) = x
fun snd (x, y) = y
fun pair x y = (x, y)
fun curry  f x y   = f (x, y)
fun curry3 f x y z = f (x, y, z)
(*unboxval*)
end
