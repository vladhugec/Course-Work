(* uscheme-val-utils-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
structure UschemeValUtils :> USCHEME_VAL_UTILS where type value =
                                                           UschemeSyntax.value =
struct
  type value = UschemeSyntax.value
  open RuntimeError
  open UschemeSyntax
  

(*****************************************************************)
(*                                                               *)
(*   UTILITY FUNCTIONS ON \USCHEME, \TUSCHEME, AND \NML\ VALUES  *)
(*                                                               *)
(*****************************************************************)

(* utility functions on \uscheme, \tuscheme, and \nml\ values 362a *)
fun embedList []     = NIL
  | embedList (h::t) = PAIR (h, embedList t)
fun embedBool b = BOOLV b
fun bool (BOOLV b) = b
  | bool _         = true
(*unboxval*)
(* utility functions on \uscheme, \tuscheme, and \nml\ values 363a *)
fun equalatoms (NIL,      NIL    )  = true
  | equalatoms (NUM  n1,  NUM  n2)  = (n1 = n2)
  | equalatoms (SYM  v1,  SYM  v2)  = (v1 = v2)
  | equalatoms (BOOLV b1, BOOLV b2) = (b1 = b2)
  | equalatoms  _                   = false
(*unboxval*)
(* utility functions on \uscheme, \tuscheme, and \nml\ values 363b *)
fun equalpairs (PAIR (car1, cdr1), PAIR (car2, cdr2)) =
      equalpairs (car1, car2) andalso equalpairs (cdr1, cdr2)
  | equalpairs (v1, v2) = equalatoms (v1, v2)
(*unboxval*)
(* utility functions on \uscheme, \tuscheme, and \nml\ values 363c *)
val testEqual = equalpairs
(*unboxval*)
(* utility functions on \uscheme, \tuscheme, and \nml\ values 1124 *)
fun cycleThrough xs =
  let val remaining = ref xs
      fun next () = case !remaining
                      of [] => (remaining := xs; next ())
                       | x :: xs => (remaining := xs; x)
  in  if null xs then
        raise InternalError "empty list given to cycleThrough"
      else
        next
  end
val unspecified =
  cycleThrough [BOOLV true, NUM 39, SYM "this value is unspecified", NIL,
                PRIMITIVE (fn _ => let exception Unspecified in raise
                                                               Unspecified end)]
(*unboxval*)
end
