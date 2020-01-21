(* <uscheme-val-utils-module.sml>=              *)
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

(* Embedding and projection                     *)
(*                                              *)
(* A micro-Scheme S-expression can represent an integer, *)
(* Boolean, name, function, list, etc. We may sometimes *)
(* have an ML Boolean, list, or function that we wish to *)
(* represent as an S-expression, or similarly, an *)
(* S-expression that we wish to represent as a value of *)
(* type [[bool]]. Here we define mappings between type  *)
(* [[value]] and some other ML types. Because the set of *)
(* values representable by an ML value of type [[value]] *)
(* strictly contains each of the sets of values *)
(* representable by these ML types, these mappings are *)
(* called embedding and projection. Because the *)
(* [[value]] type is strictly larger than these *)
(* ML types, no embedding operation ever fails, but a *)
(* projection operation might. [This property is a *)
(* general characteristic of any embedding/projection *)
(* pair. A mathematician would say that an embedding e *)
(* of S into S' is an injection from S-->S'. The *)
(* corresponding projection pi_e is a left inverse of *)
(* the embedding; that is pi_e oe is the identity *)
(* function on S. There is no corresponding guarantee *)
(* for e opi_e; for example, pi_e may be undefined (_|_) *)
(* on some elements of S', or e(pi_e(x)) may not equal x *)
(* . ] For example, although any ML function of type *)
(* [[value -> bool]] can be embedded into [[value]] by *)
(* using the [[PRIMITIVE]] constructor, there are values *)
(* of type [[value]] that cannot be projected into an *)
(* ML function of type [[value -> bool]].       *)
(*                                              *)
(* Lists and Booleans are straightforward. [*] [*] *)
(* <utility functions on \uscheme, \tuscheme, and \nml\ values>= *)
fun embedList []     = NIL
  | embedList (h::t) = PAIR (h, embedList t)
fun embedBool b = BOOLV b
fun bool (BOOLV b) = b
  | bool _         = true
(*unboxval*)
(* Function [[bool]] is the projection function, mapping *)
(* micro-Scheme values into ML Booleans. Unlike some *)
(* projection functions, [[bool]] is total: it always *)
(* succeeds. The operational semantics of micro-Scheme *)
(* treats any value other than [[#f]] as a true value, *)
(* so by projecting every non-Boolean micro-Scheme value *)
(* to [[true]], [[bool]] reflects the semantics. *)

(* Equality                                     *)
(*                                              *)
(* The interpreter uses equality in two places: in the *)
(* [[=]] primitive and in the [[check-expect]] unit *)
(* test. The primitive version permits only atoms to be *)
(* considered equal.                            *)
(* <utility functions on \uscheme, \tuscheme, and \nml\ values>= *)
fun equalatoms (NIL,      NIL    )  = true
  | equalatoms (NUM  n1,  NUM  n2)  = (n1 = n2)
  | equalatoms (SYM  v1,  SYM  v2)  = (v1 = v2)
  | equalatoms (BOOLV b1, BOOLV b2) = (b1 = b2)
  | equalatoms  _                   = false
(*unboxval*)
(* In a unit test written with [[check-expect]], lists *)
(* are compared for equality structurally, the way the *)
(* micro-Scheme function [[equal?]] does.       *)

(* <utility functions on \uscheme, \tuscheme, and \nml\ values>= *)
fun equalpairs (PAIR (car1, cdr1), PAIR (car2, cdr2)) =
      equalpairs (car1, car2) andalso equalpairs (cdr1, cdr2)
  | equalpairs (v1, v2) = equalatoms (v1, v2)
(*unboxval*)
(* The testing infrastructure expects this function to *)
(* be called [[testEqual]].                     *)

(* <utility functions on \uscheme, \tuscheme, and \nml\ values>= *)
val testEqual = equalpairs
(*unboxval*)
(* Unspecified values                           *)
(*                                              *)
(* In a [[val]] or [[letrec]] binding, the operational *)
(* semantics of micro-Scheme call for the allocation of *)
(* a location containing an unspecified value. My C code *)
(* chooses a value at random, but the initial basis of *)
(* Standard ML has no random-number generator. So unlike *)
(* the C [[unspecified]] function in \chunkref  *)
(* schemea.chunk.unspecified, the ML version just cycles *)
(* through a few different values. It's enough to *)
(* prevent careless people from assuming that such a *)
(* value is always [[NIL]].                     *)
(* <utility functions on \uscheme, \tuscheme, and \nml\ values>= *)
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
