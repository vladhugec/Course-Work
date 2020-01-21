(* <uscheme-val-utils.sig>=                     *)
signature USCHEME_VAL_UTILS = sig
  include VAL_UTILS
  val equalatoms : value * value -> bool
  val equalpairs : value * value -> bool
  val testEqual  : value * value -> bool
  val unspecified : unit -> value
end
