(* <val-utils.sig>=                             *)
signature VAL_UTILS = sig
  type value
  val embedList : value list -> value
  val embedBool : bool -> value  (* XXX two names for this function *)
  val bool : value -> bool
  val valueString : value -> string
end 
