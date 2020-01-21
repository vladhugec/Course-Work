(* <eval.sig>=                                  *)
signature EVAL_EXP = sig
  type exp
  type def
  type value
  type basis
  val eval    : exp * basis -> value
  val evaldef : def * basis -> basis * string
end

signature EVAL = sig
  include EVAL_EXP
  type interactivity = Interactivity.interactivity
  val processDef : def * basis * interactivity -> basis
end

(*
signature EVAL_TYPED = sig
  include 
  type type_env
  val elabEvalDef : Echo.echo -> def * (type_env * basis) -> (type_env * basis)
end
*)
