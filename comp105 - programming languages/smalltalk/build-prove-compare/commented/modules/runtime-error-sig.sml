(* I must not confuse [[InternalError]] with    *)
(* [[RuntimeError]]. When the interpreter raises *)
(* [[RuntimeError]], it means that a user's program got *)
(* stuck: evaluation led to a state in which the *)
(* operational semantics couldn't make progress. *)
(* The fault is the user's. But when the interpreter *)
(* raises [[InternalError]], it means there is a fault *)
(* in my code; the user's program is blameless. *)
(* <runtime-error.sig>=                         *)
signature RUNTIME_ERROR = sig (* XXX rename to COMMON_ERRORS *)
  exception RuntimeError of string
  exception InternalError of string
  type name = string
  val errorIfDups : string * name list * string -> unit
end
