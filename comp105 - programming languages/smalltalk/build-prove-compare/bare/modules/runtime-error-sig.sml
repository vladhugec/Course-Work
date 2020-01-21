(* runtime-error.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
signature RUNTIME_ERROR = sig (* XXX rename to COMMON_ERRORS *)
  exception RuntimeError of string
  exception InternalError of string
  type name = string
  val errorIfDups : string * name list * string -> unit
end
