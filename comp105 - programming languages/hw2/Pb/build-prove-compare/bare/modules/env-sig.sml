(* env.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
signature ENV = sig
  type 'a env
  type name
  exception NotFound of name
  val emptyEnv : 'a env
  val find : name * 'a env -> 'a (* raises NotFound *)
  val bind : name * 'a * 'a env -> 'a env

  exception BindListLength
  val bindList : name list * 'a list * 'a env -> 'a env

  val duplicatename : name list -> name option
end
