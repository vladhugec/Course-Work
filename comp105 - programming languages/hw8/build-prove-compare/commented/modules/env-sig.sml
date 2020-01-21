(* <env.sig>=                                   *)
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
(* Functions [[bind]] and [[bindList]] are defined by *)
(* pattern matching on lists; the infix [[::]] is ML's *)
(* way of writing [[cons]], and [[[]]] is the empty *)
(* list. And when these functions detect an error, they *)
(* don't call a [[runerror]] function; instead, they *)
(* raise an exception. The exceptions I use are listed *)
(* in Table [->].                               *)

