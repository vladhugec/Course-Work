(* schemelike-parsers.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
signature SCHEMELIKE_PARSERS = sig
  type 'a parser
  type value 
  type exp
  type name = string
  val sexp : value parser
  val fullSchemeExpOf : exp parser -> (exp parser -> exp parser) -> exp parser
                             (* atomic, non-atomic, whole *)
  val atomicSchemeExpOf : name parser -> exp parser
end

signature COMMON_SYNTAX = sig
  type value
  type exp
  type name = string

  val NUM     : int -> value
  val SYM     : string -> value

  val LITERAL : value -> exp
  val VAR     : name -> exp
  val APPLY   : exp * exp list -> exp
end
