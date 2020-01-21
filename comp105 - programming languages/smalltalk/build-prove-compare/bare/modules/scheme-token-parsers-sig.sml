(* scheme-token-parsers.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
signature SCHEME_TOKEN_PARSERS = sig
  type 'a parser
(*  val pretoken  : pretoken parser *)
  val quote     : unit parser
  val int       : int parser
  val booltok   : bool parser
  val name      : string parser
  val any_name  : string parser
end
