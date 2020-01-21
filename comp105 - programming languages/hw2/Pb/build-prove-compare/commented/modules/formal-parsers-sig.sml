(* <formal-parsers.sig>=                        *)
signature FORMAL_PARSERS = sig
  type name = string
  type 'a parser
  val formalsOf  : string -> name parser -> string -> name list parser
  val bindingsOf : string -> 'x parser -> 'e parser -> ('x * 'e) list parser
  val distinctBsIn : (name * 'e) list parser -> string -> (name * 'e) list
                                                                          parser
  val kw : string -> string parser
  val usageParsers : (string * 'a parser) list -> 'a parser
  val recordFieldsOf : name parser -> name list parser
end
