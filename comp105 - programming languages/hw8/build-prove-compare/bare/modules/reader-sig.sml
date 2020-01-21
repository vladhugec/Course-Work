(* reader.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
(* XXX this has become lumped in with parsing stuff.
   Perhaps it ought to be split out? *) 
signature READER = sig
  type token

  type line = string
  type srcloc
  type 'a stream
  type 'a lexer
  type 'a parser
  type ('a, 'b) xformer
  type 'a eol_marked
  type 'a located = srcloc * 'a
  type 'a error = 'a Error.error

  val echoTagStream : line stream -> line stream 
  val stripAndReportErrors : 'a error stream -> 'a stream
  val lexLineWith : token lexer -> line -> token stream
  val parseWithErrors : 'a parser -> token located eol_marked stream -> 'a error
                                                                          stream

  type prompts
  val stdPrompts : prompts
  val noPrompts : prompts

  val interactiveParsedStream : 't lexer * ('t located eol_marked, 'a) xformer
                                  -> string * line stream * prompts -> 'a stream
end
