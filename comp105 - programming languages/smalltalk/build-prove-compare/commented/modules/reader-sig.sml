(* Streams that lex, parse, and prompt          *)
(*                                              *)
(* In this final section I pull together all the *)
(* machinery needed to take a stream of input lines, *)
(* a lexer, and a parser, and to produce a stream of *)
(* high-level syntactic objects like definitions. With *)
(* prompts! This code is where prompts get determined, *)
(* where errors are handled, and where special tagged *)
(* lines are copied to the output to support testing. *)
(* <reader.sig>=                                *)
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
