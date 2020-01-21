(* <parser.sig>=                                *)
signature PARSER = sig  (* XXX change to PARSER_UTILS *)
  type token

  type ('a, 'b) xformer
  structure Lex : LEXER_UTILS where type ('a, 'b) xformer = ('a, 'b) xformer


  datatype 'a eol_marked
    = EOL of int (* number of the line that ends here *)
    | INLINE of 'a

  type srcloc = string * int
  type 'a located = srcloc * 'a
  type 'a parser = (token located eol_marked, 'a) xformer

  val eol      : ('a eol_marked, int) xformer
 (*  val eol_marked   : ('a eol_marked, 'a)  xformer *)

  type ('t, 'a) polyparser = ('t located eol_marked, 'a) xformer 
  type ('t, 'a) pb_parser = ('t Lex.plus_brackets, 'a) polyparser

  val token    : ('t, 't)   polyparser
  val noTokens : ('t, unit) polyparser
  val @@  : ('t, 'a) polyparser -> ('t, 'a located) polyparser
  val <?> : ('t, 'a) polyparser * string -> ('t, 'a) polyparser
  val <!> : ('t, 'a) polyparser * string -> ('t, 'b) polyparser

  type bracket_shape = Lex.bracket_shape

  datatype right_result
    = FOUND_RIGHT      of bracket_shape located
    | SCANNED_TO_RIGHT of srcloc  (* location where scanning started *)
    | NO_RIGHT


  val matchingRight : ('t, right_result) pb_parser
  val scanToClose   : ('t, right_result) pb_parser
  val matchBrackets : string -> bracket_shape located -> 'a -> right_result ->
                                                                  'a Error.error
  val bracketKeyword : ('t, 'keyword) pb_parser * string * ('t, 'a) pb_parser ->
                                                              ('t, 'a) pb_parser

  val bracket : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser

  val usageParser : (string -> ('t, string) pb_parser) -> string * ('t, 'a)
                                                 pb_parser -> ('t, 'a) pb_parser

(*  val safeTokens : 'a located eol_marked stream -> 'a list *)
  val showErrorInput : ('t -> string) -> ('t, 'a) polyparser -> ('t, 'a)
                                                                      polyparser
  val wrapAround : ('t -> string) -> string -> ('t, 'a) polyparser -> ('t, 'a)
                                                                      polyparser



  type name = string
  val nodups : string * string -> name list located -> name list Error.error

  val left : ('t, bracket_shape located) pb_parser
  val right : ('t, bracket_shape located) pb_parser
  val leftCurly : ('t, bracket_shape located) pb_parser
  val badRight : string -> ('t, 'a) pb_parser  (* signals error *)

end
