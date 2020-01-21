(* lexer-utils.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
signature LEXER_UTILS = sig
  type 'a stream (* hook for sharing constraints *)

  type ('a, 'b) xformer
  type 'a lexer = (char, 'a) xformer

  (* argument char -> bool identifies delimiters *)
  val whitespace : char list lexer
  val intChars   : (char -> bool) -> char list lexer
  val intToken   : (char -> bool) -> int lexer
  val isDelim    : char -> bool    (* common delimiters *)

  datatype bracket_shape = ROUND | SQUARE | CURLY
  val leftString  : bracket_shape -> string
  val rightString : bracket_shape -> string

  datatype 'a plus_brackets
    = LEFT  of bracket_shape
    | RIGHT of bracket_shape
    | PRETOKEN of 'a
  val bracketLexer : 'a lexer -> 'a plus_brackets lexer

  val plusBracketsString : ('a -> string) -> 'a plus_brackets -> string
end
