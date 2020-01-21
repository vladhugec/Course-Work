(* uscheme-lexer.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
signature USCHEME_LEXER = sig
  datatype pretoken = QUOTE
                    | INT     of int
                    | SHARP   of bool
                    | NAME    of string
  structure Utils : LEXER_UTILS
    where type 'a stream = 'a Std.stream
    where type ('a, 'b) xformer = ('a, 'b) Std.Xformer.xformer
  type token = pretoken Utils.plus_brackets
  val tokenString : token -> string

  type 'a stream = 'a Std.stream
  type ('a, 'b) xformer = ('a, 'b) Std.Xformer.xformer
  type 'a lexer = (char, 'a) xformer
  val schemeToken : token lexer
end
