(* uscheme-lexer-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
functor MkUschemeLexer(structure L : LEXER_UTILS
                         where type ('a, 'b) xformer = ('a, 'b)
                                                             Std.Xformer.xformer
                         where type 'a stream = 'a Std.stream
                      )
 :> USCHEME_LEXER
 =
struct
  open Std.Xformer
  open L
  open Std.Stream
  open Error
  open Stringutil
  infix 1 >>=
  infix 1 >>=+
  infix 3 <*>
  infixr 4 <$>
  infixr 4 <$
  infix 1 <|>
  infix 3 <* *>
  infixr 4 <$>!
  infixr 3 :::
  structure Utils = L
  

(*****************************************************************)
(*                                                               *)
(*   LEXICAL ANALYSIS FOR \USCHEME\ AND RELATED LANGUAGES        *)
(*                                                               *)
(*****************************************************************)

(* lexical analysis for \uscheme\ and related languages 1117b *)
datatype pretoken = QUOTE
                  | INT     of int
                  | SHARP   of bool
                  | NAME    of string
type token = pretoken plus_brackets
(* lexical analysis for \uscheme\ and related languages 1118a *)
fun pretokenString (QUOTE)     = "'"
  | pretokenString (INT  n)    = intString n
  | pretokenString (SHARP b)   = if b then "#t" else "#f"
  | pretokenString (NAME x)    = x
val tokenString = plusBracketsString pretokenString
(* lexical analysis for \uscheme\ and related languages 1118b *)
local
  (* functions used in all lexers 1118d *)
  fun noneIfLineEnds chars =
    case streamGet chars
      of NONE => NONE (* end of line *)
       | SOME (#";", cs) => NONE (* comment *)
       | SOME (c, cs) => 
           let val msg = "invalid initial character in `" ^
                         implode (c::listOfStream cs) ^ "'"
           in  SOME (ERROR msg, EOS)
           end
  (*unboxval*)
  (* functions used in the lexer for \uscheme 1118c *)
  fun atom "#t" = SHARP true
    | atom "#f" = SHARP false
    | atom x    = NAME x
in
  val schemeToken =
    whitespace *>
    bracketLexer   (  QUOTE   <$  eqx #"'" one
                  <|> INT     <$> intToken isDelim
                  <|> (atom o implode) <$> many1 (sat (not o isDelim) one)
                  <|> noneIfLineEnds
                   )
(*unboxval*)
end
end
