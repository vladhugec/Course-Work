(* lexer-utils-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
functor MkLexerUtils(X : XFORMER) :> LEXER_UTILS
   where type ('a, 'b) xformer = ('a, 'b) X.xformer
     and type 'a stream = 'a X.stream
   =
struct
  open X
  open Error
  infix 1 >>=
  infix 1 >>=+
  infix 3 <*>
  infixr 4 <$>
  infix 1 <|>
  infix 3 <* *>
  infixr 4 <$>!
  infixr 4 <$
  

(*****************************************************************)
(*                                                               *)
(*   SUPPORT FOR LEXICAL ANALYSIS                                *)
(*                                                               *)
(*****************************************************************)

(* support for lexical analysis 1035b *)
type 'a lexer = (char, 'a) xformer
(*unboxval*)
(* support for lexical analysis 1035c *)
fun isDelim c =
  Char.isSpace c orelse Char.contains "()[]{};" c
(*unboxval*)
(* support for lexical analysis 1037a *)
val whitespace = many (sat Char.isSpace one)
(*unboxval*)
(* support for lexical analysis 1037b *)
fun intChars isDelim = 
  (curry (op ::) <$> eqx #"-" one <|> pure id) <*> many1 (sat Char.isDigit one)
                                                                              <*
  notFollowedBy (sat (not o isDelim) one)
(*unboxval*)
(* support for lexical analysis 1037c *)
fun intFromChars (#"-" :: cs) = 
      intFromChars cs >>=+ Int.~
  | intFromChars cs =
      (OK o valOf o Int.fromString o implode) cs
      handle Overflow => ERROR
                        "this interpreter can't read arbitrarily large integers"
(*unboxval*)
(* support for lexical analysis 1037d *)
fun intToken isDelim =
  intFromChars <$>! intChars isDelim
(*unboxval*)
(* support for lexical analysis 1038a *)
datatype bracket_shape = ROUND | SQUARE | CURLY

fun leftString ROUND  = "("
  | leftString SQUARE = "["
  | leftString CURLY  = "{"
fun rightString ROUND  = ")"
  | rightString SQUARE = "]"
  | rightString CURLY = "}"
(* support for lexical analysis 1038b *)
datatype 'a plus_brackets
  = LEFT  of bracket_shape
  | RIGHT of bracket_shape
  | PRETOKEN of 'a

fun bracketLexer pretoken
  =  LEFT  ROUND  <$ eqx #"(" one
 <|> LEFT  SQUARE <$ eqx #"[" one
 <|> LEFT  CURLY  <$ eqx #"{" one
 <|> RIGHT ROUND  <$ eqx #")" one
 <|> RIGHT SQUARE <$ eqx #"]" one
 <|> RIGHT CURLY  <$ eqx #"}" one
 <|> PRETOKEN <$> pretoken

fun plusBracketsString _   (LEFT shape)  = leftString shape
  | plusBracketsString _   (RIGHT shape) = rightString shape
  | plusBracketsString pts (PRETOKEN pt)  = pts pt
(*unboxval*)
  structure X = X
end
