(* <uscheme-lexer-module.sml>=                  *)
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

(* Tokens of the micro-Scheme language          *)
(*                                              *)
(* [*] Our general parsing mechanism from Appendix [->] *)
(* requires a language-specific [[token]] type and a *)
(* function [[tokenString]].                    *)
(* <lexical analysis for \uscheme\ and related languages>= *)
datatype pretoken = QUOTE
                  | INT     of int
                  | SHARP   of bool
                  | NAME    of string
type token = pretoken plus_brackets
(* <lexical analysis for \uscheme\ and related languages>= *)
fun pretokenString (QUOTE)     = "'"
  | pretokenString (INT  n)    = intString n
  | pretokenString (SHARP b)   = if b then "#t" else "#f"
  | pretokenString (NAME x)    = x
val tokenString = plusBracketsString pretokenString
(* Lexical analysis for micro-Scheme            *)
(*                                              *)
(* Before a micro-Scheme token, whitespace is ignored. *)
(* The [[schemeToken]] function tries each alternative *)
(* in turn: the two brackets, a quote mark, an integer *)
(* literal, an atom, or end of line. An atom may be a *)
(* [[SHARP]] name or a normal name. [*]         *)
(* <lexical analysis for \uscheme\ and related languages>= *)
local
  (* If the lexer doesn't recognize a bracket, quote mark, *)
  (* integer, or other atom, we're expecting the line to *)
  (* end. The end of the line may present itself as the *)
  (* end of the input stream or as a stream of characters *)
  (* beginning with a semicolon, which marks a comment. *)
  (* If we encounter any other character, something has *)
  (* gone wrong. (The polymorphic type of         *)
  (* [[noneIfLineEnds]] provides a subtle but powerful *)
  (* hint that no token can be produced; the only possible *)
  (* outcomes are that nothing is produced, or the lexer *)
  (* detects an error.) [*]                       *)
  (* <functions used in all lexers>=              *)
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
  (* The [[atom]] function identifies the special literals *)
  (* [[#t]] and [[#f]]; all other atoms are names. *)
  (* <functions used in the lexer for \uscheme>=  *)
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
