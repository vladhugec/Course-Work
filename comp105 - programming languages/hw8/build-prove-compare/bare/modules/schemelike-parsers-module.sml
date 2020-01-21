(* schemelike-parsers-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
functor MkSchemelikeParsers(structure L : USCHEME_LEXER
                            structure P : PARSER
                              where type ('a, 'b) xformer = ('a, 'b) Std.xformer
                              where type 'a Lex.stream = 'a Std.stream
                              where type token = L.token
                            structure EM : EOL_MARKS
                              where type ('a, 'b) xformer = ('a, 'b) Std.xformer
                              where type 'a stream = 'a Std.stream
                            structure ST : SCHEME_TOKEN_PARSERS
                              where type 'a parser = (L.pretoken, 'a)
                                                                     P.pb_parser
                            structure VU : VAL_UTILS
                            structure CS : COMMON_SYNTAX
                              sharing type CS.value = VU.value
                            sharing type P.Lex.plus_brackets =
                                                           L.Utils.plus_brackets
                           ) :> SCHEMELIKE_PARSERS
                                  where type value = VU.value
                                  where type exp = CS.exp
                                  where type 'a parser = 'a ST.parser
  =
struct
  open Srcloc
  open Error
  open Std.Xformer
  open ST
  open VU
  open L
  open P
  open EM
  open CS
  infix 3 <*>
  infixr 4 <$>
  infix 1 <|>
  infix 3 <* *>
  infixr 4 <$
  infixr 4 <$>?
  infix 3 <&>
  infix 2 <*>!
  infixr 4 <$>!
  infix  6 --<
  infixr 7 >-- 
  infix 1 >>=
  infix 1 >>=+
  infix 4 <!>
  infix 0 <?>
  
  

(*****************************************************************)
(*                                                               *)
(*   PARSERS AND PARSER BUILDERS FOR \SCHEME-LIKE SYNTAX         *)
(*                                                               *)
(*****************************************************************)

(* parsers and parser builders for \scheme-like syntax 1120b *)
fun sexp tokens = (
     SYM       <$> (notDot <$>! @@ any_name)
 <|> NUM       <$> int
 <|> embedBool <$> booltok
 <|> leftCurly <!> "curly brackets may not be used in S-expressions"
 <|> embedList <$> bracket ("list of S-expressions", many sexp)
 <|> (fn v => embedList [SYM "quote", v]) 
               <$> (quote *> sexp)
) tokens
and notDot (loc, ".") =
      errorAt "this interpreter cannot handle . in quoted S-expressions" loc
  | notDot (_,   s)   = OK s
(*unboxval*)
(* parsers and parser builders for \scheme-like syntax 1120c *)
fun atomicSchemeExpOf name =  VAR                   <$> name
                          <|> LITERAL <$> NUM       <$> int
                          <|> LITERAL <$> embedBool <$> booltok
(* parsers and parser builders for \scheme-like syntax 1121c *)
fun fullSchemeExpOf atomic keywordsOf =
  let val exp = fn tokens => fullSchemeExpOf atomic keywordsOf tokens
  in      atomic
      <|> keywordsOf exp
      <|> quote *> (LITERAL <$> sexp)
      <|> quote *> badRight "quote ' followed by right bracket"
      <|> leftCurly <!> "curly brackets are not supported"
      <|> left *> right <!> "empty application"
      <|> bracket("function application", curry APPLY <$> exp <*> many exp)
  end
end
