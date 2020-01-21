(* <uscheme-parser-module.sml>=                 *)
functor MkUschemeParser(structure Lexer : USCHEME_LEXER
                       ) :> USCHEME_PARSER
                              where type xdef = UschemeSyntax.Xdef.xdef
                                and type prompts = { ps1 : string, ps2 : string
                                                                 } (* XXX UGH *)
=
struct
  structure EM = MkEolMarks(structure Stream = Std.StreamRep
                            structure Susp = Suspension
                            structure X = Std.Xformer)

  structure P = MkTokenParsingCombinators(structure X = Std.Xformer
                                          structure Stream = Std.StreamRep
                                          structure Susp = Suspension
                                          structure Lex = Lexer.Utils
                                          structure EM = EM
                                          type token = Lexer.token
                                          val tokenString = Lexer.tokenString
                                         )

  structure ST = MkSchemeTokenParsers(structure L = Lexer
                                      structure P = P
                                      structure EM = EM
                                     )

  structure FPS = MkFormalParsers(structure L = Lexer
                                  structure P = P
                                  structure EM = EM
                                  structure ST = ST
                                 ) 

  structure SP = MkSchemelikeParsers(structure L = Lexer
                                     structure P = P
                                     structure EM = EM
                                     structure ST = ST
                                     structure VU = UschemeValUtils
                                     structure CS = UschemeSyntax)

  structure Reader =
    MkReader (structure S = Std.Stream
              structure X = Std.Xformer
              structure L = Lexer.Utils
              structure EM = EM
              type token = Lexer.token
             )

  open Reader
  type line = Std.Stream.line
  open Lexer
  open Std.Xformer
  open Std.Stream
  open P
  open Error
  open UschemeSyntax
  open Xdef
  open UschemeValUtils
  open ST
  open FPS
  open SP
  open RuntimeError (* used by primitives to which (record ...) desugars *)
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
(*   PARSERS AND [[XDEF]] STREAMS FOR \USCHEME                   *)
(*                                                               *)
(*****************************************************************)

(* The [[exptable]] itself uses the format described in *)
(* \crefpagelazyparse.code.usageParser: each alternative *)
(* is specified by a pair containing a usage string and *)
(* a parser.                                    *)
(* <parsers and [[xdef]] streams for \uscheme>= *)
fun exptable exp =
  let val bindings = bindingsOf "(x e)" name exp
      val formals  = formalsOf "(x1 x2 ...)" name "lambda"
      val dbs      = distinctBsIn bindings
(*unboxval*)
  in usageParsers
     [ ("(if e1 e2 e3)",            curry3 IFX          <$> exp <*> exp <*> exp)
     , ("(while e1 e2)",            curry  WHILEX       <$> exp  <*> exp)
     , ("(set x e)",                curry  SET          <$> name <*> exp)
     , ("(begin e1 ...)",                  BEGIN        <$> many exp)
     , ("(lambda (names) body)",    curry  LAMBDA       <$> formals      <*> exp
                                                                               )
     , ("(let (bindings) body)",    curry3 LETX LET     <$> dbs "let"    <*> exp
                                                                               )
     , ("(letrec (bindings) body)", curry3 LETX LETREC  <$> dbs "letrec" <*> exp
                                                                               )
     , ("(let* (bindings) body)",   curry3 LETX LETSTAR <$> bindings     <*> exp
                                                                               )
     , ("(quote sexp)",             LITERAL             <$> sexp)
     (* <rows added to ML \uscheme's [[exptable]] in exercises>= *)
     (* add syntactic sugar here, each row preceded by a comma *)
     ]
  end
(* There is a placeholder for adding more syntax in *)
(* exercises.                                   *)

(* <parsers and [[xdef]] streams for \uscheme>= *)
val exp = fullSchemeExpOf (atomicSchemeExpOf name) exptable
(* Parsers for micro-Scheme definitions         *)
(*                                              *)
(* I segregate the definition parsers by the ML type of *)
(* definition they produce. Parser [[deftable]] parses *)
(* the true definitions. Function [[define]] is a *)
(* Curried function that creates a [[DEFINE]] node. *)
(* <parsers and [[xdef]] streams for \uscheme>= *)
val deftable = usageParsers
  [ ("(define f (args) body)",
        let val formals  = formalsOf "(x1 x2 ...)" name "define"
        in  curry DEFINE <$> name <*> (pair <$> formals <*> exp)
        end)
  , ("(val x e)", curry VAL <$> name <*> exp)
  ]
(*unboxval*)
(* Parser [[testtable]] parses a unit test.     *)
(* <parsers and [[xdef]] streams for \uscheme>= *)
val testtable = usageParsers
  [ ("(check-expect e1 e2)", curry CHECK_EXPECT <$> exp <*> exp)
  , ("(check-assert e)",           CHECK_ASSERT <$> exp)
  , ("(check-error e)",            CHECK_ERROR  <$> exp)
  ]
(*unboxval*)
(* Parser [[xdeftable]] handles those extended  *)
(* definitions that are not unit tests. It is also where *)
(* you would extend the parser with new syntactic forms *)
(* of definition, like the [[record]] form described in *)
(* \crefpagescheme.record-sugar.                *)
(* <parsers and [[xdef]] streams for \uscheme>= *)
val xdeftable = usageParsers
  [ ("(use filename)", USE <$> name)
  (* <rows added to \uscheme\ [[xdeftable]] in exercises>= *)
  (* add syntactic sugar here, each row preceded by a comma *) 
  ]
(*unboxval*)
(* The [[xdef]] parser combines all the types of *)
(* extended [*] definition, plus an error case. *)
(* <parsers and [[xdef]] streams for \uscheme>= *)
val xdef =  DEF  <$> deftable
        <|> TEST <$> testtable
        <|>          xdeftable
        <|> badRight "unexpected right bracket"
        <|> DEF <$> EXP <$> exp
        <?> "definition"
(*unboxval*)
(* Finally, function [[xdefstream]], which is the *)
(* externally visible interface to the parsing, uses the *)
(* lexer and parser to make a function that converts a *)
(* stream of lines to a stream of extended definitions. *)
(* <parsers and [[xdef]] streams for \uscheme>= *)
val xdefstream = 
  interactiveParsedStream (schemeToken, xdef)
(*unboxval*)
end
