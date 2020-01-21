(* <uscheme-language-module.sml>=               *)
structure UschemeLanguage :> LANGUAGE 
                               where type basis = UschemeSyntax.value ref
                                                                         Env.env
                                 and type Xdef.xdef = UschemeSyntax.Xdef.xdef
                                 and type 'a stream = 'a Std.stream
                                 and type prompts = { ps1 : string, ps2 : string
                                                                               }
  = 
struct
  structure Xdef = UschemeSyntax.Xdef
  
  structure LU = MkLexerUtils(Std.Xformer)


  structure Lexer = MkUschemeLexer(structure L = LU)

  structure Parser = MkUschemeParser(structure Stream = Std.Stream
                                     structure Susp = Suspension
                                     structure X = Std.Xformer
                                     structure Lexer = Lexer)

  structure EM = MkEolMarks(structure Stream = Std.StreamRep
                            structure Susp = Suspension
                            structure X = Std.Xformer)

  structure Reader = MkReader(structure S = Std.Stream
                              structure X = Std.Xformer
                              structure L = LU
                              structure EM = EM
                              type token = Lexer.token)

  structure Eval = MkUschemeEval(structure Env = Env)

  structure UG = MkUschemeUnitTestGood(structure Eval = Eval)
  structure U = MkUnitTests(structure Test = UG)


  type prompts = Reader.prompts
  type 'a stream = 'a Reader.stream
  type line = string

  type basis = Eval.basis
  val processDef = Eval.processDef
  val xdefstream = Parser.xdefstream
  val processTests = U.processTests
end
