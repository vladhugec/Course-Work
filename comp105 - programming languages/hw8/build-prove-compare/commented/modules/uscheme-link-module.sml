(* <uscheme-link-module.sml>=                   *)
structure UschemeParts = struct
  structure L      = MkLexerUtils(Std.Xformer)
  structure Lexer  = MkUschemeLexer(structure L = L)
  structure Parser = MkUschemeParser(structure Lexer = Lexer)
  structure Eval   = MkUschemeEval(structure Env = Env)

  structure Interpreter =
    MkUschemeRun(structure Stream = Std.Stream
                 structure Env = Env
                 structure Parser = Parser
                 structure Eval = Eval
                )
end
