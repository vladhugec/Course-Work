(* scheme-token-parsers-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
functor MkSchemeTokenParsers(structure L : USCHEME_LEXER
                             structure P : PARSER
                               where type ('a, 'b) xformer = ('a, 'b)
                                                                     Std.xformer
                               where type token = L.token
                             structure EM : EOL_MARKS
                               where type ('a, 'b) xformer = ('a, 'b)
                                                                     Std.xformer
                               where type 'a stream = 'a Std.stream
                            ) :> SCHEME_TOKEN_PARSERS
                                    where type 'a parser = (P.token, 'a)
                                                                    P.polyparser
  =
struct
  open L P EM L.Utils
  open Std.Xformer
  infixr 4 <$>?
  

(*****************************************************************)
(*                                                               *)
(*   PARSERS FOR SINGLE \USCHEME\ TOKENS                         *)
(*                                                               *)
(*****************************************************************)

(* parsers for single \uscheme\ tokens 1119a *)
type 'a parser = (token, 'a) polyparser
val pretoken  = (fn (PRETOKEN t)=> SOME t  | _ => NONE) <$>? token : pretoken
                                                                          parser
val quote     = (fn (QUOTE)     => SOME () | _ => NONE) <$>? pretoken
val int       = (fn (INT   n)   => SOME n  | _ => NONE) <$>? pretoken
val booltok   = (fn (SHARP b)   => SOME b  | _ => NONE) <$>? pretoken
val name      = (fn (NAME  n)   => SOME n  | _ => NONE) <$>? pretoken
val any_name  = name
end
