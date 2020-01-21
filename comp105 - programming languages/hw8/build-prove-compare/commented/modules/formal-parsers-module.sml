(* <formal-parsers-module.sml>=                 *)
functor MkFormalParsers(structure L : USCHEME_LEXER
                        structure P : PARSER
                          where type ('a, 'b) xformer = ('a, 'b) Std.xformer
                          where type 'a Lex.stream = 'a Std.stream
                          where type token = L.token
                        structure EM : EOL_MARKS
                          where type ('a, 'b) xformer = ('a, 'b) Std.xformer
                          where type 'a stream = 'a Std.stream
                        structure ST : SCHEME_TOKEN_PARSERS
                          where type 'a parser = (L.pretoken, 'a) P.pb_parser
                       ) :> FORMAL_PARSERS
                              where type 'a parser = 'a ST.parser
  =
struct
  open Std.Xformer
  open P
  open EM
  open ST
  open Error
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
(*   PARSERS AND PARSER BUILDERS FOR FORMAL PARAMETERS AND BINDINGS *)
(*                                                               *)
(*****************************************************************)

(* The next step up is syntactic elements used in *)
(* multiple Scheme-like languages. Function [[formals]] *)
(* parses a list of formal parameters. If the formal *)
(* parameters contain duplicates, it's treated as a *)
(* syntax error. Function [[bindings]] produces a list *)
(* of bindings suitable for use in [[let*]] expressions. *)
(* For [[let]] and [[letrec]] expressions, which do not *)
(* permit multiple bindings to the same name, use *)
(* [[distinctBsIn]].                            *)
(* <parsers and parser builders for formal parameters and bindings>= *)
fun formalsOf what name context = 
  nodups ("formal parameter", context) <$>! @@ (bracket (what, many name))

fun bindingsOf what name exp =
  let val binding = bracket (what, pair <$> name <*> exp)
  in  bracket ("(... " ^ what ^ " ...) in bindings", many binding)
  end

fun distinctBsIn bindings context =
  let fun check (loc, bs) =
        nodups ("bound name", context) (loc, map fst bs) >>=+ (fn _ => bs)
  in  check <$>! @@ bindings
  end
(*unboxval*)
(* Record fields also may not contain duplicates. *)
(* <parsers and parser builders for formal parameters and bindings>= *)
fun recordFieldsOf name =
  nodups ("record fields", "record definition") <$>!
                                    @@ (bracket ("(field ...)", many name))
(*unboxval*)
(* We parse any keyword as the name represented by the *)
(* same string as the keyword. And using the keyword *)
(* parser, we can string together ``usage'' parsers. *)
(* <parsers and parser builders for formal parameters and bindings>= *)
fun kw keyword = 
  eqx keyword any_name
fun usageParsers ps  = anyParser (map (usageParser kw) ps)
(*unboxval*)
end
