(* reader-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
functor MkReader(structure S : STREAM
                   where type 'a stream = 'a Std.Stream.stream
                 structure X : XFORMER
                   where type 'a stream = 'a Std.Stream.stream
                 structure L : LEXER_UTILS
                    where type ('a, 'b) xformer = ('a, 'b) X.xformer
                 structure EM : EOL_MARKS
                    where type 'a stream = 'a Std.Stream.stream
                    where type ('a, 'b) xformer = ('a, 'b) X.xformer
                 type token
                        ) :> READER
                        where type 'a eol_marked = 'a EM.eol_marked
                          and type srcloc = Srcloc.srcloc
                          and type 'a stream = 'a S.stream
                          and type 'a lexer = 'a L.lexer
                          and type ('a, 'b) xformer = ('a, 'b) X.xformer
                          and type 'a parser =
                                (token Srcloc.located EM.eol_marked, 'a)
                                                                       X.xformer
                          and type token = token
                          and type prompts = { ps1 : string, ps2 : string }
                                                                   (* XXX UGH *)

     =
struct
  type token = token
  type 'a parser = (token Srcloc.located EM.eol_marked, 'a) X.xformer
  open Error
  open L
  open EM
  open S
  open Stringutil
  open Srcloc
  infix 5 @@@
  fun fst (x, y) = x
  fun snd (x, y) = y
  (* XXX the chunk below is part of <common parsing code>.
     Perhaps it ought to be split out, or perhaps this module should be
                                                                   eliminated *)
  

(*****************************************************************)
(*                                                               *)
(*   STREAMS THAT ISSUE TWO FORMS OF PROMPTS                     *)
(*                                                               *)
(*****************************************************************)

(* streams that issue two forms of prompts 1046b *)
fun echoTagStream lines = 
  let fun echoIfTagged line =
        if (String.substring (line, 0, 2) = ";#" handle _ => false) then
          print line
        else
          ()
  in  postStream (lines, echoIfTagged)
  end
(*unboxval*)
(* streams that issue two forms of prompts 1047a *)
fun stripAndReportErrors xs =
  let fun next xs =
        case streamGet xs
          of SOME (ERROR msg, xs) => (eprintln msg; next xs)
           | SOME (OK x, xs) => SOME (x, xs)
           | NONE => NONE
  in  streamOfUnfold next xs
  end
(*unboxval*)
(* streams that issue two forms of prompts 1047b *)
fun lexLineWith lexer =
  stripAndReportErrors o streamOfUnfold lexer o streamOfList o explode
(*unboxval*)
(* streams that issue two forms of prompts 1047c *)
fun parseWithErrors parser =
  let fun adjust (SOME (ERROR msg, tokens)) = SOME (ERROR msg, drainLine tokens)
        | adjust other = other
  in  streamOfUnfold (adjust o parser)
  end
(*unboxval*)
(* streams that issue two forms of prompts 1047d *)
type prompts   = { ps1 : string, ps2 : string }
val stdPrompts = { ps1 = "-> ", ps2 = "   " }
val noPrompts  = { ps1 = "", ps2 = "" }
(*unboxval*)
(* streams that issue two forms of prompts 1048 *)
fun ('t, 'a) interactiveParsedStream (lexer, parser) (name, lines, prompts) =
  let val { ps1, ps2 } = prompts
      val thePrompt = ref ps1
      fun setPrompt ps = fn _ => thePrompt := ps

      val lines = preStream (fn () => print (!thePrompt), echoTagStream lines)

      fun lexAndDecorate (loc, line) =
        let val tokens = postStream (lexLineWith lexer line, setPrompt ps2)
        in  streamMap INLINE (streamZip (streamRepeat loc, tokens)) @@@
            streamOfList [EOL (snd loc)]
        end

      val xdefs_with_errors : 'a error stream = 
        (parseWithErrors parser o streamConcatMap lexAndDecorate o locatedStream
                                                                               )
        (name, lines)
(*unboxval*)
  in  
      stripAndReportErrors (preStream (setPrompt ps1, xdefs_with_errors))
  end 
end
