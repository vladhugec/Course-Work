(* <guiparser-module.sml>=                      *)
functor MkGuiparser(structure L : LEXER_UTILS
                      where type ('a, 'b) xformer = ('a, 'b) Std.xformer
                    structure EM : EOL_MARKS
                      where type 'a stream = 'a Std.Stream.stream
                      where type ('a, 'b) xformer = ('a, 'b) Std.xformer
                    type token
                    val lexer : (char, token) Std.xformer
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

  type input = token located eol_marked stream
     (* XXX todo: extend token stream to distinguish lines? *)

  fun lexAndDecorate loc line =
    let val tokens = lexLineWith lexer line
    in  streamMap INLINE (streamZip (streamRepeat loc, tokens)) @@@
        streamOfList [EOL (snd loc)]
    end

  let exception Incomplete
  val incomplete = streamOfEffects (fn () => raise Incomplete)

  fun raisesIncomplete parser tokens =
    (parser tokens; false) handle Incomplete => true

  val emptyInput = emptyStream
  fun addLines (input, loc, lines) =
    input @@@ streamConcatMap (lexAndDecorate loc) (streamOfList lines)

  fun mkParser parser tokens =
    case parser tokens
      of NONE => if emptyInput tokens then
                   EOF
                 else
                   SYNERROR ("definition expected", (), drainLine tokens)
       | SOME (OK xdef, tokens) => PARSED (xdef, tokens)
       | SOME (ERROR msg, tokens) =>
           if raisesIncomplete parser tokens then
             INCOMPLETE
           else
             SYNERROR (msg, (), drainLine tokens)
end
