(* <eol-marks-module.sml>=                      *)
functor MkEolMarks(structure Stream : STREAM_REP
                   structure Susp : SUSPENSION
                   structure X : XFORMER
                     sharing type X.stream = Stream.stream
                     sharing type Stream.susp = Susp.susp
                  ) :> EOL_MARKS
  where type 'a stream = 'a Stream.stream
    and type ('a, 'b) xformer = ('a, 'b) X.xformer
  =
struct
  open Susp
  open Stream
  open X
  open Srcloc
  type 'a stream = 'a Stream.stream
  infixr 3 :::
  infixr 4 <$> <$>?
  infix 6 <* *>
  

(*****************************************************************)
(*                                                               *)
(*   STREAMS THAT TRACK LINE BOUNDARIES                          *)
(*                                                               *)
(*****************************************************************)

(* Flushing bad tokens                          *)
(*                                              *)
(* A standard parser for a batch compiler needs only to *)
(* see a stream of tokens and to know from what *)
(* source-code location each token came. A batch *)
(* compiler can simply read all its input and report all *)
(* the errors it wants to report. [Batch compilers vary *)
(* widely in the ambitions of their parsers. Some simple *)
(* parsers report just one error and stop. Some *)
(* sophisticated parsers analyze the entire input and *)
(* report the smallest number of changes needed to make *)
(* the input syntactically correct. And some    *)
(* ill-mannered parsers become confused after an error *)
(* and start spraying meaningless error messages. But *)
(* all of them have access to the entire input. *)
(* We~don't. ] But an interactive interpreter may not *)
(* use an error as an excuse to read an indefinite *)
(* amount of input. It must instead bring its error *)
(* processing to a prompt conclusion and ready itself to *)
(* read the next line. To do so, it needs to know where *)
(* the line boundaries are! For example, if I find an *)
(* error on line 6, I want to read all the tokens on *)
(* line 6, throw them away, and start over again on *)
(* line 7. The nasty bit is that I want to do it without *)
(* reading line 7—reading line 7 will take an action and *)
(* will likely have the side effect of printing a *)
(* prompt. And I want it to be the correct prompt. *)
(* I therefore define a new type constructor    *)
(* [[eol_marked]]. A value of type \monobox'a   *)
(* [[eol_marked]] is either an end-of-line marker, or it *)
(* contains a value of type [['a]] that occurs in a *)
(* line. A stream of such values can be drained up to *)
(* the end of the line. [At~some future point I~may need *)
(* to change [[drainLine]] to keep the [[EOL]] in order *)
(* to track locations in \uprolog. ]            *)
(* <streams that track line boundaries>=        *)
datatype 'a eol_marked
  = EOL of int (* number of the line that ends here *)
  | INLINE of 'a
(* Now if I have a stream of such values, I can drain *)
(* the stream up to the end of the line. [At~some future *)
(* point I~may need to change [[drainLine]] to keep the *)
(* [[EOL]] in order to track locations in \uprolog. ] *)

(* <streams that track line boundaries>=        *)
fun drainLine EOS = EOS
  | drainLine (SUSPENDED s)     = drainLine (demand s)
  | drainLine (EOL _    ::: xs) = xs
  | drainLine (INLINE _ ::: xs) = drainLine xs
(*unboxval*)
(* <streams that track line boundaries>=        *)
local 
  fun asEol (EOL n) = SOME n
    | asEol (INLINE _) = NONE
  fun asInline (INLINE x) = SOME x
    | asInline (EOL _)    = NONE
in
  fun eol    xs = (asEol    <$>? one) xs
  fun inline xs = (asInline <$>? many eol *> one) xs
  fun srcloc xs = rewind (fst <$> inline) xs
end
(*unboxval*)
end
