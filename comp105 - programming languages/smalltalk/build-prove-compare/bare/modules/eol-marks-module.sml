(* eol-marks-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
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

(* streams that track line boundaries 1039a *)
datatype 'a eol_marked
  = EOL of int (* number of the line that ends here *)
  | INLINE of 'a
(* streams that track line boundaries 1039b *)
fun drainLine EOS = EOS
  | drainLine (SUSPENDED s)     = drainLine (demand s)
  | drainLine (EOL _    ::: xs) = xs
  | drainLine (INLINE _ ::: xs) = drainLine xs
(*unboxval*)
(* streams that track line boundaries 1039c *)
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
