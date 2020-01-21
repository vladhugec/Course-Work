(* stream-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
functor MkStreamRep(S : SUSPENSION) :> STREAM_REP where type 'a susp = 'a S.susp
                                                                               =
struct
  open S
  open Error
  

(*****************************************************************)
(*                                                               *)
(*   STREAMS                                                     *)
(*                                                               *)
(*****************************************************************)

(* streams 1016a *)
datatype 'a stream 
  = EOS
  | :::       of 'a * 'a stream
  | SUSPENDED of 'a stream susp
infixr 3 :::
(* streams 1016b *)
fun streamGet EOS = NONE
  | streamGet (x ::: xs)    = SOME (x, xs)
  | streamGet (SUSPENDED s) = streamGet (demand s)
(* streams 1016c *)
fun streamOfList xs = 
  foldr (op :::) EOS xs
(*unboxval*)
(* streams 1016d *)
fun listOfStream xs =
  case streamGet xs
    of NONE => []
     | SOME (x, xs) => x :: listOfStream xs
(* streams 1016e *)
fun delayedStream action = 
  SUSPENDED (delay action)
(*unboxval*)
(* streams 1017a *)
fun streamOfEffects action =
  delayedStream (fn () => case action () of NONE   => EOS
                                          | SOME a => a ::: streamOfEffects
                                                                         action)
(*unboxval*)
(* streams 1017b *)
type line = string
fun filelines infile = 
  streamOfEffects (fn () => TextIO.inputLine infile)
(*unboxval*)
(* streams 1017c *)
fun streamRepeat x =
  delayedStream (fn () => x ::: streamRepeat x)
(*unboxval*)
(* streams 1017d *)
fun streamOfUnfold next state =
  delayedStream (fn () => case next state
                            of NONE => EOS
                             | SOME (a, state') => a ::: streamOfUnfold next
                                                                         state')
(*unboxval*)
(* streams 1017e *)
val naturals = 
  streamOfUnfold (fn n => SOME (n, n+1)) 0   (* 0 to infinity *)
(*unboxval*)
(* streams 1018a *)
fun preStream (pre, xs) = 
  streamOfUnfold (fn xs => (pre (); streamGet xs)) xs
(* streams 1018b *)
fun postStream (xs, post) =
  streamOfUnfold (fn xs => case streamGet xs
                             of NONE => NONE
                              | head as SOME (x, _) => (post x; head)) xs
(*unboxval*)
(* streams 1018c *)
fun streamMap f xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => f x ::: streamMap f xs)
(*unboxval*)
(* streams 1018d *)
fun streamFilter p xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => if p x then x ::: streamFilter p
                                                                              xs
                                               else streamFilter p xs)
(*unboxval*)
(* streams 1019a *)
fun streamFold f z xs =
  case streamGet xs of NONE => z
                     | SOME (x, xs) => streamFold f (f (x, z)) xs
(*unboxval*)
(* streams 1019b *)
fun streamZip (xs, ys) =
  delayedStream
  (fn () => case (streamGet xs, streamGet ys)
              of (SOME (x, xs), SOME (y, ys)) => (x, y) ::: streamZip (xs, ys)
               | _ => EOS)
(* streams 1019c *)
fun streamConcat xss =
  let fun get (xs, xss) =
        case streamGet xs
          of SOME (x, xs) => SOME (x, (xs, xss))
           | NONE => case streamGet xss
                       of SOME (xs, xss) => get (xs, xss)
                        | NONE => NONE
  in  streamOfUnfold get (EOS, xss)
  end
(*unboxval*)
(* streams 1019d *)
fun streamConcatMap f xs = streamConcat (streamMap f xs)
(*unboxval*)
(* streams 1019e *)
infix 5 @@@
fun xs @@@ xs' = streamConcat (streamOfList [xs, xs'])
(*unboxval*)
(* streams 1020a *)
fun streamTake (0, xs) = []
  | streamTake (n, xs) =
      case streamGet xs
        of SOME (x, xs) => x :: streamTake (n-1, xs)
         | NONE => []
(*unboxval*)
(* streams 1020b *)
fun streamDrop (0, xs) = xs
  | streamDrop (n, xs) =
      case streamGet xs
        of SOME (_, xs) => streamDrop (n-1, xs)
         | NONE => EOS
(*unboxval*)
end

functor MkStream(S : STREAM_REP) :> STREAM where type 'a stream = 'a S.stream =
struct
  open S
end
