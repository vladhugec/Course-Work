(* haskell.sml (BUG: software can't tell where this code came from [NW2AyK2m-HNBSx-1]) *)


(*****************************************************************)
(*                                                               *)
(*   EXCEPTIONS USED IN LANGUAGES WITH TYPE INFERENCE            *)
(*                                                               *)
(*****************************************************************)

(* exceptions used in languages with type inference 1117c *)
exception TypeError of string
exception BugInTypeInference of string


(*****************************************************************)
(*                                                               *)
(*   EXCEPTIONS USED IN LANGUAGES WITH LAZY EVALUATION           *)
(*                                                               *)
(*****************************************************************)

(* exceptions used in languages with lazy evaluation (BUG: software can't tell where this code came from [NW2AyK2m-6SUeq-1]) *)
exception BlackHole


(*****************************************************************)
(*                                                               *)
(*   \FOOTNOTESIZE SHARED: NAMES, ENVIRONMENTS, STRINGS, ERRORS, PRINTING, INTERACTION, STREAMS, \&\ INITIALIZATION *)
(*                                                               *)
(*****************************************************************)

(* \footnotesize shared: names, environments, strings, errors, printing, interaction, streams, \&\ initialization 1117a *)
(* for working with curried functions: [[id]], [[fst]], [[snd]], [[pair]], [[curry]], and [[curry3]] 1145c *)
fun id x = x
fun fst (x, y) = x
fun snd (x, y) = y
fun pair x y = (x, y)
fun curry  f x y   = f (x, y)
fun curry3 f x y z = f (x, y, z)
(* type declarations for consistency checking *)
val _ = op fst    : ('a * 'b) -> 'a
val _ = op snd    : ('a * 'b) -> 'b
val _ = op pair   : 'a -> 'b -> 'a * 'b
val _ = op curry  : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
val _ = op curry3 : ('a * 'b * 'c -> 'd) -> ('a -> 'b -> 'c -> 'd)
(* support for names and environments 364 *)
type name = string
(* support for names and environments 365 *)
type 'a env = (name * 'a) list
val emptyEnv = []

(* lookup and check of existing bindings *)
exception NotFound of name
fun find (name, []) = raise NotFound name
  | find (name, (n, v)::tail) = if name = n then v else find (name, tail)

(* adding new bindings *)
exception BindListLength
fun bind (name, v, rho) = (name, v) :: rho
fun bindList (n::vars, v::vals, rho) = bindList (vars, vals, bind (n, v, rho))
  | bindList ([], [], rho) = rho
  | bindList _ = raise BindListLength
(* type declarations for consistency checking *)
val _ = op emptyEnv : 'a env
val _ = op find     : name * 'a env -> 'a
val _ = op bind     : name      * 'a      * 'a env -> 'a env
val _ = op bindList : name list * 'a list * 'a env -> 'a env
(* support for names and environments 369e *)
fun duplicatename [] = NONE
  | duplicatename (x::xs) =
      if List.exists (fn x' => x' = x) xs then
        SOME x
      else
        duplicatename xs
(* type declarations for consistency checking *)
val _ = op duplicatename : name list -> name option
(* support for detecting and signaling errors detected at run time 369d *)
exception RuntimeError of string (* error message *)
(* support for detecting and signaling errors detected at run time 370a *)
fun errorIfDups (what, xs, context) =
  case duplicatename xs
    of NONE   => ()
     | SOME x => raise RuntimeError (what ^ " " ^ x ^ " appears twice in " ^
                                                                        context)
(* type declarations for consistency checking *)
val _ = op errorIfDups : string * name list * string -> unit
(* support for detecting and signaling errors detected at run time 370b *)
exception InternalError of string (* bug in the interpreter *)
(* list functions not provided by \sml's initial basis 1122b *)
fun zip3 ([], [], []) = []
  | zip3 (x::xs, y::ys, z::zs) = (x, y, z) :: zip3 (xs, ys, zs)
  | zip3 _ = raise ListPair.UnequalLengths

fun unzip3 [] = ([], [], [])
  | unzip3 (trip::trips) =
      let val (x,  y,  z)  = trip
          val (xs, ys, zs) = unzip3 trips
      in  (x::xs, y::ys, z::zs)
      end
(* list functions not provided by \sml's initial basis 1122c *)
val reverse = rev
(* list functions not provided by \sml's initial basis 1122d *)
fun optionList [] = SOME []
  | optionList (NONE :: _) = NONE
  | optionList (SOME x :: rest) =
      (case optionList rest
         of SOME xs => SOME (x :: xs)
          | NONE    => NONE)
(* utility functions for string manipulation and printing 1118a *)
fun println  s = (print s; print "\n")
fun eprint   s = TextIO.output (TextIO.stdErr, s)
fun eprintln s = (eprint s; eprint "\n")
(* utility functions for string manipulation and printing 1118b *)
val xprinter = ref print
fun xprint   s = !xprinter s
fun xprintln s = (xprint s; xprint "\n")
(* utility functions for string manipulation and printing 1118c *)
fun tryFinally f x post =
  (f x handle e => (post (); raise e)) before post ()

fun withXprinter xp f x =
  let val oxp = !xprinter
      val ()  = xprinter := xp
  in  tryFinally f x (fn () => xprinter := oxp)
  end
(* utility functions for string manipulation and printing 1118d *)
fun bprinter () =
  let val buffer = ref []
      fun bprint s = buffer := s :: !buffer
      fun contents () = concat (rev (!buffer))
  in  (bprint, contents)
  end
(* utility functions for string manipulation and printing 1118e *)
fun predefinedFunctionError s = eprintln ("while reading predefined functions, "
                                                                            ^ s)
(* utility functions for string manipulation and printing 1119a *)
fun intString n =
  String.map (fn #"~" => #"-" | c => c) (Int.toString n)
(* utility functions for string manipulation and printing 1119b *)
fun plural what [x] = what
  | plural what _   = what ^ "s"

fun countString xs what =
  intString (length xs) ^ " " ^ plural what xs
(* utility functions for string manipulation and printing 1119c *)
fun separate (zero, sep) = 
  (* list with separator *)
  let fun s []     = zero
        | s [x]    = x
        | s (h::t) = h ^ sep ^ s t
  in  s
end
val spaceSep = separate ("", " ")   (* list separated by spaces *)
val commaSep = separate ("", ", ")  (* list separated by commas *)
(* type declarations for consistency checking *)
val _ = op intString : int -> string
(* type declarations for consistency checking *)
val _ = op spaceSep :                    string list -> string
val _ = op commaSep :                    string list -> string
val _ = op separate : string * string -> string list -> string
(* utility functions for string manipulation and printing 1120a *)
fun printUTF8 code =
  let val w = Word.fromInt code
      val (&, >>) = (Word.andb, Word.>>)
      infix 6 & >>
      val _ = if (w & 0wx1fffff) <> w then
                raise RuntimeError (intString code ^
                                    " does not represent a Unicode code point")
              else
                 ()
      val printbyte = xprint o str o chr o Word.toInt
      fun prefix byte byte' = Word.orb (byte, byte')
  in  if w > 0wxffff then
        app printbyte [ prefix 0wxf0  (w >> 0w18)
                      , prefix 0wx80 ((w >> 0w12) & 0wx3f)
                      , prefix 0wx80 ((w >>  0w6) & 0wx3f)
                      , prefix 0wx80 ((w      ) & 0wx3f)
                      ]
      else if w > 0wx7ff then
        app printbyte [ prefix 0wxe0  (w >> 0w12)
                      , prefix 0wx80 ((w >>  0w6) & 0wx3f)
                      , prefix 0wx80 ((w        ) & 0wx3f)
                      ]
      else if w > 0wx7f then
        app printbyte [ prefix 0wxc0  (w >>  0w6)
                      , prefix 0wx80 ((w        ) & 0wx3f)
                      ]
      else
        printbyte w
  end
(* utility functions for string manipulation and printing 1120b *)
fun stripNumericSuffix s =
      let fun stripPrefix []         = s   (* don't let things get empty *)
            | stripPrefix (#"-"::[]) = s
            | stripPrefix (#"-"::cs) = implode (reverse cs)
            | stripPrefix (c   ::cs) = if Char.isDigit c then stripPrefix cs
                                       else implode (reverse (c::cs))
      in  stripPrefix (reverse (explode s))
      end
(* support for representing errors as \ml\ values 1124b *)
datatype 'a error = OK of 'a | ERROR of string
(* support for representing errors as \ml\ values 1125a *)
infix 1 >>=
fun (OK x)      >>= k  =  k x
  | (ERROR msg) >>= k  =  ERROR msg
(* type declarations for consistency checking *)
val _ = op zip3   : 'a list * 'b list * 'c list -> ('a * 'b * 'c) list
val _ = op unzip3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
(* type declarations for consistency checking *)
val _ = op optionList : 'a option list -> 'a list option
(* type declarations for consistency checking *)
val _ = op >>= : 'a error * ('a -> 'b error) -> 'b error
(* support for representing errors as \ml\ values 1125b *)
infix 1 >>=+
fun e >>=+ k'  =  e >>= (OK o k')
(* type declarations for consistency checking *)
val _ = op >>=+ : 'a error * ('a -> 'b) -> 'b error
(* support for representing errors as \ml\ values 1126a *)
fun errorList es =
  let fun cons (OK x, OK xs) = OK (x :: xs)
        | cons (ERROR m1, ERROR m2) = ERROR (m1 ^ "; " ^ m2)
        | cons (ERROR m, OK _) = ERROR m
        | cons (OK _, ERROR m) = ERROR m
  in  foldr cons (OK []) es
  end
(* type declarations for consistency checking *)
val _ = op errorList : 'a error list -> 'a list error
(* type [[interactivity]] plus related functions and value 377a *)
datatype input_interactivity = PROMPTING | NOT_PROMPTING
(* type [[interactivity]] plus related functions and value 377b *)
datatype output_interactivity = PRINTING | NOT_PRINTING
(* type [[interactivity]] plus related functions and value 377c *)
type interactivity = 
  input_interactivity * output_interactivity
val noninteractive = 
  (NOT_PROMPTING, NOT_PRINTING)
fun prompts (PROMPTING,     _) = true
  | prompts (NOT_PROMPTING, _) = false
fun prints (_, PRINTING)     = true
  | prints (_, NOT_PRINTING) = false
(* type declarations for consistency checking *)
type interactivity = interactivity
val _ = op noninteractive : interactivity
val _ = op prompts : interactivity -> bool
val _ = op prints  : interactivity -> bool
(* simple implementations of set operations 1121a *)
type 'a set = 'a list
val emptyset = []
fun member x = 
  List.exists (fn y => y = x)
fun insert (x, ys) = 
  if member x ys then ys else x::ys
fun union (xs, ys) = foldl insert ys xs
fun inter (xs, ys) =
  List.filter (fn x => member x ys) xs
fun diff  (xs, ys) = 
  List.filter (fn x => not (member x ys)) xs
(* type declarations for consistency checking *)
type 'a set = 'a set
val _ = op emptyset : 'a set
val _ = op member   : ''a -> ''a set -> bool
val _ = op insert   : ''a     * ''a set  -> ''a set
val _ = op union    : ''a set * ''a set  -> ''a set
val _ = op inter    : ''a set * ''a set  -> ''a set
val _ = op diff     : ''a set * ''a set  -> ''a set
(* collections with mapping and combining functions 1121b *)
datatype 'a collection = C of 'a set
fun elemsC (C xs) = xs
fun singleC x     = C [x]
val emptyC        = C []
(* type declarations for consistency checking *)
type 'a collection = 'a collection
val _ = op elemsC  : 'a collection -> 'a set
val _ = op singleC : 'a -> 'a collection
val _ = op emptyC  :       'a collection
(* collections with mapping and combining functions 1122a *)
fun joinC     (C xs) = C (List.concat (map elemsC xs))
fun mapC  f   (C xs) = C (map f xs)
fun filterC p (C xs) = C (List.filter p xs)
fun mapC2 f (xc, yc) = joinC (mapC (fn x => mapC (fn y => f (x, y)) yc) xc)
(* type declarations for consistency checking *)
val _ = op joinC   : 'a collection collection -> 'a collection
val _ = op mapC    : ('a -> 'b)      -> ('a collection -> 'b collection)
val _ = op filterC : ('a -> bool)    -> ('a collection -> 'a collection)
val _ = op mapC2   : ('a * 'b -> 'c) -> ('a collection * 'b collection -> 'c
                                                                     collection)
(* suspensions 1131a *)
datatype 'a action
  = PENDING  of unit -> 'a
  | PRODUCED of 'a

type 'a susp = 'a action ref
(* type declarations for consistency checking *)
type 'a susp = 'a susp
(* suspensions 1131b *)
fun delay f = ref (PENDING f)
fun demand cell =
  case !cell
    of PENDING f =>  let val result = f ()
                     in  (cell := PRODUCED result; result)
                     end
     | PRODUCED v => v
(* type declarations for consistency checking *)
val _ = op delay  : (unit -> 'a) -> 'a susp
val _ = op demand : 'a susp -> 'a
(* streams 1132a *)
datatype 'a stream 
  = EOS
  | :::       of 'a * 'a stream
  | SUSPENDED of 'a stream susp
infixr 3 :::
(* streams 1132b *)
fun streamGet EOS = NONE
  | streamGet (x ::: xs)    = SOME (x, xs)
  | streamGet (SUSPENDED s) = streamGet (demand s)
(* streams 1132c *)
fun streamOfList xs = 
  foldr (op :::) EOS xs
(* type declarations for consistency checking *)
val _ = op streamGet : 'a stream -> ('a * 'a stream) option
(* type declarations for consistency checking *)
val _ = op streamOfList : 'a list -> 'a stream
(* streams 1132d *)
fun listOfStream xs =
  case streamGet xs
    of NONE => []
     | SOME (x, xs) => x :: listOfStream xs
(* streams 1132e *)
fun delayedStream action = 
  SUSPENDED (delay action)
(* type declarations for consistency checking *)
val _ = op listOfStream : 'a stream -> 'a list
(* type declarations for consistency checking *)
val _ = op delayedStream : (unit -> 'a stream) -> 'a stream
(* streams 1133a *)
fun streamOfEffects action =
  delayedStream (fn () => case action () of NONE   => EOS
                                          | SOME a => a ::: streamOfEffects
                                                                         action)
(* type declarations for consistency checking *)
val _ = op streamOfEffects : (unit -> 'a option) -> 'a stream
(* streams 1133b *)
type line = string
fun filelines infile = 
  streamOfEffects (fn () => TextIO.inputLine infile)
(* type declarations for consistency checking *)
type line = line
val _ = op filelines : TextIO.instream -> line stream
(* streams 1133c *)
fun streamRepeat x =
  delayedStream (fn () => x ::: streamRepeat x)
(* type declarations for consistency checking *)
val _ = op streamRepeat : 'a -> 'a stream
(* streams 1133d *)
fun streamOfUnfold next state =
  delayedStream (fn () => case next state
                            of NONE => EOS
                             | SOME (a, state') => a ::: streamOfUnfold next
                                                                         state')
(* type declarations for consistency checking *)
val _ = op streamOfUnfold : ('b -> ('a * 'b) option) -> 'b -> 'a stream
(* streams 1133e *)
val naturals = 
  streamOfUnfold (fn n => SOME (n, n+1)) 0   (* 0 to infinity *)
(* type declarations for consistency checking *)
val _ = op naturals : int stream
(* streams 1134a *)
fun preStream (pre, xs) = 
  streamOfUnfold (fn xs => (pre (); streamGet xs)) xs
(* streams 1134b *)
fun postStream (xs, post) =
  streamOfUnfold (fn xs => case streamGet xs
                             of NONE => NONE
                              | head as SOME (x, _) => (post x; head)) xs
(* type declarations for consistency checking *)
val _ = op preStream : (unit -> unit) * 'a stream -> 'a stream
(* type declarations for consistency checking *)
val _ = op postStream : 'a stream * ('a -> unit) -> 'a stream
(* streams 1134c *)
fun streamMap f xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => f x ::: streamMap f xs)
(* type declarations for consistency checking *)
val _ = op streamMap : ('a -> 'b) -> 'a stream -> 'b stream
(* streams 1134d *)
fun streamFilter p xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => if p x then x ::: streamFilter p
                                                                              xs
                                               else streamFilter p xs)
(* type declarations for consistency checking *)
val _ = op streamFilter : ('a -> bool) -> 'a stream -> 'a stream
(* streams 1135a *)
fun streamFold f z xs =
  case streamGet xs of NONE => z
                     | SOME (x, xs) => streamFold f (f (x, z)) xs
(* type declarations for consistency checking *)
val _ = op streamFold : ('a * 'b -> 'b) -> 'b -> 'a stream -> 'b
(* streams 1135b *)
fun streamZip (xs, ys) =
  delayedStream
  (fn () => case (streamGet xs, streamGet ys)
              of (SOME (x, xs), SOME (y, ys)) => (x, y) ::: streamZip (xs, ys)
               | _ => EOS)
(* streams 1135c *)
fun streamConcat xss =
  let fun get (xs, xss) =
        case streamGet xs
          of SOME (x, xs) => SOME (x, (xs, xss))
           | NONE => case streamGet xss
                       of SOME (xs, xss) => get (xs, xss)
                        | NONE => NONE
  in  streamOfUnfold get (EOS, xss)
  end
(* type declarations for consistency checking *)
val _ = op streamZip : 'a stream * 'b stream -> ('a * 'b) stream
(* type declarations for consistency checking *)
val _ = op streamConcat : 'a stream stream -> 'a stream
(* streams 1135d *)
fun streamConcatMap f xs = streamConcat (streamMap f xs)
(* type declarations for consistency checking *)
val _ = op streamConcatMap : ('a -> 'b stream) -> 'a stream -> 'b stream
(* streams 1135e *)
infix 5 @@@
fun xs @@@ xs' = streamConcat (streamOfList [xs, xs'])
(* type declarations for consistency checking *)
val _ = op @@@ : 'a stream * 'a stream -> 'a stream
(* streams 1136a *)
fun streamTake (0, xs) = []
  | streamTake (n, xs) =
      case streamGet xs
        of SOME (x, xs) => x :: streamTake (n-1, xs)
         | NONE => []
(* type declarations for consistency checking *)
val _ = op streamTake : int * 'a stream -> 'a list
(* streams 1136b *)
fun streamDrop (0, xs) = xs
  | streamDrop (n, xs) =
      case streamGet xs
        of SOME (_, xs) => streamDrop (n-1, xs)
         | NONE => EOS
(* type declarations for consistency checking *)
val _ = op streamDrop : int * 'a stream -> 'a stream
(* stream transformers and their combinators 1143a *)
type ('a, 'b) xformer = 
  'a stream -> ('b error * 'a stream) option
(* type declarations for consistency checking *)
type ('a, 'b) xformer = ('a, 'b) xformer
(* stream transformers and their combinators 1143b *)
fun pure y = fn xs => SOME (OK y, xs)
(* type declarations for consistency checking *)
val _ = op pure : 'b -> ('a, 'b) xformer
(* stream transformers and their combinators 1145a *)
infix 3 <*>
fun tx_f <*> tx_b =
  fn xs => case tx_f xs
             of NONE => NONE
              | SOME (ERROR msg, xs) => SOME (ERROR msg, xs)
              | SOME (OK f, xs) =>
                  case tx_b xs
                    of NONE => NONE
                     | SOME (ERROR msg, xs) => SOME (ERROR msg, xs)
                     | SOME (OK y, xs) => SOME (OK (f y), xs)
(* type declarations for consistency checking *)
val _ = op <*> : ('a, 'b -> 'c) xformer * ('a, 'b) xformer -> ('a, 'c) xformer
(* stream transformers and their combinators 1145b *)
infixr 4 <$>
fun f <$> p = pure f <*> p
(* type declarations for consistency checking *)
val _ = op <$> : ('b -> 'c) * ('a, 'b) xformer -> ('a, 'c) xformer
(* stream transformers and their combinators 1146a *)
infix 1 <|>
fun t1 <|> t2 = (fn xs => case t1 xs of SOME y => SOME y | NONE => t2 xs) 
(* type declarations for consistency checking *)
val _ = op <|> : ('a, 'b) xformer * ('a, 'b) xformer -> ('a, 'b) xformer
(* stream transformers and their combinators 1146b *)
fun pzero _ = NONE
(* stream transformers and their combinators 1146c *)
fun anyParser ts = 
  foldr op <|> pzero ts
(* type declarations for consistency checking *)
val _ = op pzero : ('a, 'b) xformer
(* type declarations for consistency checking *)
val _ = op anyParser : ('a, 'b) xformer list -> ('a, 'b) xformer
(* stream transformers and their combinators 1147a *)
infix 6 <* *>
fun p1 <*  p2 = curry fst <$> p1 <*> p2
fun p1  *> p2 = curry snd <$> p1 <*> p2

infixr 4 <$
fun v <$ p = (fn _ => v) <$> p
(* type declarations for consistency checking *)
val _ = op <*  : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'b) xformer
val _ = op  *> : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'c) xformer
val _ = op <$  : 'b               * ('a, 'c) xformer -> ('a, 'b) xformer
(* stream transformers and their combinators 1147b *)
fun one xs = case streamGet xs
               of NONE => NONE
                | SOME (x, xs) => SOME (OK x, xs)
(* type declarations for consistency checking *)
val _ = op one : ('a, 'a) xformer
(* stream transformers and their combinators 1147c *)
fun eos xs = case streamGet xs
               of NONE => SOME (OK (), EOS)
                | SOME _ => NONE
(* type declarations for consistency checking *)
val _ = op eos : ('a, unit) xformer
(* stream transformers and their combinators 1148a *)
fun peek tx xs =
  case tx xs of SOME (OK y, _) => SOME y
              | _ => NONE
(* type declarations for consistency checking *)
val _ = op peek : ('a, 'b) xformer -> 'a stream -> 'b option
(* stream transformers and their combinators 1148b *)
fun rewind tx xs =
  case tx xs of SOME (ey, _) => SOME (ey, xs)
              | NONE => NONE
(* type declarations for consistency checking *)
val _ = op rewind : ('a, 'b) xformer -> ('a, 'b) xformer
(* stream transformers and their combinators 1148c *)
fun sat p tx xs =
  case tx xs
    of answer as SOME (OK y, xs) => if p y then answer else NONE
     | answer => answer
(* type declarations for consistency checking *)
val _ = op sat : ('b -> bool) -> ('a, 'b) xformer -> ('a, 'b) xformer
(* stream transformers and their combinators 1148d *)
fun eqx y = 
  sat (fn y' => y = y') 
(* type declarations for consistency checking *)
val _ = op eqx : ''b -> ('a, ''b) xformer -> ('a, ''b) xformer
(* stream transformers and their combinators 1149a *)
infixr 4 <$>?
fun f <$>? tx =
  fn xs => case tx xs
             of NONE => NONE
              | SOME (ERROR msg, xs) => SOME (ERROR msg, xs)
              | SOME (OK y, xs) =>
                  case f y
                    of NONE => NONE
                     | SOME z => SOME (OK z, xs)
(* type declarations for consistency checking *)
val _ = op <$>? : ('b -> 'c option) * ('a, 'b) xformer -> ('a, 'c) xformer
(* stream transformers and their combinators 1149b *)
infix 3 <&>
fun t1 <&> t2 = fn xs =>
  case t1 xs
    of SOME (OK _, _) => t2 xs
     | SOME (ERROR _, _) => NONE    
     | NONE => NONE
(* type declarations for consistency checking *)
val _ = op <&> : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'c) xformer
(* stream transformers and their combinators 1149c *)
fun notFollowedBy t xs =
  case t xs
    of NONE => SOME (OK (), xs)
     | SOME _ => NONE
(* type declarations for consistency checking *)
val _ = op notFollowedBy : ('a, 'b) xformer -> ('a, unit) xformer
(* stream transformers and their combinators 1149d *)
fun many t = 
  curry (op ::) <$> t <*> (fn xs => many t xs) <|> pure []
(* type declarations for consistency checking *)
val _ = op many  : ('a, 'b) xformer -> ('a, 'b list) xformer
(* stream transformers and their combinators 1150a *)
fun many1 t = 
  curry (op ::) <$> t <*> many t
(* type declarations for consistency checking *)
val _ = op many1 : ('a, 'b) xformer -> ('a, 'b list) xformer
(* stream transformers and their combinators 1150b *)
fun optional t = 
  SOME <$> t <|> pure NONE
(* type declarations for consistency checking *)
val _ = op optional : ('a, 'b) xformer -> ('a, 'b option) xformer
(* stream transformers and their combinators 1151a *)
infix 2 <*>!
fun tx_ef <*>! tx_x =
  fn xs => case (tx_ef <*> tx_x) xs
             of NONE => NONE
              | SOME (OK (OK y),      xs) => SOME (OK y,      xs)
              | SOME (OK (ERROR msg), xs) => SOME (ERROR msg, xs)
              | SOME (ERROR msg,      xs) => SOME (ERROR msg, xs)
infixr 4 <$>!
fun ef <$>! tx_x = pure ef <*>! tx_x
(* type declarations for consistency checking *)
val _ = op <*>! : ('a, 'b -> 'c error) xformer * ('a, 'b) xformer -> ('a, 'c)
                                                                         xformer
val _ = op <$>! : ('b -> 'c error)             * ('a, 'b) xformer -> ('a, 'c)
                                                                         xformer
(* support for source-code locations and located streams 1136d *)
type srcloc = string * int
fun srclocString (source, line) =
  source ^ ", line " ^ intString line
(* support for source-code locations and located streams 1137a *)
datatype error_format = WITH_LOCATIONS | WITHOUT_LOCATIONS
val toplevel_error_format = ref WITH_LOCATIONS
(* support for source-code locations and located streams 1137b *)
fun synerrormsg (source, line) strings =
  if !toplevel_error_format = WITHOUT_LOCATIONS andalso source =
                                                                "standard input"
  then
    concat ("syntax error: " :: strings)
  else    
    concat ("syntax error in " :: srclocString (source, line) :: ": " :: strings
                                                                               )

(* support for source-code locations and located streams 1137c *)
exception Located of srcloc * exn
(* support for source-code locations and located streams 1137d *)
type 'a located = srcloc * 'a
(* type declarations for consistency checking *)
type srcloc = srcloc
val _ = op srclocString : srcloc -> string
(* type declarations for consistency checking *)
type 'a located = 'a located
(* support for source-code locations and located streams 1137e *)
fun atLoc loc f a =
  f a handle e as RuntimeError _ => raise Located (loc, e)
           | e as NotFound _     => raise Located (loc, e)

(* more handlers for [[atLoc]] (BUG: software can't tell where this code came from [NW2AyK2m-Bmx6f-1]) *)
           | e as BlackHole => raise Located (loc, e)
           (* more handlers for [[atLoc]] 1137g *)
           | e as IO.Io _   => raise Located (loc, e)
           | e as Div       => raise Located (loc, e)
           | e as Overflow  => raise Located (loc, e)
           | e as Subscript => raise Located (loc, e)
           | e as Size      => raise Located (loc, e)
(* type declarations for consistency checking *)
val _ = op atLoc : srcloc -> ('a -> 'b) -> ('a -> 'b)
(* support for source-code locations and located streams 1137f *)
fun located f (loc, a) = atLoc loc f a
fun leftLocated f ((loc, a), b) = atLoc loc f (a, b)
(* type declarations for consistency checking *)
val _ = op located : ('a -> 'b) -> ('a located -> 'b)
val _ = op leftLocated : ('a * 'b -> 'c) -> ('a located * 'b -> 'c)
(* support for source-code locations and located streams 1138a *)
fun fillComplaintTemplate (s, maybeLoc) =
  let val string_to_fill = " <at loc>"
      val (prefix, atloc) = Substring.position string_to_fill (Substring.full s)
      val suffix = Substring.triml (size string_to_fill) atloc
      val splice_in =
        Substring.full (case maybeLoc
                          of NONE => ""
                           | SOME (loc as (file, line)) =>
                               if      !toplevel_error_format =
                                                               WITHOUT_LOCATIONS
                               andalso file = "standard input"
                               then
                                 ""
                               else
                                 " in " ^ srclocString loc)
  in  if Substring.size atloc = 0 then (* <at loc> is not present *)
        s
      else
        Substring.concat [prefix, splice_in, suffix]
  end
fun fillAtLoc (s, loc) = fillComplaintTemplate (s, SOME loc)
fun stripAtLoc s = fillComplaintTemplate (s, NONE)
(* type declarations for consistency checking *)
val _ = op fillComplaintTemplate : string * srcloc option -> string
(* support for source-code locations and located streams 1138b *)
fun errorAt msg loc = 
  ERROR (synerrormsg loc [msg])
(* support for source-code locations and located streams 1138c *)
fun locatedStream (streamname, inputs) =
  let val locations = streamZip (streamRepeat streamname, streamDrop (1,
                                                                      naturals))
  in  streamZip (locations, inputs)
  end
(* type declarations for consistency checking *)
val _ = op errorAt : string -> srcloc -> 'a error
(* type declarations for consistency checking *)
val _ = op locatedStream : string * line stream -> line located stream
(* streams that track line boundaries 1155a *)
datatype 'a eol_marked
  = EOL of int (* number of the line that ends here *)
  | INLINE of 'a

fun drainLine EOS = EOS
  | drainLine (SUSPENDED s)     = drainLine (demand s)
  | drainLine (EOL _    ::: xs) = xs
  | drainLine (INLINE _ ::: xs) = drainLine xs
(* streams that track line boundaries 1155b *)
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
(* type declarations for consistency checking *)
type 'a eol_marked = 'a eol_marked
val _ = op drainLine : 'a eol_marked stream -> 'a eol_marked stream
(* type declarations for consistency checking *)
val _ = op eol      : ('a eol_marked, int) xformer
val _ = op inline   : ('a eol_marked, 'a)  xformer
val _ = op srcloc   : ('a located eol_marked, srcloc) xformer
(* support for lexical analysis 1151b *)
type 'a lexer = (char, 'a) xformer
(* type declarations for consistency checking *)
type 'a lexer = 'a lexer
(* support for lexical analysis 1151c *)
fun isDelim c =
  Char.isSpace c orelse Char.contains "()[]{};" c
(* type declarations for consistency checking *)
val _ = op isDelim : char -> bool
(* support for lexical analysis 1153a *)
val whitespace = many (sat Char.isSpace one)
(* type declarations for consistency checking *)
val _ = op whitespace : char list lexer
(* support for lexical analysis 1153b *)
fun intChars isDelim = 
  (curry (op ::) <$> eqx #"-" one <|> pure id) <*> many1 (sat Char.isDigit one)
                                                                              <*
  notFollowedBy (sat (not o isDelim) one)
(* type declarations for consistency checking *)
val _ = op intChars : (char -> bool) -> char list lexer
(* support for lexical analysis 1153c *)
fun intFromChars (#"-" :: cs) = 
      intFromChars cs >>=+ Int.~
  | intFromChars cs =
      (OK o valOf o Int.fromString o implode) cs
      handle Overflow => ERROR
                        "this interpreter can't read arbitrarily large integers"
(* type declarations for consistency checking *)
val _ = op intFromChars : char list -> int error
(* support for lexical analysis 1153d *)
fun intToken isDelim =
  intFromChars <$>! intChars isDelim
(* type declarations for consistency checking *)
val _ = op intToken : (char -> bool) -> int lexer
(* support for lexical analysis 1154a *)
datatype bracket_shape = ROUND | SQUARE | CURLY

fun leftString ROUND  = "("
  | leftString SQUARE = "["
  | leftString CURLY  = "{"
fun rightString ROUND  = ")"
  | rightString SQUARE = "]"
  | rightString CURLY = "}"
(* support for lexical analysis 1154b *)
datatype 'a plus_brackets
  = LEFT  of bracket_shape
  | RIGHT of bracket_shape
  | PRETOKEN of 'a

fun bracketLexer pretoken
  =  LEFT  ROUND  <$ eqx #"(" one
 <|> LEFT  SQUARE <$ eqx #"[" one
 <|> LEFT  CURLY  <$ eqx #"{" one
 <|> RIGHT ROUND  <$ eqx #")" one
 <|> RIGHT SQUARE <$ eqx #"]" one
 <|> RIGHT CURLY  <$ eqx #"}" one
 <|> PRETOKEN <$> pretoken

fun plusBracketsString _   (LEFT shape)  = leftString shape
  | plusBracketsString _   (RIGHT shape) = rightString shape
  | plusBracketsString pts (PRETOKEN pt)  = pts pt
(* type declarations for consistency checking *)
type 'a plus_brackets = 'a plus_brackets
val _ = op bracketLexer : 'a lexer -> 'a plus_brackets lexer
(* common parsing code 1142 *)
(* combinators and utilities for parsing located streams 1155c *)
type ('t, 'a) polyparser = ('t located eol_marked, 'a) xformer
(* combinators and utilities for parsing located streams 1156a *)
fun token    stream = (snd <$> inline)      stream
fun noTokens stream = (notFollowedBy token) stream
(* type declarations for consistency checking *)
val _ = op token    : ('t, 't)   polyparser
val _ = op noTokens : ('t, unit) polyparser
(* combinators and utilities for parsing located streams 1156b *)
fun @@ p = pair <$> srcloc <*> p
(* type declarations for consistency checking *)
val _ = op @@ : ('t, 'a) polyparser -> ('t, 'a located) polyparser
(* combinators and utilities for parsing located streams 1156c *)
infix 0 <?>
fun p <?> what = p <|> errorAt ("expected " ^ what) <$>! srcloc
(* type declarations for consistency checking *)
val _ = op <?> : ('t, 'a) polyparser * string -> ('t, 'a) polyparser
(* combinators and utilities for parsing located streams 1157 *)
infix 4 <!>
fun p <!> msg =
  fn tokens => (case p tokens
                  of SOME (OK _, unread) =>
                       (case peek srcloc tokens
                          of SOME loc => SOME (errorAt msg loc, unread)
                           | NONE => NONE)
                   | _ => NONE)
(* type declarations for consistency checking *)
val _ = op <!> : ('t, 'a) polyparser * string -> ('t, 'b) polyparser
(* combinators and utilities for parsing located streams 1160d *)
fun nodups (what, context) (loc, names) =
  let fun dup [] = OK names
        | dup (x::xs) = if List.exists (fn y : string => y = x) xs then
                          errorAt (what ^ " " ^ x ^ " appears twice in " ^
                                                                    context) loc
                        else
                          dup xs
  in  dup names
  end
(* type declarations for consistency checking *)
val _ = op nodups : string * string -> srcloc * name list -> name list error
(* transformers for interchangeable brackets 1158 *)
fun notCurly (_, CURLY) = false
  | notCurly _          = true

(* left: takes shape, succeeds or fails
   right: takes shape and
      succeeds with right bracket of correct shape
      errors with right bracket of incorrect shape
      fails with token that is not right bracket *)

fun left  tokens = ((fn (loc, LEFT  s) => SOME (loc, s) | _ => NONE) <$>? inline
                                                                        ) tokens
fun right tokens = ((fn (loc, RIGHT s) => SOME (loc, s) | _ => NONE) <$>? inline
                                                                        ) tokens
fun leftCurly tokens = sat (not o notCurly) left tokens

fun atRight expected = rewind right <?> expected

fun badRight msg =
  (fn (loc, shape) => errorAt (msg ^ " " ^ rightString shape) loc) <$>! right
(* transformers for interchangeable brackets 1159 *)
type ('t, 'a) pb_parser = ('t plus_brackets, 'a) polyparser
datatype right_result
  = FOUND_RIGHT      of bracket_shape located
  | SCANNED_TO_RIGHT of srcloc  (* location where scanning started *)
  | NO_RIGHT

fun scanToClose tokens = 
  let val loc = getOpt (peek srcloc tokens, ("end of stream", 9999))
      fun scan lpcount tokens =
        (* lpcount is the number of unmatched left parentheses *)
        case tokens
          of EOL _                  ::: tokens => scan lpcount tokens
           | INLINE (_, LEFT  t)    ::: tokens => scan (lpcount+1) tokens
           | INLINE (_, RIGHT t)    ::: tokens => if lpcount = 0 then
                                                    pure (SCANNED_TO_RIGHT loc)
                                                                          tokens
                                                  else
                                                    scan (lpcount-1) tokens
           | INLINE (_, PRETOKEN _) ::: tokens => scan lpcount tokens
           | EOS         => pure NO_RIGHT tokens
           | SUSPENDED s => scan lpcount (demand s)
  in  scan 0 tokens
  end

fun matchingRight tokens = (FOUND_RIGHT <$> right <|> scanToClose) tokens

fun matchBrackets _ (loc, left) _ NO_RIGHT =
      errorAt ("unmatched " ^ leftString left) loc
  | matchBrackets e (loc, left) _ (SCANNED_TO_RIGHT loc') =
      errorAt ("expected " ^ e) loc
  | matchBrackets _ (loc, left) a (FOUND_RIGHT (loc', right)) =
      if left = right then
        OK a
      else
        errorAt (rightString right ^ " does not match " ^ leftString left ^
                 (if loc <> loc' then " at " ^ srclocString loc else "")) loc'
(* type declarations for consistency checking *)
type right_result = right_result
val _ = op matchingRight : ('t, right_result) pb_parser
val _ = op scanToClose   : ('t, right_result) pb_parser
val _ = op matchBrackets : string -> bracket_shape located -> 'a -> right_result
                                                                     -> 'a error
(* transformers for interchangeable brackets 1160a *)
fun liberalBracket (expected, p) =
  matchBrackets expected <$> sat notCurly left <*> p <*>! matchingRight
fun bracketKeyword (keyword, expected, p) =
  liberalBracket (expected, keyword *> (p <?> expected))
fun bracket (expected, p) =
  liberalBracket (expected, p <?> expected)
fun curlyBracket (expected, p) =
  matchBrackets expected <$> leftCurly <*> (p <?> expected) <*>! matchingRight
(* type declarations for consistency checking *)
val _ = op bracketKeyword : ('t, 'keyword) pb_parser * string * ('t, 'a)
                                                 pb_parser -> ('t, 'a) pb_parser
(* transformers for interchangeable brackets 1160b *)
fun usageParser keyword =
  let val getkeyword = eqx #"(" one *> (implode <$> many1 (sat (not o isDelim)
                                                                           one))
  in  fn (usage, p) =>
        case getkeyword (streamOfList (explode usage))
          of SOME (OK k, _) => bracketKeyword (keyword k, usage, p)
           | _ => let exception BadUsage of string in raise BadUsage usage end
  end
(* type declarations for consistency checking *)
val _ = op usageParser : (string -> ('t, string) pb_parser) -> string * ('t, 'a)
                                                 pb_parser -> ('t, 'a) pb_parser
(* transformers for interchangeable brackets 1160c *)
fun pretoken stream = ((fn PRETOKEN t => SOME t | _ => NONE) <$>? token) stream
(* code used to debug parsers 1161a *)
fun safeTokens stream =
  let fun tokens (seenEol, seenSuspended) =
            let fun get (EOL _         ::: ts) = if seenSuspended then []
                                                 else tokens (true, false) ts
                  | get (INLINE (_, t) ::: ts) = t :: get ts
                  | get  EOS                   = []
                  | get (SUSPENDED (ref (PRODUCED ts))) = get ts
                  | get (SUSPENDED s) = if seenEol then []
                                        else tokens (false, true) (demand s)
            in   get
            end
  in  tokens (false, false) stream
  end
(* type declarations for consistency checking *)
val _ = op safeTokens : 'a located eol_marked stream -> 'a list
(* code used to debug parsers 1161b *)
fun showErrorInput asString p tokens =
  case p tokens
    of result as SOME (ERROR msg, rest) =>
         if String.isSubstring " [input: " msg then
           result
         else
           SOME (ERROR (msg ^ " [input: " ^
                        spaceSep (map asString (safeTokens tokens)) ^ "]"),
               rest)
     | result => result
(* type declarations for consistency checking *)
val _ = op showErrorInput : ('t -> string) -> ('t, 'a) polyparser -> ('t, 'a)
                                                                      polyparser
(* code used to debug parsers 1162a *)
fun wrapAround tokenString what p tokens =
  let fun t tok = " " ^ tokenString tok
      val _ = app eprint ["Looking for ", what, " at"]
      val _ = app (eprint o t) (safeTokens tokens)
      val _ = eprint "\n"
      val answer = p tokens
      val _ = app eprint [case answer of NONE => "Didn't find " | SOME _ =>
                                                                       "Found ",
                         what, "\n"]
  in  answer
  end handle e => ( app eprint ["Search for ", what, " raised ", exnName e, "\n"
                                                                               ]
                  ; raise e)
(* type declarations for consistency checking *)
val _ = op wrapAround : ('t -> string) -> string -> ('t, 'a) polyparser -> ('t,
                                                                  'a) polyparser
(* streams that issue two forms of prompts 1162b *)
fun echoTagStream lines = 
  let fun echoIfTagged line =
        if (String.substring (line, 0, 2) = ";#" handle _ => false) then
          print line
        else
          ()
  in  postStream (lines, echoIfTagged)
  end
(* type declarations for consistency checking *)
val _ = op echoTagStream : line stream -> line stream 
(* streams that issue two forms of prompts 1163a *)
fun stripAndReportErrors xs =
  let fun next xs =
        case streamGet xs
          of SOME (ERROR msg, xs) => (eprintln msg; next xs)
           | SOME (OK x, xs) => SOME (x, xs)
           | NONE => NONE
  in  streamOfUnfold next xs
  end
(* type declarations for consistency checking *)
val _ = op stripAndReportErrors : 'a error stream -> 'a stream
(* streams that issue two forms of prompts 1163b *)
fun lexLineWith lexer =
  stripAndReportErrors o streamOfUnfold lexer o streamOfList o explode
(* type declarations for consistency checking *)
val _ = op lexLineWith : 't lexer -> line -> 't stream
(* streams that issue two forms of prompts 1163c *)
fun parseWithErrors parser =
  let fun adjust (SOME (ERROR msg, tokens)) = SOME (ERROR msg, drainLine tokens)
        | adjust other = other
  in  streamOfUnfold (adjust o parser)
  end
(* type declarations for consistency checking *)
val _ = op parseWithErrors : ('t, 'a) polyparser -> 't located eol_marked stream
                                                              -> 'a error stream
(* streams that issue two forms of prompts 1163d *)
type prompts   = { ps1 : string, ps2 : string }
val stdPrompts = { ps1 = "-> ", ps2 = "   " }
val noPrompts  = { ps1 = "", ps2 = "" }
(* type declarations for consistency checking *)
type prompts = prompts
val _ = op stdPrompts : prompts
val _ = op noPrompts  : prompts
(* streams that issue two forms of prompts 1164 *)
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
(* type declarations for consistency checking *)
val _ = op interactiveParsedStream : 't lexer * ('t, 'a) polyparser -> string *
                                              line stream * prompts -> 'a stream
val _ = op lexAndDecorate : srcloc * line -> 't located eol_marked stream
  in  
      stripAndReportErrors (preStream (setPrompt ps1, xdefs_with_errors))
  end 
(* common parsing code ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
fun ('t, 'a) finiteStreamOfLine fail (lexer, parser) line =
  let val lines = streamOfList [line] @@@ streamOfEffects fail
      fun lexAndDecorate (loc, line) =
        let val tokens = lexLineWith lexer line
        in  streamMap INLINE (streamZip (streamRepeat loc, tokens)) @@@
            streamOfList [EOL (snd loc)]
        end

      val things_with_errors : 'a error stream = 
        (parseWithErrors parser o streamConcatMap lexAndDecorate o locatedStream
                                                                               )
        ("command line", lines)
  in  
      stripAndReportErrors things_with_errors
  end 
val _ = finiteStreamOfLine :
          (unit -> string option) -> 't lexer * ('t, 'a) polyparser -> line ->
                                                                       'a stream
(* shared utility functions for initializing interpreters 381a *)
fun override_if_testing () =                           (*OMIT*)
  if isSome (OS.Process.getEnv "NOERRORLOC") then      (*OMIT*)
    toplevel_error_format := WITHOUT_LOCATIONS         (*OMIT*)
  else                                                 (*OMIT*)
    ()                                                 (*OMIT*)
fun setup_error_format interactivity =
  if prompts interactivity then
    toplevel_error_format := WITHOUT_LOCATIONS
    before override_if_testing () (*OMIT*)
  else
    toplevel_error_format := WITH_LOCATIONS
    before override_if_testing () (*OMIT*)
(* function application with overflow checking 1123 *)
local
  val throttleCPU = case OS.Process.getEnv "BPCOPTIONS"
                      of SOME "nothrottle" => false
                       | _ => true
  val defaultRecursionLimit = 6000 (* about 1/5 of 32,000? *)
  val recursionLimit = ref defaultRecursionLimit
  val evalFuel       = ref 1000000
in
  val defaultEvalFuel = ref (!evalFuel)
  fun withFuel n f x = 
    let val old = !evalFuel
        val _ = evalFuel := n
    in  (f x before evalFuel := old) handle e => (evalFuel := old; raise e)
    end

  fun fuelRemaining () = !evalFuel

  fun applyCheckingOverflow f =
    if !recursionLimit <= 0 then
      raise RuntimeError "recursion too deep"
    else if throttleCPU andalso !evalFuel <= 0 then
      (evalFuel := !defaultEvalFuel; raise RuntimeError "CPU time exhausted")
    else
      let val _ = recursionLimit := !recursionLimit - 1
          val _ = evalFuel        := !evalFuel - 1
      in  fn arg => f arg before (recursionLimit := !recursionLimit + 1)
      end
  fun resetOverflowCheck () = ( recursionLimit := defaultRecursionLimit
                              ; evalFuel := !defaultEvalFuel
                              )
end
(* function [[forward]], for mutual recursion through mutable reference cells 1124a *)
fun forward what _ =
  let exception UnresolvedForwardDeclaration of string
  in  raise UnresolvedForwardDeclaration what
  end
exception LeftAsExercise of string



(*****************************************************************)
(*                                                               *)
(*   HINDLEY-MILNER TYPES WITH NAMED TYPE CONSTRUCTORS           *)
(*                                                               *)
(*****************************************************************)

(* Hindley-Milner types with named type constructors 520c *)
(* definitions of [[tycon]], [[eqTycon]], and [[tyconString]] for named type constructors 489a *)
type tycon = name
fun eqTycon (mu, mu') = mu = mu'
fun tyconString mu = mu
(* type declarations for consistency checking *)
type tycon = tycon
val _ = op eqTycon : tycon * tycon -> bool
val _ = op tyconString : tycon -> string
(* representation of Hindley-Milner types 488 *)
type tyvar  = name
datatype ty = TYVAR  of tyvar               (* type variable alpha *)
            | TYCON  of tycon               (* type constructor mu *)
            | CONAPP of ty * ty list        (* type-level application *)

datatype type_scheme = FORALL of tyvar list * ty
(* sets of free type variables in Hindley-Milner types 517a *)
fun freetyvars t =
  let fun f (TYVAR v,          ftvs) = insert (v, ftvs)
        | f (TYCON _,          ftvs) = ftvs
        | f (CONAPP (ty, tys), ftvs) = foldl f (f (ty, ftvs)) tys
  in  reverse (f (t, emptyset))
  end  
(* type declarations for consistency checking *)
val _ = op freetyvars : ty -> name set
val funtycon = "function"
(* functions that create or compare Hindley-Milner types with named type constructors 493 *)
val inttype  = TYCON "int"
val booltype = TYCON "bool"
val symtype  = TYCON "sym"
val alpha    = TYVAR "a"
val beta     = TYVAR "b"
val unittype = TYCON "unit"
fun listtype ty = 
  CONAPP (TYCON "list", [ty])
fun pairtype (x, y) =
  CONAPP (TYCON "pair", [x, y])
fun funtype (args, result) = 
  CONAPP (TYCON "function", [CONAPP (TYCON "arguments", args), result])
fun asFuntype (CONAPP (TYCON "function", [CONAPP (TYCON "arguments", args),
                                                                     result])) =
      SOME (args, result)
  | asFuntype _ = NONE
(* type declarations for consistency checking *)
val _ = op inttype   : ty
val _ = op booltype  : ty
val _ = op symtype   : ty
val _ = op alpha     : ty
val _ = op beta      : ty
val _ = op unittype  : ty
val _ = op listtype  : ty -> ty
val _ = op pairtype  : ty * ty -> ty
val _ = op funtype   : ty list * ty -> ty
val _ = op asFuntype : ty -> (ty list * ty) option
(* functions that create or compare Hindley-Milner types with named type constructors (BUG: software can't tell where this code came from [NW3Vs20r-2KtzCG-1]) *)
fun iotype ty  = CONAPP (TYCON "io", [ty])
(* definition of [[typeString]] for Hindley-Milner types 1269a *)
fun typeString tau =
  case asFuntype tau
    of SOME (args, result) => 
         "(" ^ spaceSep (map typeString args) ^ " -> " ^ typeString result ^ ")"
     | NONE =>
         case tau
           of TYCON c => tyconString c
            | TYVAR a => a
            | CONAPP (tau, []) => "(" ^ typeString tau ^ ")"
            | CONAPP (tau, taus) =>
                "(" ^ typeString tau ^ " " ^ spaceSep (map typeString taus) ^
                                                                             ")"
(* shared utility functions on Hindley-Milner types 490a *)
type subst = ty env
fun varsubst theta = 
  (fn a => find (a, theta) handle NotFound _ => TYVAR a)
(* type declarations for consistency checking *)
type subst = subst
val _ = op varsubst : subst -> (name -> ty)
(* shared utility functions on Hindley-Milner types 490b *)
fun tysubst theta =
  let fun subst (TYVAR a) = varsubst theta a
        | subst (TYCON c) = TYCON c
        | subst (CONAPP (tau, taus)) = CONAPP (subst tau, map subst taus)
(* type declarations for consistency checking *)
val _ = op tysubst : subst -> (ty -> ty)
val _ = op subst   :           ty -> ty
  in  subst
  end
(* shared utility functions on Hindley-Milner types 491a *)
fun dom theta = map (fn (a, _) => a) theta
fun compose (theta2, theta1) =
  let val domain  = union (dom theta2, dom theta1)
      val replace = tysubst theta2 o varsubst theta1
  in  map (fn a => (a, replace a)) domain
  end
(* type declarations for consistency checking *)
val _ = op dom     : subst -> name set
val _ = op compose : subst * subst -> subst
(* shared utility functions on Hindley-Milner types 491b *)
fun instantiate (FORALL (formals, tau), actuals) =
  tysubst (bindList (formals, actuals, emptyEnv)) tau
  handle BindListLength => raise BugInTypeInference
                                              "number of types in instantiation"
(* type declarations for consistency checking *)
val _ = op instantiate : type_scheme * ty list -> ty
(* shared utility functions on Hindley-Milner types 491c *)
val idsubst = emptyEnv
(* shared utility functions on Hindley-Milner types 491d *)
infix 7 |-->
fun a |--> (TYVAR a') = if a = a' then idsubst else bind (a, TYVAR a', emptyEnv)
  | a |--> tau        = if member a (freetyvars tau) then
                          raise BugInTypeInference "non-idempotent substitution"
                        else
                          bind (a, tau, emptyEnv)
(* type declarations for consistency checking *)
val _ = op idsubst : subst
(* type declarations for consistency checking *)
val _ = op |--> : name * ty -> subst
(* shared utility functions on Hindley-Milner types 492a *)
fun typeSchemeString (FORALL ([], tau)) =
      typeString tau
  | typeSchemeString (FORALL (a's, tau)) =
      "(forall (" ^ spaceSep a's ^ ") " ^ typeString tau ^ ")"
(* type declarations for consistency checking *)
val _ = op typeString       : ty          -> string
val _ = op typeSchemeString : type_scheme -> string
(* shared utility functions on Hindley-Milner types 492b *)
fun eqType (TYCON c, TYCON c') = eqTycon (c, c')
  | eqType (CONAPP (tau, taus), CONAPP (tau', taus')) =
      eqType (tau, tau') andalso eqTypes (taus, taus')
  | eqType (TYVAR a, TYVAR a') = a = a'
  | eqType _ = false
and eqTypes (t::taus, t'::taus') = eqType (t, t') andalso eqTypes (taus, taus')
  | eqTypes ([], []) = true
  | eqTypes _ = false
(* type declarations for consistency checking *)
val _ = op eqType : ty * ty -> bool
(* shared utility functions on Hindley-Milner types 517b *)
local
  val n = ref 1
in
  fun freshtyvar _ = TYVAR ("'t" ^ intString (!n) before n := !n + 1)
(* type declarations for consistency checking *)
val _ = op freshtyvar : 'a -> ty
end
(* shared utility functions on Hindley-Milner types 518a *)
fun canonicalize (FORALL (bound, ty)) =
  let fun canonicalTyvarName n =
        if n < 26 then "'" ^ str (chr (ord #"a" + n))
        else "'v" ^ intString (n - 25)
      val free = diff (freetyvars ty, bound)
      fun unusedIndex n =
        if member (canonicalTyvarName n) free then unusedIndex (n+1) else n
      fun newBoundVars (index, [])                = []
        | newBoundVars (index, oldvar :: oldvars) =
            let val n = unusedIndex index
            in  canonicalTyvarName n :: newBoundVars (n+1, oldvars)
            end
      val newBound = newBoundVars (0, bound)
(* type declarations for consistency checking *)
val _ = op canonicalize : type_scheme -> type_scheme
val _ = op newBoundVars : int * name list -> name list
  in  FORALL (newBound, tysubst (bindList (bound, map TYVAR newBound, emptyEnv))
                                                                             ty)
  end
(* shared utility functions on Hindley-Milner types 518b *)
fun generalize (tau, tyvars) =
  canonicalize (FORALL (diff (freetyvars tau, tyvars), tau))
(* type declarations for consistency checking *)
val _ = op generalize : ty * name set -> type_scheme
(* shared utility functions on Hindley-Milner types 518c *)
fun freshInstance (FORALL (bound, tau)) =
  instantiate (FORALL (bound, tau), map freshtyvar bound)
(* type declarations for consistency checking *)
val _ = op freshInstance : type_scheme -> ty
(* specialized environments for type schemes 519a *)
type type_env = type_scheme env * name set
(* specialized environments for type schemes 519b *)
val emptyTypeEnv = 
      (emptyEnv, emptyset)
fun findtyscheme (x, (Gamma, free)) = find (x, Gamma)
(* type declarations for consistency checking *)
val _ = op emptyTypeEnv : type_env
val _ = op findtyscheme : name * type_env -> type_scheme
(* specialized environments for type schemes 520a *)
fun bindtyscheme (x, sigma as FORALL (bound, tau), (Gamma, free)) = 
  (bind (x, sigma, Gamma), union (diff (freetyvars tau, bound), free))
(* specialized environments for type schemes 520b *)
fun freetyvarsGamma (_, free) = free



(*****************************************************************)
(*                                                               *)
(*   ABSTRACT SYNTAX AND VALUES FOR \UHASKELL                    *)
(*                                                               *)
(*****************************************************************)

(* abstract syntax and values for \uhaskell (BUG: software can't tell where this code came from [NW2AyK2m-3os3lT-1]) *)
(* definitions of [[exp]], [[value]], and [[loc]] for \uhaskell (BUG: software can't tell where this code came from [NW2AyK2m-4EjoD9-1]) *)
datatype thunk_contents 
  = VALUE       of value
  | IN_PROGRESS
  | UNEVAL       of thunk
and value (* a value in weak head normal form *)
  = CONVAL of name * loc list   (* fully saturated constructor app *)
  | SYM  of name
  | NUM  of int
  | CLOSURE   of lambda * loc env
  | PRIMITIVE of primitive
  | IO        of unit -> loc  (* monadic action *)
and exp 
  = LITERAL of value
  | VAR     of name
  | APPLY   of exp * exp list
  | LETX    of let_kind * (name * exp) list * exp
  | DO      of            (name * exp) list * exp
  | LAMBDA  of lambda
  | CASE    of exp * match list
  | IFX     of exp * exp * exp
and let_kind = LET | LETREC | LETSTAR
 withtype loc       = thunk_contents ref
      and lambda    = name list * exp
      and thunk     = exp * thunk_contents ref env
      and match     = (name * name list) * exp
      and primitive = thunk_contents ref list -> value
(* definition of [[def]] for \uml\ and \uhaskell (BUG: software can't tell where this code came from [NW2AyK2m-42TQb8-1]) *)
datatype def  = VAL    of name * exp
              | VALREC of name * exp
              | EXP    of exp
              | DEFINE of name * lambda
              | CONDEF of name * type_scheme
(* definition of [[unit_test]] for languages with Hindley-Milner types 484a *)
datatype unit_test = CHECK_EXPECT      of exp * exp
                   | CHECK_ASSERT      of exp
                   | CHECK_ERROR       of exp
                   | CHECK_TYPE        of exp * type_scheme
                   | CHECK_PTYPE       of exp * type_scheme
                   | CHECK_TYPE_ERROR  of def
(* definition of [[xdef]] (shared) 367c *)
datatype xdef = DEF    of def
              | USE    of name
              | TEST   of unit_test
              | DEFS   of def list  (*OMIT*)
(* definitions of [[locString]] and [[valueString]] for \uhaskell (BUG: software can't tell where this code came from [NW2AyK2m-19VxOg-1]) *)
fun locString (ref IN_PROGRESS) = "_|_"
  | locString (ref (UNEVAL _))  = "..."
  | locString (ref (VALUE v))  = valueString v
and valueString (CONVAL ("cons", [l, ls])) =
(* convert [[(cons l ls)]] to string (BUG: software can't tell where this code came from [NW2AyK2m-2VGVNa-1]) *)
                                             let fun tail (CONVAL ("cons", [l,
                                          ls])) = " " ^ locString l ^ tailLoc ls
                                                   | tail (CONVAL ("()", []))
                                                                           = ")"
                                                   | tail v
                                                   = " . " ^ valueString v ^ ")"
                                                 and tailLoc (ref (VALUE v))   =
                                                                          tail v
                                                   | tailLoc (ref IN_PROGRESS) =
                                                                         " _|_)"
                                                   | tailLoc (ref (UNEVAL _))  =
                                                                       " . ...)"
                                             in  "(" ^ locString l ^ tailLoc ls
                                             end
  | valueString (CONVAL (c, []))  = c
  | valueString (CONVAL (c, ls))  = "(" ^ c ^ " " ^ spaceSep (map locString ls)
                                                                           ^ ")"
  | valueString (NUM n      )   = intString n
  | valueString (SYM v      )   = v
  | valueString (CLOSURE   _)   = "<procedure>"
  | valueString (PRIMITIVE _)   = "<procedure>"
  | valueString (IO _       )   = "<I/O action>"
(* type declarations for consistency checking *)
val _ = op locString   : loc   -> string
val _ = op valueString : value -> string
(* definition of [[expString]] for \uhaskell (BUG: software can't tell where this code came from [NW2AyK2m-41PlNl-1]) *)
val rec expString = fn LITERAL v => valueString v
                     | VAR name => name
                     | APPLY (e, es) => prexps (e::es)
                     | LETX (lk, bs, e) => prWithBindings (prLetKind lk, bs, e)
                     | DO (bs, e) => prWithBindings ("do", bs, e)
                     | LAMBDA (xs, body) =>
                          "(lambda " ^ prnames xs ^ " " ^ expString body ^ ")"
                     | IFX (e1, e2, e3) => prexps [VAR "if", e1, e2, e3]
                     | CASE (e, ms) => foldl addMatch ("(case " ^ expString e)
                                                                              ms
and addMatch = fn ((m, b), pfx) => pfx ^ " (" ^ prApp m ^ " " ^ expString b ^
                                                                             ")"
and prApp = fn (c, []) => c
             | (c, xs) => prnames (c::xs)
and prexps  = fn l => "(" ^ spaceSep (map expString l) ^ ")"
and prnames = fn xs => "(" ^ spaceSep xs ^ ")"
and prWithBindings = fn (keyword, bs, e) =>
      "(" ^ keyword ^ " (" ^ prBindings bs ^ ") " ^ expString e ^ ")"
and prBindings = fn bs => separate ("", " ") (map prBinding bs)
and prBinding = fn (x, e) => "(" ^ x ^ " " ^ expString e ^ ")"
and prLetKind = fn LET => "let" | LETSTAR => "let*" | LETREC => "letrec"
(* definitions of [[defString]] and [[defName]] for \uhaskell (BUG: software can't tell where this code came from [NW32Ks5U-1tM6Z8-1]) *)
fun defString _ = "<def>"
fun defName _ = "?"


(*****************************************************************)
(*                                                               *)
(*   UTILITY FUNCTIONS ON \UHASKELL\ VALUES                      *)
(*                                                               *)
(*****************************************************************)

(* utility functions on \uhaskell\ values (BUG: software can't tell where this code came from [NW2AyK2m-2uIh9K-1]) *)
fun allocThunk (e, rho) = ref (UNEVAL (e, rho))
fun allocValue v        = ref (VALUE v)
(* type declarations for consistency checking *)
val _ = op allocThunk : exp * loc env -> loc
(* utility functions on \uhaskell\ values (BUG: software can't tell where this code came from [NW2AyK2m-2uIh9K-2]) *)
fun embedList []     = CONVAL ("()", [])
  | embedList (h::t) = CONVAL ("cons", [allocValue h, allocValue (embedList t)])
fun embedBool b      = CONVAL (if b then "#t" else "#f", [])
(* type declarations for consistency checking *)
val _ = op embedList : value list -> value
val _ = op embedBool : bool       -> value




(*****************************************************************)
(*                                                               *)
(*   TYPE INFERENCE FOR \UHASKELL                                *)
(*                                                               *)
(*****************************************************************)

(* type inference for \uhaskell (BUG: software can't tell where this code came from [NW32Ks5U-vGftD-1]) *)
(* representation of type constraints 520d *)
datatype con = ~  of ty  * ty
             | /\ of con * con
             | TRIVIAL
infix 4 ~
infix 3 /\
(* utility functions on type constraints 520e *)
fun freetyvarsConstraint (t ~  t') = union (freetyvars t, freetyvars t')
  | freetyvarsConstraint (c /\ c') = union (freetyvarsConstraint c,
                                             freetyvarsConstraint c')
  | freetyvarsConstraint TRIVIAL    = emptyset
(* utility functions on type constraints 521a *)
fun consubst theta =
  let fun subst (tau1 ~ tau2) = tysubst theta tau1 ~ tysubst theta tau2
        | subst (c1 /\ c2)    = subst c1 /\ subst c2
        | subst TRIVIAL       = TRIVIAL
  in  subst
  end
(* type declarations for consistency checking *)
val _ = op bindtyscheme : name * type_scheme * type_env -> type_env
(* type declarations for consistency checking *)
val _ = op freetyvarsGamma : type_env -> name set
(* type declarations for consistency checking *)
val _ = op consubst : subst -> con -> con
(* utility functions on type constraints 521b *)
fun conjoinConstraints []      = TRIVIAL
  | conjoinConstraints [c]     = c
  | conjoinConstraints (c::cs) = c /\ conjoinConstraints cs
(* type declarations for consistency checking *)
val _ = op conjoinConstraints : con list -> con
(* utility functions on type constraints 521c *)
(* definitions of [[constraintString]] and [[untriviate]] 1269b *)
fun constraintString (c /\ c') = constraintString c ^ " /\\ " ^ constraintString
                                                                              c'
  | constraintString (t ~  t') = typeString t ^ " ~ " ^ typeString t'
  | constraintString TRIVIAL = "TRIVIAL"

fun untriviate (c /\ c') = (case (untriviate c, untriviate c')
                              of (TRIVIAL, c) => c
                               | (c, TRIVIAL) => c
                               | (c, c') => c /\ c')
  | untriviate atomic = atomic
(* type declarations for consistency checking *)
val _ = op constraintString : con -> string
val _ = op untriviate       : con -> con
(* utility functions on type constraints 522b *)
fun isSolved TRIVIAL = true
  | isSolved (tau ~ tau') = eqType (tau, tau')
  | isSolved (c /\ c') = isSolved c andalso isSolved c'
fun solves (theta, c) = isSolved (consubst theta c)
(* type declarations for consistency checking *)
val _ = op isSolved : con -> bool
val _ = op solves : subst * con -> bool
(* constraint solving 521d *)
fun unsatisfiableEquality (t1, t2) =
  let val t1_arrow_t2 = funtype ([t1], t2)
      val FORALL (_, canonical) =
            canonicalize (FORALL (freetyvars t1_arrow_t2, t1_arrow_t2))
  in  case asFuntype canonical
        of SOME ([t1'], t2') =>
             raise TypeError ("cannot make " ^ typeString t1' ^
                              " equal to " ^ typeString t2')
         | _ => let exception ThisCan'tHappen in raise ThisCan'tHappen end
  end
(* constraint solving ((prototype)) 522a *)
fun solve c = raise LeftAsExercise "solve"
(* type declarations for consistency checking *)
val _ = op solve : con -> subst
(* constraint solving (BUG: software can't tell where this code came from [NW3Vs20r-yKfHo-1]) *)
(*asdf*)
val valtype = TYCON "value"
(* definitions of [[typeof]] and [[elabdef]] for \uhaskell (BUG: software can't tell where this code came from [NW32Ks5U-1k1gri-1]) *)
fun typeof (e, Gamma) =
  let
(* shared definition of [[typesof]], to infer the types of a list of expressions 522e *)
      fun typesof ([],    Gamma) = ([], TRIVIAL)
        | typesof (e::es, Gamma) =
            let val (tau,  c)  = typeof  (e,  Gamma)
                val (taus, c') = typesof (es, Gamma)
            in  (tau :: taus, c /\ c')
            end

(* function [[literal]], to infer the type of a literal constant ((prototype)) 523b *)
      fun literal _ = raise LeftAsExercise "literal"

(* function [[literal]], to infer the type of a literal constant ((prototype)) (BUG: software can't tell where this code came from [NW32Ks5U-dvW4M-1]) *)
      fun literal _ = (valtype, idsubst)

(* function [[ty]], to infer the type of an expression, given [[Gamma]] ((prototype)) (BUG: software can't tell where this code came from [NW32Ks5U-11TNGX-1]) *)
      fun addvar (x, g) = bindtyscheme (x, FORALL([], valtype), g)
      val valtyc = (valtype, TRIVIAL)
      fun ty (LITERAL v) = valtyc
        | ty (VAR x) = (findtyscheme (x, Gamma); valtyc)
        | ty (APPLY (e, es)) = check (e::es)
        | ty (LETX (LETSTAR, [], body)) = ty body
        | ty (LETX (LETSTAR, (b :: bs), body)) =
                         ty (LETX (LET, [b], LETX (LETSTAR, bs, body)))
        | ty (LETX (LET, bs, body)) =
             let val (xs, es) = ListPair.unzip bs
             in  ty (APPLY (LAMBDA (xs, body), es))
             end
        | ty (LETX (LETREC, bs, body)) =
             let val (xs, es) = ListPair.unzip bs
                 val Gamma' = foldl addvar Gamma xs
             in  (map (fn e => typeof(e, Gamma')) es; typeof(body, Gamma'))
             end
        | ty (DO (bs, body)) = ty (LETX (LETSTAR, bs, body))
        | ty (LAMBDA (xs, e)) = typeof(e, foldl addvar Gamma xs)
        | ty (IFX (e1, e2, e3)) =
             ty (CASE (e1, [(("#t", []), e2), (("#f", []), e3)]))
        | ty (CASE (e, matches)) =
             let val _ = map (matchtype (ty e)) matches
             in  valtyc
             end
      and check es = (map ty es; valtyc)
      and matchtype tau_case ((con, xs), e) = typeof (e, foldl addvar Gamma xs)
  in  ty e
  end
(* definitions of [[typeof]] and [[elabdef]] for \uhaskell (BUG: software can't tell where this code came from [NW32Ks5U-1k1gri-2]) *)
fun elabdef (d, Gamma) =
  case d
    of VALREC (x, e)      =>
                   (* infer and bind type for [[VALREC (x, e)]] for \nml 524c *)
                             let val alpha    = freshtyvar ()
                                 val Gamma'   = bindtyscheme (x, FORALL ([],
                                                                  alpha), Gamma)
                                 val (tau, c) = typeof (e, Gamma')
                                 val theta    = solve (c /\ alpha ~ tau)
                                 val sigma    = generalize (tysubst theta alpha,
                                                          freetyvarsGamma Gamma)
                             in  (bindtyscheme (x, sigma, Gamma),
                                                         typeSchemeString sigma)
                             end
     | VAL    (x, e)      =>
                   (* infer and bind type for [[VAL    (x, e)]] for \nml 524b *)
                             let val (tau, c) = typeof (e, Gamma)
                                 val theta    = solve c
                                 val sigma    = generalize (tysubst theta tau,
                                                          freetyvarsGamma Gamma)
                             in  (bindtyscheme (x, sigma, Gamma),
                                                         typeSchemeString sigma)
                             end
     | EXP e              =>
(* infer and bind type for [[EXP e]] ((haskell)) (BUG: software can't tell where this code came from [NW32Ks5U-3qsIwX-1]) *)
                             let val (tau, c) = typeof (e, Gamma)
                                 val theta    = solve c
                                 val sigma    = generalize (tysubst theta tau,
                                                          freetyvarsGamma Gamma)
                             in  (bindtyscheme ("it", sigma, Gamma),
                                                         typeSchemeString sigma)
                             end
     | DEFINE (x, lambda) => elabdef (VALREC (x, LAMBDA lambda), Gamma)
     | CONDEF (c, sigma)  =>
(* bind constructor [[c]] to type [[sigma]] (BUG: software can't tell where this code came from [NW32Ks5U-NCb0s-1]) *)
                             (* XXX missing kind check on sigma *)
                             (bindtyscheme (c, sigma, Gamma), typeSchemeString
                                                                          sigma)



(*****************************************************************)
(*                                                               *)
(*   LEXICAL ANALYSIS AND PARSING FOR \UHASKELL, PROVIDING [[FILEXDEFS]] AND [[STRINGSXDEFS]] *)
(*                                                               *)
(*****************************************************************)

(* lexical analysis and parsing for \uhaskell, providing [[filexdefs]] and [[stringsxdefs]] (BUG: software can't tell where this code came from [NW32Ks5U-3YhhP0-1]) *)
(* lexical analysis for \uscheme\ and related languages 1235b *)
datatype pretoken = QUOTE
                  | INT     of int
                  | SHARP   of bool
                  | NAME    of string
type token = pretoken plus_brackets
(* lexical analysis for \uscheme\ and related languages 1236a *)
fun pretokenString (QUOTE)     = "'"
  | pretokenString (INT  n)    = intString n
  | pretokenString (SHARP b)   = if b then "#t" else "#f"
  | pretokenString (NAME x)    = x
val tokenString = plusBracketsString pretokenString
(* lexical analysis for \uscheme\ and related languages 1236b *)
local
  (* functions used in all lexers 1236d *)
  fun noneIfLineEnds chars =
    case streamGet chars
      of NONE => NONE (* end of line *)
       | SOME (#";", cs) => NONE (* comment *)
       | SOME (c, cs) => 
           let val msg = "invalid initial character in `" ^
                         implode (c::listOfStream cs) ^ "'"
           in  SOME (ERROR msg, EOS)
           end
  (* type declarations for consistency checking *)
  val _ = op noneIfLineEnds : 'a lexer
  (* functions used in the lexer for \uscheme 1236c *)
  fun atom "#t" = SHARP true
    | atom "#f" = SHARP false
    | atom x    = NAME x
in
  val schemeToken =
    whitespace *>
    bracketLexer   (  QUOTE   <$  eqx #"'" one
                  <|> INT     <$> intToken isDelim
                  <|> (atom o implode) <$> many1 (sat (not o isDelim) one)
                  <|> noneIfLineEnds
                   )
(* type declarations for consistency checking *)
val _ = op schemeToken : token lexer
val _ = op atom : string -> pretoken
end
(* parsers for single \uscheme\ tokens 1237a *)
type 'a parser = (token, 'a) polyparser
val pretoken  = (fn (PRETOKEN t)=> SOME t  | _ => NONE) <$>? token : pretoken
                                                                          parser
val quote     = (fn (QUOTE)     => SOME () | _ => NONE) <$>? pretoken
val int       = (fn (INT   n)   => SOME n  | _ => NONE) <$>? pretoken
val booltok   = (fn (SHARP b)   => SOME b  | _ => NONE) <$>? pretoken
val name      = (fn (NAME  n)   => SOME n  | _ => NONE) <$>? pretoken
val any_name  = name
(* parsers for \nml\ tokens 1270d *)
val arrow = eqx "->" name
val name  = sat (fn n => n <> "->") name  (* an arrow is not a name *)
val tyvar = quote *> (curry op ^ "'" <$> name <?>
                                               "type variable (got quote mark)")
(* parsers and parser builders for formal parameters and bindings 1237b *)
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
(* type declarations for consistency checking *)
val _ = op formalsOf  : string -> name parser -> string -> name list parser
val _ = op bindingsOf : string -> 'x parser -> 'e parser -> ('x * 'e) list
                                                                          parser
val _ = op distinctBsIn : (name * 'e) list parser -> string -> (name * 'e) list
                                                                          parser
(* parsers and parser builders for formal parameters and bindings 1237c *)
fun recordFieldsOf name =
  nodups ("record fields", "record definition") <$>!
                                    @@ (bracket ("(field ...)", many name))
(* type declarations for consistency checking *)
val _ = op recordFieldsOf : name parser -> name list parser
(* parsers and parser builders for formal parameters and bindings 1238a *)
fun kw keyword = 
  eqx keyword any_name
fun usageParsers ps = anyParser (map (usageParser kw) ps)
(* type declarations for consistency checking *)
val _ = op kw : string -> string parser
val _ = op usageParsers : (string * 'a parser) list -> 'a parser
(* parsers and parser builders for \scheme-like syntax 1238b *)
fun sexp tokens = (
     SYM       <$> (notDot <$>! @@ any_name)
 <|> NUM       <$> int
 <|> embedBool <$> booltok
 <|> leftCurly <!> "curly brackets may not be used in S-expressions"
 <|> embedList <$> bracket ("list of S-expressions", many sexp)
 <|> (fn v => embedList [SYM "quote", v]) 
               <$> (quote *> sexp)
) tokens
and notDot (loc, ".") =
      errorAt "this interpreter cannot handle . in quoted S-expressions" loc
  | notDot (_,   s)   = OK s
(* type declarations for consistency checking *)
val _ = op sexp : value parser
(* parsers and parser builders for \scheme-like syntax 1238c *)
fun atomicSchemeExpOf name =  VAR                   <$> name
                          <|> LITERAL <$> NUM       <$> int
                          <|> LITERAL <$> embedBool <$> booltok
(* parsers and parser builders for \scheme-like syntax 1239c *)
fun fullSchemeExpOf atomic keywordsOf =
  let val exp = fn tokens => fullSchemeExpOf atomic keywordsOf tokens
  in      atomic
      <|> keywordsOf exp
      <|> quote *> (LITERAL <$> sexp)
      <|> quote *> badRight "quote ' followed by right bracket"
      <|> leftCurly <!> "curly brackets are not supported"
      <|> left *> right <!> "empty application"
      <|> bracket("function application", curry APPLY <$> exp <*> many exp)
  end
(* parser builders for typed languages 1250b *)
fun typedFormalOf name colon ty =
      bracket ("[x : ty]", pair <$> name <* colon <*> ty)
fun typedFormalsOf name colon ty context = 
  let val formal = typedFormalOf name colon ty
  in  distinctBsIn (bracket("(... [x : ty] ...)", many formal)) context
  end                            
(* type declarations for consistency checking *)
val _ = op typedFormalsOf : string parser -> 'b parser -> 'a parser -> string ->
                                                       (string * 'a) list parser
(* parser builders for typed languages 1258c *)
val distinctTyvars = 
  nodups ("quantified type variable", "forall") <$>! @@ (many tyvar)

fun arrowsOf conapp funty =
  let fun arrows []              [] = ERROR "empty type ()"
        | arrows (tycon::tyargs) [] = OK (conapp (tycon, tyargs))
        | arrows args            [rhs] =
            (case rhs of [result] => OK (funty (args, result))
                       | []       => ERROR "no result type after function arrow"
                       | _        => ERROR
                                   "multiple result types after function arrow")
        | arrows args (_::_::_) = ERROR "multiple arrows in function type"
  in  arrows
  end
(* parsers for Hindley-Milner types with named type constructors 1271a *)
val arrows = arrowsOf CONAPP funtype

fun ty tokens = (
     TYCON <$> sat (curry op <> "->") any_name
 <|> TYVAR <$> tyvar
 <|> usageParsers [("(forall (tyvars) type)", bracket ("('a ...)", many tyvar)
                                                                         *> ty)]
     <!> "nested 'forall' type is not a Hindley-Milner type"
 <|> bracket ("constructor application",
              arrows <$> many ty <*>! many (arrow *> many ty))
) tokens

val tyscheme =
      usageParsers [("(forall (tyvars) type)",
                     curry FORALL <$> bracket ("('a ...)", distinctTyvars) <*>
                                                                            ty)]
  <|> curry FORALL [] <$> ty
  <?> "type"
(* type declarations for consistency checking *)
val _ = op tyvar : string parser
val _ = op ty    : ty     parser
(* parsers and [[xdef]] streams for \uhaskell (BUG: software can't tell where this code came from [NW32Ks5U-1OwVPG-1]) *)
val lambdaDups   = nodups ("formal parameter", "lambda")
val conDups      = nodups ("argument of constructor", "case expression")

fun letDups LETSTAR (loc, bindings) = OK bindings
  | letDups kind    (loc, bindings) =
      let val names    = map (fn (n, _) => n) bindings
          val kindName = case kind of LET => "let" | LETREC => "letrec" | _ =>
                                                                            "??"
      in  nodups ("bound name", kindName) (loc, names) >>=+ (fn _ => bindings)
      end
(* parsers and [[xdef]] streams for \uhaskell (BUG: software can't tell where this code came from [NW32Ks5U-1OwVPG-2]) *)
val atomicExp =  VAR              <$> name
             <|> LITERAL <$> NUM  <$> int
             <|> LITERAL <$> embedBool <$> booltok

val formalsIn = formalsOf "(x1 x2 ...)" name


fun exptable exp = 
  let val bindings = bindingsOf "(x e)" name exp
      val dbs      = distinctBsIn bindings
  
      val matchlhs =
            (fn n => (n, [])) <$> name
        <|> ("()", []) <$ left *> right
        <|> bracket ("(K pat ...)", pair <$> name <*> (conDups <$>! @@ (many
                                                                         name)))
      val match = bracket ("(pat exp)", pair <$> matchlhs <*> exp)
      val letx = curry3 LETX

  in  usageParsers
      [ ("(if e1 e2 e3)",            curry3 IFX <$> exp <*> exp <*> exp)
      , ("(lambda (names) body)",    curry LAMBDA <$> formalsIn "lambda" <*> exp
                                                                               )
      , ("(let (bindings) body)",    letx LET     <$> dbs "let"    <*> exp)
      , ("(letrec (bindings) body)", letx LETREC  <$> dbs "letrec" <*> exp)
      , ("(let* (bindings) body)",   letx LETSTAR <$> bindings     <*> exp)
      , ("(case exp matches)",       curry CASE   <$> exp <*> many match)
      ]
  end

fun exp tokens = (
     atomicExp
 <|> LITERAL          <$> (quote *> sexp)
 <|> exptable exp
 <|> leftCurly <!> "curly brackets are not supported"
 <|> left *> right <!> "empty application"
 <|> bracket("function application", curry APPLY <$> exp <*> many exp)
) tokens
(*                
and sexp tokens = (   
     SYM <$> (notDot <$>! name)
 <|> NUM          <$> int
 <|> boolcon      <$> booltok
 <|> (fn v => embedList [SYM "quote", v]) <$> (quote *> sexp)
 <|> embedList    <$> "(" >-- many sexp --< ")"
) tokens
and notDot "." = ERROR
                      "this interpreter cannot handle . in quoted S-expressions"
  | notDot s   = OK s
*)
(* parsers and [[xdef]] streams for \uhaskell (BUG: software can't tell where this code came from [NW32Ks5U-1OwVPG-3]) *)
fun unexpected msg (loc, _) = errorAt loc msg
val tyvar = curry op ^ "'" <$> (quote *> (name <?> "type variable"))

fun keyword syntax words =
  let fun isKeyword s = List.exists (fn s' => s = s') words
  in  (fn (NAME n) => if isKeyword n then SOME n else NONE | _ => NONE) <$>?
                                                                        pretoken
  end

val expKeyword = keyword "type"       ["if", "lambda",
                                       "type-lambda", "let", "let*", "letrec"]

fun ty tokens = (
     TYCON <$> name
 <|> TYVAR <$> tyvar
 <|> usageParsers [("(function (types) type)",
                    curry funtype <$> bracket ("(ty ...)", many ty) <*> ty)]
 <|> badExpKeyword <$>! left *> @@ expKeyword <* matchingRight
 <|> bracket ("(ty ty ... -> ty)",
              arrowsOf CONAPP funtype <$> many ty <*>! many (arrow *> many ty))
 <|> left *> right <!> "empty type ()"
 <|> int     <!> "expected type; found integer"
 <|> booltok <!> "expected type; found Boolean literal"
) tokens
and badExpKeyword (loc, bad) =
      errorAt ("looking for type but found `" ^ bad ^ "'") loc

val tyscheme =
     usageParsers [("(forall (tyvars) type)",
                     curry FORALL <$> bracket ("('a ...)", distinctTyvars) <*>
                                                                            ty)]
 <|> curry FORALL [] <$> ty
(* parsers and [[xdef]] streams for \uhaskell (BUG: software can't tell where this code came from [NW32Ks5U-1OwVPG-4]) *)
fun define f xs body = DEFINE (f, (xs, body))

val deftable = usageParsers
  [ ("(define f (args) body)", define       <$> name <*> formalsIn "define" <*>
                                                                            exp)
  , ("(val x e)",              curry VAL    <$> name <*> exp)
  , ("(val-rec x e)",          curry VALREC <$> name <*> exp)
  , ("(vcon C ty)",            curry CONDEF <$> name <*> tyscheme)
  ]

val xdef = 
     DEF <$> deftable
 <|> usageParsers [("(use filename)", USE <$> name)]
 <|> badRight "unexpected right bracket"
 <|> DEF <$> EXP <$> exp
 <?> "definition"
(* parsers and [[xdef]] streams for \uhaskell (BUG: software can't tell where this code came from [NW32Ks5U-1OwVPG-5]) *)
val xdefstream = interactiveParsedStream (schemeToken, xdef)
(* shared definitions of [[filexdefs]] and [[stringsxdefs]] 1136c *)
fun filexdefs (filename, fd, prompts) = xdefstream (filename, filelines fd,
                                                                        prompts)
fun stringsxdefs (name, strings) = xdefstream (name, streamOfList strings,
                                                                      noPrompts)
(* type declarations for consistency checking *)
val _ = op xdefstream   : string * line stream     * prompts -> xdef stream
val _ = op filexdefs    : string * TextIO.instream * prompts -> xdef stream
val _ = op stringsxdefs : string * string list               -> xdef stream



(*****************************************************************)
(*                                                               *)
(*   EVALUATION, TESTING, AND THE READ-EVAL-PRINT LOOP FOR \UHASKELL *)
(*                                                               *)
(*****************************************************************)

(* evaluation, testing, and the read-eval-print loop for \uhaskell (BUG: software can't tell where this code came from [NW2AyK2m-2e7fLo-1]) *)
(* diagnostic code for lazy evaluation, including [[showforce]] (BUG: software can't tell where this code came from [NW2AyK2m-yEctb-1]) *)
val showingforce = ref false
val forceindent = ref 0
fun indent 0 = ()
  | indent n = (print "  "; indent (n-1))
fun startforce exp =
  if !showingforce then
     ( indent (!forceindent)
     ; app print ["Forcing <|", expString exp, ", rho|>\n"]
     ; forceindent := !forceindent + 1
     )
  else ()
    
fun showforce(exp, v) =
  if !showingforce then
     ( forceindent := !forceindent - 1
     ; indent (!forceindent)
     ; app print ["Forced <|", expString exp, ", rho|> to ", valueString  v,
                                                                           "\n"]
     )
  else ()
(* definitions of [[force]], [[eval]], and [[evaldef]] for \uhaskell (BUG: software can't tell where this code came from [NW2AyK2m-4KSm8w-1]) *)
fun force (ref (VALUE v)) = v
  | force (ref IN_PROGRESS) =
(* tried to evaluate a thunk whose evaluation is in progress (BUG: software can't tell where this code came from [NW2AyK2m-3nM36E-1]) *)
                              raise BlackHole
  | force (thunk as ref (UNEVAL (exp, rho))) =
       let val () = thunk := IN_PROGRESS
           val () = startforce exp
           val v  = eval (exp, rho)
           val () = thunk := VALUE v
           val () = showforce (exp, v)
(* type declarations for consistency checking *)
val _ = op eval  : thunk -> value
val _ = op force : loc   -> value
       in  v
       end
(* definitions of [[force]], [[eval]], and [[evaldef]] for \uhaskell (BUG: software can't tell where this code came from [NW2AyK2m-4KSm8w-2]) *)
and eval (e, rho) =
  let fun toThunk e = allocThunk (e, rho)
      fun ev (LITERAL v) = v
        | ev (VAR     x) = force (find (x, rho))
        | ev (CASE (e, arms)) =
            (case ev e
               of CONVAL (c, args) =>
                    (case List.filter (fn ((c', _), _) => c = c') arms
                       of [((_, formals), body)] =>
                            (eval(body, bindList (formals, args, rho))
                             handle BindListLength =>

(* bleat about [[c]] with [[args]] and [[formals]] (BUG: software can't tell where this code came from [NW2AyK2m-3pVr4P-1]) *)
                                 raise RuntimeError ("Constructor " ^ c ^
                                                            " was applied to " ^
                                                     intString (length args) ^
                                            " arguments, but case expression " ^
                                                     "expected " ^ intString (
                                               length formals) ^ " arguments "))
                        | [] => raise RuntimeError ("Case expression had no " ^

                                              "alternative for constructor " ^ c
 ^ "; alternatives were: " ^ separate ("<none>", ", ") (map (fn ((c, _), _) => c
                                                                        ) arms))
                        | _ :: _ :: _ =>
                            raise RuntimeError ("Case expression had multiple "
                                                                               ^
                                                "alternatives for constructor "
                                                                            ^ c)
                    )
                | v => raise RuntimeError (
                                     "Case discrimination on non-constructor " ^
                                           valueString v)
             )

(* cases for evaluation of [[do]] notation (BUG: software can't tell where this code came from [NW2AyK2m-FXSOr-1]) *)
        | ev (DO ([], body)) = ev body
        | ev (DO ((x, e) :: bindings, body)) =
           ev (APPLY (LITERAL (PRIMITIVE monadicBindList),
                      [e, LAMBDA([x], DO (bindings, body))]))

(* other cases for internal function [[ev]] ((prototype)) (BUG: software can't tell where this code came from [NW2AyK2m-1OSf1W-1]) *)
        | ev _ = raise LeftAsExercise "evaluating thunks"
  in  ev e
  end
(* [[and]] definitions of monadic functions (BUG: software can't tell where this code came from [NW2AyK2m-19vSZ1-1]) *)
and monadicBind (mLoc, kLoc) =
  IO (fn () => let val a = runIO (force mLoc)
               in  runIO (force (apply (kLoc, [a])))
               end)
and runIO (IO f) = f ()
  | runIO _      = raise BugInTypeInference "expected I/O action"
and apply (f, args) =
  allocThunk (APPLY (LITERAL (force f), map (LITERAL o force) args), emptyEnv)
and monadicBindList [m, k] = monadicBind (m, k)
  | monadicBindList _ = let exception ThisCan'tHappen in raise ThisCan'tHappen
                                                                             end
(* definitions of [[force]], [[eval]], and [[evaldef]] for \uhaskell (BUG: software can't tell where this code came from [NW2AyK2m-4KSm8w-3]) *)
fun evaldef (d, rho : loc env) =
  case d
    of VALREC (name, e)      => let val cell = ref IN_PROGRESS
                                    val rho  = bind (name, cell, rho)
                                    val _    = cell := UNEVAL (e, rho)
                                in  (rho, locString cell)
                                end
     | VAL    (name, e)      => let val cell = ref IN_PROGRESS
                                    val _    = cell := UNEVAL (e, rho)
                                    val rho  = bind (name, cell, rho)
                                in  (rho, locString cell)
                                end
     | EXP e                 =>
(* evaluate [[e]] and run or bind the result (BUG: software can't tell where this code came from [NW2AyK2m-1O2J6a-1]) *)
                                (case eval (e, rho)
                                   of IO action => (action () ; (rho, ""))
                                    | v => let val cell = allocValue v
                                           in  (bind ("it", cell, rho),
                                                                 locString cell)
                                           end)
     | DEFINE (name, lambda) => evaldef (VALREC (name, LAMBDA lambda), rho)
     | CONDEF (name, ty)     => let val cell = allocValue (vconValue (name, ty))
                                    val rho  = bind (name, cell, rho)
                                in  (rho, name)
                                end
and vconValue (name, sigma) =
  if takesArgs sigma then
    PRIMITIVE (fn args => CONVAL (name, args))
  else
    CONVAL (name, [])
and takesArgs (FORALL (_, CONAPP (TYCON "function", _))) = true
  | takesArgs (FORALL (_, _))                            = false
(* utility functions that use [[force]] (BUG: software can't tell where this code came from [NW2AyK2m-4erVoR-1]) *)
fun testEqual (CONVAL (c, locs), CONVAL (c', locs')) =
      c = c' andalso ListPair.allEq testEqual (map force locs, map force locs')
  | testEqual (SYM n, SYM n') = (n = n')
  | testEqual (NUM n, NUM n') = (n = n')
  | testEqual _ = false
(* utility functions that use [[force]] (BUG: software can't tell where this code came from [NW2AyK2m-4erVoR-2]) *)
fun strict f = f o map force
fun arityError n args =
  raise RuntimeError ("primitive function expected " ^ intString n ^
                      "arguments; got " ^ intString (length args))
fun binary_valop f = strict (fn [a, b] => f(a, b) | args => arityError 2 args)
fun unary_valop  f = strict (fn [a]    => f a     | args => arityError 1 args)
(* type declarations for consistency checking *)
val _ = op + : int * int -> int
(* utility functions that use [[force]] (BUG: software can't tell where this code came from [NW2AyK2m-4erVoR-3]) *)
fun arithop f = binary_valop (fn (NUM n1, NUM n2) => NUM (f (n1, n2))
                               | _ => raise RuntimeError "integers expected")
val arithtype    = funtype ([inttype, inttype], inttype)
fun comptype tau = funtype ([tau, tau], booltype)
(* utility functions that use [[force]] (BUG: software can't tell where this code came from [NW2AyK2m-4erVoR-4]) *)
fun inject_bool x =
      CONVAL (if x then "#t" else "#f", [])
fun project_bool (CONVAL ("#t", [])) = true
  | project_bool (CONVAL ("#f", [])) = false
  | project_bool _ = raise RuntimeError "projected non-boolean"

val bool = project_bool  (* I'm just here to pass my regression tests *)

fun inject_predicate f = fn x => inject_bool (f x)
fun predop f = unary_valop  (inject_predicate f)

fun comparison f = binary_valop (inject_predicate f)
fun intcompare f = comparison (
                     fn (NUM n1, NUM n2) => f (n1, n2)
                      | _ => raise BugInTypeInference "integers expected")
(* type declarations for consistency checking *)
val _ = op inject_bool  : bool -> value
val _ = op project_bool : value -> bool
(* definitions of [[basis]] and [[processDef]] for \uhaskell (BUG: software can't tell where this code came from [NW2AyK2m-2eqWcj-1]) *)
type basis = type_env * loc env
fun processDef (d, (Gamma, rho), interactivity) =
  let val (Gamma, tystring)  = elabdef (d, Gamma)
      val (rho,   valstring) = evaldef (d, rho)
      val _ = if prints interactivity andalso size valstring > 0 then
                (* don't print when running an IO action *)
                println (valstring ^ " : " ^ tystring)
              else
                ()
(* type declarations for consistency checking *)
val _ = op evaldef : def * loc env -> loc env * string
(* type declarations for consistency checking *)
val _ = op processDef : def * basis * interactivity -> basis
        in  (Gamma, rho)
        end
fun dump_names (types, values) = app (println o fst) values  (*OMIT*)
(* shared definition of [[withHandlers]] 380a *)
fun withHandlers f a caught =
  f a
  handle RuntimeError msg   => caught ("Run-time error <at loc>: " ^ msg)
       | NotFound x         => caught ("Name " ^ x ^ " not found <at loc>")
       | Located (loc, exn) =>
           withHandlers (fn _ => raise exn) a (fn s => caught (fillAtLoc (s, loc
                                                                             )))

(* other handlers that catch non-fatal exceptions and pass messages to [[caught]] (BUG: software can't tell where this code came from [NW2AyK2m-1bszo-1]) *)
       | BlackHole => caught
                          "Run-time error <at loc>: evaluation hit a black hole"

(* other handlers that catch non-fatal exceptions and pass messages to [[caught]] 380b *)
       | Div                => caught ("Division by zero <at loc>")
       | Overflow           => caught ("Arithmetic overflow <at loc>")
       | Subscript          => caught ("Array index out of bounds <at loc>")
       | Size               => caught (
                                "Array length too large (or negative) <at loc>")
       | IO.Io { name, ...} => caught ("I/O error <at loc>: " ^ name)
(* shared unit-testing utilities 1128d *)
fun failtest strings = (app eprint strings; eprint "\n"; false)
(* shared unit-testing utilities 1128e *)
fun reportTestResultsOf what (npassed, nthings) =
  case (npassed, nthings)
    of (_, 0) => ()  (* no report *)
     | (0, 1) => println ("The only " ^ what ^ " failed.")
     | (1, 1) => println ("The only " ^ what ^ " passed.")
     | (0, 2) => println ("Both " ^ what ^ "s failed.")
     | (1, 2) => println ("One of two " ^ what ^ "s passed.")
     | (2, 2) => println ("Both " ^ what ^ "s passed.")
     | _ => if npassed = nthings then
               app print ["All ", intString nthings, " " ^ what ^ "s passed.\n"]
            else if npassed = 0 then
               app print ["All ", intString nthings, " " ^ what ^ "s failed.\n"]
            else
               app print [intString npassed, " of ", intString nthings,
                          " " ^ what ^ "s passed.\n"]
val reportTestResults = reportTestResultsOf "test"
(* definition of [[testIsGood]] for \nml 1272a *)
(* definition of [[skolemTypes]] for languages with named type constructors 1273c *)
val skolemTypes = streamMap (fn n => TYCON ("skolem type " ^ intString n))
                                                                        naturals
(* shared definitions of [[typeSchemeIsAscribable]] and [[typeSchemeIsEquivalent]] 1273d *)
fun asGeneralAs (sigma_g, sigma_i as FORALL (a's, tau)) =
  let val theta = bindList (a's, streamTake (length a's, skolemTypes), emptyEnv)
                                                                                
      val skolemized = tysubst theta tau
      val tau_g = freshInstance sigma_g
  in  (solve (tau_g ~ skolemized); true) handle _ => false
  end
(* shared definitions of [[typeSchemeIsAscribable]] and [[typeSchemeIsEquivalent]] 1273e *)
fun eqTypeScheme (sigma1, sigma2) =
  asGeneralAs (sigma1, sigma2) andalso asGeneralAs (sigma2, sigma1)
(* type declarations for consistency checking *)
val _ = op xdef : xdef parser
(* type declarations for consistency checking *)
val _ = op skolemTypes  : ty stream
(* type declarations for consistency checking *)
val _ = op asGeneralAs  : type_scheme * type_scheme -> bool
(* shared definitions of [[typeSchemeIsAscribable]] and [[typeSchemeIsEquivalent]] 1273f *)
fun typeSchemeIsAscribable (e, sigma_e, sigma) =
  if asGeneralAs (sigma_e, sigma) then
    true
  else
    failtest ["check-type failed: expected ", expString e, " to have type ",
              typeSchemeString sigma, ", but it has type ", typeSchemeString
                                                                        sigma_e]
(* shared definitions of [[typeSchemeIsAscribable]] and [[typeSchemeIsEquivalent]] 1274a *)
fun typeSchemeIsEquivalent (e, sigma_e, sigma) =
  if typeSchemeIsAscribable (e, sigma_e, sigma) then
    if asGeneralAs (sigma, sigma_e) then
      true
    else
      failtest ["check-principal-type failed: expected ", expString e,
                " to have principal type ", typeSchemeString sigma,
                ", but it has the more general type ", typeSchemeString sigma_e]
  else
    false  (* error message already issued *)
fun testIsGood (test, (Gamma, rho)) =
  let fun ty e = typeof (e, Gamma)
                 handle NotFound x =>
                   raise TypeError ("name " ^ x ^ " is not defined")
      fun deftystring d =
        snd (elabdef (d, Gamma))
        handle NotFound x => raise TypeError ("name " ^ x ^ " is not defined")

(* definitions of [[check{Expect,Assert,Error}Checks]] that use type inference 1272b *)
      fun checkExpectChecks (e1, e2) = 
        let val (tau1, c1) = ty e1
            val (tau2, c2) = ty e2
            val c = tau1 ~ tau2
            val theta = solve (c1 /\ c2 /\ c)
        in  true
        end handle TypeError msg =>
            failtest ["In (check-expect ", expString e1, " ", expString e2,
                                                                     "), ", msg]

(* definitions of [[check{Expect,Assert,Error}Checks]] that use type inference 1273a *)
      fun checkExpChecksIn what e =
        let val (tau, c) = ty e
            val theta = solve c
        in  true
        end handle TypeError msg =>
            failtest ["In (", what, " ", expString e, "), ", msg]
      val checkAssertChecks = checkExpChecksIn "check-assert"
      val checkErrorChecks  = checkExpChecksIn "check-error"

(* definitions of [[check{Expect,Assert,Error}Checks]] that use type inference 1272b *)
      fun checkExpectChecks (e1, e2) = 
        let val (tau1, c1) = ty e1
            val (tau2, c2) = ty e2
            val c = tau1 ~ tau2
            val theta = solve (c1 /\ c2 /\ c)
        in  true
        end handle TypeError msg =>
            failtest ["In (check-expect ", expString e1, " ", expString e2,
                                                                     "), ", msg]

(* definitions of [[check{Expect,Assert,Error}Checks]] that use type inference 1273a *)
      fun checkExpChecksIn what e =
        let val (tau, c) = ty e
            val theta = solve c
        in  true
        end handle TypeError msg =>
            failtest ["In (", what, " ", expString e, "), ", msg]
      val checkAssertChecks = checkExpChecksIn "check-assert"
      val checkErrorChecks  = checkExpChecksIn "check-error"
      (* definition of [[checkTypeChecks]] using type inference 1273b *)
      fun checkTypeChecks form (e, sigma) = 
        let val (tau, c) = ty e
            val theta  = solve c
        in  true
        end handle TypeError msg =>
            failtest ["In (", form, " ", expString e, " " ^ typeSchemeString
                                                                   sigma, "), ",
                      msg]
      fun checks (CHECK_EXPECT (e1, e2)) = checkExpectChecks (e1, e2)
        | checks (CHECK_ASSERT e)        = checkAssertChecks e
        | checks (CHECK_ERROR e)         = checkErrorChecks e
        | checks (CHECK_TYPE  (e, tau))  = checkTypeChecks "check-type" (e, tau)
        | checks (CHECK_PTYPE (e, tau))  = checkTypeChecks
                                                          "check-principal-type"
                                                                        (e, tau)
        | checks (CHECK_TYPE_ERROR e)    = true

      fun outcome e = withHandlers (fn () => OK (eval (e, rho))) () (ERROR o
                                                                     stripAtLoc)

   (* [[asSyntacticValue]] for \uscheme, \timpcore, \tuscheme, and \nml 1241b *)
      fun asSyntacticValue (LITERAL v) = SOME v
        | asSyntacticValue _           = NONE
      (* type declarations for consistency checking *)
      val _ = op asSyntacticValue : exp -> value option

 (* shared [[check{Expect,Assert,Error}Passes]], which call [[outcome]] 1128c *)
      (* shared [[whatWasExpected]] 1126b *)
      fun whatWasExpected (e, outcome) =
        case asSyntacticValue e
          of SOME v => valueString v
           | NONE =>
               case outcome
                 of OK v => valueString v ^ " (from evaluating " ^ expString e ^
                                                                             ")"
                  | ERROR _ =>  "the result of evaluating " ^ expString e
      (* type declarations for consistency checking *)
      val _ = op whatWasExpected  : exp * value error -> string
      val _ = op asSyntacticValue : exp -> value option
      (* shared [[checkExpectPassesWith]], which calls [[outcome]] 1127 *)
      val cxfailed = "check-expect failed: "
      fun checkExpectPassesWith equals (checkx, expectx) =
        case (outcome checkx, outcome expectx)
          of (OK check, OK expect) => 
               equals (check, expect) orelse
               failtest [cxfailed, " expected ", expString checkx,
                                                             " to evaluate to ",
                         whatWasExpected (expectx, OK expect), ", but it's ",
                         valueString check, "."]
           | (ERROR msg, tried) =>
               failtest [cxfailed, " expected ", expString checkx,
                                                             " to evaluate to ",
                         whatWasExpected (expectx, tried), ", but evaluating ",
                         expString checkx, " caused this error: ", msg]
           | (_, ERROR msg) =>
               failtest [cxfailed, " expected ", expString checkx,
                                                             " to evaluate to ",
                         whatWasExpected (expectx, ERROR msg),
                                                            ", but evaluating ",
                         expString expectx, " caused this error: ", msg]
      (* type declarations for consistency checking *)
      val _ = op checkExpectPassesWith : (value * value -> bool) -> exp * exp ->
                                                                            bool
      val _ = op outcome  : exp -> value error
      val _ = op failtest : string list -> bool

(* shared [[checkAssertPasses]] and [[checkErrorPasses]], which call [[outcome]] 1128a *)
      val cafailed = "check-assert failed: "
      fun checkAssertPasses checkx =
            case outcome checkx
              of OK check => bool check orelse
                             failtest [cafailed, " expected assertion ",
                                                               expString checkx,
                                       " to hold, but it doesn't"]
               | ERROR msg =>
                   failtest [cafailed, " expected assertion ", expString checkx,
                             " to hold, but evaluating it caused this error: ",
                                                                            msg]
      (* type declarations for consistency checking *)
      val _ = op checkAssertPasses : exp -> bool

(* shared [[checkAssertPasses]] and [[checkErrorPasses]], which call [[outcome]] 1128b *)
      val cefailed = "check-error failed: "
      fun checkErrorPasses checkx =
            case outcome checkx
              of ERROR _ => true
               | OK check =>
                   failtest [cefailed, " expected evaluating ", expString checkx
                                                                               ,
                             " to cause an error, but evaluation produced ",
                             valueString check]
      (* type declarations for consistency checking *)
      val _ = op checkErrorPasses : exp -> bool
      fun checkExpectPasses (cx, ex) = checkExpectPassesWith testEqual (cx, ex)
      (* definitions of [[check*Type*Passes]] using type inference 1274b *)
      fun checkTypePasses (e, sigma) =
        let val (tau, c) = ty e
            val theta    = solve c
            val sigma_e  = generalize (tysubst theta tau, freetyvarsGamma Gamma)
        in  typeSchemeIsAscribable (e, sigma_e, sigma)
        end handle TypeError msg =>
            failtest ["In (check-type ", expString e, " ", typeSchemeString
                                                              sigma, "), ", msg]
      (* definitions of [[check*Type*Passes]] using type inference 1274c *)
      fun checkPrincipalTypePasses (e, sigma) =
        let val (tau, c) = ty e
            val theta    = solve c
            val sigma_e  = generalize (tysubst theta tau, freetyvarsGamma Gamma)
        in  typeSchemeIsEquivalent (e, sigma_e, sigma)
        end handle TypeError msg =>
            failtest ["In (check-principal-type ", expString e, " ",
                      typeSchemeString sigma, "), ", msg]
      (* definitions of [[check*Type*Passes]] using type inference 1274d *)
      fun checkTypeErrorPasses (EXP e) =
            (let val (tau, c) = ty e
                 val theta    = solve c
                 val sigma'   = generalize (tysubst theta tau, freetyvarsGamma
                                                                          Gamma)
             in  failtest ["check-type-error failed: expected ", expString e,
                           " not to have a type, but it has type ",
                                                        typeSchemeString sigma']
             end handle TypeError msg => true
                      | Located (_, TypeError _) => true)
        | checkTypeErrorPasses d =
            (let val t = deftystring d
             in  failtest ["check-type-error failed: expected ", defString d,

                         " to cause a type error, but it successfully defined ",
                           defName d, " : ", t
                          ] 
             end handle TypeError msg => true
                      | Located (_, TypeError _) => true)
      fun passes (CHECK_EXPECT (c, e))  = checkExpectPasses (c, e)
        | passes (CHECK_ASSERT c)       = checkAssertPasses c
        | passes (CHECK_ERROR c)        = checkErrorPasses  c
        | passes (CHECK_TYPE  (c, tau)) = checkTypePasses          (c, tau)
        | passes (CHECK_PTYPE (c, tau)) = checkPrincipalTypePasses (c, tau)
        | passes (CHECK_TYPE_ERROR c)   = checkTypeErrorPasses c

  in  checks test andalso passes test
  end
(* shared definition of [[processTests]] 1129a *)
fun numberOfGoodTests (tests, rho) =
  foldr (fn (t, n) => if testIsGood (t, rho) then n + 1 else n) 0 tests
fun processTests (tests, rho) =
      reportTestResults (numberOfGoodTests (tests, rho), length tests)
(* type declarations for consistency checking *)
val _ = op processTests : unit_test list * basis -> unit
(* shared read-eval-print loop and [[processPredefined]] 377d *)
fun processPredefined (def,basis) = 
  processDef (def, basis, noninteractive)
(* type declarations for consistency checking *)
val _ = op noninteractive    : interactivity
val _ = op processPredefined : def * basis -> basis
(* shared read-eval-print loop and [[processPredefined]] 378a *)
fun readEvalPrintWith errmsg (xdefs, basis, interactivity) =
  let val unitTests = ref []

(* definition of [[processXDef]], which can modify [[unitTests]] and call [[errmsg]] 379a *)
      fun processXDef (xd, basis) =
        let (* definition of [[useFile]], to read from a file 378b *)
            fun useFile filename =
              let val fd = TextIO.openIn filename
                  val (_, printing) = interactivity
                  val inter' = (NOT_PROMPTING, printing)
              in  readEvalPrintWith errmsg (filexdefs (filename, fd, noPrompts),
                                                                  basis, inter')
                  before TextIO.closeIn fd
              end
            fun try (USE filename) = useFile filename
              | try (TEST t)       = (unitTests := t :: !unitTests; basis)
              | try (DEF def)      = processDef (def, basis, interactivity)
              | try (DEFS ds)      = foldl processXDef basis (map DEF ds)
                                                                        (*OMIT*)
            fun caught msg = (errmsg (stripAtLoc msg); basis)
            val _ = resetOverflowCheck ()     (* OMIT *)
        in  withHandlers try xd caught
        end 
      (* type declarations for consistency checking *)
      val _ = op errmsg     : string -> unit
      val _ = op processDef : def * basis * interactivity -> basis
      val basis = streamFold processXDef basis xdefs
      val _     = processTests (!unitTests, basis)
(* type declarations for consistency checking *)
val _ = op readEvalPrintWith : (string -> unit) ->                     xdef
                                         stream * basis * interactivity -> basis
val _ = op processXDef       : xdef * basis -> basis
  in  basis
  end



(*****************************************************************)
(*                                                               *)
(*   IMPLEMENTATIONS OF \UHASKELL\ PRIMITIVES AND DEFINITION OF [[INITIALBASIS]] *)
(*                                                               *)
(*****************************************************************)

(* implementations of \uhaskell\ primitives and definition of [[initialBasis]] (BUG: software can't tell where this code came from [NW2AyK2m-3mNfJX-1]) *)
(* utility functions for building \uhaskell\ primitives (BUG: software can't tell where this code came from [NW2AyK2m-1OtgpW-1]) *)
fun ns_unary  f = (fn [a]    => f a      | args => arityError 1 args)
fun ns_binary f = (fn [a, b] => f (a, b) | args => arityError 2 args)
(* implementations of \uhaskell\ primitives (BUG: software can't tell where this code came from [NW2AyK2m-1bYI9R-1]) *)
fun fullyEval v =
  let fun full (CONVAL (c, args)) = app (full o force) args
        | full (NUM n)       = ()
        | full (SYM v)       = ()
        | full (CLOSURE   _) = ()
        | full (PRIMITIVE _) = ()
        | full (IO _       ) = ()
      val _ = full v
(* type declarations for consistency checking *)
val _ = op fullyEval : value -> value
  in  v
  end
(* implementations of \uhaskell\ primitives (BUG: software can't tell where this code came from [NW2AyK2m-1bYI9R-2]) *)
val unitval   = CONVAL ("unit", [])
val unitthunk = allocValue unitval
(* type declarations for consistency checking *)
val _ = op apply : loc * loc list -> loc
val _ = op monadicBind : loc * loc -> value
val initialBasis =
  let fun addPrim ((name, prim, tau), (Gamma, rho)) = 
        ( bindtyscheme (name, generalize (tau, freetyvarsGamma Gamma), Gamma)
        , bind (name, allocValue (PRIMITIVE prim), rho)
        )
      val primBasis = foldl addPrim (emptyTypeEnv, emptyEnv) (
(* primitives for \uhaskell\ [[::]] (BUG: software can't tell where this code came from [NW2AyK2m-4ALRv3-1]) *)
                                                              ("full",
                             unary_valop fullyEval, funtype ([alpha], alpha)) ::

(* primitives for \uhaskell\ [[::]] (BUG: software can't tell where this code came from [NW2AyK2m-4ALRv3-2]) *)
                                                              ( "showforce"
                                                              , unary_valop (fn
                                       x => (showingforce := project_bool x; x))
                                                              , funtype ([
                                                            booltype], unittype)
                                                              ) ::

(* primitives for \uhaskell\ [[::]] (BUG: software can't tell where this code came from [NW2AyK2m-4ALRv3-3]) *)
                                                              ("+", arithop op +
                                                                 , arithtype) ::
                                                              ("-", arithop op -
                                                                 , arithtype) ::
                                                              ("*", arithop op *
                                                                 , arithtype) ::
                                                              ("/", arithop op
                                                              div, arithtype) ::

(* primitives for \uhaskell\ [[::]] (BUG: software can't tell where this code came from [NW2AyK2m-4ALRv3-4]) *)
                                                              ("<", intcompare
                                                      op <, comptype inttype) ::
                                                              (">", intcompare
                                                      op >, comptype inttype) ::
                                                              ("=", comparison (
                                                  fn (NUM n1, NUM n2) => n1 = n2

                                                   | (SYM v1, SYM v2) => v1 = v2

                   |  _ => raise RuntimeError "equality used on non-base type"),
                                                                    comptype
                                                                       alpha) ::
                                                              (*
                                                              ("number?",
                                   predop (fn (NUM  _) => true | _ => false)) ::
                                                              ("symbol?",
                                   predop (fn (SYM  _) => true | _ => false)) ::
                                                              *)

(* primitives for \uhaskell\ [[::]] (BUG: software can't tell where this code came from [NW2AyK2m-4ALRv3-5]) *)
                                                              ("error",
           unary_valop (fn v => raise RuntimeError (valueString (fullyEval v))),

                                                     funtype ([alpha], beta)) ::
                                                              ("show",
                                                unary_valop (SYM o valueString),
                                                                        funtype
                                                          ([alpha], symtype)) ::
                                                              ("symwidth",
                                         unary_valop (fn (SYM s) => NUM (size s)

                     | _ => raise BugInTypeInference "symwidth got non-symbol"),
                                                                        funtype
                                                        ([symtype], inttype)) ::

(* primitives for \uhaskell\ [[::]] (BUG: software can't tell where this code came from [NW2AyK2m-4ALRv3-6]) *)
                                                              ("cons", ns_binary
                                          (fn (a, b) => CONVAL("cons", [a, b])),

                           funtype ([alpha, listtype alpha], listtype alpha)) ::

                               (* ("tuple", fn es => CONVAL ("tuple", es)) :: *)

(* primitives for \uhaskell\ [[::]] (BUG: software can't tell where this code came from [NW2AyK2m-4ALRv3-7]) *)
                                                              ("trace", let fun
                                       bleat s = TextIO.output(TextIO.stdErr, s)
                                                                        in
                                                       ns_binary (fn (msg, v) =>

                                                      (app bleat ["**TRACE**: ",

                                      valueString (fullyEval (force msg)), "\n"]

                                                                     ; force v))
                                                                        end,

                                               funtype ([alpha, beta], beta)) ::

(* primitives for \uhaskell\ [[::]] (BUG: software can't tell where this code came from [NW2AyK2m-4ALRv3-8]) *)
                                                              ("print",
    unary_valop (fn v => IO (fn () => ( print (valueString (fullyEval v) ^ "\n")

                                                                 ; unitthunk))),
                                                                        funtype
                                                  ([alpha], iotype unittype)) ::
                                                              ("return",
                                     ns_unary (fn thunk => IO (fn () => thunk)),
                                                                        funtype
                                                     ([alpha], iotype alpha)) ::
                                                              (">>=", ns_binary
                                                                    monadicBind,
                                                                        funtype
          ([iotype alpha, funtype ([alpha], iotype beta)], iotype beta)) :: nil)
      val fundefs =

      (* predefined {\uhaskell} functions, as strings (generated by a script) *)

         [ "(define map (f xs)"
         , "  (case xs"
         , "     (() '())"
         , "     ((cons y ys) (cons (f y) (map f ys)))))"
         , "(vcon pair (forall ('a 'b) (function ('a 'b) (pair 'a 'b))))"
         , "(define fst (p)"
         , "   (case p ((pair x y) x)))"
         , "(define snd (p)"
         , "   (case p ((pair x y) y)))"
         , "(define list1 (x) (cons x '()))"
         , "(define bind (x y alist)"
         , "  (case alist"
         , "       (() (list1 (pair x y)))"
         , "       ((cons p ps)"
         , "          (if (= x (fst p))"
         , "              (cons (pair x y) ps)"
         , "              (cons p (bind x y ps))))))"
         , "(define null? (xs)"
         , "   (case xs (() #t)"
         , "            ((cons y ys) #f)))"
         , "(define car (xs)"
         , "   (case xs (() (error 'car-of-empty-list))"
         , "            ((cons x _) x)))"
         , "(define cdr (xs)"
         , "   (case xs (() (error 'cdr-of-empty-list))"
         , "            ((cons _ xs) xs)))"
         , "(define isbound? (x alist)"
         , "  (if (null? alist) "
         , "    #f"
         , "    (if (= x (fst (car alist)))"
         , "      #t"
         , "      (isbound? x (cdr alist)))))"
         , "(define find (x alist)"
         , "  (if (null? alist) "
         , "    (error 'not-found)"
         , "    (if (= x (fst (car alist)))"
         , "      (snd (car alist))"
         , "      (find x (cdr alist)))))"
         , "(vcon unit unit)"
         , "(define >> (m1 m2) (>>= m1 (lambda (_) m2)))"
         , "(define mapM_ (f xs)"
         , "  (case xs (() (return unit))"
         , "           ((cons y ys) (>> (f y) (mapM_ f ys)))))"
         , "(vcon some (forall ('a) (function ('a) (option 'a))))"
         , "(vcon none (forall ('a) (option 'a)))"
         , "(define tails (xs)"
         , "  (cons xs (if (null? xs) '() (tails (cdr xs)))))"
         , "(define caar (xs) (car (car xs)))"
         , "(define cadr (xs) (car (cdr xs)))"
         , "(define cdar (xs) (cdr (car xs)))"
         , "(define cddr (xs) (cdr (cdr xs)))"
         , "(define cdddr (xs) (cdr (cddr xs)))"
         , "(define length (xs)"
         , "  (if (null? xs) 0"
         , "    (+ 1 (length (cdr xs)))))"
         , "(define and (b c) (if b  c  b))"
         , "(define or  (b c) (if b  b  c))"
         , "(define not (b)   (if b #f #t))"
         , "(define append (xs ys)"
         , "  (if (null? xs)"
         , "     ys"
         , "     (cons (car xs) (append (cdr xs) ys))))"
         , "(define revapp (xs ys)"
         , "  (if (null? xs)"
         , "     ys"
         , "     (revapp (cdr xs) (cons (car xs) ys))))"
         , "(define reverse (xs) (revapp xs '()))"
         , "(define o (f g) (lambda (x) (f (g x))))"
         , "(define curry   (f) (lambda (x) (lambda (y) (f x y))))"
         , "(define uncurry (f) (lambda (x y) ((f x) y)))"
         , "(define filter (p? l)"
         , "  (if (null? l)"
         , "    '()"
         , "    (if (p? (car l))"
         , "      (cons (car l) (filter p? (cdr l)))"
         , "      (filter p? (cdr l)))))"
         , "(define exists? (p? l)"
         , "  (if (null? l)"
         , "    #f"
         , "    (if (p? (car l)) "
         , "      #t"
         , "      (exists? p? (cdr l)))))"
         , "(define all? (p? l)"
         , "  (if (null? l)"
         , "    #t"
         , "    (if (p? (car l))"
         , "      (all? p? (cdr l))"
         , "      #f)))"
         , "(define foldr (op zero l)"
         , "  (if (null? l)"
         , "    zero"
         , "    (op (car l) (foldr op zero (cdr l)))))"
         , "(define foldl (op zero l)"
         , "  (if (null? l)"
         , "    zero"
         , "    (foldl op (op (car l) zero) (cdr l))))"
         , "(define <= (x y) (not (> x y)))"
         , "(define >= (x y) (not (< x y)))"
         , "(define != (x y) (not (= x y)))"
         , "(define max (x y) (if (> x y) x y))"
         , "(define min (x y) (if (< x y) x y))"
         , "(define mod (m n) (- m (* n (/ m n))))"
         , "(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))"
         , "(define lcm (m n) (* m (/ n (gcd m n))))"
         , "(define min* (l) (foldr min (car l) (cdr l)))"
         , "(define max* (l) (foldr max (car l) (cdr l)))"
         , "(define gcd* (l) (foldr gcd (car l) (cdr l)))"
         , "(define lcm* (l) (foldr lcm (car l) (cdr l)))"
         , "(define list1 (x)               (cons x '()))"
         , "(define list2 (x y)             (cons x (list1 y)))"
         , "(define list3 (x y z)           (cons x (list2 y z)))"
         , "(define list4 (x y z a)         (cons x (list3 y z a)))"
         , "(define list5 (x y z a b)       (cons x (list4 y z a b)))"
         , "(define list6 (x y z a b c)     (cons x (list5 y z a b c)))"
         , "(define list7 (x y z a b c d)   (cons x (list6 y z a b c d)))"
         , "(define list8 (x y z a b c d e) (cons x (list7 y z a b c d e)))"
         , "(define takewhile (p? l)"
         , "  (if (null? l)"
         , "     '()"
         , "     (if (p? (car l))"
         , "         (cons (car l) (takewhile p? (cdr l)))"
         , "         '())))"
         , "(define dropwhile (p? l)"
         , "  (if (null? l)"
         , "     '()"
         , "     (if (p? (car l))"
         , "         (dropwhile p? (cdr l))"
         , "         l)))"
         , "(define drop (n l)"
         , "  (if (null? l)"
         , "     '()"
         , "     (if (= n 0)"
         , "         l"
         , "         (drop (- n 1) (cdr l)))))"
         , "(define nth (xs n)"
         , "   (car (drop n xs)))"
         , "(define take (n xs)"
         , "  (if (or (= n 0) (null? xs))"
         , "      '()"
         , "      (cons (car xs) (take (- n 1) (cdr xs)))))"
          ]
      val xdefs = stringsxdefs ("predefined functions", fundefs)
  in  readEvalPrintWith predefinedFunctionError (xdefs, primBasis,
                                                                 noninteractive)
  end


(*****************************************************************)
(*                                                               *)
(*   FUNCTION [[RUNAS]], WHICH EVALUATES STANDARD INPUT GIVEN [[INITIALBASIS]] *)
(*                                                               *)
(*****************************************************************)

(* function [[runAs]], which evaluates standard input given [[initialBasis]] 381b *)
fun runAs interactivity = 
  let val _ = setup_error_format interactivity
      val prompts = if prompts interactivity then stdPrompts else noPrompts
      val xdefs = filexdefs ("standard input", TextIO.stdIn, prompts)
  in  ignore (readEvalPrintWith eprintln (xdefs, initialBasis, interactivity))
  end 
(* type declarations for consistency checking *)
val _ = op runAs : interactivity -> unit


(*****************************************************************)
(*                                                               *)
(*   CODE THAT LOOKS AT COMMAND-LINE ARGUMENTS AND CALLS [[RUNAS]] TO RUN THE INTERPRETER *)
(*                                                               *)
(*****************************************************************)

(* code that looks at command-line arguments and calls [[runAs]] to run the interpreter 381c *)
val _ = case CommandLine.arguments ()
          of []     => runAs (PROMPTING,     PRINTING)
           | ["-q"] => runAs (NOT_PROMPTING, PRINTING)
           | ["-qq"]=> runAs (NOT_PROMPTING, NOT_PRINTING)   (*OMIT*)
           | ["-names"]=> dump_names initialBasis (*OMIT*)
           | _      => eprintln ("Usage: " ^ CommandLine.name () ^ " [-q]")
