(* tuscheme.sml 458 *)


(*****************************************************************)
(*                                                               *)
(*   EXCEPTIONS USED IN LANGUAGES WITH TYPE CHECKING             *)
(*                                                               *)
(*****************************************************************)

(* exceptions used in languages with type checking 1117b *)
exception TypeError of string
exception BugInTypeChecking of string


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
           (* more handlers for [[atLoc]] 1137g *)
           | e as IO.Io _   => raise Located (loc, e)
           | e as Div       => raise Located (loc, e)
           | e as Overflow  => raise Located (loc, e)
           | e as Subscript => raise Located (loc, e)
           | e as Size      => raise Located (loc, e)
           (* more handlers for [[atLoc]] ((type-checking)) 412c *)
           | e as TypeError _         => raise Located (loc, e)
           | e as BugInTypeChecking _ => raise Located (loc, e)
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
(*   KINDS FOR TYPED LANGUAGES                                   *)
(*                                                               *)
(*****************************************************************)

(* kinds for typed languages 427a *)
datatype kind = TYPE                          (* kind of all types *)
              | ARROW of kind list * kind     (* kind of many constructors *)
(* kinds for typed languages 427b *)
fun eqKind (TYPE, TYPE) = true
  | eqKind (ARROW (args, result), ARROW (args', result')) =
      eqKinds (args, args') andalso eqKind (result, result')
  | eqKind (_, _) = false
and eqKinds (ks, ks') = ListPair.allEq eqKind (ks, ks')


(*****************************************************************)
(*                                                               *)
(*   TYPES FOR {\TUSCHEME}                                       *)
(*                                                               *)
(*****************************************************************)

(* types for {\tuscheme} 429a *)
datatype tyex = TYCON  of name                (* type constructor *)
              | CONAPP of tyex * tyex list    (* type-level application *)
              | FUNTY  of tyex list * tyex    (* function type *)
              | FORALL of name list * tyex
              | TYVAR  of name                (* type variable *)
(* types for {\tuscheme} 455c *)
val inttype  = TYCON "int"
val booltype = TYCON "bool"
val symtype  = TYCON "sym"
val unittype = TYCON "unit"
val tyvarA   = TYVAR "'a"
fun listtype ty = CONAPP (TYCON "list",[ty])
(* type declarations for consistency checking *)
val _ = op inttype   : tyex
val _ = op booltype  : tyex
val _ = op symtype   : tyex
val _ = op unittype  : tyex
val _ = op tyvarA    : tyex
val _ = op listtype  : tyex -> tyex
(* types for {\tuscheme} 1257a *)
fun typeString (TYCON c) = c
  | typeString (TYVAR a) = a
  | typeString (FUNTY (args, result)) =
      "(" ^ spaceSep (map typeString args) ^ " -> " ^ typeString result ^ ")"
  | typeString (CONAPP (tau, [])) = "(" ^ typeString tau ^ ")"
  | typeString (CONAPP (tau, tys)) =
      "(" ^ typeString tau ^ " " ^ spaceSep (map typeString tys) ^ ")"
  | typeString (FORALL (tyvars, tau)) =
      "(forall (" ^ spaceSep tyvars ^ ") " ^ typeString tau ^ ")"


(*****************************************************************)
(*                                                               *)
(*   SETS OF FREE TYPE VARIABLES IN \TUSCHEME                    *)
(*                                                               *)
(*****************************************************************)

(* sets of free type variables in \tuscheme 447 *)
fun freetyvars t =
  let fun free (TYVAR v,          ftvs) = insert (v, ftvs)
        | free (TYCON _,          ftvs) = ftvs
        | free (CONAPP (ty, tys), ftvs) = foldl free (free (ty, ftvs)) tys
        | free (FUNTY  (tys, ty), ftvs) = foldl free (free (ty, ftvs)) tys
        | free (FORALL (alphas, tau), ftvs) =
               union (diff (free (tau, emptyset), alphas), ftvs)
  in  reverse (free (t, emptyset))
  end  
(* type declarations for consistency checking *)
val _ = op freetyvars : tyex -> name set
(* sets of free type variables in \tuscheme 448 *)
fun freetyvarsGamma Gamma =
  foldl (fn ((x, tau), ftvs) => union (ftvs, freetyvars tau)) emptyset Gamma


(*****************************************************************)
(*                                                               *)
(*   SHARED UTILITY FUNCTIONS ON SETS OF TYPE VARIABLES          *)
(*                                                               *)
(*****************************************************************)

(* shared utility functions on sets of type variables 470 *)
fun freshName (alpha, avoid) =
  let val basename = stripNumericSuffix alpha
      val candidates = streamMap (fn n => basename ^ "-" ^ intString n) naturals
      fun ok beta = not (member beta avoid)
  in  case streamGet (streamFilter ok candidates)
        of SOME (beta, _) => beta
         | NONE => let exception ThisCan'tHappen in raise ThisCan'tHappen end
  end
(* type declarations for consistency checking *)
val _ = op freshName : name * name set -> name


(*****************************************************************)
(*                                                               *)
(*   KIND CHECKING FOR {\TUSCHEME}                               *)
(*                                                               *)
(*****************************************************************)

(* kind checking for {\tuscheme} 453e *)
fun kindof (tau, Delta) =
  let (* definition of internal function [[kind]] 454a *)
      fun kind (TYVAR a) =
            (find (a, Delta)
             handle NotFound _ => raise TypeError ("unknown type variable " ^ a)
                                                                               )
      (* definition of internal function [[kind]] 454b *)
        | kind (TYCON c) =
            (find (c, Delta)
             handle NotFound _ => raise TypeError ("unknown type constructor " ^
                                                                             c))
      (* definition of internal function [[kind]] 454c *)
        | kind (FUNTY (args, result)) =
            let fun badKind tau = not (eqKind (kind tau, TYPE))
            in  if badKind result then
                  raise TypeError "function result is not a type"
                else if List.exists badKind args then
                  raise TypeError "argument list includes a non-type"
                else
                  TYPE
            end
      (* definition of internal function [[kind]] 454d *)
        | kind (CONAPP (tau, actuals)) =
            (case kind tau
               of ARROW (formal_kinds, result_kind) =>
                    if eqKinds (formal_kinds, map kind actuals) then
                        result_kind
                    else
                        raise TypeError ("type constructor " ^ typeString tau ^
                                         " applied to the wrong arguments")
                | TYPE =>
                    raise TypeError ("tried to apply type " ^ typeString tau ^
                                     " as type constructor"))
      (* definition of internal function [[kind]] 455a *)
        | kind (FORALL (alphas, tau)) =
            let val Delta' = foldl (fn (a, Delta) => bind (a, TYPE, Delta))
                                                                    Delta alphas
            in  case kindof (tau, Delta')
                  of TYPE    => TYPE
                   | ARROW _ => raise TypeError
                                      "quantifed a non-nullary type constructor"
            end
(* type declarations for consistency checking *)
val _ = op kindof : tyex * kind env -> kind
val _ = op kind   : tyex            -> kind
  in  kind tau
  end
(* kind checking for {\tuscheme} 455b *)
fun asType (ty, Delta) =
  case kindof (ty, Delta)
    of TYPE    => ty
     | ARROW _ => raise TypeError ("used type constructor `" ^ typeString ty ^
                                   "' as a type")
(* type declarations for consistency checking *)
val _ = op asType : tyex * kind env -> tyex



(*****************************************************************)
(*                                                               *)
(*   ABSTRACT SYNTAX AND VALUES FOR {\TUSCHEME}                  *)
(*                                                               *)
(*****************************************************************)

(* abstract syntax and values for {\tuscheme} 435b *)
(* definitions of [[exp]] and [[value]] for {\tuscheme} 434a *)
datatype exp = LITERAL  of value
             | VAR      of name
             | SET      of name * exp
             | IFX      of exp * exp * exp
             | WHILEX   of exp * exp
             | BEGIN    of exp list
             | APPLY    of exp * exp list
             | LETX     of let_kind * (name * exp) list * exp
             | LETRECX  of ((name * tyex) * exp) list * exp
             | LAMBDA   of lambda_exp
             | TYLAMBDA of name list * exp
             | TYAPPLY  of exp * tyex list
and let_kind = LET | LETSTAR
(* definitions of [[exp]] and [[value]] for {\tuscheme} 434b *)
and    value = NIL
             | BOOLV     of bool   
             | NUM       of int
             | SYM       of name
             | PAIR      of value * value
             | CLOSURE   of lambda_value * value ref env
             | PRIMITIVE of primitive
withtype primitive    = value list -> value (* raises RuntimeError *)
     and lambda_exp   = (name * tyex) list * exp
     and lambda_value = name          list * exp
(* definition of [[def]] for {\tuscheme} 434c *)
datatype def  = VAL    of name * exp
              | VALREC of name * tyex * exp
              | EXP    of exp
              | DEFINE of name * tyex * lambda_exp
(* definition of [[unit_test]] for explicitly typed languages 434d *)
datatype unit_test = CHECK_EXPECT      of exp * exp
                   | CHECK_ASSERT      of exp
                   | CHECK_ERROR       of exp
                   | CHECK_TYPE        of exp * tyex
                   | CHECK_TYPE_ERROR  of def
(* definition of [[xdef]] (shared) 367c *)
datatype xdef = DEF    of def
              | USE    of name
              | TEST   of unit_test
              | DEFS   of def list  (*OMIT*)
(* definition of [[valueString]] for \uscheme, \tuscheme, and \nml 368b *)
fun valueString (NIL)     = "()"
  | valueString (BOOLV b) = if b then "#t" else "#f"
  | valueString (NUM n)   = intString n
  | valueString (SYM v)   = v
  | valueString (PAIR (car, cdr))  = 
      let fun tail (PAIR (car, cdr)) = " " ^ valueString car ^ tail cdr
            | tail NIL = ")"
            | tail v = " . " ^ valueString v ^ ")"
      in  "(" ^ valueString car ^ tail cdr
      end
  | valueString (CLOSURE   _) = "<procedure>"
  | valueString (PRIMITIVE _) = "<procedure>"
(* type declarations for consistency checking *)
val _ = op valueString : value -> string
(* definition of [[expString]] for {\tuscheme} 1267a *)
fun expString e =
  let fun bracket s = "(" ^ s ^ ")"
      val bracketSpace = bracket o spaceSep
      fun exps es = map expString es
      fun formal (x, tau) = bracketSpace [typeString tau, x]
      fun withBindings (keyword, bs, e) =
        bracket (spaceSep [keyword, bindings bs, expString e])
      and bindings bs = bracket (spaceSep (map binding bs))
      and binding (x, e) = bracket (x ^ " " ^ expString e)

      fun tybinding ((x, ty), e) = bracketSpace [formal (x, ty), expString e]
      and tybindings bs = bracket (spaceSep (map tybinding bs))
      val letkind = fn LET => "let" | LETSTAR => "let*"
  in  case e
        of LITERAL v         => valueString v
         | VAR name          => name
         | SET (x, e)        => bracketSpace ["set", x, expString e]
         | IFX (e1, e2, e3)  => bracketSpace ("if" :: exps [e1, e2, e3])
         | WHILEX (cond, body) => 
                       bracketSpace ["while", expString cond, expString body]
         | BEGIN es          => bracketSpace ("begin" :: exps es)
         | APPLY (e, es)     => bracketSpace (exps (e::es))
         | LETX (lk, bs, e)  => bracketSpace [letkind lk, bindings bs, expString
                                                                              e]
         | LETRECX (bs, e)  => bracketSpace ["letrec", tybindings bs, expString
                                                                              e]
         | LAMBDA (xs, e)    =>
             bracketSpace ["lambda", bracketSpace (map formal xs), expString e]
         | TYLAMBDA (alphas, e) =>
             bracketSpace ["lambda", bracketSpace alphas, expString e]
         | TYAPPLY (e, taus) =>
             bracketSpace ("@" :: expString e :: map typeString taus)
  end
(* definitions of [[defString]] and [[defName]] for {\tuscheme} 1267b *)
fun defString d =
  let fun bracket s = "(" ^ s ^ ")"
      val bracketSpace = bracket o spaceSep
      fun formal (x, t) = "[" ^ x ^ " : " ^ typeString t ^ "]"
  in  case d
        of EXP e => expString e
         | VAL (x, e) => bracketSpace ["val", x, expString e]
         | VALREC (x, tau, e) =>
             bracketSpace ["val-rec", formal (x, tau), expString e]
         | DEFINE (f, rtau, (formals, body)) =>
             bracketSpace ["define", typeString rtau, f,
                           bracketSpace (map formal formals), expString body]
  end
fun defName (VAL (x, _))       = x
  | defName (VALREC (x, _, _)) = x
  | defName (DEFINE (x, _, _)) = x
  | defName (EXP _) = raise InternalError "asked for name defined by expression"


(*****************************************************************)
(*                                                               *)
(*   UTILITY FUNCTIONS ON \USCHEME, \TUSCHEME, AND \NML\ VALUES  *)
(*                                                               *)
(*****************************************************************)

(* utility functions on \uscheme, \tuscheme, and \nml\ values 455d *)
val unitVal = NIL
(* type declarations for consistency checking *)
val _ = op unitVal : value
(* utility functions on \uscheme, \tuscheme, and \nml\ values 368a *)
fun embedList []     = NIL
  | embedList (h::t) = PAIR (h, embedList t)
fun embedBool b = BOOLV b
fun bool (BOOLV b) = b
  | bool _         = true
(* type declarations for consistency checking *)
val _ = op embedBool : bool       -> value
val _ = op embedList : value list -> value
val _ = op bool      : value      -> bool
(* utility functions on \uscheme, \tuscheme, and \nml\ values 369a *)
fun equalatoms (NIL,      NIL    )  = true
  | equalatoms (NUM  n1,  NUM  n2)  = (n1 = n2)
  | equalatoms (SYM  v1,  SYM  v2)  = (v1 = v2)
  | equalatoms (BOOLV b1, BOOLV b2) = (b1 = b2)
  | equalatoms  _                   = false
(* type declarations for consistency checking *)
val _ = op equalatoms : value * value -> bool
(* utility functions on \uscheme, \tuscheme, and \nml\ values 369b *)
fun equalpairs (PAIR (car1, cdr1), PAIR (car2, cdr2)) =
      equalpairs (car1, car2) andalso equalpairs (cdr1, cdr2)
  | equalpairs (v1, v2) = equalatoms (v1, v2)
(* type declarations for consistency checking *)
val _ = op equalpairs : value * value -> bool
(* utility functions on \uscheme, \tuscheme, and \nml\ values 369c *)
val testEqual = equalpairs
(* type declarations for consistency checking *)
val _ = op testEqual : value * value -> bool
(* utility functions on \uscheme, \tuscheme, and \nml\ values 1242 *)
fun cycleThrough xs =
  let val remaining = ref xs
      fun next () = case !remaining
                      of [] => (remaining := xs; next ())
                       | x :: xs => (remaining := xs; x)
  in  if null xs then
        raise InternalError "empty list given to cycleThrough"
      else
        next
  end
val unspecified =
  cycleThrough [BOOLV true, NUM 39, SYM "this value is unspecified", NIL,
                PRIMITIVE (fn _ => let exception Unspecified in raise
                                                               Unspecified end)]
(* type declarations for consistency checking *)
val _ = op cycleThrough : 'a list -> (unit -> 'a)
val _ = op unspecified  : unit -> value



(*****************************************************************)
(*                                                               *)
(*   CAPTURE-AVOIDING SUBSTITUTION FOR {\TUSCHEME}               *)
(*                                                               *)
(*****************************************************************)

(* capture-avoiding substitution for {\tuscheme} 450 *)
fun tysubst (tau, varenv) =
  let
  (* definition of [[renameForallAvoiding]] for {\tuscheme} ((prototype)) 469 *)
      fun renameForallAvoiding (alphas, tau, captured) =
        raise LeftAsExercise "renameForallAvoiding"
      (* type declarations for consistency checking *)
      val _ = op renameForallAvoiding : name list * tyex * name set -> tyex
      fun subst (TYVAR a) = (find (a, varenv) handle NotFound _ => TYVAR a)
        | subst (TYCON c) = (TYCON c)
        | subst (CONAPP (tau, taus)) = CONAPP (subst tau, map subst taus)
        | subst (FUNTY  (taus, tau)) = FUNTY  (map subst taus, subst tau)
        | subst (FORALL (alphas, tau)) =

(* substitute in [[tau]] as specified by [[varenv]], without capturing or substituting for any [[alphas]] 451b *)
            let val free               = freetyvars (FORALL (alphas, tau))
                val new_taus           = map (subst o TYVAR) free
                val potential_captures = foldl union emptyset (map freetyvars
                                                                       new_taus)
                val actual_captures    = inter (potential_captures, alphas)
            in  if true then

(* substitute [[varenv]] in [[FORALL (alphas, tau)]] (works only if there is no capture) 451a *)
                  let val varenv' = bindList (alphas, map TYVAR alphas, varenv)
                  in  FORALL (alphas, tysubst (tau, varenv'))
                  end
                else
                  subst (renameForallAvoiding (alphas, tau, potential_captures))
            end
(* type declarations for consistency checking *)
val _ = op freetyvarsGamma : tyex env -> name set
(* type declarations for consistency checking *)
val _ = op tysubst : tyex * tyex env -> tyex
val _ = op subst   : tyex            -> tyex
  in  subst tau
  end
(* capture-avoiding substitution for {\tuscheme} 451c *)
fun rename (alphas, betas, tau) =
  tysubst (tau, bindList (alphas, map TYVAR betas, emptyEnv))
(* type declarations for consistency checking *)
val _ = op rename : name list * name list * tyex -> tyex
(* capture-avoiding substitution for {\tuscheme} 452a *)
fun instantiate (FORALL (formals, tau), actuals, Delta) =
      (case List.find (fn t => not (eqKind (kindof (t, Delta), TYPE))) actuals
         of SOME t => raise TypeError ("instantiated at type constructor `" ^
                                       typeString t ^ "', which is not a type")
          | NONE =>
              (tysubst (tau, bindList (formals, actuals, emptyEnv))
               handle BindListLength =>
                 raise TypeError
                   "instantiated polymorphic term at wrong number of types"))
  | instantiate (tau, _, _) =
       raise TypeError ("tried to instantiate term of non-quantified type " ^
                        typeString tau)
(* type declarations for consistency checking *)
val _ = op instantiate : tyex * tyex list * kind env -> tyex
val _ = List.find : ('a -> bool) -> 'a list -> 'a option


(*****************************************************************)
(*                                                               *)
(*   TYPE EQUIVALENCE FOR {\TUSCHEME}                            *)
(*                                                               *)
(*****************************************************************)

(* type equivalence for {\tuscheme} 445 *)
(* infinite supply of type variables 446b *)
val infiniteTyvars = 
  streamMap (fn n => "'b-" ^ intString n) naturals
(* type declarations for consistency checking *)
val _ = op naturals       : int stream
val _ = op infiniteTyvars : name stream
fun eqType (TYVAR a, TYVAR a') = a = a'
  | eqType (TYCON c, TYCON c') = c = c'
  | eqType (CONAPP (tau, taus), CONAPP (tau', taus')) =
      eqType (tau, tau') andalso eqTypes (taus, taus')
  | eqType (FUNTY (taus, tau), FUNTY (taus', tau')) =
      eqType (tau, tau') andalso eqTypes (taus, taus')
  | eqType (FORALL (alphas, tau), FORALL (alphas', tau')) =

(* Boolean saying if \monobox{FORALL (alphas, tau)} $\equiv$ \monobox{FORALL (alphas', tau')} 446a *)
      let fun ok a  = not (member a (freetyvars tau) orelse member a (freetyvars
                                                                          tau'))
          val betas = streamTake (length alphas, streamFilter ok infiniteTyvars)
      in  length alphas = length alphas' andalso
          eqType (rename (alphas, betas, tau), rename (alphas', betas, tau'))
      end
  | eqType _ = false
and eqTypes (t::taus, t'::taus') = eqType (t, t') andalso eqTypes (taus, taus')
  | eqTypes ([], []) = true
  | eqTypes _ = false
(* type declarations for consistency checking *)
val _ = op eqType  : tyex      * tyex      -> bool
val _ = op eqTypes : tyex list * tyex list -> bool


(*****************************************************************)
(*                                                               *)
(*   TYPE CHECKING FOR {\TUSCHEME}                               *)
(*                                                               *)
(*****************************************************************)

(* type checking for {\tuscheme} ((prototype)) 440 *)
fun typeof _  = raise LeftAsExercise "typeof"
fun elabdef _ = raise LeftAsExercise "elabdef"
(* type declarations for consistency checking *)
val _ = op eqKind  : kind      * kind      -> bool
val _ = op eqKinds : kind list * kind list -> bool
(* type declarations for consistency checking *)
val _ = op typeof  : exp * kind env * tyex env -> tyex
val _ = op elabdef : def * kind env * tyex env -> tyex env * string



(*****************************************************************)
(*                                                               *)
(*   LEXICAL ANALYSIS AND PARSING FOR \TUSCHEME, PROVIDING [[FILEXDEFS]] AND [[STRINGSXDEFS]] *)
(*                                                               *)
(*****************************************************************)

(* lexical analysis and parsing for \tuscheme, providing [[filexdefs]] and [[stringsxdefs]] 1257b *)
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
(* parsers for \tuscheme\ tokens 1257c *)
val arrow   = (fn (NAME "->") => SOME () | _ => NONE) <$>? pretoken
val name    = sat (fn n => n <> "->") name  (* an arrow is not a name *)
(* parsers for \tuscheme\ tokens 1258b *)
val tyvar = 
  quote *> (curry op ^ "'" <$> name <?> "type variable (got quote mark)")
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
(* parsers and [[xdef]] streams for {\tuscheme} 1258a *)
fun keyword words =
  let fun isKeyword s = List.exists (fn s' => s = s') words
  in  sat isKeyword name
  end

val expKeyword = keyword ["if", "while", "set", "begin", "lambda",
                          "type-lambda", "let", "let*", "@"]
val tyKeyword  = keyword ["forall", "function", "->"]

val tlformals = nodups ("formal type parameter", "type-lambda") <$>! @@ (many
                                                                           name)

fun nodupsty what (loc, xts) = nodups what (loc, map fst xts) >>=+ (fn _ => xts)

                                                  (* error on duplicate names *)

fun letDups LETSTAR (_, bindings) = OK bindings
  | letDups LET     bindings       = nodupsty ("bound variable", "let") bindings
(* parsers and [[xdef]] streams for {\tuscheme} 1258d *)
val arrows = arrowsOf CONAPP FUNTY
fun ty tokens =
  let fun badExpKeyword (loc, bad) =
        errorAt ("looking for type but found `" ^ bad ^ "'") loc
  in     TYCON <$> name
     <|> TYVAR <$> tyvar
     <|> bracketKeyword (kw "forall", "(forall (tyvars) type)",
                         curry FORALL <$> bracket ("('a ...)", distinctTyvars)
                                                                         <*> ty)
     <|> badExpKeyword <$>! (left *> @@ expKeyword <* matchingRight)
     <|> bracket ("type application or function type",
                  arrows <$> many ty <*>! many (arrow *> many ty))
     <|> int <!> "expected type; found integer"
     <|> booltok <!> "expected type; found Boolean literal"
  end tokens
(* type declarations for consistency checking *)
val _ = op tyvar : string parser
(* type declarations for consistency checking *)
val _ = op distinctTyvars : name list parser
(* type declarations for consistency checking *)
val _ = op ty : tyex   parser
(* parsers and [[xdef]] streams for {\tuscheme} 1259 *)
fun flipPair tau x = (x, tau)
val formal = bracket ("[x : ty]", pair <$> name <* kw ":" <*> ty)
val lformals = bracket ("([x : ty] ...)", many formal)
val tformals = bracket ("('a ...)", many tyvar)

fun lambda xs exp =
      nodupsty ("formal parameter", "lambda") xs >>=+ (fn xs => LAMBDA (xs, exp)
                                                                               )
fun tylambda a's exp =
      nodups ("formal type parameter", "type-lambda") a's >>=+ (fn a's =>
      TYLAMBDA (a's, exp))

fun cb key usage parser = bracketKeyword (eqx key name, usage, parser)

fun exp tokens = (
     VAR               <$> name
 <|> LITERAL <$> NUM   <$> int
 <|> LITERAL <$> BOOLV <$> booltok
 <|> quote *> (LITERAL <$> sexp)
 <|> quote *> badRight "quote mark ' followed by right bracket"
 <|> cb "quote"  "(quote sx)"               (       LITERAL <$> sexp)
 <|> cb "if"     "(if e1 e2 e3)"            (curry3 IFX     <$> exp  <*> exp <*>
                                                                            exp)
 <|> cb "while"  "(while e1 e2)"            (curry  WHILEX  <$> exp  <*> exp)
 <|> cb "set"    "(set x e)"                (curry  SET     <$> name <*> exp)
 <|> cb "begin"  ""                         (       BEGIN   <$> many exp)
 <|> cb "lambda" "(lambda (formals) body)"  (       lambda  <$> @@ lformals <*>!
                                                                            exp)
 <|> cb "type-lambda" "(type-lambda (tyvars) body)"
                                            (       tylambda <$> @@ tformals
                                                                       <*>! exp)
 <|> cb "let"    "(let (bindings) body)"    (letx   LET     <$> @@ bindings <*>!
                                                                            exp)
 <|> cb "letrec" "(letrec (bindings) body)" (curry  LETRECX <$> tybindings <*>
                                                                            exp)
 <|> cb "let*"   "(let* (bindings) body)"   (letx   LETSTAR <$> @@ bindings <*>!
                                                                            exp)
 <|> cb "@"      "(@ exp types)"            (curry  TYAPPLY <$> exp <*> many1 ty
                                                                               )
 <|> badTyKeyword <$>! left *> @@ tyKeyword <* matchingRight
 <|> leftCurly <!> "curly brackets are not supported"
 <|> left *> right <!> "empty application"
 <|> bracket ("function application", curry APPLY <$> exp <*> many exp)
) tokens

and letx kind bs exp = letDups kind bs >>=+ (fn bs => LETX (kind, bs, exp))
and tybindings ts = bindingsOf "([x : ty] e)" formal exp ts
and bindings ts = bindingsOf "(x e)" name exp ts

and badTyKeyword (loc, bad) =
      errorAt ("looking for expression but found `" ^ bad ^ "'") loc
(* parsers and [[xdef]] streams for {\tuscheme} 1260a *)
fun define tau f formals body =
  nodupsty ("formal parameter", "definition of function " ^ f) formals >>=+ (fn
                                                                          xts =>
  DEFINE (f, tau, (xts, body)))

fun valrec (x, tau) e = VALREC (x, tau, e)

val def =
     cb "define" "(define type f (args) body)"
                                     (define <$> ty <*> name <*> @@ lformals
                                                                       <*>! exp)
 <|> cb "val"    "(val x e)"                (curry VAL <$> name   <*> exp)
 <|> cb "val-rec" "(val-rec [x : type] e)"  (valrec    <$> formal <*> exp)
(* parsers and [[xdef]] streams for {\tuscheme} 1260b *)
val unit_test =
      cb "check-expect" "(check-expect e1 e2)" (curry CHECK_EXPECT <$> exp <*>
                                                                            exp)
  <|> cb "check-assert" "(check-assert e)"     (      CHECK_ASSERT <$> exp)
  <|> cb "check-error"  "(check-error e)"      (      CHECK_ERROR  <$> exp)
  <|> cb "check-type"   "(check-type e tau)"   (curry CHECK_TYPE   <$> exp <*>
                                                                             ty)
  <|> cb "check-type-error" "(check-type-error e)"
                                           (CHECK_TYPE_ERROR <$> (def <|> EXP
                                                                       <$> exp))
(* type declarations for consistency checking *)
val _ = op unit_test : unit_test parser
(* parsers and [[xdef]] streams for {\tuscheme} 1260c *)
val xdef = 
     DEF <$> def
 <|> cb "use" "(use filename)" (USE <$> name)
 <|> TEST <$> unit_test
 <|> badRight "unexpected right bracket"
 <|> DEF <$> EXP <$> exp
 <?> "definition"
(* parsers and [[xdef]] streams for {\tuscheme} 1260d *)
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
(*   EVALUATION, TESTING, AND THE READ-EVAL-PRINT LOOP FOR {\TUSCHEME} *)
(*                                                               *)
(*****************************************************************)

(* evaluation, testing, and the read-eval-print loop for {\tuscheme} 1260e *)
(* definition of [[namedValueString]] for functional bridge languages 1263a *)
fun namedValueString x v =
  case v of CLOSURE _ => x
          | PRIMITIVE _ => x
          | _ => valueString v
(* type declarations for consistency checking *)
val _ = op namedValueString : name -> value -> string
(* definitions of [[eval]] and [[evaldef]] for {\tuscheme} 1261a *)
fun eval (e, rho) =
  let val go = applyCheckingOverflow id in go end (* OMIT *)
  let fun ev (LITERAL n) = n
        (* alternatives for [[ev]] for [[TYAPPLY]] and [[TYLAMBDA]] 456 *)
        | ev (TYAPPLY  (e, _)) = ev e
        | ev (TYLAMBDA (_, e)) = ev e
        (* more alternatives for [[ev]] for {\tuscheme} 1261b *)
        | ev (VAR v) = !(find (v, rho))
        | ev (SET (n, e)) = 
            let val v = ev e
            in  find (n, rho) := v;
                v
            end
        (* more alternatives for [[ev]] for {\tuscheme} 1261c *)
        | ev (IFX (e1, e2, e3)) = ev (if bool (ev e1) then e2 else e3)
        | ev (WHILEX (guard, body)) = 
            if bool (ev guard) then 
              (ev body; ev (WHILEX (guard, body)))
            else
              unitVal
        | ev (BEGIN es) =
            let fun b (e::es, lastval) = b (es, ev e)
                  | b (   [], lastval) = lastval
            in  b (es, unitVal)
            end
        (* more alternatives for [[ev]] for {\tuscheme} 1261d *)
        | ev (LAMBDA (args, body)) = CLOSURE ((map (fn (x, ty) => x) args, body)
                                                                          , rho)
        (* more alternatives for [[ev]] for {\tuscheme} 1261e *)
        | ev (APPLY (f, args))  = 
               (case ev f
                  of PRIMITIVE prim => prim (map ev args)
                   | CLOSURE clo =>
                       (* apply closure [[clo]] to [[args]] ((mlscheme)) 371e *)
                                    let val ((formals, body), savedrho) = clo
                                        val actuals = map ev args
                                    in  eval (body, bindList (formals, map ref
                                                             actuals, savedrho))
                                        handle BindListLength => 
                                            raise RuntimeError (
                                      "Wrong number of arguments to closure; " ^
                                                                "expected (" ^
                                                         spaceSep formals ^ ")")
                                    end
                   | v => raise BugInTypeChecking "applied non-function"
               )
        (* more alternatives for [[ev]] for {\tuscheme} 1262a *)
        | ev (LETX (LET, bs, body)) =
            let val (names, values) = ListPair.unzip bs
            in  eval (body, bindList (names, map (ref o ev) values, rho))
            end
        | ev (LETX (LETSTAR, bs, body)) =
            let fun step ((n, e), rho) = bind (n, ref (eval (e, rho)), rho)
            in  eval (body, foldl step rho bs)
            end
        (* more alternatives for [[ev]] for {\tuscheme} 1262b *)
        | ev (LETRECX (bs, body)) = 
            let val (tynames, values) = ListPair.unzip bs
                val names = map fst tynames
                val _ = errorIfDups ("bound name", names, "letrec")
                val rho' = bindList (names, map (fn _ => ref (unspecified()))
                                                                    values, rho)
                val updates = map (fn ((x, _), e) => (x, eval (e, rho'))) bs
            in  List.app (fn (x, v) => find (x, rho') := v) updates; 
                eval (body, rho')
            end
(* type declarations for consistency checking *)
val _ = op xdef : xdef parser
(* type declarations for consistency checking *)
val _ = op eval : exp * value ref env -> value
val _ = op ev   : exp                 -> value
  in  ev e
  end
(* definitions of [[eval]] and [[evaldef]] for {\tuscheme} 1262c *)
fun evaldef (VAL (x, e), rho) =
      let val v   = eval (e, rho)
          val rho = bind (x, ref v, rho)
      in  (rho, namedValueString x v)
      end
  | evaldef (VALREC (x, tau, e), rho) =
      let val this = ref NIL
          val rho' = bind (x, this, rho)
          val v    = eval (e, rho')
          val _    = this := v
      in  (rho', namedValueString x v)
      end
  | evaldef (EXP e, rho) = (* differs from VAL ("it", e) only in its response *)
      let val v   = eval (e, rho)
          val rho = bind ("it", ref v, rho)
      in  (rho, valueString v)
      end
  | evaldef (DEFINE (f, tau, lambda), rho) =
      evaldef (VALREC (f, tau, LAMBDA lambda), rho)
(* type declarations for consistency checking *)
val _ = op evaldef : def * value ref env -> value ref env * string
(* definitions of [[basis]] and [[processDef]] for {\tuscheme} 457a *)
type basis = kind env * tyex env * value ref env
fun processDef (d, (Delta, Gamma, rho), interactivity) =
  let val (Gamma, tystring)  = elabdef (d, Delta, Gamma)
      val (rho,   valstring) = evaldef (d, rho)
      val _ = if prints interactivity then
                println (valstring ^ " : " ^ tystring)
              else
                ()
(* type declarations for consistency checking *)
type basis = basis
val _ = op processDef : def * basis * interactivity -> basis
  in  (Delta, Gamma, rho)
  end
fun dump_names (kinds, types, values) = app (println o fst) values  (*OMIT*)
(* shared definition of [[withHandlers]] 380a *)
fun withHandlers f a caught =
  f a
  handle RuntimeError msg   => caught ("Run-time error <at loc>: " ^ msg)
       | NotFound x         => caught ("Name " ^ x ^ " not found <at loc>")
       | Located (loc, exn) =>
           withHandlers (fn _ => raise exn) a (fn s => caught (fillAtLoc (s, loc
                                                                             )))

(* other handlers that catch non-fatal exceptions and pass messages to [[caught]] 380b *)
       | Div                => caught ("Division by zero <at loc>")
       | Overflow           => caught ("Arithmetic overflow <at loc>")
       | Subscript          => caught ("Array index out of bounds <at loc>")
       | Size               => caught (
                                "Array length too large (or negative) <at loc>")
       | IO.Io { name, ...} => caught ("I/O error <at loc>: " ^ name)

(* other handlers that catch non-fatal exceptions and pass messages to [[caught]] ((type-checking)) 412b *)
       | TypeError         msg => caught ("type error <at loc>: " ^ msg)
       | BugInTypeChecking msg => caught ("bug in type checking: " ^ msg)
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
(* definition of [[testIsGood]] for {\tuscheme} 1266c *)
fun testIsGood (test, (Delta, Gamma, rho)) =
  let fun ty e = typeof (e, Delta, Gamma)
                 handle NotFound x => raise TypeError ("name " ^ x ^
                                                              " is not defined")

 (* shared [[check{Expect,Assert,Error,Type}Checks]], which call [[ty]] 1247b *)
      fun checkExpectChecks (e1, e2) = 
        let val tau1 = ty e1
            val tau2 = ty e2
        in  if eqType (tau1, tau2) then
              true
            else
              raise TypeError ("Expressions have types " ^ typeString tau1 ^
                                  " and " ^ typeString tau2)
        end handle TypeError msg =>
        failtest ["In (check-expect ", expString e1, " ", expString e2, "), ",
                                                                            msg]

 (* shared [[check{Expect,Assert,Error,Type}Checks]], which call [[ty]] 1247c *)
      fun checkOneExpChecks inWhat e =
        let val tau1 = ty e
        in  true
        end handle TypeError msg =>
        failtest ["In (", inWhat, " ", expString e, "), ", msg]
      val checkAssertChecks = checkOneExpChecks "check-assert"
      val checkErrorChecks  = checkOneExpChecks "check-error"

 (* shared [[check{Expect,Assert,Error,Type}Checks]], which call [[ty]] 1266d *)
      fun checkTypeChecks (e, tau) =
        let val tau' = ty e
        in  true
        end
        handle TypeError msg => 
               failtest ["In (check-type ", expString e, " " ^ typeString tau,
                                                                     "), ", msg]
      fun checks (CHECK_EXPECT (e1, e2)) = checkExpectChecks (e1, e2)
        | checks (CHECK_ASSERT e)        = checkAssertChecks e
        | checks (CHECK_ERROR e)         = checkErrorChecks e
        | checks (CHECK_TYPE (e, tau))   = checkTypeChecks (e, tau)
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
      fun deftystring d =
        snd (elabdef (d, Delta, Gamma))
        handle NotFound x => raise TypeError ("name " ^ x ^ " is not defined")

(* shared [[checkTypePasses]] and [[checkTypeErrorPasses]], which call [[ty]] 1246c *)
      fun checkTypePasses (e, tau) =
        let val tau' = ty e
        in  if eqType (tau, tau') then
              true
            else
              failtest ["check-type failed: expected ", expString e,
                                                               " to have type ",
                     typeString tau, ", but it has type ", typeString tau']
        end handle TypeError msg =>
            failtest ["In (check-type ", expString e, " " ^ typeString tau,
                                                                     "), ", msg]

(* shared [[checkTypePasses]] and [[checkTypeErrorPasses]], which call [[ty]] 1247a *)
      fun checkTypeErrorPasses (EXP e) =
            (let val tau = ty e
             in  failtest ["check-type-error failed: expected ", expString e,
                       " not to have a type, but it has type ", typeString tau]
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
      fun passes (CHECK_EXPECT (c, e)) = checkExpectPasses (c, e)
        | passes (CHECK_ASSERT c)      = checkAssertPasses c
        | passes (CHECK_ERROR c)       = checkErrorPasses  c
        | passes (CHECK_TYPE (c, tau)) = checkTypePasses   (c, tau)
        | passes (CHECK_TYPE_ERROR d)  = checkTypeErrorPasses d

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
(*   IMPLEMENTATIONS OF \TUSCHEME\ PRIMITIVES AND DEFINITION OF [[INITIALBASIS]] *)
(*                                                               *)
(*****************************************************************)

(* implementations of \tuscheme\ primitives and definition of [[initialBasis]] 457b *)
(* shared utility functions for building primitives in languages with type checking 1254c *)
fun binaryOp f = (fn [a, b] => f (a, b) | _ => raise BugInTypeChecking "arity 2"
                                                                               )
fun unaryOp  f = (fn [a]    => f a      | _ => raise BugInTypeChecking "arity 1"
                                                                               )
(* type declarations for consistency checking *)
val _ = op unaryOp  : (value         -> value) -> (value list -> value)
val _ = op binaryOp : (value * value -> value) -> (value list -> value)
(* shared utility functions for building primitives in languages with type checking 1254d *)
fun arithOp f =
      binaryOp (fn (NUM n1, NUM n2) => NUM (f (n1, n2)) 
                 | _ => raise BugInTypeChecking "arithmetic on non-numbers")
(* type declarations for consistency checking *)
val _ = op arithOp   : (int * int -> int) -> (value list -> value)
(* utility functions and types for making \tuscheme\ primitives 1263b *)
val arithtype = FUNTY ([inttype, inttype], inttype)
(* type declarations for consistency checking *)
val _ = op arithtype : tyex
(* utility functions and types for making \tuscheme\ primitives 1263d *)
fun comparison f = binaryOp (BOOLV o f)
fun intcompare f = 
      comparison (fn (NUM n1, NUM n2) => f (n1, n2)
                   | _ => raise BugInTypeChecking "comparing non-numbers")
val comptype = FUNTY ([inttype, inttype], booltype)
(* type declarations for consistency checking *)
val _ = op comparison : (value * value -> bool) -> (value list -> value)
val _ = op intcompare : (int   * int   -> bool) -> (value list -> value)
val _ = op comptype   : tyex
val initialBasis =
  let fun addKind ((name, kind), kinds) = bind (name, kind, kinds)
      val kinds   = foldl addKind emptyEnv
                    (
                    (* primitive type constructors for \tuscheme\ [[::]] 427c *)
                     ("int",  TYPE) ::
                     ("bool", TYPE) ::
                     ("sym",  TYPE) ::
                     ("unit", TYPE) ::
                     ("list", ARROW ([TYPE], TYPE)) :: [])
      fun addPrim ((name, prim, funty), (types, values)) = 
        ( bind (name, funty, types)
        , bind (name, ref (PRIMITIVE prim), values)
        )
      val (types, values) = foldl addPrim (emptyEnv, emptyEnv)
                            (
                           (* primitive functions for \tuscheme\ [[::]] 1263c *)
                             ("+", arithOp op +,   arithtype) :: 
                             ("-", arithOp op -,   arithtype) :: 
                             ("*", arithOp op *,   arithtype) :: 
                             ("/", arithOp op div, arithtype) ::

                           (* primitive functions for \tuscheme\ [[::]] 1263e *)
                             ("<", intcompare op <, comptype) :: 
                             (">", intcompare op >, comptype) ::
                             ("=", comparison equalatoms,
                                                    FORALL (["'a"], FUNTY ([
                                                 tyvarA, tyvarA], booltype))) ::

                           (* primitive functions for \tuscheme\ [[::]] 1264a *)
                             ("null?", unaryOp (BOOLV o (fn (NIL   ) => true | _
                                                                      => false))
                                     , FORALL (["'a"], FUNTY ([listtype tyvarA],
                                                                  booltype))) ::
                             ("cons", binaryOp (fn (a, b) => PAIR (a, b))
                                    , FORALL (["'a"], FUNTY ([tyvarA, listtype
                                                  tyvarA], listtype tyvarA))) ::
                             ("car",  unaryOp  (fn (PAIR (car, _)) => car 
                                                 | v => raise RuntimeError
                                                                (
                                    "car applied to non-list " ^ valueString v))
                                   , FORALL (["'a"], FUNTY ([listtype tyvarA],
                                                                    tyvarA))) ::
                             ("cdr",  unaryOp  (fn (PAIR (_, cdr)) => cdr 
                                                 | v => raise RuntimeError
                                                                (
                                    "cdr applied to non-list " ^ valueString v))
                                   , FORALL (["'a"], FUNTY ([listtype tyvarA],
                                                           listtype tyvarA))) ::

                           (* primitive functions for \tuscheme\ [[::]] 1264b *)
                             ("println", unaryOp (fn x => (print (valueString x^
                                                               "\n"); unitVal)),
                                 FORALL (["'a"], FUNTY ([tyvarA], unittype))) ::
                             ("print", unaryOp (fn x => (print (valueString x);
                                                                      unitVal)),
                                 FORALL (["'a"], FUNTY ([tyvarA], unittype))) ::
                             ("printu",  unaryOp (fn NUM n => (printUTF8 n;
                                                                        unitVal)
                                                   | v => raise
                                      BugInTypeChecking "printu of non-number"),
                                 FUNTY ([inttype], unittype)) :: [])
      fun addVal ((name, v, ty), (types, values)) = 
        ( bind (name, ty, types)
        , bind (name, ref v, values)
        )
      val (types, values) =
                      foldl addVal (types, values)
                      (
             (* primitives that aren't functions, for \tuscheme\ [[::]] 1264c *)

(* if this space is completely empty, something goes wrong with the software OMIT *)
                                                                             [])
      val primBasis = (kinds, types, values)
      val fundefs   =
      (* predefined {\tuscheme} functions, as strings (generated by a script) *)

                       [
 "(val list1 (type-lambda ['a] (lambda ([x : 'a]) ((@ cons 'a) x (@ '() 'a)))))"
                       ,
                      "(val list2 (type-lambda ['a] (lambda ([x : 'a] [y : 'a])"
                       ,
            "                               ((@ cons 'a) x ((@ list1 'a) y)))))"
                       ,
             "(val list3 (type-lambda ['a] (lambda ([x : 'a] [y : 'a] [z : 'a])"
                       ,
          "                               ((@ cons 'a) x ((@ list2 'a) y z)))))"
                       , "(val o (type-lambda ['a 'b 'c]"
                       , "  (lambda ([f : ('b -> 'c)]"
                       , "           [g : ('a -> 'b)])"
                       , "     (lambda ([x : 'a]) (f (g x))))))"
                       , ""
                       , "(val curry (type-lambda ['a 'b 'c]"
                       , "   (lambda ([f : ('a 'b -> 'c)])"
                       ,
                      "      (lambda ([x : 'a]) (lambda ([y : 'b]) (f x y))))))"
                       , ""
                       , "(val uncurry (type-lambda ['a 'b 'c]"
                       , "   (lambda ([f : ('a -> ('b -> 'c))])"
                       , "      (lambda ([x : 'a] [y : 'b]) ((f x) y)))))"
                       , "(val length"
                       ,
  "  (type-lambda ['a]                 ; value of thing: a polymorphic function"
                       , "    (letrec"
                       , "       [([length-mono : ((list 'a) -> int)]"
                       , "            (lambda ([xs : (list 'a)])"
                       , "              (if ((@ null? 'a) xs)"
                       , "                  0"
                       ,
                     "                  (+ 1 (length-mono ((@ cdr 'a) xs))))))]"
                       , "       length-mono)))"
                       , "(val revapp"
                       , "  (type-lambda ['a]"
                       ,
              "    (letrec [([revapp-mono : ((list 'a) (list 'a) -> (list 'a))]"
                       ,
                  "                 (lambda ([xs : (list 'a)] [ys : (list 'a)])"
                       , "                   (if ((@ null? 'a) xs)"
                       , "                     ys"
                       ,
"                     (revapp-mono ((@ cdr 'a) xs) ((@ cons 'a) ((@ car 'a) xs) ys)))))]"
                       , "       revapp-mono)))"
                       , "(val caar"
                       , "   (type-lambda ('a)"
                       , "      (lambda ([xs : (list (list 'a))])"
                       , "          ((@ car 'a) ((@ car (list 'a)) xs)))))"
                       , "(val cadr"
                       , "   (type-lambda ('a)"
                       , "      (lambda ([xs : (list (list 'a))])"
                       ,
                       "          ((@ car (list 'a)) ((@ cdr (list 'a)) xs)))))"
                       ,
                        "(define bool and ([b : bool] [c : bool]) (if b  c  b))"
                       ,
                        "(define bool or  ([b : bool] [c : bool]) (if b  b  c))"
                       ,
                        "(define bool not ([b : bool])            (if b #f #t))"
                       , "(val append"
                       , "  (type-lambda ('a)"
                       ,
             "     (letrec [([append-mono : ((list 'a) (list 'a) -> (list 'a))]"
                       ,
                  "                 (lambda ([xs : (list 'a)] [ys : (list 'a)])"
                       , "                   (if ((@ null? 'a) xs)"
                       , "                     ys"
                       ,
"                     ((@ cons 'a) ((@ car 'a) xs) (append-mono ((@ cdr 'a) xs) ys)))))]"
                       , "        append-mono)))"
                       , "(val filter"
                       , "  (type-lambda ('a)"
                       , "    (letrec"
                       ,
                 "      [([filter-mono : (('a -> bool) (list 'a) -> (list 'a))]"
                       ,
                     "           (lambda ([p? : ('a -> bool)] [xs : (list 'a)])"
                       , "             (if ((@ null? 'a) xs)"
                       , "                 (@ '() 'a)"
                       , "                 (if (p? ((@ car 'a) xs))"
                       ,
"                     ((@ cons 'a) ((@ car 'a) xs) (filter-mono p? ((@ cdr 'a) xs)))"
                       ,
                    "                     (filter-mono p? ((@ cdr 'a) xs))))))]"
                       , "      filter-mono)))"
                       , "(val map"
                       , "  (type-lambda ('a 'b)"
                       , "    (letrec"
                       ,
                      "      [([map-mono : (('a -> 'b) (list 'a) -> (list 'b))]"
                       ,
                        "           (lambda ([f : ('a -> 'b)] [xs : (list 'a)])"
                       , "              (if ((@ null? 'a) xs)"
                       , "                  (@ '() 'b)"
                       ,
"                  ((@ cons 'b) (f ((@ car 'a) xs)) (map-mono f ((@ cdr 'a) xs))))))]"
                       , "      map-mono)))"
                       , "(define bool <= ([x : int] [y : int]) (not (> x y)))"
                       , "(define bool >= ([x : int] [y : int]) (not (< x y)))"
                       ,
 "(val != (type-lambda ('a) (lambda ([x : 'a] [y : 'a]) (not ((@ = 'a) x y)))))"
                       ,
                       "(define int max ([m : int] [n : int]) (if (> m n) m n))"
                       ,
                       "(define int min ([m : int] [n : int]) (if (< m n) m n))"
                       ,
                    "(define int mod ([m : int] [n : int]) (- m (* n (/ m n))))"
                       ,
"(define int gcd ([m : int] [n : int]) (if ((@ = int) n 0) m (gcd n (mod m n))))"
                       ,
                  "(define int lcm ([m : int] [n : int]) (* m (/ n (gcd m n))))"
                        ]
(* type declarations for consistency checking *)
val _ = op kinds     : kind      env
val _ = op types     : tyex      env
val _ = op values    : value ref env
val _ = op primBasis : basis
      val xdefs     = stringsxdefs ("predefined functions", fundefs)
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
