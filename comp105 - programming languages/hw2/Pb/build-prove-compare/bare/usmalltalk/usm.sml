(* usm.sml 899a *)


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
(*   ABSTRACT SYNTAX AND VALUES FOR \USMALLTALK                  *)
(*                                                               *)
(*****************************************************************)

(* abstract syntax and values for \usmalltalk 892c *)
(* definitions of [[exp]], [[value]], [[rep]], [[class]], and [[method]] for \usmalltalk 890b *)
datatype exp = VAR     of name
             | SET     of name * exp
             | SEND    of srcloc * name * exp * exp list
             | BEGIN   of exp list
             | BLOCK   of name list * exp list
             | LITERAL of rep
             | VALUE   of value
             | SUPER
(* definitions of [[exp]], [[value]], [[rep]], [[class]], and [[method]] for \usmalltalk 891b *)
and rep = USER     of value ref env (* collection of named instance variables *)
        | ARRAY    of value Array.array
        | NUM      of int
        | SYM      of name
        | CLOSURE  of name list * exp list * value ref env * class
        | CLASSREP of class
(* definitions of [[exp]], [[value]], [[rep]], [[class]], and [[method]] for \usmalltalk 891c *)
and class  = CLASS of { name    : name            (* name of the class *)
                      , super   : class option    (* superclass, if any *)
                      , ivars   : string list     (* instance variables *)
                      , methods : method env
                                                 (* both exported and private *)
                      , id      : int
                                                 (* uniquely identifies class *)
                      }
(* definitions of [[exp]], [[value]], [[rep]], [[class]], and [[method]] for \usmalltalk 892a *)
and method
  = PRIM_METHOD of name * (value * value list -> value)
  | USER_METHOD of { name : name, formals : name list, locals : name list, body
                                                                           : exp
                   , superclass : class (* used to send messages to super *)
                   }
(* definitions of [[exp]], [[value]], [[rep]], [[class]], and [[method]] for \usmalltalk 892b *)
withtype value = class * rep
(* definition of [[def]] for \usmalltalk 891a *)
datatype def = VAL    of name * exp
             | EXP    of exp
             | DEFINE of name * name list * exp
             | CLASSD of { name    : string
                         , super   : string
                         , ivars   : string list     (* instance variables *)
                         , methods : method_def list
                         }
and method_kind = IMETHOD          (* instance method *)
                | CMETHOD          (* class method    *)
and method_impl = USER_IMPL of name list * name list * exp
                | PRIM_IMPL of name
  withtype method_def = method_kind * name * method_impl
(* definition of [[unit_test]] for \usmalltalk 892d *)
(* definition of [[unit_test]] for untyped languages (shared) 367b *)
datatype unit_test = CHECK_EXPECT of exp * exp
                   | CHECK_ASSERT of exp
                   | CHECK_ERROR  of exp
             | CHECK_PRINT of exp * string
(* definition of [[xdef]] (shared) 367c *)
datatype xdef = DEF    of def
              | USE    of name
              | TEST   of unit_test
              | DEFS   of def list  (*OMIT*)
fun className (CLASS {name, ...}) = name
(* definition of [[valueString]] for \usmalltalk 1370b *)
fun valueString (c, NUM n) = intString n ^ valueString(c, USER [])
  | valueString (_, SYM v) = v
  | valueString (c, _) = "<" ^ className c ^ ">"
(* definition of [[expString]] for \usmalltalk 1373 *)
fun expString e =
  let fun bracket s = "(" ^ s ^ ")"
      val bracketSpace = bracket o spaceSep
      fun exps es = map expString es
      fun symString x = x
      fun valueString (_, NUM n) = intString n
        | valueString (_, SYM x) = symString x
        | valueString (c, _) = "<" ^ className c ^ ">"
  in  case e
        of LITERAL (NUM n) => intString n
         | LITERAL (SYM n) => symString n
         | LITERAL _ => "<wildly unexpected literal>"
         | VAR name => name
         | SET (x, e) => bracketSpace ["set", x, expString e]
         | SEND (_, msg, e, es) => bracketSpace (msg :: exps (e::es))
         | BEGIN es => bracketSpace ("begin" :: exps es)
         | BLOCK ([], es) => "[" ^ spaceSep (exps es) ^ "]"
         | BLOCK (xs, es) => bracketSpace ["block", bracketSpace xs,
                                           spaceSep (exps es)]
         | VALUE v => valueString v
         | SUPER => "super"
  end


(*****************************************************************)
(*                                                               *)
(*   UTILITY FUNCTIONS ON \USMALLTALK\ CLASSES, METHODS, AND VALUES *)
(*                                                               *)
(*****************************************************************)

(* utility functions on \usmalltalk\ classes, methods, and values 900a *)
fun className (CLASS {name, ...}) = name
fun classId   (CLASS {id,   ...}) = id
(* type declarations for consistency checking *)
val _ = op className : class -> name
val _ = op classId   : class -> int
(* utility functions on \usmalltalk\ classes, methods, and values 900b *)
fun methodName (PRIM_METHOD (n, _)) = n
  | methodName (USER_METHOD { name, ... }) = name
fun renameMethod (n, PRIM_METHOD (_, f)) = PRIM_METHOD (n, f)
  | renameMethod _ = raise InternalError "renamed user method"
fun methods ms = foldl (fn (m, rho) => bind (methodName m, m, rho)) emptyEnv ms
(* type declarations for consistency checking *)
val _ = op methodName   : method -> name
val _ = op methods      : method list -> method env
val _ = op renameMethod : name * method -> method
(* utility functions on \usmalltalk\ classes, methods, and values 900c *)
local 
  val next_id = ref 10 (* new classes start here *)
  fun uid () = !next_id before next_id := !next_id + 1
in
  fun mkClass name super ivars ms =
    CLASS { name = name, super = SOME super, ivars = ivars, methods = methods ms
                                                                               ,
            id = uid () }
end
(* type declarations for consistency checking *)
val _ = op mkClass : name -> class -> name list -> method list -> class



(*****************************************************************)
(*                                                               *)
(*   SUPPORT FOR BOOTSTRAPPING CLASSES AND VALUES USED DURING PARSING *)
(*                                                               *)
(*****************************************************************)

(* support for bootstrapping classes and values used during parsing 901a *)
local 
  val intClass    = ref NONE : class option ref
  val symbolClass = ref NONE : class option ref
  val arrayClass  = ref NONE : class option ref
  fun badlit what = 
    raise InternalError
        ("(bootstrapping) -- cannot " ^ what ^ " anywhere in predefined classes"
                                                                               )
in
  fun mkInteger n = (valOf (!intClass), NUM n)
    handle Option => badlit "evaluate integer literal or use array literal"
  
  fun mkSymbol s = (valOf (!symbolClass), SYM s)
    handle Option => badlit "evaluate symbol literal or use array literal"
  
  fun mkArray a = (valOf (!arrayClass), ARRAY (Array.fromList a))
    handle Option => badlit "use array literal"
(* type declarations for consistency checking *)
val _ = op mkInteger : int        -> value
val _ = op mkSymbol  : string     -> value
val _ = op mkArray   : value list -> value
(* support for bootstrapping classes and values used during parsing 901b *)
  fun findInitialClass (name, xi) =
        case !(find (name, xi))
          of (_, CLASSREP c) => c
           | _ => raise InternalError (name ^
                                          " is'nt a class in the initial basis")
  fun closeLiteralsCycle xi =
    ( intClass    := SOME (findInitialClass ("SmallInteger", xi))
    ; symbolClass := SOME (findInitialClass ("Symbol",       xi))
    ; arrayClass  := SOME (findInitialClass ("Array",        xi))
    )
end
(* type declarations for consistency checking *)
val _ = op findInitialClass : string * value ref env -> class
(* support for bootstrapping classes and values used during parsing 902a *)
local
  val trueValue  = ref NONE : value option ref
  val falseValue = ref NONE : value option ref
in
  fun mkBoolean b = valOf (!(if b then trueValue else falseValue))
    handle Option => 
        raise InternalError 

         "Bad booleanClass; evaluated boolean expression in predefined classes?"
  fun closeBooleansCycle xi =
    ( trueValue  := SOME (!(find ("true",  xi)))
    ; falseValue := SOME (!(find ("false", xi)))
    )
end
(* type declarations for consistency checking *)
val _ = op mkBoolean : bool -> value
(* support for bootstrapping classes and values used during parsing 902b *)
local
  val blockClass = ref NONE : class option ref
in
  fun mkBlock c = (valOf (!blockClass), CLOSURE c)
    handle Option => 
        raise InternalError 
            "Bad blockClass; evaluated block expression in predefined classes?"
  fun closeBlocksCycle xi =
    blockClass := SOME (findInitialClass ("Block", xi))
end
(* type declarations for consistency checking *)
val _ = op mkBlock : name list * exp list * value ref env * class -> value



(*****************************************************************)
(*                                                               *)
(*   LEXICAL ANALYSIS AND PARSING FOR \USMALLTALK, PROVIDING [[FILEXDEFS]] AND [[STRINGSXDEFS]] *)
(*                                                               *)
(*****************************************************************)

(* lexical analysis and parsing for \usmalltalk, providing [[filexdefs]] and [[stringsxdefs]] 1363a *)
(* lexical analysis for \usmalltalk 1363b *)
val nullsrc : srcloc = ("internally generated SEND node", 1)
(* lexical analysis for \usmalltalk 1363c *)
datatype pretoken = INTCHARS of char list
                  | NAME     of name
                  | QUOTE    of string option (* symbol or array *)
type token = pretoken plus_brackets
(* lexical analysis for \usmalltalk 1363d *)
fun pretokenString (INTCHARS ds)    = implode ds
  | pretokenString (NAME    x)      = x
  | pretokenString (QUOTE NONE)     = "'"
  | pretokenString (QUOTE (SOME s)) = "'" ^ s
(* lexical analysis for \usmalltalk 1364a *)
local
  val nondelims = many1 (sat (not o isDelim) one)

  fun validate NONE = NONE (* end of line *)
    | validate (SOME (#";", cs)) = NONE (* comment *)
    | validate (SOME (c, cs)) = 
        let val msg = "invalid initial character in `" ^
                      implode (c::listOfStream cs) ^ "'"
        in  SOME (ERROR msg, EOS) : (pretoken error * char stream) option
        end
in
  val smalltalkToken =
    whitespace *> bracketLexer (
            (QUOTE o SOME o implode) <$> (eqx #"'" one *> nondelims)
        <|> QUOTE NONE               <$  eqx #"'" one
        <|> INTCHARS                 <$> intChars isDelim   
        <|> (NAME o implode)         <$> nondelims                          
        <|> (validate o streamGet)
        )
(* type declarations for consistency checking *)
val _ = op smalltalkToken : token lexer
end
(* parsers for single \usmalltalk\ tokens 1365a *)
type 'a parser = (token, 'a) polyparser
val token : token parser = token (* make it monomorphic *)
val pretoken  = (fn (PRETOKEN t)=> SOME t  | _ => NONE) <$>? token
val name = (fn (NAME s)         => SOME s  | _ => NONE) <$>? pretoken
val intchars = (fn (INTCHARS ds)=> SOME ds | _ => NONE) <$>? pretoken
val sym  = (fn (QUOTE (SOME s)) => SOME s  | _ => NONE) <$>? pretoken
val quote= (fn (QUOTE NONE    ) => SOME () | _ => NONE) <$>? pretoken
val any_name = name

val int = intFromChars <$>! intchars

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
(* parsers and [[xdef]] streams for \usmalltalk 1364b *)
fun arity "if"    = 2
  | arity "while" = 1
  | arity name =
      let val cs = explode name
      in  if Char.isAlpha (hd cs) then
            length (List.filter (fn c => c = #":") cs)
          else
            1
      end

fun arityOk "value" _ = true
  | arityOk name args = arity name = length args

fun arityErrorAt loc what msgname args =
  let fun argn n = if n = 1 then "1 argument" else intString n ^ " arguments"
  in  errorAt ("in " ^ what ^ ", message " ^ msgname ^ " expects " ^
                         argn (arity msgname) ^ ", but gets " ^
                         argn (length args)) loc
  end
(* parsers and [[xdef]] streams for \usmalltalk 1365b *)
fun isImmutable x =
  List.exists (fn x' => x' = x) ["true", "false", "nil", "self", "super"] 
val immutable = sat isImmutable name

val mutable =
  let fun can'tMutate (loc, x) =
        ERROR (srclocString loc ^
               ": you cannot set or val-bind pseudovariable " ^ x)
  in  can'tMutate <$>! @@ immutable <|> OK <$>! name
  end
(* parsers and [[xdef]] streams for \usmalltalk 1365c *)
val atomicExp =  LITERAL <$> NUM    <$> int
             <|> LITERAL <$> SYM    <$> (sym <|> (quote *> name)
                                             <|> (quote *> (intString <$> int)))
             <|> SUPER              <$  eqx "super" name
             <|> VAR                <$> name
(* parsers and [[xdef]] streams for \usmalltalk 1365d *)
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
fun formalsIn context = formalsOf "(x1 x2 ...)" name context
fun exptable exp = usageParsers
  [ ("(set x e)",             curry SET   <$> mutable <*> exp)
  , ("(begin e ...)",               BEGIN <$> many exp)
  , ("(block (x ...) e ...)", curry BLOCK <$> formalsIn "block" <*> many exp)
  , ("(locals x ...)",
     pure () <!> "found '(locals ...)' where an expression was expected")
  ]
(* parsers and [[xdef]] streams for \usmalltalk 1366a *)
fun exp tokens = (
      atomicExp
  <|> quote       *> (VALUE <$> quotelit)
                                      (* not while reading predefined classes *)
  <|> curlyBracket ("{exp ...}", curry BLOCK [] <$> many exp)
  <|> exptable exp
  <|> liberalBracket ("(msgname exp ...)",
                      messageSend <$> @@ name <*> exp <*>! many exp)
  <|> liberalBracket ("(msgname exp ...)", noReceiver <$>! @@ name)
  <|> left *> right <!> "empty message send ()"
  ) 
  tokens
and noReceiver (loc, m) = errorAt ("sent message " ^ m ^ " to no object") loc
and messageSend (loc, msgname) receiver args =
      if arityOk msgname args then
          OK (SEND (loc, msgname, receiver, args))
      else
          arityErrorAt loc "message send" msgname args
(* parsers and [[xdef]] streams for \usmalltalk 1366b *)
and quotelit tokens = (
         mkSymbol  <$> name
    <|>  mkInteger <$> int
    <|>  shaped ROUND left <&> mkArray <$> bracket("(literal ...)", many
                                                                       quotelit)
    <|>  quote               <!> "' within ' is not legal" 
    <|>  shaped SQUARE left  <!> "[ within ' is not legal"
    <|>  shaped SQUARE right <!> "] within ' is not legal"
    <|>  shaped CURLY  left  <!> "{ within ' is not legal"
    <|>  shaped CURLY  right <!> "} within ' is not legal"
    ) tokens
and shaped shape delim = sat (fn (_, s) => s = shape) delim
(* type declarations for consistency checking *)
val _ = op name : string parser
val _ = op int  : int    parser
(* type declarations for consistency checking *)
val _ = op exp      : exp parser
val _ = op quotelit : value parser
(* type declarations for consistency checking *)
val _ = op quotelit : value parser
(* parsers and [[xdef]] streams for \usmalltalk 1366c *)
val printable = name <|> implode <$> intchars

val testtable = usageParsers
  [ ("(check-expect e1 e2)", curry CHECK_EXPECT <$> exp <*> exp)
  , ("(check-assert e)",           CHECK_ASSERT <$> exp)
  , ("(check-error e)",            CHECK_ERROR  <$> exp)
  , ("(check-print e chars)", curry CHECK_PRINT <$> exp <*> printable)
  ]
(* type declarations for consistency checking *)
val _ = op testtable : unit_test parser
(* parsers and [[xdef]] streams for \usmalltalk 1367a *)
val method =
  let datatype ('a, 'b) imp = PRIM of 'a | USER of 'b
      val locals = usageParsers [("(locals ivars)", many name)] <|> pure []
      fun imp kind =  PRIM <$> eqx "primitive" name *> name
                  <|> curry3 USER <$> @@ (formalsIn kind) <*> locals <*> many
                                                                             exp
      fun method kind name impl =
            check (kname kind, name, impl) >>=+ (fn impl => (kind, name, impl))
      and kname IMETHOD = "method"
        | kname CMETHOD = "class-method"
      and check (_, _, PRIM p) = OK (PRIM_IMPL p)  (* no checking possible *)
        | check (kind, name, USER (formals as (loc, xs), locals, body)) = 
            if arityOk name xs then
              OK (USER_IMPL (xs, locals, BEGIN body))
            else
              arityErrorAt loc (kind ^ " definition") name xs
  in  usageParsers
      [ ("(method f (args) body)", method IMETHOD <$> name <*>! imp "method")
      , ("(class-method f (args) body)",
                                   method CMETHOD <$> name <*>! imp
                                                                 "class method")
      ]
  end
(* type declarations for consistency checking *)
val _ = op method : method_def parser
(* parsers and [[xdef]] streams for \usmalltalk 1367b *)
fun classDef name super ivars methods =
      CLASSD { name = name, super = super, ivars = ivars, methods = methods }

val ivars = nodups ("instance variable", "class definition") <$>! @@ (many name)

val deftable = usageParsers
  [ ("(val x e)", curry  VAL    <$> mutable <*> exp)
  , ("(define f (args) body)",
                  curry3 DEFINE <$> name <*> formalsIn "define" <*> exp)
  , ("(class name super [instance vars] methods)",
                  classDef <$> name <*> name <*>
                               bracket ("(x y ...)", ivars) <*> many method)
  ]
(* type declarations for consistency checking *)
val _ = op deftable : def parser
(* parsers and [[xdef]] streams for \usmalltalk 1368a *)
val xdeftable = 
  let fun bad what =
        "unexpected `(" ^ what ^ "...'; " ^
        "did a class definition end prematurely?"
  in  usageParsers
      [ ("(use filename)",      USE <$> name)
      , ("(method ...)",        pzero <!> bad "method")
      , ("(class-method ...)",  pzero <!> bad "class-method")
      ]
  end

val xdef =  DEF  <$> deftable
        <|> TEST <$> testtable
        <|> xdeftable
        <|> badRight "unexpected right bracket"
        <|> DEF <$> EXP <$> exp
        <?> "definition"
(* type declarations for consistency checking *)
val _ = op xdeftable : xdef parser
val _ = op xdef      : xdef parser
(* parsers and [[xdef]] streams for \usmalltalk 1368b *)
val xdefstream = interactiveParsedStream (smalltalkToken, xdef)
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
(*   EVALUATION, TESTING, AND THE READ-EVAL-PRINT LOOP FOR \USMALLTALK *)
(*                                                               *)
(*****************************************************************)

(* evaluation, testing, and the read-eval-print loop for \usmalltalk 918b *)
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
(* support for primitive methods and built-in classes 899b *)
(* utility functions for building primitives in \usmalltalk 902c *)
type primitive = value * value list -> value
fun arityError n (receiver, args) =
  raise RuntimeError ("primitive method expected " ^ intString n ^
                      " arguments; got " ^ intString (length args))
fun unaryPrim  f = (fn (a, [])  => f  a     | xs => arityError 0 xs)
fun binaryPrim f = (fn (a, [b]) => f (a, b) | xs => arityError 1 xs)
fun primMethod name f = PRIM_METHOD (name, f)
(* type declarations for consistency checking *)
val _ = op unaryPrim  : (value         -> value) -> primitive
val _ = op binaryPrim : (value * value -> value) -> primitive
val _ = op primMethod : name -> primitive -> method
(* utility functions for building primitives in \usmalltalk 903 *)
fun userMethod name formals locals body = 
  let val bogusSuper = CLASS { name = "should never be used", super = NONE,
                               ivars = [], methods = [], id = 0 }
  in  USER_METHOD { name = name, formals = formals, locals = locals,
                    body = internalExp body, superclass = bogusSuper }
  end
and internalExp s = 
  let val name = "internal expression \"" ^ s ^ "\""
      val input = interactiveParsedStream (smalltalkToken, exp <?> "expression")
                                          (name, streamOfList [s], noPrompts)
      exception BadUserMethodInInterpreter of string (* can't be caught *)
  in  case streamGet input
        of SOME (e, _) => e
         | NONE => raise BadUserMethodInInterpreter s
  end
(* type declarations for consistency checking *)
val _ = op internalExp : string -> exp
(* \ml\ functions for [[Object]]'s and [[UndefinedObject]]'s primitives 904a *)
fun eqRep ((cx, x), (cy, y)) = 
  classId cx = classId cy andalso
  case (x, y)
    of (ARRAY x,    ARRAY    y) => x = y
     | (NUM   x,    NUM      y) => x = y
     | (SYM   x,    SYM      y) => x = y
     | (USER  x,    USER     y) => x = y
     | (CLOSURE  x, CLOSURE  y) => false
     | (CLASSREP x, CLASSREP y) => classId x = classId y
     | _ => false
(* type declarations for consistency checking *)
val _ = op eqRep : value * value -> bool
(* \ml\ functions for [[Object]]'s and [[UndefinedObject]]'s primitives 904c *)
fun defaultPrint (self as (c, _)) = ( app xprint ["<", className c, ">"]; self )
(* \ml\ functions for [[Object]]'s and [[UndefinedObject]]'s primitives 904e *)
fun memberOf ((c, _), (_, CLASSREP c')) = mkBoolean (classId c = classId c')
  | memberOf _ = raise RuntimeError "argument of isMemberOf: must be a class"

fun kindOf ((c, _), (_, CLASSREP (CLASS {id=u', ...}))) =
      let fun subclassOfClassU' (CLASS {super, id=u, ... }) =
            u = u' orelse (case super of NONE => false | SOME c =>
                                                            subclassOfClassU' c)
      in  mkBoolean (subclassOfClassU' c)
      end
  | kindOf _ = raise RuntimeError "argument of isKindOf: must be a class"
(* \ml\ functions for [[Object]]'s and [[UndefinedObject]]'s primitives 904f *)
fun error (_, (_, SYM s)) = raise RuntimeError s
  | error (_, (c, _    )) =
      raise RuntimeError ("error: got class " ^ className c ^
                                                            "; expected Symbol")
(* built-in classes [[Object]] and [[UndefinedObject]] 909a *)
val objectClass = 
  CLASS { name = "Object", super = NONE, ivars = ["self"], id = 1
        , methods = methods
            [ primMethod "print"     (unaryPrim defaultPrint)
            , userMethod "println"   [] []
                                     "(begin (print self) (print newline) self)"
            , primMethod "isNil"     (unaryPrim (fn _ => mkBoolean false))
            , primMethod "notNil"    (unaryPrim (fn _ => mkBoolean true))
            , primMethod "error:"    (binaryPrim error)
            , primMethod "="         (binaryPrim (mkBoolean o eqRep))
            , primMethod "isKindOf:"    (binaryPrim kindOf)
            , primMethod "isMemberOf:"  (binaryPrim memberOf)
            , primMethod "subclassResponsibility"
               (unaryPrim
                  (fn _ => raise RuntimeError

           "subclass failed to implement a method that was its responsibility"))
            , userMethod "!="          ["x"] [] "(not (= self x))"
            , userMethod "similar:"    ["x"] [] "(= self x)"
            , userMethod "dissimilar:" ["x"] [] "(not (similar: self x))"
            ]
        }
(* built-in classes [[Object]] and [[UndefinedObject]] 909b *)
val nilClass = 
  mkClass "UndefinedObject" objectClass []
    [ primMethod "isNil"  (unaryPrim (fn _ => mkBoolean true))
    , primMethod "notNil" (unaryPrim (fn _ => mkBoolean false))
    , primMethod "print"  (unaryPrim (fn x => (xprint "nil"; x)))
    ]
(* built-in classes [[Object]] and [[UndefinedObject]] 909c *)
val nilValue = 
  let val nilCell  = ref (nilClass, USER []) : value ref
      val nilValue = (nilClass, USER (bind ("self", nilCell, emptyEnv)))
      val _        = nilCell := nilValue
  in  nilValue
  end
(* \ml\ functions for remaining classes' primitives 904g *)
fun printInt (self as (_, NUM n)) = ( xprint (intString n); self )
  | printInt _ = raise RuntimeError (
                                   "cannot print when object inherits from Int")
(* \ml\ functions for remaining classes' primitives 904h *)
fun printu (self as (_, NUM n)) = ( printUTF8 n; self )
  | printu _ = raise RuntimeError ("receiver of printu is not a small integer")
(* \ml\ functions for remaining classes' primitives 905a *)
fun binaryInt mk operator ((_, NUM n), (_, NUM m)) = mk (operator (n, m))
  | binaryInt _ _         ((_, NUM n), (c, _)) =
      raise RuntimeError ("numeric primitive expected numeric argument, got <"
                          ^ className c ^ ">")
  | binaryInt _ _         ((c, _), _) =
      raise RuntimeError ("numeric primitive method defined on <" ^ className c
                                                                          ^ ">")
fun arithop    operator = binaryPrim (binaryInt mkInteger operator)
fun intcompare operator = binaryPrim (binaryInt mkBoolean operator)
(* \ml\ functions for remaining classes' primitives 905b *)
fun newInteger ((_, CLASSREP c), (_, NUM n)) = (c, NUM n)
  | newInteger _ = raise RuntimeError (
                                   "made new integer with non-int or non-class")
(* type declarations for consistency checking *)
val _ = op binaryInt  : ('a -> value) -> (int * int -> 'a)   -> value * value ->
                                                                           value
val _ = op arithop    :                  (int * int -> int)  -> primitive
val _ = op intcompare :                  (int * int -> bool) -> primitive
(* \ml\ functions for remaining classes' primitives 905d *)
fun printSymbol (self as (_, SYM s)) = (xprint s; self)
  | printSymbol _ = raise RuntimeError
                                 "cannot print when object inherits from Symbol"
(* \ml\ functions for remaining classes' primitives 905e *)
fun newSymbol ((_, CLASSREP c), (_, SYM s)) = (c, SYM s)
  | newSymbol _ = raise RuntimeError (
                                 "made new symbol with non-symbol or non-class")
(* \ml\ functions for remaining classes' primitives 906b *)
fun newArray ((_, CLASSREP c), (_, NUM n)) = (c, ARRAY (Array.array (n, nilValue
                                                                             )))
  | newArray _ = raise RuntimeError
                                "Array new sent to non-class or got non-integer"
(* \ml\ functions for remaining classes' primitives 906c *)
fun arrayPrimitive f ((c, ARRAY a), l) = f (a, l)
  | arrayPrimitive f _ = raise RuntimeError "Array primitive used on non-array"

fun arraySize (a, []) = mkInteger (Array.length a)
  | arraySize ra      = arityError 0 ra
(* \ml\ functions for remaining classes' primitives 906d *)
fun arrayAt (a, [(_, NUM n)]) = Array.sub (a, n)
  | arrayAt (_, [_])  = raise RuntimeError "Non-integer used as array subscript"
  | arrayAt ra        = arityError 1 ra

fun arrayAtUpdate (a, [(_, NUM n), x]) = (Array.update (a, n, x); nilValue)
  | arrayAtUpdate (_, [_, _]) = raise RuntimeError
                                           "Non-integer used as array subscript"
  | arrayAtUpdate ra          = arityError 2 ra
(* \ml\ functions for remaining classes' primitives 907a *)
fun classPrimitive f = 
  unaryPrim (fn (meta, CLASSREP c) => f (meta, c)
              | _ => raise RuntimeError "class primitive sent to non-class")
(* type declarations for consistency checking *)
val _ = op arrayPrimitive : (value array * value list -> value) -> primitive
(* type declarations for consistency checking *)
val _ = op classPrimitive : (class * class -> value) -> primitive
(* \ml\ functions for remaining classes' primitives 907b *)
local
  fun mkIvars (CLASS { ivars, super, ... }) =
    let val supervars = case super of NONE => emptyEnv | SOME c => mkIvars c
    in  foldl (fn (n, rho) => bind (n, ref nilValue, rho)) supervars ivars
    end
in
  fun newUserObject (_, c) =
        let val ivars = mkIvars c
            val self = (c, USER ivars)
        in  (find ("self", ivars) := self; self)
        end
(* type declarations for consistency checking *)
val _ = op mkIvars       : class -> value ref env
val _ = op newUserObject : class * class -> value
end
(* \ml\ functions for remaining classes' primitives 908 *)
local
  fun showProtocol doSuper kind c =
    let fun member x l = List.exists (fn x' : string => x' = x) l
        fun insert (x, []) = [x]
          | insert (x, (h::t)) =
              case compare x h
                of LESS    => x :: h :: t
                 | EQUAL   => x :: t (* replace *)
                 | GREATER => h :: insert (x, t)
        and compare (name, _) (name', _) = String.compare (name, name')
        fun methods (CLASS { super, methods = ms, name, ... }) =
              if not doSuper orelse (kind = "class-method" andalso name =
                                                                   "Class") then
                foldl insert [] ms
              else
                foldl insert (case super of NONE => [] | SOME c => methods c) ms
        fun show (name, PRIM_METHOD _) =
              app xprint ["(", kind, " ", name, " primitive ...)\n"]
          | show (name, USER_METHOD { formals, ... }) =
              app xprint ["(", kind, " ", name,
                          " (", spaceSep formals, ") ...)\n"]
    in  app show (methods c)
    end
in
  fun protocols all (meta, c) =
    ( showProtocol all "class-method" meta
    ; showProtocol all "method" c
    ; (meta, CLASSREP c)
    )
end
(* \ml\ functions for remaining classes' primitives 950b *)
fun withOverflow binop ((_, NUM n), [(_, NUM m), ovflw]) = 
      (mkBlock ([], [VALUE (mkInteger (binop (n, m)))], emptyEnv, objectClass)
       handle Overflow => ovflw)
  | withOverflow _ (_, [_, _]) =
      raise RuntimeError "numeric primitive with overflow expects numbers"
  | withOverflow _ _ =
      raise RuntimeError
                     "numeric primitive with overflow expects receiver + 2 args"
(* remaining built-in classes 910a *)
val classClass =
  mkClass "Class" objectClass []
    [ primMethod "new"           (classPrimitive newUserObject) 
    , primMethod "protocol"      (classPrimitive (protocols true))
    , primMethod "localProtocol" (classPrimitive (protocols false))
    ]
(* definition of [[newClassObject]] and supporting functions 916a *)
(* definition of function [[primitiveMethod]] 916c *)
val primitiveMethods = methods (
                            (* primitive methods for \usmalltalk\ [[::]] 904b *)
                                primMethod "eqObject" (binaryPrim (mkBoolean o
                                                                      eqRep)) ::

                            (* primitive methods for \usmalltalk\ [[::]] 904d *)
                                primMethod "print" (unaryPrim defaultPrint) ::

                            (* primitive methods for \usmalltalk\ [[::]] 905c *)
                                primMethod "printSmallInteger" (unaryPrim
                                                                    printInt) ::
                                primMethod "printu"            (unaryPrim printu
                                                                          )   ::
                                primMethod "newSmallInteger:"  (binaryPrim
                                                                  newInteger) ::
                                primMethod "+"   (arithop op +  )  ::
                                primMethod "-"   (arithop op -  )  ::
                                primMethod "*"   (arithop op *  )  ::
                                primMethod "div" (arithop op div)  ::
                                primMethod "<"   (intcompare op <) ::
                                primMethod ">"   (intcompare op >) ::

                            (* primitive methods for \usmalltalk\ [[::]] 905f *)
                                primMethod "printSymbol" (unaryPrim  printSymbol
                                                                            ) ::
                                primMethod "newSymbol"   (binaryPrim newSymbol
                                                                            ) ::

                            (* primitive methods for \usmalltalk\ [[::]] 906e *)
                                primMethod "arrayNew:"       (binaryPrim
                                                                  newArray)   ::
                                primMethod "arraySize"       (arrayPrimitive
                                                                  arraySize)  ::
                                primMethod "arrayAt:"        (arrayPrimitive
                                                                  arrayAt)    ::
                                primMethod "arrayAt:update:" (arrayPrimitive
                                                               arrayAtUpdate) ::

                            (* primitive methods for \usmalltalk\ [[::]] 906f *)
                                primMethod "value" (fn _ => raise InternalError
                                              "hit primitive method 'value'") ::

                            (* primitive methods for \usmalltalk\ [[::]] 950c *)
                                primMethod "add:withOverflow:" (withOverflow op
                                                                          + ) ::
                                primMethod "sub:withOverflow:" (withOverflow op
                                                                          - ) ::
                                primMethod "mul:withOverflow:" (withOverflow op
                                                                     * ) :: nil)
fun primitiveMethod name =
  find (name, primitiveMethods)
  handle NotFound n => raise RuntimeError ("There is no primitive method named "
                                                                            ^ n)
fun newClassObject {name, super, ivars, methods = ms} xi =
  let val (superMeta, super) = findClass (super, xi)
        handle NotFound s => raise RuntimeError ("Superclass " ^ s ^
                                                                   " not found")
      fun method (kind, name, PRIM_IMPL prim) =
            renameMethod (name, primitiveMethod prim)
        | method (kind, name, USER_IMPL (formals, ls, body)) =
            USER_METHOD { name = name, formals = formals, body = body, locals =
                                                                              ls
                        , superclass = case kind of IMETHOD => super
                                                  | CMETHOD => superMeta
                        }
      fun addMethodDefn (m as (CMETHOD, _, _), (c's, i's)) = (method m :: c's,
                                                                            i's)
        | addMethodDefn (m as (IMETHOD, _, _), (c's, i's)) = (c's, method m ::
                                                                            i's)
      val (cmethods, imethods) = foldr addMethodDefn ([], []) ms
      val metaclassName = "class " ^ name
      val class     = mkClass name          super      ivars imethods
      val metaclass = mkClass metaclassName superMeta  []    cmethods
(* type declarations for consistency checking *)
val _ = op primitiveMethod : name -> method
val _ = op method : method_def -> method
  in  (metaclass, CLASSREP class)
  end
(* definition of [[newClassObject]] and supporting functions 916b *)
and findClass (supername, xi) =
  case !(find (supername, xi))
    of (meta, CLASSREP c) => (meta, c)
     | v => raise RuntimeError ("object " ^ supername ^ " = " ^ valueString v ^
                                " is not a class")
(* functions for managing and printing a \usmalltalk\ stack trace 1368c *)
local
  (* private state and functions for printing indented traces ((usm)) 1368d *)
  fun traceMe xi =
    let val count = find("&trace", xi)
    in  case !count
          of (c, NUM n) =>
              if n = 0 then false
              else ( count := (c, NUM (n - 1))
                   ; if n = 1 then (xprint "<trace ends>\n"; false) else true
                   )
           | _ => false
    end handle NotFound _ => false
  (* type declarations for consistency checking *)
  val _ = op traceMe : value ref env -> bool
  (* private state and functions for printing indented traces ((usm)) 1369a *)
  val tindent = ref 0
  fun indent 0 = ()
    | indent n = (xprint "  "; indent (n-1))
  (* private state and functions for printing indented traces ((usm)) 1369b *)
  datatype indentation = INDENT_AFTER | OUTDENT_BEFORE

  fun tracePrint direction xi f =
      if traceMe xi then
        let val msg = f () (* could change tindent *)
        in  ( if direction = OUTDENT_BEFORE then tindent := !tindent - 1 else ()
            ; indent (!tindent)
            ; app xprint msg
            ; xprint "\n"
            ; if direction = INDENT_AFTER   then tindent := !tindent + 1 else ()
            )
        end
      else
          ()    

(* private state and functions for maintaining a stack of source-code locations ((usm)) 1369c *)
  val locationStack = ref [] : (string * srcloc) list ref
  fun push srcloc = locationStack := srcloc :: !locationStack
  fun pop () = case !locationStack
                 of []     => raise InternalError "tracing stack underflows"
                  | h :: t => locationStack := t
in
  (* exposed tracing functions ((usm)) 1370a *)
  fun resetTrace ()       = (locationStack := []; tindent := 0)
  fun traceIndent what xi = (push what; tracePrint INDENT_AFTER   xi)
  fun outdentTrace     xi = (pop ();    tracePrint OUTDENT_BEFORE xi)
  fun showStackTrace () =
    let fun show (msg, (file, n)) =
          app xprint ["  Sent '", msg, "' in ", file, ", line ", intString n,
                                                                           "\n"]
    in  case !locationStack
          of [] => ()
           | l  => ( xprint "Method-stack traceback:\n"; app show (!
                                                                locationStack) )
    end
  fun eprintlnTrace s = ( eprintln s; showStackTrace (); resetTrace () )
  (* type declarations for consistency checking *)
  val _ = op resetTrace     : unit -> unit
  val _ = op traceIndent    : string * srcloc -> value ref env -> (unit ->
                                                            string list) -> unit
  val _ = op outdentTrace   :                    value ref env -> (unit ->
                                                            string list) -> unit
  val _ = op showStackTrace : unit -> unit
  val _ = op eprintlnTrace  : string -> unit
end
(* definition of [[optimizedBind]] 917b *)
fun optimizedBind (x, v, xi) =
  let val loc = find (x, xi)
  in  (loc := v; xi)
  end handle NotFound _ => bind (x, ref v, xi)
(* definitions of [[eval]], [[evaldef]], [[basis]], and [[processDef]] for \usmalltalk 910b *)
fun eval (e, rho, superclass, xi) =
  let val go = applyCheckingOverflow id in go end (* OMIT *)
  let (* local helper functions of [[eval]] 911a *)
      fun findMethod (name, class) =
        let fun fm (CLASS { methods, super, ...}) =
              find (name, methods)
              handle NotFound m =>
                case super
                  of SOME c => fm c
                   | NONE   => raise RuntimeError
                                 (className class ^
                                            " does not understand message " ^ m)
      (* type declarations for consistency checking *)
      val _ = op findMethod : name * class -> method
      val _ = op fm         : class        -> method
        in  fm class
        end
      (* local helper functions of [[eval]] 911b *)
      fun evalMethod (PRIM_METHOD (name, f), receiver, actuals) = f (receiver,
                                                                        actuals)
        | evalMethod (USER_METHOD { name, superclass, formals, locals, body },
                      receiver, actuals) =
            let val rho'  = instanceVars receiver
                val rho_x = bindList (formals, map ref actuals, rho')
                val rho_y = bindList (locals, map (fn _ => ref nilValue) locals,
                                                                          rho_x)
            in  eval (body, rho_y, superclass, xi)
            end
              handle BindListLength => 
                  raise RuntimeError
                      ("wrong number of arguments to method " ^ name ^
                                                                "; expected (" ^
                       spaceSep (name :: "self" :: formals) ^ ")")
      and instanceVars (_, USER rep) = rep
        | instanceVars self = bind ("self", ref self, emptyEnv)
      (* type declarations for consistency checking *)
      val _ = op evalMethod   : method * value * value list -> value
      val _ = op instanceVars : value -> value ref env
      (* local helper functions of [[eval]] 912a *)
      fun applyClosure ((formals, body, rho, superclass), actuals) =
        eval (BEGIN body, bindList (formals, map ref actuals, rho), superclass,
                                                                             xi)
        handle BindListLength => 
            raise RuntimeError ("wrong number of arguments to block; expected "
                                                                               ^
                                "(value <block> " ^ spaceSep formals ^ ")")
      (* type declarations for consistency checking *)
      val _ = op applyClosure : (name list * exp list * value ref env * class) *
                                                             value list -> value
      (* function [[ev]], the evaluator proper ((usm)) 912b *)
      fun ev (VALUE v) = v
      (* function [[ev]], the evaluator proper ((usm)) 912c *)
        | ev (LITERAL c) = 
            (case c of NUM n => mkInteger n
                     | SYM n => mkSymbol n
                     | _ => raise InternalError "unexpected literal")
      (* function [[ev]], the evaluator proper ((usm)) 913a *)
        | ev (VAR v) = !(find (v, rho) handle NotFound _ => find (v, xi))
        | ev (SET (n, e)) =
            let val v = ev e
                val cell = find (n, rho) handle NotFound _ => find (n, xi)
            in  cell := v; v
            end 
      (* function [[ev]], the evaluator proper ((usm)) 913b *)
        | ev (SUPER) = ev (VAR "self")
      (* function [[ev]], the evaluator proper ((usm)) 913c *)
        | ev (BEGIN es) =
            let fun b (e::es, lastval) = b (es, ev e)
                  | b (   [], lastval) = lastval
            in  b (es, nilValue)
            end
      (* function [[ev]], the evaluator proper ((usm)) 913d *)
        | ev (BLOCK (formals, body)) = mkBlock (formals, body, rho, superclass)
      (* function [[ev]], the evaluator proper ((usm)) 914 *)
        | ev (SEND (srcloc, message, receiver, args))  =
            let val obj as (class, rep) = ev receiver
                val args = map ev args
                val dispatchingClass = case receiver of SUPER => superclass | _
                                                                        => class
                (* definitions of message-tracing procedures 915 *)
                fun traceSend (file, line) =
                  traceIndent (message, (file, line)) xi (fn () =>
                    let val c   = className dispatchingClass
                        val obj = if String.isPrefix "class " c then c
                                  else "an object of class " ^ c
                    in  [file, ", line ", intString line, ": ",
                         "Sending message (", spaceSep (message :: map
                                                         valueString args), ")",
                         " to ", obj]
                    end)
                fun traceAnswer (file, line) answer =
                  ( outdentTrace xi (fn () =>
                       [file, ", line ", intString line, ": ",
                        "(", spaceSep (message :: map valueString (obj :: args))
                                                                          , ")",
                        " = ", valueString answer])
                  ; answer
                  )
                fun performSend () =
                  case (message, rep)
                    of ("value", CLOSURE clo) => applyClosure (clo, args)
                     | _ => evalMethod (findMethod (message, dispatchingClass),
                                                                      obj, args)

            in  ( traceSend srcloc
                ; traceAnswer srcloc (performSend ())
                )
            end
      (* type declarations for consistency checking *)
      val _ = op ev : exp -> value
(* type declarations for consistency checking *)
val _ = op eval: exp * value ref env * class * value ref env -> value
val _ = op ev  : exp -> value
  in  ev e
  end
(* definitions of [[eval]], [[evaldef]], [[basis]], and [[processDef]] for \usmalltalk 917a *)
fun evaldef (d, xi) =
  let fun ev e = eval (e, emptyEnv, objectClass, xi)
      val (defined, v) =
        case d
          of VAL (name, e)             => (name, ev e)
           | EXP e                     => ("it", ev e)
           | DEFINE (name, args, body) => (name, ev (BLOCK (args, [body])))
           | CLASSD (d as {name, ...}) => (name, newClassObject d xi)
      val xi' = optimizedBind (defined, v, xi)
      val _ = closeLiteralsCycle xi handle NotFound _ => ()
(* type declarations for consistency checking *)
val _ = op findClass : name * value ref env -> class * class
(* type declarations for consistency checking *)
val _ = op evaldef : def * value ref env -> value ref env * value
val _ = op ev      : exp -> value
  in  (xi', v)
  end
(* definitions of [[eval]], [[evaldef]], [[basis]], and [[processDef]] for \usmalltalk 918a *)
type basis = value ref env
fun processDef (d, xi, interactivity) =
  let val (xi', v) = evaldef (d, xi)
      val _ = if prints interactivity then 
                ignore (eval (SEND (nullsrc, "println", VALUE v, []),
                              emptyEnv, objectClass, xi'))
              else
                ()
  in  xi'
  end
fun dump_names basis = app (println o fst) basis  (*OMIT*)
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
(* definition of [[testIsGood]] for \usmalltalk 1371a *)
fun testIsGood (test, xi) =
  let fun ev e = eval (e, emptyEnv, objectClass, xi)
      fun outcome e = withHandlers (OK o ev) e (ERROR o stripAtLoc)
      fun testSimilar (v1, v2) =
        let val areSimilar = ev (SEND (nullsrc, "similar:", VALUE v1, [VALUE v2]
                                                                              ))
        in  eqRep (areSimilar, mkBoolean true)
        end
      fun printsAs v =
        let val (bprint, contents) = bprinter ()
            val _ = withXprinter bprint ev (SEND (nullsrc, "print", VALUE v, [])
                                                                               )
        in  contents ()
        end
      fun valueString _ =
        raise RuntimeError "internal error: called the wrong ValueString"

(* definitions of [[check{Expect,Assert,Error}Passes]] that call [[printsAs]] 1371b *)
      fun whatWasExpected (LITERAL (NUM n), _) = printsAs (mkInteger n)
        | whatWasExpected (LITERAL (SYM x), _) = printsAs (mkSymbol x)
        | whatWasExpected (e, OK v) =
            concat [printsAs v, " (from evaluating ", expString e, ")"]
        | whatWasExpected (e, ERROR _) =
            concat ["the result of evaluating ", expString e]

(* definitions of [[check{Expect,Assert,Error}Passes]] that call [[printsAs]] 1372a *)
      val cxfailed = "check-expect failed: "
      fun checkExpectPasses (checkx, expectx) =
        case (outcome checkx, outcome expectx)
          of (OK check, OK expect) => 
               (case withHandlers (OK o testSimilar) (check, expect) (ERROR o
                                                                     stripAtLoc)
                  of OK true => true
                   | OK false =>
                       failtest [cxfailed, "expected ", expString checkx, 
                                 " to be similar to ", whatWasExpected (expectx,
                                                                     OK expect),
                                 ", but it's ", printsAs check]
                   | ERROR msg =>
                       failtest [cxfailed, "testing similarity of ", expString
                                                                 checkx, " to ",
                                 expString expectx, " caused error ", msg])
           | (ERROR msg, tried) =>
               failtest [cxfailed, "evaluating ", expString checkx,
                                                          " caused error ", msg]
           | (_, ERROR msg) =>
               failtest  [cxfailed, "evaluating ", expString expectx,
                                                          " caused error ", msg]

(* definitions of [[check{Expect,Assert,Error}Passes]] that call [[printsAs]] 1372b *)
      val cafailed = "check-assert failed: "
      fun checkAssertPasses checkx =
        case outcome checkx
          of OK check =>
               eqRep (check, mkBoolean true) orelse
               failtest [cafailed, "expected assertion ", expString checkx,
                         " to hold, but it doesn't"]
           | ERROR msg =>
               failtest [cafailed, "evaluating ", expString checkx,
                                                          " caused error ", msg]

(* definitions of [[check{Expect,Assert,Error}Passes]] that call [[printsAs]] 1372c *)
      val cefailed = "check-error failed: "
      fun checkErrorPasses checkx =
            case outcome checkx
              of ERROR _ => true
               | OK check =>
                   failtest [cefailed, "expected evaluating ", expString checkx,
                             " to cause an error, but evaluation produced ",
                             printsAs check]
      (* definition of [[checkPrintPasses]] 1372d *)

      val cpfailed = "check-print failed: "
      fun checkPrintPasses (checkx, s) =
        case outcome checkx 
          of OK check =>
               (case withHandlers (OK o printsAs) check (ERROR o stripAtLoc)
                  of OK s' =>
                       s = s' orelse
                       failtest [cpfailed, "expected \"", s, "\" but got \"", s'
                                                                         , "\""]
                   | ERROR msg =>
                       failtest [cpfailed, "calling print method on ",
                                 expString checkx, " caused error ", msg])
           | ERROR msg =>
               failtest [cpfailed, "evaluating ", expString checkx,
                                                          " caused error ", msg]
      fun passes (CHECK_EXPECT (c, e)) = checkExpectPasses (c, e)
        | passes (CHECK_ASSERT c)      = checkAssertPasses c
        | passes (CHECK_ERROR c)       = checkErrorPasses  c
        | passes (CHECK_PRINT (c, s))  = checkPrintPasses  (c, s)
  in  passes test
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
(*   IMPLEMENTATIONS OF \USMALLTALK\ PRIMITIVES AND DEFINITION OF [[INITIALBASIS]] *)
(*                                                               *)
(*****************************************************************)

(* implementations of \usmalltalk\ primitives and definition of [[initialBasis]] 918c *)
val initialXi = emptyEnv

fun mkMeta c = mkClass ("class " ^ className c) classClass [] []
fun addClass (c, xi) = bind (className c, ref (mkMeta c, CLASSREP c), xi)
val initialXi = foldl addClass initialXi [ objectClass, nilClass, classClass ]
(* implementations of \usmalltalk\ primitives and definition of [[initialBasis]] 919a *)
val initialXi =
  let val xdefs =
        stringsxdefs ("predefined classes", 

(* predefined {\usmalltalk} classes and values, as strings (generated by a script) *)

                       [ "(class Boolean Object []"
                       ,
"    (method ifTrue:ifFalse: (trueBlock falseBlock) (subclassResponsibility self))"
                       , "  "
                       , "    (method ifFalse:ifTrue: (falseBlock trueBlock) "
                       , "        (ifTrue:ifFalse: self trueBlock falseBlock))"
                       ,
        "    (method ifTrue:  (trueBlock)  (ifTrue:ifFalse: self trueBlock {}))"
                       ,
       "    (method ifFalse: (falseBlock) (ifTrue:ifFalse: self {} falseBlock))"
                       , "    "
                       ,
   "    (method not ()          (ifTrue:ifFalse: self {false}          {true}))"
                       ,
"    (method eqv: (aBoolean) (ifTrue:ifFalse: self {aBoolean}       {(not aBoolean)}))"
                       ,
"    (method xor: (aBoolean) (ifTrue:ifFalse: self {(not aBoolean)} {aBoolean}))"
                       , ""
                       ,
            "    (method & (aBoolean) (ifTrue:ifFalse: self {aBoolean} {self}))"
                       ,
            "    (method | (aBoolean) (ifTrue:ifFalse: self {self} {aBoolean}))"
                       , "  "
                       ,
"    (method and: (alternativeBlock) (ifTrue:ifFalse: self alternativeBlock {self}))"
                       ,
"    (method or:  (alternativeBlock) (ifTrue:ifFalse: self {self} alternativeBlock))"
                       , ""
                       ,
"    (method if (trueBlock falseBlock) (ifTrue:ifFalse: self trueBlock falseBlock))"
                       , ")"
                       , "(class True Boolean []"
                       ,
           "  (method ifTrue:ifFalse: (trueBlock falseBlock) (value trueBlock))"
                       , ")"
                       , "(class False Boolean []"
                       ,
          "  (method ifTrue:ifFalse: (trueBlock falseBlock) (value falseBlock))"
                       , ")"
                       , "(class Block Object "
                       , "    [] ; internal representation"
                       , "    (class-method new () {})"
                       , "    (method value primitive value)"
                       , "    (method whileTrue: (body)"
                       , "        (ifTrue:ifFalse: (value self)"
                       , "            {(value body)"
                       , "             (whileTrue: self body)}"
                       , "            {nil}))"
                       , "    (method while (body) (whileTrue: self body))"
                       , "    (method whileFalse: (body) "
                       , "         (ifTrue:ifFalse: (value self)"
                       , "             {nil}"
                       , "             {(value body) "
                       , "              (whileFalse: self body)}))"
                       , ")"
                       , "(class Symbol Object"
                       , "    [] ; internal representation"
                       ,
             "    (class-method new () (error: self 'can't-send-new-to-Symbol))"
                       , "    (class-method new:   primitive newSymbol)"
                       , "    (method       print  primitive printSymbol)"
                       , ")"
                       , "(class Magnitude Object "
                       , "    [] ; abstract class"
                       ,
"    (method =  (x) (subclassResponsibility self)) ; may not inherit from Object"
                       , "    (method <  (x) (subclassResponsibility self))"
                       , "    (method >  (y) (< y self))"
                       , "    (method <= (x) (not (> self x)))"
                       , "    (method >= (x) (not (< self x)))"
                       ,
   "    (method min: (aMagnitude) (if (< self aMagnitude) {self} {aMagnitude}))"
                       ,
   "    (method max: (aMagnitude) (if (> self aMagnitude) {self} {aMagnitude}))"
                       , ")"
                       , "(class Number Magnitude"
                       , "    []  ; abstract class"
                       , "    ;;;;;;; basic Number protocol"
                       ,
                  "    (method +   (aNumber)     (subclassResponsibility self))"
                       ,
                  "    (method *   (aNumber)     (subclassResponsibility self))"
                       ,
                  "    (method negated    ()     (subclassResponsibility self))"
                       ,
                  "    (method reciprocal ()     (subclassResponsibility self))"
                       , "    "
                       ,
                  "    (method asInteger  ()     (subclassResponsibility self))"
                       ,
                  "    (method asFraction ()     (subclassResponsibility self))"
                       ,
                  "    (method asFloat    ()     (subclassResponsibility self))"
                       ,
                  "    (method coerce: (aNumber) (subclassResponsibility self))"
                       , "    (method -   (y) (+ self (negated  y)))"
                       ,
            "    (method abs ()  (if (negative self) {(negated  self)} {self}))"
                       , "    (method /   (y) (* self (reciprocal y)))"
                       ,
                   "    (method negative         () (<  self (coerce: self 0)))"
                       ,
                   "    (method nonnegative      () (>= self (coerce: self 0)))"
                       ,
                   "    (method strictlyPositive () (>  self (coerce: self 0)))"
                       , "    (method squared () (* self self))"
                       , "    (method raisedToInteger: (anInteger)"
                       , "        (if (= anInteger 0)"
                       , "            {(coerce: self 1)}"
                       , "            {(if (= anInteger 1) {self}"
                       ,
      "                {(* (squared (raisedToInteger: self (div: anInteger 2)))"
                       ,
          "                    (raisedToInteger: self (mod: anInteger 2)))})}))"
                       ,
               "    (method sqrt () (sqrtWithin: self (coerce: self (/ 1 10))))"
                       ,
                    "    (method sqrtWithin: (epsilon) [locals two x<i-1> x<i>]"
                       , "        ; find square root of receiver within epsilon"
                       , "        (set two    (coerce: self 2))"
                       , "        (set x<i-1> (coerce: self 1))"
                       ,
                       "        (set x<i>   (/ (+ x<i-1> (/ self x<i-1>)) two))"
                       , "        (while {(> (abs (- x<i-1> x<i>)) epsilon)}"
                       , "               {(set x<i-1> x<i>)"
                       ,
               "                (set x<i> (/ (+ x<i-1> (/ self x<i-1>)) two))})"
                       , "        x<i>)"
                       , ")"
                       , "(class Integer Number"
                       , "    [] ; abstract class"
                       , "    (method div: (n) (subclassResponsibility self))"
                       , "    (method mod: (n) (- self (* n (div: self n))))"
                       ,
"    (method gcd: (n) (if (= n (coerce: self 0)) {self} {(gcd: n (mod: self n))}))"
                       , "    (method lcm: (n) (* self (div: n (gcd: self n))))"
                       ,
                        "    (method asFraction () (num:den:  Fraction self 1))"
                       ,
                        "    (method asFloat    () (mant:exp: Float    self 0))"
                       , "    (method asInteger  () self)"
                       , "    (method coerce: (aNumber) (asInteger aNumber))"
                       ,
                        "    (method reciprocal () (num:den: Fraction 1 self)) "
                       ,
                        "    (method / (aNumber) (/ (asFraction self) aNumber))"
                       , "    (method timesRepeat: (aBlock) [locals count]"
                       ,
      "        (ifTrue: (negative self) {(error: self 'negative-repeat-count)})"
                       , "        (set count self)"
                       , "        (while {(!= count 0)}"
                       , "             {(value aBlock)"
                       , "              (set count (- count 1))}))"
                       , ")"
                       , "(class SmallInteger Integer"
                       , "    [] ; primitive representation"
                       , "    (class-method new: primitive newSmallInteger:)"
                       , "    (class-method new  () (new: self 0))"
                       , "    (method negated    () (- 0 self))"
                       , "    (method print  primitive printSmallInteger)"
                       , "    (method printu primitive printu)"
                       , "    (method +      primitive +)"
                       , "    (method -      primitive -)"
                       , "    (method *      primitive *)"
                       , "    (method div:   primitive div)"
                       , "    (method =      primitive eqObject)"
                       , "    (method <      primitive <)"
                       , "    (method >      primitive >)"
                       , ")"
                       , "(class Fraction Number"
                       , "    [num den]"
                       ,
               "    (class-method num:den: (a b) (initNum:den: (new self) a b))"
                       , "    (method initNum:den: (a b) ; private"
                       , "        (setNum:den: self a b)"
                       , "        (signReduce self)"
                       , "        (divReduce self))"
                       ,
         "    (method setNum:den: (a b) (set num a) (set den b) self) ; private"
                       , "    (method signReduce () ; private"
                       , "        (ifTrue: (negative den)"
                       ,
                "            {(set num (negated num)) (set den (negated den))})"
                       , "        self)"
                       , ""
                       , "    (method divReduce () [locals temp] ; private"
                       , "        (if (= num 0)"
                       , "            {(set den 1)}"
                       , "            {(set temp (gcd: (abs num) den))"
                       , "             (set num  (div: num temp))"
                       , "             (set den  (div: den temp))})"
                       , "        self)"
                       , "    (method num () num)  ; private"
                       , "    (method den () den)  ; private"
                       , "    (method = (f)"
                       , "        (and: (= num (num f)) {(= den (den f))}))"
                       , "    (method < (f)"
                       , "        (< (* num (den f)) (* (num f) den)))"
                       ,
        "    (method negated () (setNum:den: (new Fraction) (negated num) den))"
                       , "    (method * (f)"
                       , "        (divReduce"
                       , "            (setNum:den: (new Fraction)"
                       , "                            (* num (num f))"
                       , "                            (* den (den f)))))"
                       , "    (method + (f) [locals temp]"
                       , "        (set temp (lcm: den (den f)))"
                       , "        (divReduce"
                       , "            (setNum:den: (new Fraction)"
                       ,
"                         (+ (* num (div: temp den)) (* (num f) (div: temp (den f))))"
                       , "                         temp)))"
                       , "    (method reciprocal ()"
                       ,
                     "       (signReduce (setNum:den: (new Fraction) den num)))"
                       ,
                 "    (method print () (print num) (print '/) (print den) self)"
                       , "    (method asInteger  () (div: num den))"
                       ,
                    "    (method asFloat    () (/ (asFloat num) (asFloat den)))"
                       , "    (method asFraction () self)"
                       , "    (method coerce: (aNumber) (asFraction aNumber))"
                       , "    (method negative         () (negative num))"
                       , "    (method nonnegative      () (nonnegative num))"
                       ,
                       "    (method strictlyPositive () (strictlyPositive num))"
                       , ")"
                       , "(class Float Number"
                       , "    [mant exp]"
                       ,
             "    (class-method mant:exp: (m e) (initMant:exp: (new self) m e))"
                       , "    (method initMant:exp: (m e) ; private"
                       , "        (set mant m) (set exp e) (normalize self))"
                       , "    (method normalize ()    ; private"
                       , "        (while {(> (abs mant) 32767)}"
                       , "               {(set mant (div: mant 10))"
                       , "                (set exp (+ exp 1))})"
                       , "        self)"
                       , "    (method mant () mant)  ; private"
                       , "    (method exp  () exp)   ; private"
                       , "    (method < (x) (negative (- self x)))"
                       , "    (method = (x) (isZero   (- self x)))"
                       , "    (method isZero () (= mant 0))  ; private"
                       ,
                  "    (method negated () (mant:exp: Float (negated mant) exp))"
                       , "    (method + (prime) "
                       , "        (if (>= exp (exp prime))"
                       ,
"            {(mant:exp: Float (+ (* mant (raisedToInteger: 10 (- exp (exp prime))))"
                       , "                                 (mant prime))"
                       , "                              (exp prime))}"
                       , "            {(+ prime self)}))"
                       , "    (method * (prime) "
                       ,
          "        (mant:exp: Float (* mant (mant prime)) (+ exp (exp prime))))"
                       , "    (method reciprocal ()"
                       ,
                  "        (mant:exp: Float (div: 1000000000 mant) (- -9 exp)))"
                       , "    (method coerce: (aNumber) (asFloat aNumber))"
                       , "    (method asFloat () self)"
                       , "    (method asInteger ()"
                       , "        (if (negative exp)"
                       ,
                 "            {(div: mant (raisedToInteger: 10 (negated exp)))}"
                       , "            {(*    mant (raisedToInteger: 10 exp))}))"
                       , "    (method asFraction ()"
                       , "        (if (< exp 0)"
                       ,
    "            {(num:den: Fraction mant (raisedToInteger: 10 (negated exp)))}"
                       ,
      "            {(num:den: Fraction (* mant (raisedToInteger: 10 exp)) 1)}))"
                       , "    (method negative         () (negative mant))"
                       , "    (method nonnegative      () (nonnegative mant))"
                       ,
                      "    (method strictlyPositive () (strictlyPositive mant))"
                       , "    (method print () "
                       , "        (print-normalize self) "
                       , "        (print mant) (print 'x10^) (print exp)"
                       , "        (normalize self))"
                       , ""
                       , "    (method print-normalize ()"
                       ,
                      "        (while {(and: (< exp 0) {(= (mod: mant 10) 0)})}"
                       ,
                 "            {(set exp (+ exp 1)) (set mant (div: mant 10))}))"
                       , ")"
                       , "(class Char Object"
                       , "   [code-point]"
                       , "   (class-method new: (n) (init: (new self) n))"
                       ,
                 "   (method init:      (n) (set code-point n) self) ;; private"
                       , "   (method print      ()  (printu code-point))"
                       ,
                      "   (method =          (c) (= code-point (code-point c)))"
                       , "   (method code-point ()  code-point) ;; private"
                       , ")"
                       ,
        "(val newline      (new: Char 10))   (val left-paren   (new: Char  40))"
                       ,
        "(val space        (new: Char 32))   (val right-paren  (new: Char  41))"
                       ,
        "(val semicolon    (new: Char 59))   (val left-curly   (new: Char 123))"
                       ,
        "(val quote        (new: Char 39))   (val right-curly  (new: Char 125))"
                       ,
        "                                    (val left-square  (new: Char  91))"
                       ,
        "                                    (val right-square (new: Char  93))"
                       , "(class Collection Object"
                       , "  [] ; abstract"
                       ,
               "  (method do:     (aBlock)       (subclassResponsibility self))"
                       ,
               "  (method add:    (newObject)    (subclassResponsibility self))"
                       , "  (method remove:ifAbsent: (oldObject exnBlock)"
                       ,
               "                                 (subclassResponsibility self))"
                       ,
               "  (method similar: (aCollection) (subclassResponsibility self))"
                       ,
               "  (method species ()             (subclassResponsibility self))"
                       , "  (class-method with: (anObject) [locals temp]"
                       , "      (add: (new self) anObject))"
                       , "  (method remove: (oldObject) "
                       ,
"      (remove:ifAbsent: self oldObject {(error: self 'tried-to-remove-absent-object)}))"
                       , "  (method addAll: (aCollection) "
                       , "      (do: aCollection [block (x) (add: self x)])"
                       , "      self)"
                       , "  (method removeAll: (aCollection) "
                       , "      (do: aCollection [block (x) (remove: self x)])"
                       , "      self)"
                       , "  (method isEmpty () (= (size self) 0))"
                       , "  (method size () [locals temp]"
                       , "      (set temp 0)"
                       , "      (do: self [block (_) (set temp (+ temp 1))])"
                       , "      temp)"
                       , "  (method occurrencesOf: (anObject) [locals temp]"
                       , "      (set temp 0)"
                       , "      (do: self [block (x)"
                       ,
                   "         (ifTrue: (= x anObject) {(set temp (+ temp 1))})])"
                       , "      temp)"
                       ,
          "  (method includes: (anObject) (< 0 (occurrencesOf: self anObject)))"
                       , "  (method detect: (aBlock)"
                       ,
       "      (detect:ifNone: self aBlock {(error: self 'no-object-detected)}))"
                       ,
          "  (method detect:ifNone: (aBlock exnBlock) [locals answer searching]"
                       , "      (set searching true)"
                       , "      (do: self [block (x)"
                       ,
                        "          (ifTrue: (and: searching {(value aBlock x)})"
                       , "               {(set searching false)"
                       , "                (set answer x)})])"
                       , "      (ifTrue:ifFalse: searching exnBlock {answer}))"
                       , "  (method inject:into: (thisValue binaryBlock)"
                       ,
   "     (do: self [block (x) (set thisValue (value binaryBlock x thisValue))])"
                       , "     thisValue)"
                       , "  (method select: (aBlock) [locals temp]"
                       , "     (set temp (new (species self)))"
                       ,
        "     (do: self [block (x) (ifTrue: (value aBlock x) {(add: temp x)})])"
                       , "     temp)"
                       , "  (method reject: (aBlock)"
                       ,
                       "     (select: self [block (x) (not (value aBlock x))]))"
                       , "  (method collect: (aBlock) [locals temp]"
                       , "     (set temp (new (species self)))"
                       ,
                      "     (do: self [block (x) (add: temp (value aBlock x))])"
                       , "     temp)"
                       , "  (method asSet () [locals temp]"
                       , "       (set temp (new Set))"
                       , "       (do: self [block (x) (add: temp x)])"
                       , "       temp)"
                       , "  (method print ()"
                       , "      (printName self)"
                       , "      (print left-paren)"
                       , "      (do: self [block (x) (print space) (print x)])"
                       , "      (print space)"
                       , "      (print right-paren)"
                       , "      self)"
                       , "  (method printName () (print 'Collection))"
                       , ")"
                       , "(class Set Collection"
                       ,
                     "    [members]  ; list of elements [invariant: no repeats]"
                       , "    (class-method new () (initSet (new super)))"
                       ,
             "    (method initSet   () (set members (new List)) self) ; private"
                       , "    (method do: (aBlock) (do: members aBlock))"
                       , "    (method add: (item)"
                       ,
             "        (ifFalse: (includes: members item) {(add: members item)})"
                       , "        self)"
                       , "    (method remove:ifAbsent: (item exnBlock) "
                       , "        (remove:ifAbsent: members item exnBlock)"
                       , "        self)"
                       , "    (method similar:  (s) [locals looks-similar]"
                       , "       (set looks-similar (= (size self) (size s)))"
                       , "       (ifTrue: looks-similar"
                       ,
                    "           {(do: self [block (x) (ifFalse: (includes: s x)"
                       ,
   "                                           {(set looks-similar false)})])})"
                       , "       looks-similar)"
                       , "    (method species   () Set)"
                       , "    (method printName () (print 'Set))"
                       , "    (method asSet     () self)"
                       , ")"
                       , "(class KeyedCollection Collection"
                       , "    []  ; abstract class"
                       ,
 "    (method at:put: (key value)                (subclassResponsibility self))"
                       ,
 "    (method associationsDo: (aBlock)           (subclassResponsibility self))"
                       ,
 "    (method removeKey:ifAbsent: (key exnBlock) (subclassResponsibility self))"
                       , "    (method do: (aBlock) "
                       ,
"        (associationsDo: self [block (anAssoc) (value aBlock (value anAssoc))]))"
                       , "    (method at: (key)    "
                       ,
               "        (at:ifAbsent: self key {(error: self 'key-not-found)}))"
                       , "    (method at:ifAbsent: (key exnBlock) "
                       , "        (value (associationAt:ifAbsent: self key "
                       ,
         "                   {(key:value: Association nil (value exnBlock))})))"
                       , "    (method includesKey: (key) "
                       ,
        "        (isKindOf: (associationAt:ifAbsent: self key {}) Association))"
                       , "    (method associationAt: (key) "
                       ,
    "        (associationAt:ifAbsent: self key {(error: self 'key-not-found)}))"
                       ,
       "    (method associationAt:ifAbsent: (key exnBlock) [locals finishBlock]"
                       , "        (set finishBlock exnBlock)"
                       , "        (associationsDo: self [block (x) "
                       ,
               "            (ifTrue: (= (key x) key) {(set finishBlock {x})})])"
                       , "        (value finishBlock))"
                       , "    (method keyAtValue: (value) "
                       ,
   "        (keyAtValue:ifAbsent: self value {(error: self 'value-not-found)}))"
                       ,
          "    (method keyAtValue:ifAbsent: (value aBlock) [locals finishBlock]"
                       , "        (set finishBlock aBlock)"
                       , "        (associationsDo: self [block (x) "
                       ,
     "            (ifTrue: (= (value x) value) {(set finishBlock {(key x)})})])"
                       , "        (value finishBlock))"
                       , "    (method removeKey: (key)    "
                       ,
        "        (removeKey:ifAbsent: self key {(error: self 'key-not-found)}))"
                       ,
                      "    (method similar: (collection) [locals looks-similar]"
                       ,
                 "        (set looks-similar (= (size self) (size collection)))"
                       , "        (ifTrue: looks-similar"
                       , "            {(associationsDo: self"
                       ,
"                [block (assn) (ifFalse: (and: (includesKey: collection (key assn))"
                       ,
"                                              {(similar: (at: collection (key assn))"
                       ,
      "                                                         (value assn))})"
                       ,
      "                                        {(set looks-similar false)})])})"
                       , "         looks-similar)"
                       , ")"
                       , "(class Association Object "
                       , "   [key value]"
                       ,
             "   (class-method key:value: (x y) (setKey:value: (new self) x y))"
                       ,
      "   (method setKey:value: (x y) (set key x) (set value y) self) ; private"
                       , "   (method key    ()  key)"
                       , "   (method value  ()  value)"
                       , "   (method key:   (x) (set key   x))"
                       , "   (method value: (y) (set value y))"
                       ,
                "   (method =      (a) (& (= key (key a)) (= value (value a))))"
                       , ")"
                       , "(class Dictionary KeyedCollection"
                       , "    [table] ; list of Associations"
                       ,
                   "    (class-method new ()      (initDictionary (new super)))"
                       ,
          "    (method initDictionary () (set table (new List)) self) ; private"
                       , "    (method printName ()      (print 'Dictionary))"
                       , "    (method species ()        Dictionary)"
                       ,
                      "    (method associationsDo: (aBlock) (do: table aBlock))"
                       , "    (method at:put: (key value) [locals tempassoc]"
                       ,
                 "        (set tempassoc (associationAt:ifAbsent: self key {}))"
                       , "        (if (isNil tempassoc)"
                       ,
                "             {(add: table (key:value: Association key value))}"
                       , "             {(value: tempassoc value)})"
                       , "        self)"
                       , "    (method removeKey:ifAbsent: (key exnBlock)"
                       ,
"       [locals absent value-removed] ; flag to show absence, value found if not absent"
                       , "       (set absent false)"
                       ,
        "       (set value-removed (at:ifAbsent: self key {(set absent true)}))"
                       , "       (ifTrue:ifFalse: absent"
                       , "          {exnBlock}"
                       ,
"          {(set table (reject: table [block (assn) (= key (key assn))])) ; remove assoc"
                       , "           value-removed}))"
                       , "    (method remove:ifAbsent: (value exnBlock)"
                       ,
                "       (error: self 'Dictionary-uses-remove:key:-not-remove:))"
                       ,
          "    (method add: (_) (error: self 'can't-just-add:-to-a-Dictionary))"
                       , "    (method print () [locals print-comma]"
                       , "        (set print-comma false)"
                       , "        (printName self)"
                       , "        (print left-paren)"
                       , "        (associationsDo: self"
                       , "            [block (x) (print space)"
                       ,
       "                       (ifTrue: print-comma {(print #,) (print space)})"
                       , "                       (set print-comma true)"
                       ,
                        "                       (print (key x))   (print space)"
                       ,
                        "                       (print '|-->)     (print space)"
                       , "                       (print (value x))])"
                       , "        (print space)"
                       , "        (print right-paren)"
                       , "        self)"
                       , ")"
                       , "(class SequenceableCollection KeyedCollection"
                       , "    [] ; abstract class"
                       ,
                        "    (method firstKey () (subclassResponsibility self))"
                       ,
                        "    (method lastKey  () (subclassResponsibility self))"
                       , "    (method last     () (at: self (lastKey  self)))"
                       , "    (method first    () (at: self (firstKey self)))"
                       ,
        "    (method at:ifAbsent: (index exnBlock) [locals current resultBlock]"
                       , "        (set resultBlock exnBlock)"
                       , "        (set current (firstKey self))"
                       , "        (do: self [block (v)"
                       ,
               "            (ifTrue: (= current index) {(set resultBlock {v})})"
                       , "            (set current (+ current 1))])"
                       , "        (value resultBlock))"
                       ,
                       "    (method associationsDo: (bodyBlock) [locals i last]"
                       , "        (set i    (firstKey self))"
                       , "        (set last (lastKey self))"
                       , "        (whileTrue: {(<= i last)}"
                       ,
        "            {(value bodyBlock (key:value: Association i (at: self i)))"
                       , "             (set i (+ i 1))}))"
                       , ")"
                       , "(class Cons Object"
                       , "    [car cdr]"
                       , "    (method car ()           car)"
                       , "    (method cdr ()           cdr)"
                       , "    (method car: (anObject)  (set car anObject) self)"
                       , "    (method cdr: (anObject)  (set cdr anObject) self)"
                       , "    (method pred: (aCons)    nil)"
                       , "    (method deleteAfter () [locals answer]"
                       , "        (set answer (car cdr))"
                       , "        (set cdr    (cdr cdr))"
                       , "        (pred: cdr self)"
                       , "        answer)"
                       , "    (method insertAfter: (anObject)"
                       ,
                       "        (set cdr (car: (cdr: (new Cons) cdr) anObject))"
                       , "        (pred: (cdr cdr) cdr)"
                       , "        anObject)"
                       , "    (method do: (aBlock)"
                       , "        (value aBlock car)"
                       , "        (do: cdr aBlock))"
                       ,
               "    (method rejectOne:ifAbsent:withPred: (aBlock exnBlock pred)"
                       , "        (if (value aBlock self)"
                       , "            {(deleteAfter pred)}"
                       ,
       "            {(rejectOne:ifAbsent:withPred: cdr aBlock exnBlock self)}))"
                       , ")"
                       , "(class ListSentinel Cons"
                       , "    [pred]"
                       , "    (method pred: (aCons)   (set pred aCons))"
                       , "    (method pred  ()        pred)"
                       , "    (class-method new ()    [locals tmp]"
                       , "        (set tmp (new super))"
                       , "        (pred: tmp tmp)"
                       , "        (cdr:  tmp tmp)"
                       , "        tmp)"
                       , "    (method do: (aBlock) nil)"
                       ,
               "    (method rejectOne:ifAbsent:withPred: (aBlock exnBlock pred)"
                       , "        (value exnBlock)))"
                       , "(class List SequenceableCollection"
                       , "    [sentinel]"
                       ,
   "    (class-method new ()        (sentinel: (new super) (new ListSentinel)))"
                       ,
              "    (method sentinel: (s)       (set sentinel s) self) ; private"
                       ,
                  "    (method isEmpty   ()        (= sentinel (cdr sentinel)))"
                       ,
                        "    (method last      ()        (car (pred sentinel)))"
                       ,
                  "    (method do:       (aBlock)  (do: (cdr sentinel) aBlock))"
                       , "    (method species   ()        List)"
                       , "    (method printName ()        (print 'List))"
                       ,
      "    (method addLast:  (item)   (insertAfter: (pred sentinel) item) self)"
                       ,
      "    (method addFirst: (item)   (insertAfter: sentinel item)        self)"
                       , "    (method add:      (item)   (addLast: self item))"
                       ,
                        "    (method removeFirst ()     (deleteAfter sentinel))"
                       , "    (method remove:ifAbsent: (oldObject exnBlock)"
                       , "        (rejectOne:ifAbsent:withPred:"
                       , "            (cdr sentinel)"
                       , "            [block (x) (= oldObject (car x))]"
                       , "            exnBlock"
                       , "            sentinel))"
                       , "    (method removeKey:ifAbsent: (n exnBlock)"
                       ,
     "       (error: self 'removeKey:ifAbsent:-on-List-is-left-as-an-exercise))"
                       , "    (method firstKey () 0)"
                       , "    (method lastKey  () (- (size self) 1))"
                       , "    (method at:put: (n value) [locals tmp]"
                       , "        (set tmp (cdr sentinel))"
                       , "        (whileFalse: {(= n 0)}"
                       , "           {(set n (- n 1))"
                       , "            (set tmp (cdr tmp))})"
                       , "        (car: tmp value)"
                       , "        self)"
                       , ")"
                       , "(class Array SequenceableCollection"
                       , "    [] ; representation is primitive"
                       , "    (class-method new: primitive arrayNew:)"
                       ,
     "    (class-method new  () (error: self 'size-of-Array-must-be-specified))"
                       , "    (method size       primitive arraySize)"
                       , "    (method at:        primitive arrayAt:)"
                       ,
                   "    (method at:update: primitive arrayAt:update:) ; private"
                       ,
          "    (method at:put:    (key value) (at:update: self key value) self)"
                       , "    (method species    () Array)"
                       ,
               "    (method printName  () nil) ; names of arrays aren't printed"
                       ,
                  "    (method add:                (x)   (fixedSizeError self))"
                       ,
                  "    (method remove:ifAbsent:    (x b) (fixedSizeError self))"
                       ,
                  "    (method removeKey:ifAbsent: (x b) (fixedSizeError self))"
                       ,
  "    (method fixedSizeError      ()    (error: self 'arrays-have-fixed-size))"
                       ,
     "    (method select:  (_) (error: self 'select-on-arrays-not-implemented))"
                       ,
    "    (method collect: (_) (error: self 'collect-on-arrays-not-implemented))"
                       , "    (method firstKey () 0)"
                       , "    (method lastKey  () (- (size self) 1))"
                       , "    (method do: (aBlock) [locals index]"
                       , "        (set index (firstKey self))"
                       , "        (timesRepeat: (size self)"
                       , "           {(value aBlock (at: self index))"
                       , "            (set index (+ index 1))}))"
                       , ")"
                        ])
      fun errmsg s = eprintlnTrace ("error in predefined class: " ^ s)
  in  readEvalPrintWith errmsg (xdefs, initialXi, noninteractive)
  end
(* implementations of \usmalltalk\ primitives and definition of [[initialBasis]] 919b *)
local 
  fun newInstance classname = SEND (nullsrc, "new", VAR classname, [])
in
  val initialXi = processPredefined (VAL ("true",  newInstance "True" ),
                                                                      initialXi)
  val initialXi = processPredefined (VAL ("false", newInstance "False"),
                                                                      initialXi)
end
(* implementations of \usmalltalk\ primitives and definition of [[initialBasis]] 919c *)
val _ =
  ( closeLiteralsCycle initialXi
  ; closeBooleansCycle initialXi
  ; closeBlocksCycle   initialXi
  ) handle NotFound n =>
      ( app eprint ["Fatal error: ", n, " is not predefined\n"]
      ; raise InternalError "this can't happen"
      )
  | e => ( eprintln "Error binding predefined classes into interpreter"; raise e
                                                                               )
(* implementations of \usmalltalk\ primitives and definition of [[initialBasis]] 919e *)
val initialXi = processPredefined (VAL ("nil", VALUE nilValue), initialXi)
val initialBasis = initialXi


(*****************************************************************)
(*                                                               *)
(*   FUNCTION [[RUNAS]] FOR \USMALLTALK, WHICH PRINTS STACK TRACES *)
(*                                                               *)
(*****************************************************************)

(* function [[runAs]] for \usmalltalk, which prints stack traces 1370c *)
fun runAs interactivity = 
  let val _ = setup_error_format interactivity
      val prompts = if prompts interactivity then stdPrompts else noPrompts
      val xdefs = filexdefs ("standard input", TextIO.stdIn, prompts)
  in  ignore (readEvalPrintWith eprintlnTrace (xdefs, initialBasis,
                                                                 interactivity))
  end 
(* type declarations for consistency checking *)
val _ = op runAs : interactivity -> unit
fun dump_global_names () = app (println o fst) initialBasis  (*OMIT*)


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
