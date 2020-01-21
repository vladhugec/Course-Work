(* mcl.sml 773b *)
exception Unimp of string
fun unimp s = raise Unimp s


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
(* support for names and environments 589b *)
fun extend (rho, bindings) =
  foldr (fn ((x, a), rho) => bind (x, a, rho)) rho bindings
(* type declarations for consistency checking *)
val _ = op extend : 'a env * 'a env -> 'a env
(* support for names and environments 591c *)
exception DisjointUnionFailed of name
fun disjointUnion envs =
  let val env = List.concat envs
  in  case duplicatename (map fst env)
        of NONE => env
         | SOME x => raise DisjointUnionFailed x
  end
(* type declarations for consistency checking *)
val _ = op disjointUnion : 'a env list -> 'a env
(* support for names and environments 1315d *)
fun isbound (x, E) = (find (x, E); true) handle NotFound _ => false
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
(*   ABSTRACT SYNTAX AND VALUES FOR \MCL                         *)
(*                                                               *)
(*****************************************************************)

(* abstract syntax and values for \mcl 753c *)
(* paths for \mcl 741a *)
type modcon = { printName : name, serial : int }
datatype modident = MODCON of modcon | MODTYPLACEHOLDER of name

local
  val timesDefined : int env ref = ref emptyEnv
     (* how many times each modident is defined *)
in
  fun genmodident name =
    let val n = find (name, !timesDefined) handle NotFound _ => 0
val n = 0 (* XXX fix this later *)
        val _ = timesDefined := bind (name, n + 1, !timesDefined)
    in  MODCON { printName = name, serial = n }
    end
end

(* type declarations for consistency checking *)
val _ = op genmodident : name -> modident

datatype 'modname path' = PNAME of 'modname
                        | PDOT of 'modname path' * name
                        | PAPPLY of 'modname path' * 'modname path' list

type pathex = name located path'
type path   = modident path'

fun plast (PDOT (_, x)) = x
  | plast (PNAME (_, x)) = x
  | plast (PAPPLY _) = "??last??"

(* definition of [[ty]] for \mcl 741b *)
datatype 'modname ty' = TYNAME of 'modname path'
                      | FUNTY  of 'modname ty' list * 'modname ty'
                      | ANYTYPE   (* type of (error ...) *)
type tyex = name located ty'
type ty   = modident ty'
(* definition of [[modty]] for \mcl 742b *)
datatype modty
  = MTEXPORTS of (name * declarable) list
  | MTARROW   of (modident * modty) list * modty
  | MTALLOF   of modty list
and declarable
  = DECVAL    of ty
  | DECOVLN   of ty list  (* overloaded name *)
  | DECMANTY  of ty
  | DECABSTY  of path
  | DECMOD    of path * modty
  | DECMODTY  of modty  (* only at top level *)


datatype declarablex
  = DECVALX    of tyex
  | DECABSTYX
  | DECMANTYX  of tyex
  | DECMODX    of modtyx
  | DECMODTYX  of modtyx  (* only at top level *)
and modtyx
  = MTNAMEDX   of name
  | MTEXPORTSX of (name * declarablex) located list
  | MTALLOFX   of modtyx located list
  | MTARROWX   of (name located * modtyx located) list * modtyx located
(* definition of [[pat]], for patterns 585b *)
type vcon = name   (* a value constructor *)
datatype pat = WILDCARD
             | PVAR     of name
             | CONPAT   of vcon * pat list
(* definitions of [[exp]] and [[value]] for \mcl 752b *)
type overloading = int ref
type formal = name * tyex
datatype exp 
  = LITERAL    of value
  | VAR        of pathex
  | VCONX      of vcon
  | CASE       of exp * (pat * exp) list   (* XXX pat needs to hold a path *)
  | IFX        of exp * exp * exp (* could be syntactic sugar for CASE *)
  | SET        of name * exp
  | WHILEX     of exp * exp
  | BEGIN      of exp list
  | APPLY      of exp * exp list * overloading
  | LETX       of let_kind * (name * exp) list * exp
  | LETRECX    of ((name * tyex) * exp) list * exp
  | LAMBDA     of formal list * exp
  | MODEXP     of (name * exp) list    (* from body of a generic module *)
  | ERRORX     of exp list
  | EXP_AT     of srcloc * exp
and let_kind = LET | LETSTAR
(* definitions of [[exp]] and [[value]] for \mcl 753a *)
and value
  = CONVAL of vcon * value ref list
  | SYM  of name
  | NUM  of int
  | MODVAL of value ref env
  | CLOSURE   of lambda * value ref env
  | PRIMITIVE of primop
  | ARRAY     of value array
 withtype lambda = name list * exp
      and primop = value list -> value
(* type declarations for consistency checking *)
type value = value
val unitVal = SYM "unit"  (* XXX placeholder *)
(* definition of [[def]] for \mcl 752c *)
type modtyex = modtyx
datatype baredef  = VAL    of name * exp
              | VALREC of name * tyex * exp
              | EXP    of exp
                                                           (* not in a module *)
              | QNAME  of pathex
                                                           (* not in a module *)
              | DEFINE of name * tyex * (formal list * exp)
              | TYPE   of name * tyex
              | DATA   of data_def
              | MODULE of name * moddef
              | GMODULE  of name * (name * modtyex) list * moddef
              | MODULETYPE of name * modtyex
                                                           (* not in a module *)
              | OVERLOAD of pathex list
and moddef = MPATH       of pathex
           | MPATHSEALED of modtyex * pathex
           | MSEALED     of modtyex * def list
           | MUNSEALED   of def list
  withtype data_def = name * (vcon * tyex) list
       and def = baredef located
(* type declarations for consistency checking *)
type exp = exp
(* type declarations for consistency checking *)
type def = def
type data_def = data_def
(*<definition of [[implicit_data_def]] for \mcl>*)
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
val BugInTypeInference = BugInTypeChecking (* to make \uml utils work *)
(* definition of [[valueString]] for \mcl 1327b *)
fun valueString (CONVAL ("cons", [ref v, ref vs])) = consString (v, vs)
  | valueString (CONVAL ("'()",  []))      = "()"
  | valueString (CONVAL (c, []))  = c
  | valueString (CONVAL (c, vs))  =
      "(" ^ c ^ " " ^ spaceSep (map (valueString o !) vs) ^ ")"
  | valueString (NUM n      )   = String.map (fn #"~" => #"-" | c => c) (
                                                                 Int.toString n)
  | valueString (SYM v      )   = v
  | valueString (CLOSURE   _)   = "<procedure>"
  | valueString (PRIMITIVE _)   = "<procedure>"
  | valueString (MODVAL _)      = "<module>"
  | valueString (ARRAY a)       =
      "[" ^ spaceSep (map valueString (Array.foldr op :: [] a)) ^ "]"
(* definition of [[valueString]] for \mcl 1327c *)
and consString (v, vs) =
      let fun tail (CONVAL ("cons", [ref v, ref vs])) = " " ^ valueString v ^
                                                                         tail vs
            | tail (CONVAL ("'()", []))       = ")"
            | tail _ =
                raise BugInTypeChecking
                  "bad list constructor (or cons/'() redefined)"
      in  "(" ^ valueString v ^ tail vs
	  end
(* definition of [[patString]] for \uml\ and \uhaskell 1294e *)
fun patString WILDCARD    = "_"
  | patString (PVAR x)    = x
  | patString (CONPAT (vcon, []))   = vcon
  | patString (CONPAT (vcon, pats)) = "(" ^ spaceSep (vcon :: map patString pats
                                                                         ) ^ ")"
(* definition of [[typeString]] for \mcl\ types 1357a *)
fun modidentString (MODCON { printName = m, serial = 0 }) = m
  | modidentString (MODCON { printName = m, serial = k }) = m ^ "@{" ^ intString
                                                                         k ^ "}"
  | modidentString (MODTYPLACEHOLDER s) = "<signature: " ^ s ^ ">"

fun pathString' base =
  let fun s (PNAME a) = base a
        | s (PDOT (p, x)) = s p ^ "." ^ x
        | s (PAPPLY (f, args)) =
            String.concat ("(@m " :: s f ::
                           foldr (fn (a, tail) => " " :: s a :: tail) [")"] args
                                                                               )
  in  s
  end

fun pathString (PNAME a) = modidentString a
  | pathString (PDOT (PNAME (MODTYPLACEHOLDER _), x)) = x
  | pathString (PDOT (p, x)) = pathString p ^ "." ^ x
  | pathString (PAPPLY (f, args)) =
      String.concat ("(@m " :: pathString f ::
                     foldr (fn (a, tail) => " " :: pathString a :: tail) [")"]
                                                                           args)

(*val pathString = pathString' modidentString*)
val pathexString : pathex -> string = pathString' snd
(* definition of [[typeString]] for \mcl\ types 1357b *)
fun typeString' ps (TYNAME p) = ps p
  | typeString' ps (FUNTY (args, res)) = 
      "(" ^ spaceSep (map (typeString' ps) args) ^ " -> " ^ (typeString' ps) res
                                                                           ^ ")"
  | typeString' ps ANYTYPE = "<any type>"

val typeString = typeString' pathString
val tyexString : tyex -> string = typeString' (pathString' snd)
(* definition of [[typeString]] for \mcl\ types 1357c *)
fun mtString (MTEXPORTS []) = "(exports)"
  | mtString (MTEXPORTS comps) = 
      "(exports " ^ spaceSep (map ncompString comps) ^ ")"
  | mtString (MTALLOF  mts) = "(allof " ^ spaceSep (map mtString mts) ^ ")"
  | mtString (MTARROW (args, res)) =
      "(" ^ spaceSep (map modformalString args) ^ " --m-> " ^ mtString res ^ ")"
and modformalString (m, t) = "[" ^ modidentString m ^ " : " ^ mtString t ^ "]"
and ncompString (x, c) =
  case c
    of DECVAL tau => "[" ^ x ^ " : " ^ typeString tau ^ "]"
     | DECABSTY _   => "(abstype " ^ x ^ ")"
     | DECMANTY tau => "(type " ^ x ^ " " ^ typeString tau ^ ")"
     | DECMOD (_, mt) => "(module [" ^ x ^ " : " ^ mtString mt ^ "])"
     | DECOVLN _ => "<overloaded name " ^ x ^ " ...>"
     | DECMODTY mt => "(module-type " ^ x ^ " " ^ mtString mt ^ ")"

(* definition of [[typeString]] for \mcl\ types 1358a *)
(*****
fun mtxString (MTNAMEDX _) = raise BugInTypeChecking "named module, elaborated"
  | mtxString (MTEXPORTSX []) = "(exports)"
  | mtxString (MTEXPORTSX lcomps) = 
      "(exports " ^ spaceSep (map ncompxString lcomps) ^ ")"
  | mtxString (MTALLOFX  mts) = "(allof " ^ spaceSep (map (mtxString o snd) mts)
                                                                           ^ ")"
  | mtxString (MTARROWX (args, res)) =
      "(" ^ spaceSep (map modformalString args) ^ " --m-> " ^ mtxString (snd res
                                                                         ) ^ ")"
and modformalString (m, t) = "[" ^ snd m ^ " : " ^ mtxString (snd t) ^ "]"
and ncompxString (x, (loc, c)) =
  case c
    of DECVALX tau => "[" ^ x ^ " : " ^ tyexString tau ^ "]"
     | DECABSTYX   => "(abstype " ^ x ^ ")"
     | DECMANTYX tau => "(type " ^ x ^ " " ^ typeString tau ^ ")"
     | DECMODX (_, mt) => "(module [" ^ x ^ " : " ^ mtxString mt ^ "])"
     | DECOVLX _ => "(overload-group " ^ x ^ " ...)"
     | DECMODTYX mt => "(module-type " ^ x ^ " " ^ mtxString mt ^ ")"
***)
(* definition of [[typeString]] for \mcl\ types 1358b *)
fun boolString b = if b then "#t" else "#f"
(* definition of [[expString]] for \mcl 1359 *)
fun stripExpAt (EXP_AT (_, e)) = stripExpAt e
  | stripExpAt e = e

fun expString e =
  let fun bracket s = "(" ^ s ^ ")"
      fun sqbracket s = "[" ^ s ^ "]"
      val bracketSpace = bracket o spaceSep
      fun exps es = map expString es
      fun withBindings (keyword, bs, e) =
        bracket (spaceSep [keyword, bindings bs, expString e])
      and bindings bs = bracket (spaceSep (map binding bs))
      and binding (x, e) = sqbracket (x ^ " " ^ expString e)
      fun formal (x, ty) = sqbracket (x ^ " : " ^ tyexString ty)
      fun tbindings bs = bracket (spaceSep (map tbinding bs))
      and tbinding ((x, tyex), e) = bracket (formal (x, tyex) ^ " " ^ expString
                                                                              e)
      val letkind = fn LET => "let" | LETSTAR => "let*"
  in  case e
        of LITERAL v => valueString v
         | VAR name => pathexString name
         | IFX (e1, e2, e3) => bracketSpace ("if" :: exps [e1, e2, e3])
         | SET (x, e) => bracketSpace ["set", x, expString e]
         | WHILEX (c, b) => bracketSpace ["while", expString c, expString b]
         | BEGIN es => bracketSpace ("begin" :: exps es)
         | APPLY (e, es, _) => bracketSpace (exps (e::es))
         | LETX (lk, bs, e) => bracketSpace [letkind lk, bindings bs, expString
                                                                              e]
         | LETRECX (bs, e) => bracketSpace ["letrec", tbindings bs, expString e]
         | LAMBDA (xs, body) => bracketSpace ("lambda" :: map formal xs @ [
                                                                expString body])
         | VCONX vcon => vcon
         | CASE (e, matches) =>
             let fun matchString (pat, e) = sqbracket (spaceSep [patString pat,
                                                                   expString e])
             in  bracketSpace ("case" :: expString e :: map matchString matches)
             end
         | MODEXP components => bracketSpace ("modexp" :: map binding components
                                                                               )
         | ERRORX es => bracketSpace ("error" :: exps es)
         | EXP_AT (_, e) => expString e
  end
(* utility functions on \uml\ values ((mcl)) 615b *)
fun primitiveEquality (v, v') =
  let fun noFun () = raise RuntimeError "compared functions for equality"
  in  case (v, v')
        of (NUM  n1,  NUM  n2)  => (n1 = n2)
         | (SYM  v1,  SYM  v2)  => (v1 = v2)
         | (CONVAL (vcon, vs), CONVAL (vcon', vs')) =>
             vcon = vcon' andalso ListPair.allEq primitiveEquality (map ! vs,
                                                                      map ! vs')
         | (CLOSURE   _, _) => noFun ()
         | (PRIMITIVE _, _) => noFun ()
         | (_, CLOSURE   _) => noFun ()
         | (_, PRIMITIVE _) => noFun ()
         | _ => raise BugInTypeInference
                        ("compared incompatible values " ^ valueString v ^
                                                                       " and " ^
                         valueString v' ^ " for equality")
  end
val testEqual = primitiveEquality
(* utility functions on \uml\ values ((mcl)) 615c *)
fun embedList []      = CONVAL ("'()", [])
  | embedList (v::vs) = CONVAL ("cons", [ref v, ref (embedList vs)])
(* utility functions on \uml\ values 616b *)
fun embedBool b = CONVAL (if b then "#t" else "#f", [])
fun bool (CONVAL ("#t", [])) = true
  | bool _                 = false
(* type declarations for consistency checking *)
val _ = op bool      : value -> bool
val _ = op embedBool : bool -> value


(*****************************************************************)
(*                                                               *)
(*   SUPPORT FOR OPERATOR OVERLOADING IN \MCL                    *)
(*                                                               *)
(*****************************************************************)

(* support for operator overloading in \mcl 773a *)
val notOverloadedIndex = ~1
val overloadTable = "overloaded operators"
                                         (* name cannot appear in source code *)
val emptyOverloadTable = Array.tabulate (10, fn _ => SYM
                                              "<empty entry in overload table>")
fun overloadCell rho =
  find (overloadTable, rho) handle NotFound _ => raise InternalError
                                                        "missing overload table"
fun overloadedAt (rho, i) =
  case overloadCell rho
    of ref (ARRAY a) => Array.sub (a, i)
     | _ => raise InternalError "representation of overload table"
local
  val next = ref 0
in
  fun nextOverloadedIndex () = !next before next := !next + 1
end

fun overloadedPut (i, v, rho) =
  let val cell = overloadCell rho
      val a  = case cell of ref (ARRAY a) => a | _ => raise InternalError
                                                         "rep of overload table"
      val a' = if i >= Array.length a then
                 let val n = 2 * Array.length a
                     val a' = Array.tabulate (n, fn j => if j < n then Array.sub
                                                                  (a, j) else v)
                     val _ = cell := ARRAY a'
                 in  a'
                 end
               else
                 a
  in  Array.update (a', i, v)
  end



(*****************************************************************)
(*                                                               *)
(*   LEXICAL ANALYSIS AND PARSING FOR \MCL, PROVIDING [[FILEXDEFS]] AND [[STRINGSXDEFS]] *)
(*                                                               *)
(*****************************************************************)

(* lexical analysis and parsing for \mcl, providing [[filexdefs]] and [[stringsxdefs]] 1341b *)
(* lexical analysis for {\mcl} 1341c *)
datatype pretoken = QUOTE
                  | INT      of int
                  | RESERVED of string
                  | DOTTED   of string * string list
                               (* name, possibly followed by dotted selection *)
                  | DOTNAMES of string list (* .x.y and so on *)
type token = pretoken plus_brackets
(* lexical analysis for {\mcl} 1341d *)
fun pretokenString (QUOTE)      = "'"
  | pretokenString (INT  n)     = intString n
  | pretokenString (DOTTED (s, ss))  = separate ("", ".") (s::ss)
  | pretokenString (DOTNAMES ss)= (concat o map (fn s => "." ^ s)) ss
  | pretokenString (RESERVED x) = x
val tokenString = plusBracketsString pretokenString
(* lexical analysis for {\mcl} 1342 *)
local
  val isDelim = fn c => isDelim c orelse c = #"."
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
  val reserved = 
    [ (* words reserved for \mcl\ types 1343b *)
      "->", ":"
    , (* words reserved for \mcl\ expressions 1345b *)
      "@m", "if", "&&", "||", "set", "let", "let*", "letrec", "case", "lambda",
      "val", "set", "while", "begin", "error",
      "when", "unless", "assert"
      (* , "assert" *)
    , (* words reserved for \mcl\ definitions 1347c *)
      ":", 
      "val", "define", "exports", "allof", "module-type", "module", "--m->",
      "generic-module", "unsealed-module", "type", "abstype", "data",
      "record-module",
      "use", "check-expect", "check-assert",
      "check-error", "check-type", "check-type-error",
      "overload"
    ]
  fun isReserved x = member x reserved
  datatype part = DOT | NONDELIMS of string
  val nondelims = (NONDELIMS o implode) <$> many1 (sat (not o isDelim) one)
  val dot       = DOT <$ eqx #"." one
  fun dottedNames things =
    let exception Can'tHappen
        fun preDot (ss', DOT :: things)    = postDot (ss', things)
          | preDot (ss', nil)              = OK (rev ss')
          | preDot (ss', NONDELIMS _ :: _) = raise Can'tHappen
        and postDot (ss', DOT :: _) = ERROR
                             "A qualified name may not contain consecutive dots"
          | postDot (ss', nil)      = ERROR
                                       "A qualified name may not end with a dot"
          | postDot (ss', NONDELIMS s :: things) =
              if isReserved s then
                ERROR ("reserved word '" ^ s ^ "' used in qualified name")
              else
                preDot (s :: ss', things)
    in  case things
          of NONDELIMS s :: things => preDot  ([], things) >>=+ curry DOTTED s
           | DOT         :: things => postDot ([], things) >>=+ DOTNAMES
           | [] => ERROR "Lexer is broken; report to nr@cs.tufts.edu"
    end

  fun reserve (token as DOTTED (s, [])) =
        if isReserved s then
          RESERVED s
        else
          token
    | reserve token = token

in
  val mclToken =
    whitespace *>
    bracketLexer (  QUOTE   <$  eqx #"'" one
                <|> INT     <$> intToken isDelim
                <|> reserve <$> (dottedNames <$>! many1 (nondelims <|> dot))
                <|> noneIfLineEnds
                 )
(* type declarations for consistency checking *)
val _ = op mclToken : token lexer
end
fun 'a parseAt at p = at <$> @@ p
(* parsers for \mcl\ tokens 1343a *)
type 'a parser = (token, 'a) polyparser
val pretoken  = (fn (PRETOKEN t)=> SOME t  | _ => NONE) <$>? token : pretoken
                                                                          parser
val quote     = (fn (QUOTE)     => SOME () | _ => NONE) <$>? pretoken
val int       = (fn (INT   n)   => SOME n  | _ => NONE) <$>? pretoken
val name  = (fn (DOTTED (x, []))   => SOME x  | _ => NONE) <$>? pretoken
val dotted  = (fn (DOTTED (x, xs))   => SOME (x, xs)  | _ => NONE) <$>? pretoken
val dotnames = (fn (DOTNAMES xs)  => SOME xs | _ => NONE) <$>? pretoken
val reserved = (fn RESERVED r => SOME r | _ => NONE) <$>? pretoken
val any_name = name

val arrow = eqx "->" reserved <|> eqx "--m->" reserved

val showErrorInput = (fn p => showErrorInput tokenString p)
val booltok = pzero  (* depressing *)
(* parsers for \uml\ value constructors and value variables 1282b *)
fun isVcon x =
  let val first = String.sub (x, 0)
  in  x = "cons" orelse x = "'()" orelse
      Char.isUpper first orelse first = #"#" orelse
      String.isPrefix "make-" x
  end
fun isVvar x = x <> "->" andalso not (isVcon x)
(* parsers for \uml\ value constructors and value variables 1282c *)
val arrow = sat (fn n => n = "->") any_name
val vvar  = sat isVvar any_name
val tyname = vvar
val vcon  = 
  let fun isEmptyList (left, right) = notCurly left andalso snd left = snd right
      val boolcon  = (fn p => if p then "#t" else "#f") <$> booltok
  in  boolcon <|> sat isVcon any_name <|>
      "'()" <$ quote <* sat isEmptyList (pair <$> left <*> right)
  end
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
val tyvar = sat (fn _ => false) name  (* must have a monomorphic type *)
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
(* parsers and [[xdef]] streams for \mcl 1343c *)
fun kw keyword = eqx keyword reserved
fun usageParsers ps = anyParser (map (usageParser kw) ps)
(* parsers and [[xdef]] streams for \mcl 1343d *)
fun getkeyword (usage:string) = (one *> one *> one) (lexLineWith mclToken usage)
(* parsers and [[xdef]] streams for \mcl 1343e *)
fun wrap  what = wrapAround tokenString what
fun wrap_ what p = p
(* parsers and [[xdef]] streams for \mcl 1343f *)
fun showParsed show p =
  let fun diagnose a = (eprintln ("parsed " ^ show a); a)
  in  diagnose <$> p
  end

fun showParsed_ show p = p
(* parsers and [[xdef]] streams for \mcl 1343g *)
fun bracketOrFail (_, p) =
  let fun matches (_, l) a (loc, r) =
        if l = r then OK a
        else errorAt (leftString l ^ " closed by " ^ rightString r) loc
  in  matches <$> left <*> p <*>! right
  end
(* parsers and [[xdef]] streams for \mcl 1344a *)

fun addDots p xs = foldl (fn (x, p) => PDOT (p, x)) p xs
fun dotsPath (loc, (x, xs)) = addDots (PNAME (loc, x)) xs
fun path tokens =
  ( dotsPath <$> @@ dotted
  <|>
      addDots <$>
        bracketKeyword
           (kw "@m", "(@m name path ...)", curry PAPPLY <$> (PNAME <$> @@ name)
                                                                  <*> many path)
              <*> (dotnames <|> pure [])
  ) tokens

fun mkTyex br tokens =
  let val ty = wrap_ "inner type" (showErrorInput (mkTyex br))
      fun arrows []              [] = ERROR "empty type ()"
        | arrows (tycon::tyargs) [] = ERROR "missing @@ or ->"
        | arrows args            [rhs] =
            (case rhs of [result] => OK (FUNTY (args, result))
                       | []       => ERROR "no result type after function arrow"
                       | _        => ERROR
                                   "multiple result types after function arrow")
        | arrows args (_::_::_) = ERROR "multiple arrows in function type"
      val parser =
            TYNAME <$> path
        <|> br
               ( "(ty ty ... -> ty)"
               ,  arrows <$> many ty <*>! many (kw "->" *> many ty)
               )
  in  parser (* curry TYEX_AT () <$> @@ parser *)
  end tokens
val tyex = wrap_ "tyex" (mkTyex (showErrorInput o bracket)) : tyex parser
val liberalTyex = mkTyex bracketOrFail
(* parsers and [[xdef]] streams for \mcl 1344b *)
fun pattern tokens =  (
                WILDCARD    <$  eqx "_" vvar
      <|>       PVAR        <$> vvar
      <|> curry CONPAT      <$> vcon <*> pure []
      <|> bracket ( "(C x1 x2 ...) in pattern"
                  , curry CONPAT <$> vcon <*> many pattern
                  )
       ) tokens
(* parsers and [[xdef]] streams for \mcl 1345a *)
fun badReserved r = 
  ERROR ("reserved word '" ^ r ^ "' where name was expected")

fun quoteName "#f" = CONVAL ("#f", [])
  | quoteName "#t" = CONVAL ("#t", [])
  | quoteName s    = SYM s

fun quotelit tokens = (
         quoteName <$> name
    <|>  NUM <$> int
    <|>  (ARRAY o Array.fromList) <$> bracket ("(literal ...)", many quotelit)
    ) tokens

val atomicExp =  VAR           <$> path
             <|> badReserved <$>! reserved
             <|> dotnames <!> "a qualified name may not begin with a dot"
             <|> LITERAL <$> NUM <$> int
             <|> (VAR o PNAME) <$> @@ vcon
             <|> quote *> (LITERAL <$> quotelit)

fun bindTo exp = bracket ("[x e]", pair <$> name <*> exp)
(* parsers and [[xdef]] streams for \mcl 1346 *)
val formal = bracket ("[x : ty]", pair <$> name <* kw ":" <*> tyex)
val lformals = bracket ("([x : ty] ...)", many formal)
fun nodupsty what (loc, xts) = nodups what (loc, map fst xts) >>=+ (fn _ => xts)

                                                  (* error on duplicate names *)


fun smartBegin [e] = e
  | smartBegin es = BEGIN es

fun exptable exp =
  let val zero = LITERAL (NUM 0)
      fun single binding = [binding]
      fun badReserved words =
        let fun die w = ERROR ("while trying to parse an expression, I see " ^
                               "reserved word " ^ w ^

                            "... did you misspell a statement keyword earlier?")
        in  die <$>! sat (fn w => member w words) (left *> reserved)
        end
      val bindings = bindingsOf "[x e]" name exp
      val tbindings = bindingsOf "[x : ty]" formal exp
      val dbs       = distinctBsIn bindings

      val choice   = bracket ("(pattern exp)", pair <$> pattern <*> exp)
      val body = smartBegin <$> many1 exp
      val nothing = pure (BEGIN [])

      fun cand [e] = e
        | cand (e::es) = IFX (e, cand es, LITERAL (embedBool false))
        | cand [] = raise InternalError "parsing &&"

      fun cor [e] = e
        | cor (e::es) = IFX (e, LITERAL (embedBool true), cor es)
        | cor [] = raise InternalError "parsing ||"

     fun lambda (xs : (name * tyex) list located) exp =
       nodupsty ("formal parameter", "lambda") xs >>=+ (fn xs => LAMBDA (xs, exp
                                                                              ))

  in usageParsers
     [ ("(if e1 e2 e3)",            curry3 IFX          <$> exp <*> exp <*> exp)
     , ("(when e1 e ...)",          curry3 IFX          <$> exp <*> body <*>
                                                                        nothing)
     , ("(unless e1 e ...)",        curry3 IFX          <$> exp <*> nothing <*>
                                                                           body)
     , ("(set x e)",                curry  SET          <$> name <*> exp)
     , ("(while e body)",           curry  WHILEX       <$> exp  <*> body)
     , ("(begin e ...)",                   BEGIN        <$> many exp)
     , ("(error e ...)",                   ERRORX       <$> many exp)
     , ("(let (bindings) body)",    curry3 LETX LET     <$> dbs "let"    <*>
                                                                           body)
     , ("(let* (bindings) body)",   curry3 LETX LETSTAR <$> bindings     <*>
                                                                           body)
     , ("(letrec (typed-bindings) body)", curry LETRECX <$> tbindings <*> body)
     , ("(case exp (pattern exp) ...)", curry CASE <$> exp <*> many choice)
     , ("(lambda ([x : ty] ...) body)", lambda <$> @@ (lformals : (name * tyex)
                                                         list parser) <*>! body)
     , ("(&& e ...)",               cand <$> many1 exp)
     , ("(|| e ...)",               cor  <$> many1 exp)
     , ("(assert e)",
        curry3 IFX <$> exp <*> nothing <*> pure (ERRORX [LITERAL (SYM
                                                         "assertion-failure")]))
     , ("(quote sx)",               LITERAL <$> quotelit)
     ]
    <|> badReserved [(* words reserved for \mcl\ types 1343b *)
                     "->", ":",
                     (* words reserved for \mcl\ definitions 1347c *)
                     ":", 
                     "val", "define", "exports", "allof", "module-type",
                                                              "module", "--m->",
                     "generic-module", "unsealed-module", "type", "abstype",
                                                                         "data",
                     "record-module",
                     "use", "check-expect", "check-assert",
                     "check-error", "check-type", "check-type-error",
                     "overload"]
  end
(* parsers and [[xdef]] streams for \mcl 1347a *)
fun applyNode f args = APPLY (f, args, ref notOverloadedIndex)
fun exp tokens = showParsed_ expString (parseAt EXP_AT replExp) tokens
and replExp tokens = showErrorInput 
       (  (* component here only if type with reserved word *)
          atomicExp
      <|> exptable exp
      <|> leftCurly <!> "curly brackets are not supported"
      <|> left *> right <!> "empty application"
      <|> bracket("function application", applyNode <$> exp <*> many exp)
  ) tokens

(* type declarations for consistency checking *)
val _ = op exptable : exp parser -> exp parser
val _ = op exp      : exp parser

val replExp = showParsed_ expString (parseAt EXP_AT replExp)
(* parsers and [[xdef]] streams for \mcl 1347b *)
fun formalWith whatTy aTy =
  bracket ("[x : " ^ whatTy ^ "]", pair <$> name <* kw ":" <*> aTy)

val formal = formalWith "ty" tyex

fun prightmap f (x, a) = (x, f a)
fun crightmap f x a = (x, f a)

fun decl tokens =
  (  usageParsers
       [ ("(abstype t)",          pair <$> name <*> pure DECABSTYX)
       , ("(type t ty)",          crightmap DECMANTYX  <$> name <*> tyex)
       , ("(module [A : modty])", prightmap DECMODX <$> modformal)
       ]
 <|> prightmap DECVALX <$> formal
  )
  tokens
and locmodformal tokens =
  bracket ("[M : modty]", pair <$> @@ name <* kw ":" <*> @@ modtype) tokens
and modformal tokens =
  ((fn (x, t) => (snd x, snd t)) <$> locmodformal) tokens
and modtype tokens = (
  usageParsers
  [ ("(exports component...)", MTEXPORTSX <$> many (@@ decl))
  , ("(allof module-type...)",  MTALLOFX    <$> many (@@ modtype))
  ] 
  <|> MTNAMEDX <$> name
  <|> bracket ("([A : modty] ... --m-> modty)",
               curry MTARROWX <$> many locmodformal <*> kw "--m->" *> @@ modtype
                                                                               )
  ) tokens
(* type declarations for consistency checking *)
val _ = op decl : (name * declarablex) parser
val _ = op locmodformal : (name located * modtyex located) parser
val _ = op modformal    : (name * modtyex) parser
val _ = op modtype      : modtyex parser
(* parsers and [[xdef]] streams for \mcl 1348a *)
val tyex : tyex parser = tyex
(* parsers and [[xdef]] streams for \mcl 1348b *)
val vvar  = sat isVvar name
val vcon  = 
  let fun isEmptyList (left, right) = notCurly left andalso snd left = snd right
      val boolcon  = (fn p => if p then "#t" else "#f") <$> booltok
  in  boolcon <|> sat isVcon name <|>
      "'()" <$ quote <* sat isEmptyList (pair <$> left <*> right)
  end



(* parsers and [[xdef]] streams for \mcl 1350 *)
val defFwd    = ref (forward "def" : def parser)
fun def arg    = !defFwd arg

fun def tokens =
  let val returnTypes = bracket("[ty ...]", many tyex) <|> pure []
  in  showErrorInput (!defFwd)
  end tokens

val def = wrap_ "def" def : def parser

val defbasic : baredef parser = 
  let (* parser for binding to names *)
      val formals = lformals : (name * tyex) list parser
  (*    val formals = vvarFormalsIn "define" *)

      (* parsers for clausal definitions, a.k.a. define* *)
(*
      val lhs = bracket ("(f p1 p2 ...)", pair <$> vvar <*> many pattern)
      val clause =
        bracket ("[(f p1 p2 ...) e]",
                 (fn (f, ps) => fn e => (f, (ps, e))) <$> lhs <*> exp)
*)
      (* definition builders used in all parsers *)
      fun flipPair tx c = (c, tx)


      (* definition builders that expect to bind names *)
      fun define tau f formals body =
        nodupsty ("formal parameter", "definition of function " ^ f) formals
                                                                            >>=+
          (fn xts => DEFINE (f, tau, (xts, body)))
      fun definestar _ = ERROR "define* is left as an exercise"
      val tyname = name
        
      fun valrec (x, tau) e = VALREC (x, tau, e)

      fun sealedWith f (m : name, mt : modtyex) rhs = (m, f (mt, rhs))

      val conTy = typedFormalOf vcon (kw ":") tyex

      fun recordModule (loc, name) (formals : (name * tyex) list) =
        let val t = TYNAME (PNAME (loc, "t"))
            val vcon = "make-" ^ name ^ ".t"
            val conpat = CONPAT (vcon, map (PVAR o fst) formals)
            val unitty  = TYNAME (PDOT (PNAME (loc, "Unit"), "t"))
            val conty = FUNTY (map snd formals, t)
            val conname = name ^ ".make"
            fun getterty (x, tau) = (loc, (x, DECVALX (FUNTY ([t], tau))))
            fun setname x = "set-" ^ x ^ "!"
            fun setterty (x, tau) = (loc, (setname x, DECVALX(FUNTY ([t, tau],
                                                                      unitty))))
            fun var x = VAR (PNAME (loc, x))
            val conval =
              LAMBDA (formals, APPLY (VCONX vcon, map (var o fst) formals, ref
                                                            notOverloadedIndex))
            fun getter n =
              (LAMBDA ([("r", t)],
                       CASE (var "r", [(conpat, var (fst (List.nth (formals, n))
                                                                          ))])))
            fun setter n = 
              (LAMBDA ([("the record", t), ("the value", snd (List.nth(formals,
                                                                          n)))],
                       CASE (var "the record",
                             [(conpat, SET (fst (List.nth (formals, n)), var
                                                               "the value"))])))
            val exports = (loc, ("t", DECABSTYX)) :: (loc, ("make", DECVALX
                                                                      conty)) ::
                          map getterty formals @ map setterty formals
            val modty = MTEXPORTSX exports

            fun prim (x, f) = VAL (x, f)
            val indices = List.tabulate (length formals, id)
            val components =
              DATA ("t", [(vcon, FUNTY (map snd formals, t))]) ::
              prim ("make", conval) ::
              ListPair.mapEq (fn ((x,_), i) => prim (x, getter i)) (formals,
                                                                      indices) @
              ListPair.mapEq (fn ((x,_), i) => prim (setname x, setter i)) (
                                                               formals, indices)
        in  MODULE (name, MSEALED (modty, map (fn d => (loc, d)) components))
        end

      val body = smartBegin <$> many1 exp

  in  usageParsers
      [ ("(define type f (args) body)",
                                      define <$> tyex <*> name <*> @@ lformals
                                                                      <*>! body)
      , ("(val x e)",                 curry VAL <$> vvar <*> exp)
      , ("(val-rec [x : type] e)",    valrec <$> formal <*> exp)

      , ("(data t [vcon : ty] ...)",
         wrap_ "data definition" (curry DATA <$> tyname <*> many conTy))
      , ("(type t ty)",           curry TYPE <$> name <*> tyex)
      , ("(module-type T modty)", curry MODULETYPE <$> name <*> modtype)
      , ("(module M path) or (module [M : T] path/defs)",
            MODULE <$> (  (pair <$> name <*> MPATH <$> path : (name * moddef)
                                                                         parser)
                      <|> (sealedWith MPATHSEALED <$> modformal <*> path : (name
                                                               * moddef) parser)
                      <|> (sealedWith MSEALED <$> modformal <*> many def : (name
                                                               * moddef) parser)
                       ))

      , ("(generic-module [M : T] defs)",
            let fun strip ((_, m), (_, t)) = (m, t)
                fun gen ((loc, M), (loc', T)) defs =
                  case T
                    of MTARROWX (formals, result) =>
                         OK (GMODULE (M, map strip formals, MSEALED (snd result,
                                                                         defs)))
                     | _ => ERROR ("at " ^ srclocString loc' ^
                                                           ", generic module " ^
                                   M ^ " does not have an arrow type")
            in   gen <$> locmodformal <*>! many def
            end)
      , ("(unsealed-module M defs)", 
            MODULE <$> (crightmap MUNSEALED <$> name <*> many def))
      , ("(record-module M ([x : ty] ...))",
            recordModule <$> @@ name <*> formals)
      , ("(overload qname ...)", OVERLOAD <$> many path)
      ]
     <|> QNAME <$> path
     <|> EXP <$> exp : baredef parser
  end

val _ = defFwd := @@ defbasic
(* type declarations for consistency checking *)
val _ = op def : def parser

(* parsers and [[xdef]] streams for \mcl 1351a *)
val testtable = usageParsers
  [ ("(check-expect e1 e2)",          curry CHECK_EXPECT     <$> exp <*> exp)
  , ("(check-assert e)",                    CHECK_ASSERT     <$> exp)
  , ("(check-error e)",                     CHECK_ERROR      <$> exp)
  , ("(check-type e tau)",            curry CHECK_TYPE       <$> exp <*> tyex)
  , ("(check-type-error e)",                CHECK_TYPE_ERROR <$> def)
  ]
(* parsers and [[xdef]] streams for \mcl 1351b *)
fun filenameOfDotted (x, xs) = separate ("", ".") (x :: xs) 
val xdeftable = usageParsers
  [ ("(use filename)", (USE o filenameOfDotted) <$> dotted)
  ]
(* parsers and [[xdef]] streams for \mcl 1351c *)
val xdef =  TEST <$> testtable
        <|>          xdeftable
        <|> DEF <$>  def
        <|> badRight "unexpected right bracket"
        <?> "definition"
(* parsers and [[xdef]] streams for \mcl 1351d *)
val xdefstream = 
  interactiveParsedStream (mclToken, xdef)
(* shared definitions of [[filexdefs]] and [[stringsxdefs]] 1136c *)
fun filexdefs (filename, fd, prompts) = xdefstream (filename, filelines fd,
                                                                        prompts)
fun stringsxdefs (name, strings) = xdefstream (name, streamOfList strings,
                                                                      noPrompts)
(* type declarations for consistency checking *)
val _ = op xdefstream   : string * line stream     * prompts -> xdef stream
val _ = op filexdefs    : string * TextIO.instream * prompts -> xdef stream
val _ = op stringsxdefs : string * string list               -> xdef stream

(*<\mcl's overloaded operators>*)


(*****************************************************************)
(*                                                               *)
(*   ENVIRONMENTS FOR \MCL'S DEFINED NAMES                       *)
(*                                                               *)
(*****************************************************************)

(* environments for \mcl's defined names 1328a *)
(*
fun whatkind (COMPVAL _) = "a value"
  | whatkind (COMPTY _)  = "an ordinary type"
  | whatkind (COMPOVL _) = "an overloading group"
  | whatkind (COMPMOD _) = "a module"
*)

fun whatcomp (DECVAL _) = "a value"
  | whatcomp (DECABSTY _) = "an abstract type"
  | whatcomp (DECMANTY _) = "a manifest type"
  | whatcomp (DECOVLN _) = "an overloaded name"
  | whatcomp (DECMOD _) = "a module"
  | whatcomp (DECMODTY _) = "a module type"

fun compString (DECVAL tau) = "a value of type " ^ typeString tau
  | compString (DECABSTY path) = "abstract type " ^ pathString path
  | compString (DECMANTY tau) = "manifest type " ^ typeString tau
  | compString (DECOVLN _) = "an overloaded name"
  | compString (DECMOD (path, mt)) = "module " ^ pathString path ^ " of type " ^
                                                                     mtString mt
  | compString (DECMODTY _) = "a module type"



(*
fun findModty (t, Gamma) =
  case find (t, Gamma)
    of MODTY mt => mt
     | COMPONENT c =>
         raise TypeError ("Used " ^ t ^ " to name a module type, but " ^ t ^
                          " is " ^ whatkind c)
*)



(*****************************************************************)
(*                                                               *)
(*   TYPE CHECKING FOR {\MCL}                                    *)
(*                                                               *)
(*****************************************************************)

(* type checking for {\mcl} 772c *)
(* [[context]] for a {\mcl} definition 757 *)
datatype context
  = TOPLEVEL
  | INMODULE of path

fun contextDot (TOPLEVEL, name) = PNAME (genmodident name)
                                                     (* XXX key to uniqueness *)
  | contextDot (INMODULE path, name) = PDOT (path, name)

fun contextString TOPLEVEL = "at top level"
  | contextString (INMODULE p) = "in module " ^ pathString p
(* type declarations for consistency checking *)
type context = context
val _ = op contextDot : context * name -> path
(* type equality for \mcl 742a *)
fun eqType (TYNAME p, TYNAME p') = p = p'
  | eqType (FUNTY (args, res), FUNTY (args', res')) =
      eqTypes (args, args') andalso eqType (res, res')
  | eqType (ANYTYPE, _) = true
  | eqType (_, ANYTYPE) = true
  | eqType _ = false
and eqTypes (taus, tau's) = ListPair.allEq eqType (taus, tau's)
(* type declarations for consistency checking *)
val _ = op eqType  : ty      * ty      -> bool
val _ = op eqTypes : ty list * ty list -> bool
(* substitutions for \mcl 742c *)
type subst = (modident * path) list
val idsubst = []
(* type declarations for consistency checking *)
type subst = subst
val _ = op idsubst : subst
(* substitutions for \mcl 742d *)
infix 7 |-->
fun id |--> p = [(id, p)]
(* type declarations for consistency checking *)
val _ = op |--> : modident * path -> subst
(* substitutions for \mcl 743a *)
fun pathsubst theta =
  let fun subst (PNAME id) =
            (case List.find (fn (id', p') => id = id') theta
               of SOME (_, p) => p
                | NONE => PNAME id)
        | subst (PDOT (p, x)) = PDOT (subst p, x)
        | subst (PAPPLY (p, ps)) = PAPPLY (subst p, map subst ps)
  in  subst
  end
(* type declarations for consistency checking *)
val _ = op pathsubst : subst -> path -> path
(* substitutions for \mcl 743b *)
fun tysubst theta (TYNAME p)          = TYNAME (pathsubst theta p)
  | tysubst theta (FUNTY (args, res)) =
      FUNTY (map (tysubst theta) args, tysubst theta res)
  | tysubst theta ANYTYPE = ANYTYPE
(* type declarations for consistency checking *)
val _ = op tysubst : subst -> ty -> ty
(* substitutions for \mcl 743c *)
fun dom theta = map (fn (a, _) => a) theta
fun compose (theta2, theta1) =
  let val domain  = union (dom theta2, dom theta1)
      val replace = pathsubst theta2 o pathsubst theta1 o PNAME
  in  map (fn a => (a, replace a)) domain
  end
(* type declarations for consistency checking *)
val _ = op dom     : subst -> modident set
val _ = op compose : subst * subst -> subst
(* substitutions for \mcl 743d *)
fun bsubst s = 
  map (fn (x, a) => (x, s a))

fun mtsubst theta =
  let fun s (MTEXPORTS comps)     = MTEXPORTS (bsubst (declsubst theta) comps)
        | s (MTALLOF mts)         = MTALLOF (map s mts)
        | s (MTARROW (args, res)) = MTARROW (bsubst s args, s res)
  in  s
  end
and declsubst theta =
  let fun s (DECVAL t) = DECVAL (tysubst theta t)
        | s (DECABSTY path) = DECABSTY (pathsubst theta path)
        | s (DECMANTY t)  = DECMANTY (tysubst theta t)
        | s (DECOVLN taus) = DECOVLN (map (tysubst theta) taus)
              (* below, ??? *)
        | s (DECMOD (p, mt))  = DECMOD (pathsubst theta p, mtsubst theta mt)
        | s (DECMODTY _)  =
           raise BugInTypeChecking "can't happen: substitute into module type"
  in  s
  end
(* type declarations for consistency checking *)
val _ = op mtsubst   : subst -> modty      -> modty
val _ = op declsubst : subst -> declarable -> declarable
(* type components of module types 744a *)
fun abstractTypes (path, MTEXPORTS cs) =
      let fun mts (x, DECABSTY tau) = [(PDOT (path, x), tau)]
            | mts (x, DECMOD (p', mt)) = abstractTypes (p', mt)
            | mts _ = []
      in  (List.concat o map mts) cs
      end
  | abstractTypes (path, MTALLOF mts) =
      (List.concat o map (fn mt => abstractTypes (path, mt))) mts
  | abstractTypes (path, MTARROW _) = []
                                          (* could be bogus, cf Leroy rule 21 *)
(* module-type realization 746c *)
fun manifestTypes (path, MTEXPORTS cs) =
      let fun mts (x, DECMANTY tau) = [(PDOT (path, x), tau)]
            | mts (x, DECMOD (p', mt)) = manifestTypes (p', mt)
            | mts _ = []
      in  (List.concat o map mts) cs
      end
  | manifestTypes (path, MTALLOF mts) =
      (List.concat o map (fn mt => manifestTypes (path, mt))) mts
  | manifestTypes (path, MTARROW _) = []
                                          (* could be bogus, cf Leroy rule 21 *)
(* type declarations for consistency checking *)
val _ = op manifestTypes : path * modty -> (path * ty) list
(* module-type realization 746d *)
fun associatedWith (x, []) =
      NONE
  | associatedWith (x, (key, value) :: pairs) =
      if x = key then SOME value else associatedWith (x, pairs)

fun hasKey x [] = false
  | hasKey x ((key, value) :: pairs) = x = key orelse hasKey x pairs

fun realizeType mantypes =
  let fun r (TYNAME path) = getOpt (associatedWith (path, mantypes), TYNAME path
                                                                               )
        | r (FUNTY (args, res)) = FUNTY (map r args, r res)
        | r (ANYTYPE) = ANYTYPE
  in  r
  end
(* type declarations for consistency checking *)
val _ = op realizeType : (path * ty) list -> ty -> ty
(* module-type realization 747a *)
fun filterdec p (path, MTARROW f) = MTARROW f
  | filterdec p (path, MTALLOF mts) = MTALLOF (map (fn mt => filterdec p (path,
                                                                       mt)) mts)
  | filterdec p (path, MTEXPORTS xcs) =
      let fun cons ((x, c), xcs) =
            let val path = PDOT (path, x)
                val c = case c of DECMOD (path', mt) => DECMOD (path', filterdec
                                                                   p (path, mt))
                                | _ => c
            in  if p (path, c) then
                  (x, c) :: xcs
                else
                  xcs
            end
      in  MTEXPORTS (foldr cons [] xcs)
      end
(* module-type realization 747b *)
fun emptyExports (MTEXPORTS []) = true
  | emptyExports _ = false
(* module-type realization 747c *)
val joinMts : modty list -> modty = (* simple syntactic join *)
      let val path = PNAME (MODTYPLACEHOLDER "syntactic join")
          fun filterManifest (prev', []) = rev prev'
            | filterManifest (prev', mt :: mts) =
                let val manifests = manifestTypes (path, MTALLOF prev')
                    fun redundant (p, DECMANTY tau) =
                          (case associatedWith (p, manifests)
                             of SOME tau' => eqType (tau, tau')
                              | NONE => false)
                      | redundant _ = false
                in  filterManifest (filterdec (not o redundant) (path, mt) ::
                                                                     prev', mts)
                end
          val filterManifest = fn mts => filterManifest ([], mts)
          fun mtall [mt] = mt
            | mtall mts  = MTALLOF mts
      in  mtall o List.filter (not o emptyExports) o filterManifest
      end

fun simpleSyntacticJoin (MTALLOF mts) = joinMts mts
  | simpleSyntacticJoin mt = mt
(* module-type realization 747d *)
fun allWithManifest (path, mts) =
  let val mt = MTALLOF mts
      val mantypes = manifestTypes (path, mt)
      val abstypes = abstractTypes (path, mt)
  in  if List.exists (fn (p, _) => hasKey p mantypes) abstypes then
        simpleSyntacticJoin (realize (path, mantypes, mt))
      else
        mt
  end
(* module-type realization 748a *)
and realize (p, mantypes, mt) =
  let val newty = realizeType mantypes
      fun newmt (MTEXPORTS cs) = MTEXPORTS (map (fn (x, c) => (x, newdecl c)) cs
                                                                               )
        | newmt (MTALLOF mts)  = allWithManifest (p, map newmt mts)
        | newmt (MTARROW (args, result)) =
            MTARROW (map (fn (x, mt) => (x, newmt mt)) args, newmt result)
      and newdecl (DECVAL tau) = DECVAL (newty tau)
        | newdecl (DECABSTY p) =
           (case associatedWith (p, mantypes)
              of SOME tau => DECMANTY tau
               | NONE => DECABSTY p)   (* used to be this on every path *)
        | newdecl (DECMANTY tau) = DECMANTY (newty tau)
        | newdecl (DECMOD (path, mt)) = DECMOD (path, newmt mt)
        | newdecl (DECOVLN taus) = DECOVLN (map newty taus)
        | newdecl (DECMODTY _) = raise BugInTypeChecking
                                                     "module type inside module"
  in  newmt mt
  end
(* type declarations for consistency checking *)
val _ = op realize : path * (path * ty) list * modty -> modty
(* invariants of \mcl 744b *)
fun mixedManifestations mt =
  let val path = PNAME (MODTYPLACEHOLDER "invariant checking")
      val manifests = manifestTypes (path, mt)
      val abstracts = abstractTypes (path, mt)
  in  List.exists (fn (p, _) => hasKey p manifests) abstracts
  end
(* [[implements]] relation, based on [[subtype]] of two module types 745 *)
infix 1 >>
fun (OK ()) >> c = c
  | (ERROR msg) >> _ = ERROR msg

fun allE []      = OK ()
  | allE (e::es) = e >> allE es

fun subtype mts =
  let fun remove (x, c) exports' =
        let val c' = find (x, exports')
            (* definition of [[csubtype]] 746b *)
            fun csubtype (DECVAL tau, DECVAL tau') =
                  if eqType (tau, tau') then OK ()
                  else ERROR ("interface calls for value " ^ x ^
                                                              " to have type " ^
                              typeString tau' ^ ", but it has type " ^
                                                                 typeString tau)
              | csubtype (DECABSTY _, DECABSTY _) = OK ()
                                   (* XXX really OK? without comparing paths? *)
              | csubtype (DECMANTY _, DECABSTY _) = OK ()  (* XXX likewise? *)
              | csubtype (DECMANTY tau, DECMANTY tau') = 
                  if eqType (tau, tau') then OK ()
                  else ERROR ("interface calls for type " ^ x ^
                                                       " to manifestly equal " ^
                              typeString tau' ^ ", but it is " ^ typeString tau)
              | csubtype (DECABSTY path, DECMANTY tau') =
                  if eqType (TYNAME path, tau') then OK ()
                  else ERROR ("interface calls for type " ^ x ^
                                                       " to manifestly equal " ^
                              typeString tau' ^ ", but it is " ^ typeString (
                                                                   TYNAME path))
              | csubtype (DECMOD (_, m), DECMOD (_, m')) =
                  subtype (m, m')
              | csubtype (c, c') =
                  ERROR ("interface calls for " ^ x ^ " to be " ^ whatcomp c' ^
                         ", but implementation provides " ^ whatcomp c)
            (* type declarations for consistency checking *)
            val _ = op csubtype : declarable * declarable -> unit error
(* type declarations for consistency checking *)
val _ = op csubtype : declarable * declarable -> unit error
val _ = op subtype  : modty * modty -> unit error
        in  csubtype (c, c') >> OK (List.filter (fn (y, _) => y <> x) exports')
        end handle NotFound x => OK exports'

      fun st (MTARROW (args, res), MTARROW (args', res')) =
            unimp "subtyping on arrow modules"
        | st (MTARROW (args, _), _) =
            ERROR ("expected an exporting module but got one that takes " ^
                   countString args "parameter")
        | st (_, MTARROW (args, _)) =
            ERROR ("expected a module that takes " ^
                   countString args "parameter" ^
                                                ", but got an exporting module")
        | st (mt, MTALLOF mts') =
            allE (map (fn mt' => st (mt, mt')) mts')
        | st (mt, MTEXPORTS comps') =
            (removeExported (mt, comps') >>=
             (fn [] => OK ()
               | xcs =>
                  ERROR (
                    "an interface expected some components that are missing: " ^
                         separate ("", ", ")
                         (map (fn (x, c) => x ^ " (" ^ whatcomp c ^ ")") xcs))))
                                                                       (* XXX *)
      and removeExported (MTEXPORTS xcs, exports') =            
             foldl (fn (xc, e) => e >>= remove xc) (OK exports') xcs
        | removeExported (MTALLOF mts, exports') =
             foldl (fn (mt, e) => e >>= curry removeExported mt) (OK exports')
                                                                             mts
        | removeExported _ =
             raise BugInTypeChecking "bad case reached removeExported"
      
  in  st mts
  end
(* [[implements]] relation, based on [[subtype]] of two module types 748b *)
fun implements (p : path, submt, supermt) =
  (*  (app eprint ["At ", pathString p,
                   "\n  sub:  ", mtString submt, "\n  sup: ", mtString supermt,
                                                                   "\n"]; id) *)
  subtype (submt, realize (p, manifestTypes (p, submt), supermt))
(* path-expression lookup 748c *)
fun notModule (px, dcl) =
  raise TypeError ("looking for a module, but " ^ pathexString px ^
                   " is a " ^ whatcomp dcl)
fun pathfind (PNAME x, Gamma) = find (snd x, Gamma)
  | pathfind (PDOT (path, x), Gamma) =
      let (* definition of [[mtfind]] 749e *)
          fun mtfind (x, mt as MTEXPORTS comps) : declarable option =
                 (SOME (find (x, comps)) handle NotFound _ => NONE)
            | mtfind (x, MTARROW _) =
                 raise TypeError ("tried to select component " ^ x ^
                                  " from generic module " ^ pathexString path)
            | mtfind (x, mt as MTALLOF mts) =
                (case List.mapPartial (fn mt => mtfind (x, mt)) mts
                   of [comp] => SOME comp
                    | [] => NONE
                    | _ :: _ :: _ => unimp "component in multiple signatures")
          fun noComponent (path, x, mt) =
            raise TypeError ("module " ^ pathexString path ^
                                                 " does not have a component " ^
                             pathexString (PDOT (path, x)) ^ "; its type is " ^
                                                                    mtString mt)
          (* type declarations for consistency checking *)
          val _ = op mtfind : name *  modty -> declarable option
      in  case pathfind (path, Gamma)
            of DECMOD (_, mt) =>
                 (valOf (mtfind (x, mt)) handle Option =>
                   noComponent (path, x, mt))
             | comp =>
            (* tried to select [[path]].[[x]] but [[path]] is a [[comp]] 752a *)
                       raise TypeError ("Tried to select " ^ pathexString (PDOT
                                                         (path, x)) ^ ", but " ^
                                        pathexString path ^ " is " ^ whatcomp
                                                    comp ^ ", which does not " ^
                                        " have components")
      end
  | pathfind (PAPPLY (fpx, actualpxs) : pathex, Gamma) =
     (* specialization of module [[fpx]] to [[actualpxs]] 749a *)
     let fun taggedModule px = case pathfind (px, Gamma)
                                 of DECMOD (p, mt) => (p, mt)
                                  | dec => notModule (px, dec)
         val (fmod, actuals) = (taggedModule fpx, map taggedModule actualpxs)
         val (formals, result) = case fmod
                                   of (_, MTARROW fr) => fr
                                    | _ =>
                                 (* specialized exporting module [[fpx]] 749b *)
                                           raise TypeError ("module " ^
                       pathexString fpx ^ " is an exporting module, and only " ^

                                         " a generic module can be specialized")
         fun resty ([],                    [],                       result) =
                                                                          result
           | resty ((formalid, formalmt) :: formals, (actp, actmt) :: actuals,
                                                                       result) =
               let val theta = formalid |--> actp
                   fun fsubst (modid, mt) = (modid, mtsubst theta mt)
               in  case implements (actp, actmt, mtsubst theta formalmt)
                     of OK () => resty (map fsubst formals, actuals, mtsubst
                                                                   theta result)
                      | ERROR msg =>
                       (* can't pass [[actp]] as [[formalid]] to [[fpx]] 749c *)
                                     raise TypeError ("module " ^ pathString
                                         actp ^ " cannot be used as argument " ^
                                                      modidentString formalid ^
                                      " to generic module " ^ pathexString fpx ^
                                                      ": " ^ msg)
               end
           | resty _ = (* wrong number of arguments to [[fpx]] 749d *)
                       raise TypeError ("generic module " ^ pathexString fpx ^
                                                              " is expecting " ^
                                        countString formals "parameter" ^
                                                                  ", but got " ^
                                        countString actuals "actual parameter")
     in  DECMOD (PAPPLY (fst fmod, map fst actuals), resty (formals, actuals,
                                                                        result))
     end
(* type declarations for consistency checking *)
val _ = op pathfind   : pathex * declarable env -> declarable
(* translation of {\mcl} type syntax into types 750a *)
fun txpath (px, Gamma) =
  let fun tx (PAPPLY (f, args)) = PAPPLY (tx f, map tx args)
        | tx (PDOT (p, x)) = PDOT (tx p, x)
        | tx (PNAME (loc, m)) =
            let fun bad aThing =
                  raise TypeError ("I was expecting " ^ m ^
                                                     " to refer to a module, " ^
                                   "but at " ^ srclocString loc ^ ", it's " ^
                                                                         aThing)
            in  case find (m, Gamma)
                  of DECMODTY _ => bad "a module type"
                   | DECMOD (p, mt) => p
                   | c => bad (whatcomp c)
            end
  in  tx px
  end
(* type declarations for consistency checking *)
val _ = op txpath : pathex * declarable env -> path
(* translation of {\mcl} type syntax into types 750b *)
fun txty (t, Gamma) =
  let fun tx (TYNAME px) =
            (case pathfind (px, Gamma)
               of DECMANTY tau => tau
                | DECABSTY path => TYNAME path
                | comp => raise TypeError ("I was expecting a type, but " ^
                                           pathexString px ^ " is " ^ whatcomp
                                                                          comp))
        | tx (FUNTY (args, res)) = FUNTY (map tx args, tx res)
        | tx ANYTYPE = ANYTYPE
  in  tx t
  end
(* type declarations for consistency checking *)
val _ = op txty : tyex * declarable env -> ty
(* translation of {\mcl} type syntax into types 750c *)
fun findModty (x, Gamma) =
  case find (x, Gamma)
    of DECMODTY mt => mt
     | comp => raise TypeError ("Tried to use " ^ whatcomp comp ^ " " ^ x ^
                                " as a module type")
(* type declarations for consistency checking *)
val _ = op findModty : name * declarable env -> modty
(* translation of {\mcl} type syntax into types 751a *)
fun txModtype (path, mtx : modtyx, Gamma) =
  let val resultName = PNAME (MODTYPLACEHOLDER "functor result")
      fun tx (MTNAMEDX t) = mtsubst (MODTYPLACEHOLDER t |--> path) (findModty (t
                                                                       , Gamma))
        | tx (MTEXPORTSX exports) =
             let val (this', _) = foldl (leftLocated export) ([], Gamma) exports
             in  MTEXPORTS (rev this')
             end
        | tx (MTALLOFX mts) = allWithManifest (path, map (located tx) mts)
        | tx (MTARROWX (args, body)) =
            let fun txArrow ([], (loc, body), Gamma : declarable env) =
                      ([], atLoc loc txModtype (resultName, body, Gamma))
                  | txArrow (((mloc, m), (mtloc, mtx)) :: rest, body, Gamma) =
                      let val modid = genmodident m
                          val modty = atLoc mtloc txModtype (PNAME modid, mtx,
                                                                          Gamma)
                          val Gamma' = bind (m, DECMOD (PNAME modid, modty),
                                                                          Gamma)
                             (* XXX check 1st arg to DECMOD *)
                          val (rest', body') = txArrow (rest, body, Gamma')
                      in  ((modid, modty) :: rest', body')
                      end
            in  MTARROW (txArrow (args, body, Gamma))
            end

      and export ((x, ctx : declarablex), (theseDecls, Gamma)) =
            if isbound (x, theseDecls) then
              raise TypeError ("duplicate declaration of " ^ x ^
                                                              " in module type")
            else
              let val c = txDecl (PDOT (path, x), ctx, Gamma)
              in  ((x, c) :: theseDecls, bind (x, c, Gamma))
              end
(* type declarations for consistency checking *)
val _ = op txModtype : path * modtyx * declarable env -> modty
  in  tx mtx
  end
(* translation of {\mcl} type syntax into types 751b *)
and txDecl (path, comp : declarablex, Gamma : declarable env) : declarable =
  let fun ty t = txty (t, Gamma)
(* type declarations for consistency checking *)
val _ = op txDecl    : path * declarablex * declarable env -> declarable
  in  case comp
        of DECVALX tau  => DECVAL (ty tau)
         | DECABSTYX    => DECABSTY path
         | DECMANTYX t  => DECMANTY (ty t)
         | DECMODX mt   => DECMOD (unimp "id of module", txModtype (path, mt,
                                                                         Gamma))
         | DECMODTYX mt => DECMODTY (txModtype (path, mt, Gamma))
  end
val txModtype = fn a =>
  let val mt = txModtype a
  in  if mixedManifestations mt then
        raise BugInTypeChecking ("invariant violation (mixed M): " ^ mtString mt
                                                                               )
      else
        mt
  end
(* primitive modules and types used to type literal expressions 769 *)
val arraymodname = "Array"

val intmodident = genmodident "Int"
val symmodident = genmodident "Sym"
val boolmodident = genmodident "Bool"
val unitmodident = genmodident "Unit"
val arraymodident = genmodident arraymodname
val uarraymodident = genmodident "UnsafeArray"

val inttype = TYNAME (PDOT (PNAME intmodident, "t"))
val symtype = TYNAME (PDOT (PNAME symmodident, "t"))
val booltype = TYNAME (PDOT (PNAME boolmodident, "t"))
val unittype = TYNAME (PDOT (PNAME unitmodident, "t"))

fun arraytype tau =
  case tau
    of TYNAME (PDOT (module, "t")) =>
         TYNAME (PDOT (PAPPLY (PNAME arraymodident, [module]), "t"))
     | _ => raise InternalError "unable to form internal array type"


fun addValWith f ((x, v, ty), rho) = bind (x, f v, rho)
fun decval (x, v, ty) = (x, DECVAL ty)


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
(* primitives ((mcl)) 770a *)
fun eqPrintPrims tau strip =
  let val comptype = FUNTY ([tau, tau], booltype)
      fun comparison f = binaryOp (embedBool o (fn (x, y) => f (strip x, strip y
                                                                             )))
  in  ("similar?",  comparison op =,  comptype) ::
      ("dissimilar?",  comparison op =,  comptype) ::
      ("=",  comparison op =,  comptype) ::
      ("!=", comparison op <>, comptype) ::
      ("print", unaryOp (fn x => (print (valueString x);unitVal)), FUNTY ([tau],
                                                                   unittype)) ::
      ("println", unaryOp (fn x => (println (valueString x);unitVal)), FUNTY ([
                                                             tau], unittype)) ::
      []
  end

val symPrims =
  eqPrintPrims symtype (fn SYM s => s | _ => raise BugInTypeChecking
                                                        "comparing non-symbols")

val boolPrims =
  eqPrintPrims booltype (fn CONVAL (K, []) => K
                          | _ => raise BugInTypeChecking
                                                       "comparing non-Booleans")

(* primitives ((mcl)) 770b *)
fun comparison f = binaryOp (embedBool o f)
fun intcompare f = 
      comparison (fn (NUM n1, NUM n2) => f (n1, n2)
                   | _ => raise BugInTypeChecking "comparing non-numbers")

fun asInt (NUM n) = n
  | asInt _ = raise BugInTypeChecking "expected a number"

val arithtype = FUNTY ([inttype, inttype], inttype)
val comptype  = FUNTY ([inttype, inttype], booltype)


val intPrims = 
  ("+", arithOp op +,   arithtype) :: 
  ("-", arithOp op -,   arithtype) :: 
  ("*", arithOp op *,   arithtype) :: 
  ("/", arithOp op div, arithtype) ::

  ("negated", unaryOp (NUM o ~ o asInt), FUNTY ([inttype], inttype)) ::

  ("<",  intcompare op <,  comptype) :: 
  (">",  intcompare op >,  comptype) ::
  ("<=", intcompare op <=, comptype) :: 
  (">=", intcompare op >=, comptype) ::
  ("printu", unaryOp (fn n => (printUTF8 (asInt n); unitVal)), FUNTY ([inttype],
                                                                   unittype)) ::
  eqPrintPrims inttype (fn NUM n => n | _ => raise BugInTypeChecking
                                                        "comparing non-numbers")
(* primitives ((mcl)) 771 *)
local
  val arraypath = PNAME arraymodident
  val arrayarg  = genmodident "Elem"
  val argpath   = PNAME arrayarg
  val resultpath = PAPPLY (arraypath, [argpath])
  val elemtype   = TYNAME (PDOT (argpath, "t"))
  val arraytype  = TYNAME (PDOT (resultpath, "t"))


  fun protect f x = f x
    handle Size      => raise RuntimeError "array too big"
         | Subscript => raise RuntimeError "array index out of bounds"


  fun asArray (ARRAY a) = a
    | asArray _         = raise BugInTypeChecking "non-array value as array"
  fun arrayLeft f (a, x) = f (asArray a, x)
in
  val arrayPrims = 
    ("size", unaryOp (NUM o Array.length o asArray), FUNTY ([arraytype], inttype
                                                                           )) ::
    ("new", binaryOp (fn (NUM n, a) => ARRAY (protect Array.array (n, a))
                       | _ => raise BugInTypeChecking "array sizez not a number"
                                                                              ),
            FUNTY ([inttype, elemtype], arraytype)) ::
    ("at", binaryOp (fn (ARRAY a, NUM i) => protect Array.sub (a, i)
                      | _ => raise BugInTypeChecking "Array.at array or index"),
            FUNTY ([arraytype, inttype], elemtype)) ::
    ("at-put", fn [ARRAY a, NUM i, x] => (protect Array.update (a, i, x);
                                                                        unitVal)
                | _ => raise BugInTypeChecking
                                      "number or types of args to Array.at-put",
            FUNTY ([arraytype, inttype, elemtype], unittype)) ::
    []

  val arraymodtype : modty =
    MTARROW ([(arrayarg, MTEXPORTS [("t", DECABSTY (PDOT (argpath, "t")))]  :
                                                                        modty)],
             MTEXPORTS (("t", DECABSTY (PDOT (resultpath, "t"))) ::
                        ("elem", DECMANTY elemtype) ::
                        map decval arrayPrims) : modty)

  val uarrayPrims = 
    ("new", unaryOp (fn (NUM n) => ARRAY (protect Array.array (n, CONVAL (
                                                          "uninitialized", [])))
                       | _ => raise BugInTypeChecking "array size not a number")
                                                                               ,
            FUNTY ([inttype], arraytype)) ::
    []

  val uarraymodtype : modty =
    MTARROW ([(arrayarg, MTEXPORTS [("t", DECABSTY (PDOT (argpath, "t")))]  :
                                                                        modty)],
             MTEXPORTS (("t", DECABSTY (PDOT (resultpath, "t"))) ::
                        map decval uarrayPrims) : modty)
end
(* primitives ((mcl)) 1361a *)
fun inject_bool x =
      CONVAL (if x then "#t" else "#f", [])
fun project_bool (CONVAL ("#t", [])) = true
  | project_bool (CONVAL ("#f", [])) = false
  | project_bool _ = raise RuntimeError "projected non-boolean"

fun inject_predicate f = fn x => inject_bool (f x)
fun predop f = unaryOp (inject_predicate f)

fun comparison f = binaryOp (inject_predicate f)
fun intcompare f = comparison (
                     fn (NUM n1, NUM n2) => f (n1, n2)
                      | _ => raise BugInTypeChecking "integers expected")
(* type declarations for consistency checking *)
val _ = op inject_bool  : bool -> value
val _ = op project_bool : value -> bool

local
  fun module id primvals : declarable =
    DECMOD (PNAME id,
            MTEXPORTS (("t", DECABSTY (PDOT (PNAME id, "t"))) :: map decval
                                                                      primvals))
in
  val intmod  = module intmodident intPrims
  val symmod  = module symmodident symPrims
  val boolmod = module boolmodident boolPrims
  val unitmod = module unitmodident []
  val arraymod  = DECMOD (PNAME arraymodident, arraymodtype)
  val uarraymod  = DECMOD (PNAME uarraymodident, uarraymodtype)
end

(* utility functions on {\mcl} types 754a *)
fun firstArgType (x, FUNTY (tau :: _, _)) = OK tau
  | firstArgType (x, FUNTY ([], _)) =
      ERROR ("function " ^ x ^
                 " cannot be overloaded because it does not take any arguments")
  | firstArgType (x, _) =
      ERROR (x ^ " cannot be overloaded because it is not a function")

(* utility functions on {\mcl} types 754b *)
fun okOrTypeError (OK a) = a
  | okOrTypeError (ERROR msg) = raise TypeError msg

fun ok a = okOrTypeError a handle _ => raise InternalError
                                                      "overloaded non-function?"
fun resolveOverloaded (f, argty : ty, tys : ty list) : (ty * int) error =
  let fun findAt (tau :: taus, i) = if eqType (argty, ok (firstArgType (f, tau))
                                                                          ) then
                                      OK (tau, i)
                                    else
                                      findAt (taus, i + 1)
        | findAt ([], _) =
            ERROR ("cannot figure out how to resolve overloaded name " ^ f ^
                   " when applied to first argument of type " ^ typeString argty
                                                                               ^
                   " (resolvable: " ^ separate ("", ", ") (map typeString tys) ^
                                                                            ")")
  in  findAt (tys, 0)
  end
(* type declarations for consistency checking *)
val _ = op resolveOverloaded : name * ty * ty list -> (ty * int) error
(* [[typeof]] a {\mcl} expression ((prototype)) 754c *)
fun typeof (e, Gamma) : ty = raise LeftAsExercise "typeof"
(* principal type of a module 756b *)
fun strengthen (p, MTEXPORTS comps) =
      let fun comp (c as (x, dc)) =
            case dc
              of DECABSTY _ => (x, DECMANTY (TYNAME (PDOT (p, x))))
               | DECMOD (p, mt) =>
                   (x, DECMOD (p, strengthen (p, mt)))  (* XXX check me *)
               | DECVAL   _ => c
               | DECMANTY _ => c
               | DECOVLN  _ => c
               | DECMODTY _ => raise BugInTypeChecking
                                                      "module type as component"
      in  MTEXPORTS (map comp comps)
      end
  | strengthen (p, MTALLOF mts) =
      allWithManifest (p, map (fn mt => strengthen (p, mt)) mts)
  | strengthen (p, mt as MTARROW _) =
      mt
(* type declarations for consistency checking *)
val _ = op strengthen : path * modty -> modty
(* elaborate a {\mcl} definition 758a *)
fun declarableResponse c =
      case c
        of DECMODTY mt => mtString mt
         | DECVAL tau => typeString tau
         | DECABSTY _ => "abstract type"
         | DECMANTY _ => "manifest type"
         | DECMOD (_, mt) => mtString mt
         | DECOVLN _ => "overloaded name"
(* elaborate a {\mcl} definition 758c *)
fun printStrings ss _ vs = app print ss
type value_printer = (name -> ty -> value -> unit) -> value list -> unit

fun defResponse (x, c) =
  case c
    of DECVAL tau =>
             (fn printfun => fn [v] => (printfun x tau v; app print [" : ",
                                                                typeString tau])
                              | _ => raise InternalError
                                            "val definition not a single value")
     | DECABSTY path => if pathString path = x then
                          printStrings ["abstract type ", x]
                        else
                          printStrings ["type ", x, " = ", pathString path]
     | DECMANTY tau => printStrings ["type ", x, " = ", typeString tau]
     | DECMOD (_, mt as MTARROW _) => printStrings ["generic module ", x, " : ",
                                                                    mtString mt]
     | DECMOD (_, mt) =>  printStrings ["module ", x, " : ", mtString mt]
     | DECMODTY mt => printStrings ["module type ", x, " = ", mtString mt]
     | DECOVLN [] => raise InternalError "empty overloaded name"
     | DECOVLN (tau :: taus) =>
           printStrings ( "overloaded " :: x :: " : " :: typeString tau ::
                         map (fn t => "\n           " ^ x ^ " : " ^ typeString t
                                                                               )
                         taus)

(* elaborate a {\mcl} definition 759a *)
fun dataDefDeclarables (path, (T, vcons), Gamma) =
  let val tau = TYNAME (PDOT (path, T))
      val declaredty = DECABSTY (PDOT (path, T))
      val Gamma' = bind (T, DECABSTY (PDOT (path, T)), Gamma)
      fun translateVcon (K, tx) = (K, txty (tx, Gamma'))
            handle TypeError msg =>
              raise TypeError ("in type of value constructor " ^ K ^ ", " ^ msg)
      val Ktaus = map translateVcon vcons
      fun validate (K, FUNTY (_, result)) =
            if eqType (result, tau) then
              ()
            else
              raise TypeError ("value constructor " ^ K ^ " should return " ^
                                                                typeString tau ^
                 ", but it returns type " ^ typeString result)
        | validate (K, tau') =
            if eqType (tau', tau) then
              ()
            else
              raise TypeError ("value constructor " ^ K ^ " should have type " ^
                                                                typeString tau ^
                 ", but it has type " ^ typeString tau')
      val ()     = app validate Ktaus

  in  (T, declaredty) :: map (fn (x, tau) => (x, DECVAL tau)) Ktaus
  end
(* elaborate a {\mcl} definition 759b *)
fun addOverload (p, Gamma) =
  let val (tau, first) =
        case pathfind (p, Gamma)
          of DECVAL tau => (tau, okOrTypeError (firstArgType (pathexString p,
                                                                          tau)))
           | c => (* can't overload a [[c]] 765d *)
                  raise TypeError ("only functions can be overloaded, but " ^
                                                              whatcomp c ^ " " ^
                                   pathexString p ^ " is not a function")
      val x = plast p

      val currentTypes =
        (case find (x, Gamma)
           of DECOVLN vals => vals
            | _ => []) handle NotFound _ => []
      val newTypes = tau :: currentTypes
  in  bind (x, DECOVLN newTypes, Gamma)
  end
(* elaborate a {\mcl} definition 760 *)
fun declarable (context, d, Gamma) =
  let fun notcomponent what =
        raise TypeError (what ^ " cannot appear " ^ contextString context)
      (* definition of [[mtypeof]] 762a *)
      fun findModule (px, Gamma) =
        case pathfind (px, Gamma)
          of DECMOD (_, mt) => mt
           | comp => raise TypeError ("looking for a module, but " ^
                                                               pathexString px ^
                                      " is a " ^ whatcomp comp)

      fun mtypeof (path, m, Gamma) =
        let fun ty (MPATH p) = strengthen (txpath (p, Gamma), findModule (p,
                                                                         Gamma))
              | ty (MPATHSEALED (mtx, p)) = sealed (mtx, ty (MPATH p))
              | ty (MUNSEALED defs)       = principal defs
              | ty (MSEALED (mtx, defs))  = sealed (mtx, principal defs)
            and sealed (mtx, mt') =
                  let val mt = txModtype (path, mtx, Gamma)
                  in  case implements (path, mt', mt)
                        of OK () => mt
                         | ERROR msg => raise TypeError msg
                  end
            and principal ds =
              let fun defs ([],    Gamma, seen) = []
                    | defs ((loc, DATA dd)::ds, Gamma, seen) =
                        bindAll (atLoc loc dataDefDeclarables (path, dd, Gamma),
                                                                ds, Gamma, seen)
                    | defs ((loc, OVERLOAD ovls)::ds, Gamma, seen) =
                        let val Gamma' = foldl (atLoc loc addOverload) Gamma
                                                                            ovls
                        in  defs (ds, Gamma', seen)
                        end
                    | defs ((loc, d)::ds, Gamma, seen) =
                        bindAll ([atLoc loc declarable (INMODULE path, d, Gamma)
                                                             ], ds, Gamma, seen)
                  and bindAll (xcs, ds, Gamma, seen) =
                       let fun addBindings ([], Gamma, seen) = defs (ds, Gamma,
                                                                           seen)
                             | addBindings ((x, DECMODTY _) :: xcs, _, _) =
                                 raise TypeError "module type in module"
                             | addBindings ((x, c) :: xcs, Gamma, seen) =
                                 if false andalso member x seen then
                                                     (* maybe this is OK now? *)

                             (* duplicate definition of [[x]] in context 762b *)
                                   raise TypeError ("Redefinition of " ^ x ^
                                                         " in <unknown-module>")
                                 else
                                   (x, c) :: addBindings (xcs, bind (x, c, Gamma
                                                                   ), x :: seen)
                       in  addBindings (xcs, Gamma, seen)
                       end
              in  MTEXPORTS (defs (ds, Gamma, []))
              end
        in  ty m
        end
      (* type declarations for consistency checking *)
      val _ = op mtypeof : path * moddef * declarable env -> modty
  in  case d
        of MODULETYPE (name, mtx) => notcomponent ("a module type (like " ^ name
                                                                          ^ ")")
         | MODULE (name, mx) =>
             (name, DECMOD (contextDot (context, name),
                            mtypeof (contextDot (context, name), mx, Gamma)))
         | GMODULE (f, formals, body) =>
             let val fpath     = contextDot (context, f)
                 val idformals = map (fn (x, mtx) => (genmodident x, (x, mtx)))
                                                                         formals
                 val resultpath = PAPPLY (fpath, map (PNAME o fst) idformals)

                 fun addarg arg (args, res) = (arg :: args, res)

                 fun arrowtype ((mid : modident, (x, mtx)) :: rest, Gamma) =
                       let val mt = txModtype (PNAME mid, mtx, Gamma)
                           val Gamma' = bind (x, DECMOD (PNAME mid, mt), Gamma)
                       in  addarg (mid, mt) (arrowtype (rest, Gamma'))
                       end
                   | arrowtype ([], Gamma) = ([], mtypeof (resultpath, body,
                                                                         Gamma))
             in  (f, DECMOD (fpath, MTARROW (arrowtype (idformals, Gamma))))
             end       
                           
         | EXP e => notcomponent ("an expression (like " ^ expString e ^ ")")
         | QNAME px => notcomponent ("a qualified name (like " ^ pathexString px
                                                                          ^ ")")
         | DEFINE (name, tau, lambda as (formals, body)) =>
             let val funty = FUNTY (map (fn (n, ty) => ty) formals, tau)
             in  declarable (context, VALREC (name, funty, LAMBDA lambda), Gamma
                                                                               )
             end
         | VAL (name, e) =>
             (name, DECVAL (typeof (e, Gamma)))
         | VALREC (name, tau, e as LAMBDA _) =>
             let val tau    = txty (tau, Gamma)
                 val Gamma' = bind (name, DECVAL tau, Gamma)
                 val tau'   = typeof (e, Gamma')
             in  if not (eqType (tau, tau')) then
                   raise TypeError ("identifier " ^ name ^
                                    " is declared to have type " ^
                                    typeString tau ^ " but has actual type " ^
                                    typeString tau')
                 else
                   (name, DECVAL tau)
             end
         | VALREC (name, tau, _) =>
             raise TypeError ("(val-rec [" ^ name ^ " : " ^ tyexString tau ^
                             "] ...) must used (lambda ...) on right-hand side")
         | TYPE (t, tx) =>
             let val tau = txty (tx, Gamma)
             in  (t, DECMANTY tau)
             end
         | DATA _ => raise BugInTypeChecking
                                            "data definition reached declarable"
         | OVERLOAD _ => raise BugInTypeChecking
                                     "overloading definition reached declarable"
  end
(* type declarations for consistency checking *)
val _ = op declarable : context * baredef * declarable env -> name * declarable
(* type declarations for consistency checking *)
val _ = op printStrings : string list -> value_printer
val _ = op defResponse : name * declarable -> value_printer
(* elaborate a {\mcl} definition 761b *)
fun elabdef (d, Gamma) =
  case d
    of EXP e => elabdef (VAL ("it", e), Gamma)
     | MODULETYPE (x, mtx) =>
            let val c = DECMODTY (txModtype (PNAME (MODTYPLACEHOLDER x), mtx,
                                                                         Gamma))
            in  (x, bind (x, c, Gamma), defResponse (x, c))
            end
     | d => let val (x, c) = declarable (TOPLEVEL, d, Gamma)
            in  (x, bind (x, c, Gamma), defResponse (x, c))
            end
(* type declarations for consistency checking *)
type value_printer = value_printer
val _ = op elabdef : baredef * declarable env -> name * declarable env *
                                                                   value_printer



(*****************************************************************)
(*                                                               *)
(*   SUBSTITUTIONS FOR \MCL                                      *)
(*                                                               *)
(*****************************************************************)

(* substitutions for \mcl 742c *)
type subst = (modident * path) list
val idsubst = []
(* type declarations for consistency checking *)
type subst = subst
val _ = op idsubst : subst
(* substitutions for \mcl 742d *)
infix 7 |-->
fun id |--> p = [(id, p)]
(* type declarations for consistency checking *)
val _ = op |--> : modident * path -> subst
(* substitutions for \mcl 743a *)
fun pathsubst theta =
  let fun subst (PNAME id) =
            (case List.find (fn (id', p') => id = id') theta
               of SOME (_, p) => p
                | NONE => PNAME id)
        | subst (PDOT (p, x)) = PDOT (subst p, x)
        | subst (PAPPLY (p, ps)) = PAPPLY (subst p, map subst ps)
  in  subst
  end
(* type declarations for consistency checking *)
val _ = op pathsubst : subst -> path -> path
(* substitutions for \mcl 743b *)
fun tysubst theta (TYNAME p)          = TYNAME (pathsubst theta p)
  | tysubst theta (FUNTY (args, res)) =
      FUNTY (map (tysubst theta) args, tysubst theta res)
  | tysubst theta ANYTYPE = ANYTYPE
(* type declarations for consistency checking *)
val _ = op tysubst : subst -> ty -> ty
(* substitutions for \mcl 743c *)
fun dom theta = map (fn (a, _) => a) theta
fun compose (theta2, theta1) =
  let val domain  = union (dom theta2, dom theta1)
      val replace = pathsubst theta2 o pathsubst theta1 o PNAME
  in  map (fn a => (a, replace a)) domain
  end
(* type declarations for consistency checking *)
val _ = op dom     : subst -> modident set
val _ = op compose : subst * subst -> subst
(* substitutions for \mcl 743d *)
fun bsubst s = 
  map (fn (x, a) => (x, s a))

fun mtsubst theta =
  let fun s (MTEXPORTS comps)     = MTEXPORTS (bsubst (declsubst theta) comps)
        | s (MTALLOF mts)         = MTALLOF (map s mts)
        | s (MTARROW (args, res)) = MTARROW (bsubst s args, s res)
  in  s
  end
and declsubst theta =
  let fun s (DECVAL t) = DECVAL (tysubst theta t)
        | s (DECABSTY path) = DECABSTY (pathsubst theta path)
        | s (DECMANTY t)  = DECMANTY (tysubst theta t)
        | s (DECOVLN taus) = DECOVLN (map (tysubst theta) taus)
              (* below, ??? *)
        | s (DECMOD (p, mt))  = DECMOD (pathsubst theta p, mtsubst theta mt)
        | s (DECMODTY _)  =
           raise BugInTypeChecking "can't happen: substitute into module type"
  in  s
  end
(* type declarations for consistency checking *)
val _ = op mtsubst   : subst -> modty      -> modty
val _ = op declsubst : subst -> declarable -> declarable



(*****************************************************************)
(*                                                               *)
(*   TRANSLATION OF {\MCL} TYPE SYNTAX INTO TYPES                *)
(*                                                               *)
(*****************************************************************)

(* translation of {\mcl} type syntax into types 750a *)
fun txpath (px, Gamma) =
  let fun tx (PAPPLY (f, args)) = PAPPLY (tx f, map tx args)
        | tx (PDOT (p, x)) = PDOT (tx p, x)
        | tx (PNAME (loc, m)) =
            let fun bad aThing =
                  raise TypeError ("I was expecting " ^ m ^
                                                     " to refer to a module, " ^
                                   "but at " ^ srclocString loc ^ ", it's " ^
                                                                         aThing)
            in  case find (m, Gamma)
                  of DECMODTY _ => bad "a module type"
                   | DECMOD (p, mt) => p
                   | c => bad (whatcomp c)
            end
  in  tx px
  end
(* type declarations for consistency checking *)
val _ = op txpath : pathex * declarable env -> path
(* translation of {\mcl} type syntax into types 750b *)
fun txty (t, Gamma) =
  let fun tx (TYNAME px) =
            (case pathfind (px, Gamma)
               of DECMANTY tau => tau
                | DECABSTY path => TYNAME path
                | comp => raise TypeError ("I was expecting a type, but " ^
                                           pathexString px ^ " is " ^ whatcomp
                                                                          comp))
        | tx (FUNTY (args, res)) = FUNTY (map tx args, tx res)
        | tx ANYTYPE = ANYTYPE
  in  tx t
  end
(* type declarations for consistency checking *)
val _ = op txty : tyex * declarable env -> ty
(* translation of {\mcl} type syntax into types 750c *)
fun findModty (x, Gamma) =
  case find (x, Gamma)
    of DECMODTY mt => mt
     | comp => raise TypeError ("Tried to use " ^ whatcomp comp ^ " " ^ x ^
                                " as a module type")
(* type declarations for consistency checking *)
val _ = op findModty : name * declarable env -> modty
(* translation of {\mcl} type syntax into types 751a *)
fun txModtype (path, mtx : modtyx, Gamma) =
  let val resultName = PNAME (MODTYPLACEHOLDER "functor result")
      fun tx (MTNAMEDX t) = mtsubst (MODTYPLACEHOLDER t |--> path) (findModty (t
                                                                       , Gamma))
        | tx (MTEXPORTSX exports) =
             let val (this', _) = foldl (leftLocated export) ([], Gamma) exports
             in  MTEXPORTS (rev this')
             end
        | tx (MTALLOFX mts) = allWithManifest (path, map (located tx) mts)
        | tx (MTARROWX (args, body)) =
            let fun txArrow ([], (loc, body), Gamma : declarable env) =
                      ([], atLoc loc txModtype (resultName, body, Gamma))
                  | txArrow (((mloc, m), (mtloc, mtx)) :: rest, body, Gamma) =
                      let val modid = genmodident m
                          val modty = atLoc mtloc txModtype (PNAME modid, mtx,
                                                                          Gamma)
                          val Gamma' = bind (m, DECMOD (PNAME modid, modty),
                                                                          Gamma)
                             (* XXX check 1st arg to DECMOD *)
                          val (rest', body') = txArrow (rest, body, Gamma')
                      in  ((modid, modty) :: rest', body')
                      end
            in  MTARROW (txArrow (args, body, Gamma))
            end

      and export ((x, ctx : declarablex), (theseDecls, Gamma)) =
            if isbound (x, theseDecls) then
              raise TypeError ("duplicate declaration of " ^ x ^
                                                              " in module type")
            else
              let val c = txDecl (PDOT (path, x), ctx, Gamma)
              in  ((x, c) :: theseDecls, bind (x, c, Gamma))
              end
(* type declarations for consistency checking *)
val _ = op txModtype : path * modtyx * declarable env -> modty
  in  tx mtx
  end
(* translation of {\mcl} type syntax into types 751b *)
and txDecl (path, comp : declarablex, Gamma : declarable env) : declarable =
  let fun ty t = txty (t, Gamma)
(* type declarations for consistency checking *)
val _ = op txDecl    : path * declarablex * declarable env -> declarable
  in  case comp
        of DECVALX tau  => DECVAL (ty tau)
         | DECABSTYX    => DECABSTY path
         | DECMANTYX t  => DECMANTY (ty t)
         | DECMODX mt   => DECMOD (unimp "id of module", txModtype (path, mt,
                                                                         Gamma))
         | DECMODTYX mt => DECMODTY (txModtype (path, mt, Gamma))
  end
val txModtype = fn a =>
  let val mt = txModtype a
  in  if mixedManifestations mt then
        raise BugInTypeChecking ("invariant violation (mixed M): " ^ mtString mt
                                                                               )
      else
        mt
  end


(*****************************************************************)
(*                                                               *)
(*   TYPE CHECKING FOR {\MCL}                                    *)
(*                                                               *)
(*****************************************************************)

(* type checking for {\mcl} 772c *)
(* [[context]] for a {\mcl} definition 757 *)
datatype context
  = TOPLEVEL
  | INMODULE of path

fun contextDot (TOPLEVEL, name) = PNAME (genmodident name)
                                                     (* XXX key to uniqueness *)
  | contextDot (INMODULE path, name) = PDOT (path, name)

fun contextString TOPLEVEL = "at top level"
  | contextString (INMODULE p) = "in module " ^ pathString p
(* type declarations for consistency checking *)
type context = context
val _ = op contextDot : context * name -> path
(* type equality for \mcl 742a *)
fun eqType (TYNAME p, TYNAME p') = p = p'
  | eqType (FUNTY (args, res), FUNTY (args', res')) =
      eqTypes (args, args') andalso eqType (res, res')
  | eqType (ANYTYPE, _) = true
  | eqType (_, ANYTYPE) = true
  | eqType _ = false
and eqTypes (taus, tau's) = ListPair.allEq eqType (taus, tau's)
(* type declarations for consistency checking *)
val _ = op eqType  : ty      * ty      -> bool
val _ = op eqTypes : ty list * ty list -> bool
(* substitutions for \mcl 742c *)
type subst = (modident * path) list
val idsubst = []
(* type declarations for consistency checking *)
type subst = subst
val _ = op idsubst : subst
(* substitutions for \mcl 742d *)
infix 7 |-->
fun id |--> p = [(id, p)]
(* type declarations for consistency checking *)
val _ = op |--> : modident * path -> subst
(* substitutions for \mcl 743a *)
fun pathsubst theta =
  let fun subst (PNAME id) =
            (case List.find (fn (id', p') => id = id') theta
               of SOME (_, p) => p
                | NONE => PNAME id)
        | subst (PDOT (p, x)) = PDOT (subst p, x)
        | subst (PAPPLY (p, ps)) = PAPPLY (subst p, map subst ps)
  in  subst
  end
(* type declarations for consistency checking *)
val _ = op pathsubst : subst -> path -> path
(* substitutions for \mcl 743b *)
fun tysubst theta (TYNAME p)          = TYNAME (pathsubst theta p)
  | tysubst theta (FUNTY (args, res)) =
      FUNTY (map (tysubst theta) args, tysubst theta res)
  | tysubst theta ANYTYPE = ANYTYPE
(* type declarations for consistency checking *)
val _ = op tysubst : subst -> ty -> ty
(* substitutions for \mcl 743c *)
fun dom theta = map (fn (a, _) => a) theta
fun compose (theta2, theta1) =
  let val domain  = union (dom theta2, dom theta1)
      val replace = pathsubst theta2 o pathsubst theta1 o PNAME
  in  map (fn a => (a, replace a)) domain
  end
(* type declarations for consistency checking *)
val _ = op dom     : subst -> modident set
val _ = op compose : subst * subst -> subst
(* substitutions for \mcl 743d *)
fun bsubst s = 
  map (fn (x, a) => (x, s a))

fun mtsubst theta =
  let fun s (MTEXPORTS comps)     = MTEXPORTS (bsubst (declsubst theta) comps)
        | s (MTALLOF mts)         = MTALLOF (map s mts)
        | s (MTARROW (args, res)) = MTARROW (bsubst s args, s res)
  in  s
  end
and declsubst theta =
  let fun s (DECVAL t) = DECVAL (tysubst theta t)
        | s (DECABSTY path) = DECABSTY (pathsubst theta path)
        | s (DECMANTY t)  = DECMANTY (tysubst theta t)
        | s (DECOVLN taus) = DECOVLN (map (tysubst theta) taus)
              (* below, ??? *)
        | s (DECMOD (p, mt))  = DECMOD (pathsubst theta p, mtsubst theta mt)
        | s (DECMODTY _)  =
           raise BugInTypeChecking "can't happen: substitute into module type"
  in  s
  end
(* type declarations for consistency checking *)
val _ = op mtsubst   : subst -> modty      -> modty
val _ = op declsubst : subst -> declarable -> declarable
(* type components of module types 744a *)
fun abstractTypes (path, MTEXPORTS cs) =
      let fun mts (x, DECABSTY tau) = [(PDOT (path, x), tau)]
            | mts (x, DECMOD (p', mt)) = abstractTypes (p', mt)
            | mts _ = []
      in  (List.concat o map mts) cs
      end
  | abstractTypes (path, MTALLOF mts) =
      (List.concat o map (fn mt => abstractTypes (path, mt))) mts
  | abstractTypes (path, MTARROW _) = []
                                          (* could be bogus, cf Leroy rule 21 *)
(* module-type realization 746c *)
fun manifestTypes (path, MTEXPORTS cs) =
      let fun mts (x, DECMANTY tau) = [(PDOT (path, x), tau)]
            | mts (x, DECMOD (p', mt)) = manifestTypes (p', mt)
            | mts _ = []
      in  (List.concat o map mts) cs
      end
  | manifestTypes (path, MTALLOF mts) =
      (List.concat o map (fn mt => manifestTypes (path, mt))) mts
  | manifestTypes (path, MTARROW _) = []
                                          (* could be bogus, cf Leroy rule 21 *)
(* type declarations for consistency checking *)
val _ = op manifestTypes : path * modty -> (path * ty) list
(* module-type realization 746d *)
fun associatedWith (x, []) =
      NONE
  | associatedWith (x, (key, value) :: pairs) =
      if x = key then SOME value else associatedWith (x, pairs)

fun hasKey x [] = false
  | hasKey x ((key, value) :: pairs) = x = key orelse hasKey x pairs

fun realizeType mantypes =
  let fun r (TYNAME path) = getOpt (associatedWith (path, mantypes), TYNAME path
                                                                               )
        | r (FUNTY (args, res)) = FUNTY (map r args, r res)
        | r (ANYTYPE) = ANYTYPE
  in  r
  end
(* type declarations for consistency checking *)
val _ = op realizeType : (path * ty) list -> ty -> ty
(* module-type realization 747a *)
fun filterdec p (path, MTARROW f) = MTARROW f
  | filterdec p (path, MTALLOF mts) = MTALLOF (map (fn mt => filterdec p (path,
                                                                       mt)) mts)
  | filterdec p (path, MTEXPORTS xcs) =
      let fun cons ((x, c), xcs) =
            let val path = PDOT (path, x)
                val c = case c of DECMOD (path', mt) => DECMOD (path', filterdec
                                                                   p (path, mt))
                                | _ => c
            in  if p (path, c) then
                  (x, c) :: xcs
                else
                  xcs
            end
      in  MTEXPORTS (foldr cons [] xcs)
      end
(* module-type realization 747b *)
fun emptyExports (MTEXPORTS []) = true
  | emptyExports _ = false
(* module-type realization 747c *)
val joinMts : modty list -> modty = (* simple syntactic join *)
      let val path = PNAME (MODTYPLACEHOLDER "syntactic join")
          fun filterManifest (prev', []) = rev prev'
            | filterManifest (prev', mt :: mts) =
                let val manifests = manifestTypes (path, MTALLOF prev')
                    fun redundant (p, DECMANTY tau) =
                          (case associatedWith (p, manifests)
                             of SOME tau' => eqType (tau, tau')
                              | NONE => false)
                      | redundant _ = false
                in  filterManifest (filterdec (not o redundant) (path, mt) ::
                                                                     prev', mts)
                end
          val filterManifest = fn mts => filterManifest ([], mts)
          fun mtall [mt] = mt
            | mtall mts  = MTALLOF mts
      in  mtall o List.filter (not o emptyExports) o filterManifest
      end

fun simpleSyntacticJoin (MTALLOF mts) = joinMts mts
  | simpleSyntacticJoin mt = mt
(* module-type realization 747d *)
fun allWithManifest (path, mts) =
  let val mt = MTALLOF mts
      val mantypes = manifestTypes (path, mt)
      val abstypes = abstractTypes (path, mt)
  in  if List.exists (fn (p, _) => hasKey p mantypes) abstypes then
        simpleSyntacticJoin (realize (path, mantypes, mt))
      else
        mt
  end
(* module-type realization 748a *)
and realize (p, mantypes, mt) =
  let val newty = realizeType mantypes
      fun newmt (MTEXPORTS cs) = MTEXPORTS (map (fn (x, c) => (x, newdecl c)) cs
                                                                               )
        | newmt (MTALLOF mts)  = allWithManifest (p, map newmt mts)
        | newmt (MTARROW (args, result)) =
            MTARROW (map (fn (x, mt) => (x, newmt mt)) args, newmt result)
      and newdecl (DECVAL tau) = DECVAL (newty tau)
        | newdecl (DECABSTY p) =
           (case associatedWith (p, mantypes)
              of SOME tau => DECMANTY tau
               | NONE => DECABSTY p)   (* used to be this on every path *)
        | newdecl (DECMANTY tau) = DECMANTY (newty tau)
        | newdecl (DECMOD (path, mt)) = DECMOD (path, newmt mt)
        | newdecl (DECOVLN taus) = DECOVLN (map newty taus)
        | newdecl (DECMODTY _) = raise BugInTypeChecking
                                                     "module type inside module"
  in  newmt mt
  end
(* type declarations for consistency checking *)
val _ = op realize : path * (path * ty) list * modty -> modty
(* invariants of \mcl 744b *)
fun mixedManifestations mt =
  let val path = PNAME (MODTYPLACEHOLDER "invariant checking")
      val manifests = manifestTypes (path, mt)
      val abstracts = abstractTypes (path, mt)
  in  List.exists (fn (p, _) => hasKey p manifests) abstracts
  end
(* [[implements]] relation, based on [[subtype]] of two module types 745 *)
infix 1 >>
fun (OK ()) >> c = c
  | (ERROR msg) >> _ = ERROR msg

fun allE []      = OK ()
  | allE (e::es) = e >> allE es

fun subtype mts =
  let fun remove (x, c) exports' =
        let val c' = find (x, exports')
            (* definition of [[csubtype]] 746b *)
            fun csubtype (DECVAL tau, DECVAL tau') =
                  if eqType (tau, tau') then OK ()
                  else ERROR ("interface calls for value " ^ x ^
                                                              " to have type " ^
                              typeString tau' ^ ", but it has type " ^
                                                                 typeString tau)
              | csubtype (DECABSTY _, DECABSTY _) = OK ()
                                   (* XXX really OK? without comparing paths? *)
              | csubtype (DECMANTY _, DECABSTY _) = OK ()  (* XXX likewise? *)
              | csubtype (DECMANTY tau, DECMANTY tau') = 
                  if eqType (tau, tau') then OK ()
                  else ERROR ("interface calls for type " ^ x ^
                                                       " to manifestly equal " ^
                              typeString tau' ^ ", but it is " ^ typeString tau)
              | csubtype (DECABSTY path, DECMANTY tau') =
                  if eqType (TYNAME path, tau') then OK ()
                  else ERROR ("interface calls for type " ^ x ^
                                                       " to manifestly equal " ^
                              typeString tau' ^ ", but it is " ^ typeString (
                                                                   TYNAME path))
              | csubtype (DECMOD (_, m), DECMOD (_, m')) =
                  subtype (m, m')
              | csubtype (c, c') =
                  ERROR ("interface calls for " ^ x ^ " to be " ^ whatcomp c' ^
                         ", but implementation provides " ^ whatcomp c)
            (* type declarations for consistency checking *)
            val _ = op csubtype : declarable * declarable -> unit error
(* type declarations for consistency checking *)
val _ = op csubtype : declarable * declarable -> unit error
val _ = op subtype  : modty * modty -> unit error
        in  csubtype (c, c') >> OK (List.filter (fn (y, _) => y <> x) exports')
        end handle NotFound x => OK exports'

      fun st (MTARROW (args, res), MTARROW (args', res')) =
            unimp "subtyping on arrow modules"
        | st (MTARROW (args, _), _) =
            ERROR ("expected an exporting module but got one that takes " ^
                   countString args "parameter")
        | st (_, MTARROW (args, _)) =
            ERROR ("expected a module that takes " ^
                   countString args "parameter" ^
                                                ", but got an exporting module")
        | st (mt, MTALLOF mts') =
            allE (map (fn mt' => st (mt, mt')) mts')
        | st (mt, MTEXPORTS comps') =
            (removeExported (mt, comps') >>=
             (fn [] => OK ()
               | xcs =>
                  ERROR (
                    "an interface expected some components that are missing: " ^
                         separate ("", ", ")
                         (map (fn (x, c) => x ^ " (" ^ whatcomp c ^ ")") xcs))))
                                                                       (* XXX *)
      and removeExported (MTEXPORTS xcs, exports') =            
             foldl (fn (xc, e) => e >>= remove xc) (OK exports') xcs
        | removeExported (MTALLOF mts, exports') =
             foldl (fn (mt, e) => e >>= curry removeExported mt) (OK exports')
                                                                             mts
        | removeExported _ =
             raise BugInTypeChecking "bad case reached removeExported"
      
  in  st mts
  end
(* [[implements]] relation, based on [[subtype]] of two module types 748b *)
fun implements (p : path, submt, supermt) =
  (*  (app eprint ["At ", pathString p,
                   "\n  sub:  ", mtString submt, "\n  sup: ", mtString supermt,
                                                                   "\n"]; id) *)
  subtype (submt, realize (p, manifestTypes (p, submt), supermt))
(* path-expression lookup 748c *)
fun notModule (px, dcl) =
  raise TypeError ("looking for a module, but " ^ pathexString px ^
                   " is a " ^ whatcomp dcl)
fun pathfind (PNAME x, Gamma) = find (snd x, Gamma)
  | pathfind (PDOT (path, x), Gamma) =
      let (* definition of [[mtfind]] 749e *)
          fun mtfind (x, mt as MTEXPORTS comps) : declarable option =
                 (SOME (find (x, comps)) handle NotFound _ => NONE)
            | mtfind (x, MTARROW _) =
                 raise TypeError ("tried to select component " ^ x ^
                                  " from generic module " ^ pathexString path)
            | mtfind (x, mt as MTALLOF mts) =
                (case List.mapPartial (fn mt => mtfind (x, mt)) mts
                   of [comp] => SOME comp
                    | [] => NONE
                    | _ :: _ :: _ => unimp "component in multiple signatures")
          fun noComponent (path, x, mt) =
            raise TypeError ("module " ^ pathexString path ^
                                                 " does not have a component " ^
                             pathexString (PDOT (path, x)) ^ "; its type is " ^
                                                                    mtString mt)
          (* type declarations for consistency checking *)
          val _ = op mtfind : name *  modty -> declarable option
      in  case pathfind (path, Gamma)
            of DECMOD (_, mt) =>
                 (valOf (mtfind (x, mt)) handle Option =>
                   noComponent (path, x, mt))
             | comp =>
            (* tried to select [[path]].[[x]] but [[path]] is a [[comp]] 752a *)
                       raise TypeError ("Tried to select " ^ pathexString (PDOT
                                                         (path, x)) ^ ", but " ^
                                        pathexString path ^ " is " ^ whatcomp
                                                    comp ^ ", which does not " ^
                                        " have components")
      end
  | pathfind (PAPPLY (fpx, actualpxs) : pathex, Gamma) =
     (* specialization of module [[fpx]] to [[actualpxs]] 749a *)
     let fun taggedModule px = case pathfind (px, Gamma)
                                 of DECMOD (p, mt) => (p, mt)
                                  | dec => notModule (px, dec)
         val (fmod, actuals) = (taggedModule fpx, map taggedModule actualpxs)
         val (formals, result) = case fmod
                                   of (_, MTARROW fr) => fr
                                    | _ =>
                                 (* specialized exporting module [[fpx]] 749b *)
                                           raise TypeError ("module " ^
                       pathexString fpx ^ " is an exporting module, and only " ^

                                         " a generic module can be specialized")
         fun resty ([],                    [],                       result) =
                                                                          result
           | resty ((formalid, formalmt) :: formals, (actp, actmt) :: actuals,
                                                                       result) =
               let val theta = formalid |--> actp
                   fun fsubst (modid, mt) = (modid, mtsubst theta mt)
               in  case implements (actp, actmt, mtsubst theta formalmt)
                     of OK () => resty (map fsubst formals, actuals, mtsubst
                                                                   theta result)
                      | ERROR msg =>
                       (* can't pass [[actp]] as [[formalid]] to [[fpx]] 749c *)
                                     raise TypeError ("module " ^ pathString
                                         actp ^ " cannot be used as argument " ^
                                                      modidentString formalid ^
                                      " to generic module " ^ pathexString fpx ^
                                                      ": " ^ msg)
               end
           | resty _ = (* wrong number of arguments to [[fpx]] 749d *)
                       raise TypeError ("generic module " ^ pathexString fpx ^
                                                              " is expecting " ^
                                        countString formals "parameter" ^
                                                                  ", but got " ^
                                        countString actuals "actual parameter")
     in  DECMOD (PAPPLY (fst fmod, map fst actuals), resty (formals, actuals,
                                                                        result))
     end
(* type declarations for consistency checking *)
val _ = op pathfind   : pathex * declarable env -> declarable
(* translation of {\mcl} type syntax into types 750a *)
fun txpath (px, Gamma) =
  let fun tx (PAPPLY (f, args)) = PAPPLY (tx f, map tx args)
        | tx (PDOT (p, x)) = PDOT (tx p, x)
        | tx (PNAME (loc, m)) =
            let fun bad aThing =
                  raise TypeError ("I was expecting " ^ m ^
                                                     " to refer to a module, " ^
                                   "but at " ^ srclocString loc ^ ", it's " ^
                                                                         aThing)
            in  case find (m, Gamma)
                  of DECMODTY _ => bad "a module type"
                   | DECMOD (p, mt) => p
                   | c => bad (whatcomp c)
            end
  in  tx px
  end
(* type declarations for consistency checking *)
val _ = op txpath : pathex * declarable env -> path
(* translation of {\mcl} type syntax into types 750b *)
fun txty (t, Gamma) =
  let fun tx (TYNAME px) =
            (case pathfind (px, Gamma)
               of DECMANTY tau => tau
                | DECABSTY path => TYNAME path
                | comp => raise TypeError ("I was expecting a type, but " ^
                                           pathexString px ^ " is " ^ whatcomp
                                                                          comp))
        | tx (FUNTY (args, res)) = FUNTY (map tx args, tx res)
        | tx ANYTYPE = ANYTYPE
  in  tx t
  end
(* type declarations for consistency checking *)
val _ = op txty : tyex * declarable env -> ty
(* translation of {\mcl} type syntax into types 750c *)
fun findModty (x, Gamma) =
  case find (x, Gamma)
    of DECMODTY mt => mt
     | comp => raise TypeError ("Tried to use " ^ whatcomp comp ^ " " ^ x ^
                                " as a module type")
(* type declarations for consistency checking *)
val _ = op findModty : name * declarable env -> modty
(* translation of {\mcl} type syntax into types 751a *)
fun txModtype (path, mtx : modtyx, Gamma) =
  let val resultName = PNAME (MODTYPLACEHOLDER "functor result")
      fun tx (MTNAMEDX t) = mtsubst (MODTYPLACEHOLDER t |--> path) (findModty (t
                                                                       , Gamma))
        | tx (MTEXPORTSX exports) =
             let val (this', _) = foldl (leftLocated export) ([], Gamma) exports
             in  MTEXPORTS (rev this')
             end
        | tx (MTALLOFX mts) = allWithManifest (path, map (located tx) mts)
        | tx (MTARROWX (args, body)) =
            let fun txArrow ([], (loc, body), Gamma : declarable env) =
                      ([], atLoc loc txModtype (resultName, body, Gamma))
                  | txArrow (((mloc, m), (mtloc, mtx)) :: rest, body, Gamma) =
                      let val modid = genmodident m
                          val modty = atLoc mtloc txModtype (PNAME modid, mtx,
                                                                          Gamma)
                          val Gamma' = bind (m, DECMOD (PNAME modid, modty),
                                                                          Gamma)
                             (* XXX check 1st arg to DECMOD *)
                          val (rest', body') = txArrow (rest, body, Gamma')
                      in  ((modid, modty) :: rest', body')
                      end
            in  MTARROW (txArrow (args, body, Gamma))
            end

      and export ((x, ctx : declarablex), (theseDecls, Gamma)) =
            if isbound (x, theseDecls) then
              raise TypeError ("duplicate declaration of " ^ x ^
                                                              " in module type")
            else
              let val c = txDecl (PDOT (path, x), ctx, Gamma)
              in  ((x, c) :: theseDecls, bind (x, c, Gamma))
              end
(* type declarations for consistency checking *)
val _ = op txModtype : path * modtyx * declarable env -> modty
  in  tx mtx
  end
(* translation of {\mcl} type syntax into types 751b *)
and txDecl (path, comp : declarablex, Gamma : declarable env) : declarable =
  let fun ty t = txty (t, Gamma)
(* type declarations for consistency checking *)
val _ = op txDecl    : path * declarablex * declarable env -> declarable
  in  case comp
        of DECVALX tau  => DECVAL (ty tau)
         | DECABSTYX    => DECABSTY path
         | DECMANTYX t  => DECMANTY (ty t)
         | DECMODX mt   => DECMOD (unimp "id of module", txModtype (path, mt,
                                                                         Gamma))
         | DECMODTYX mt => DECMODTY (txModtype (path, mt, Gamma))
  end
val txModtype = fn a =>
  let val mt = txModtype a
  in  if mixedManifestations mt then
        raise BugInTypeChecking ("invariant violation (mixed M): " ^ mtString mt
                                                                               )
      else
        mt
  end
(* primitive modules and types used to type literal expressions 769 *)
val arraymodname = "Array"

val intmodident = genmodident "Int"
val symmodident = genmodident "Sym"
val boolmodident = genmodident "Bool"
val unitmodident = genmodident "Unit"
val arraymodident = genmodident arraymodname
val uarraymodident = genmodident "UnsafeArray"

val inttype = TYNAME (PDOT (PNAME intmodident, "t"))
val symtype = TYNAME (PDOT (PNAME symmodident, "t"))
val booltype = TYNAME (PDOT (PNAME boolmodident, "t"))
val unittype = TYNAME (PDOT (PNAME unitmodident, "t"))

fun arraytype tau =
  case tau
    of TYNAME (PDOT (module, "t")) =>
         TYNAME (PDOT (PAPPLY (PNAME arraymodident, [module]), "t"))
     | _ => raise InternalError "unable to form internal array type"


fun addValWith f ((x, v, ty), rho) = bind (x, f v, rho)
fun decval (x, v, ty) = (x, DECVAL ty)


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
(* primitives ((mcl)) 770a *)
fun eqPrintPrims tau strip =
  let val comptype = FUNTY ([tau, tau], booltype)
      fun comparison f = binaryOp (embedBool o (fn (x, y) => f (strip x, strip y
                                                                             )))
  in  ("similar?",  comparison op =,  comptype) ::
      ("dissimilar?",  comparison op =,  comptype) ::
      ("=",  comparison op =,  comptype) ::
      ("!=", comparison op <>, comptype) ::
      ("print", unaryOp (fn x => (print (valueString x);unitVal)), FUNTY ([tau],
                                                                   unittype)) ::
      ("println", unaryOp (fn x => (println (valueString x);unitVal)), FUNTY ([
                                                             tau], unittype)) ::
      []
  end

val symPrims =
  eqPrintPrims symtype (fn SYM s => s | _ => raise BugInTypeChecking
                                                        "comparing non-symbols")

val boolPrims =
  eqPrintPrims booltype (fn CONVAL (K, []) => K
                          | _ => raise BugInTypeChecking
                                                       "comparing non-Booleans")

(* primitives ((mcl)) 770b *)
fun comparison f = binaryOp (embedBool o f)
fun intcompare f = 
      comparison (fn (NUM n1, NUM n2) => f (n1, n2)
                   | _ => raise BugInTypeChecking "comparing non-numbers")

fun asInt (NUM n) = n
  | asInt _ = raise BugInTypeChecking "expected a number"

val arithtype = FUNTY ([inttype, inttype], inttype)
val comptype  = FUNTY ([inttype, inttype], booltype)


val intPrims = 
  ("+", arithOp op +,   arithtype) :: 
  ("-", arithOp op -,   arithtype) :: 
  ("*", arithOp op *,   arithtype) :: 
  ("/", arithOp op div, arithtype) ::

  ("negated", unaryOp (NUM o ~ o asInt), FUNTY ([inttype], inttype)) ::

  ("<",  intcompare op <,  comptype) :: 
  (">",  intcompare op >,  comptype) ::
  ("<=", intcompare op <=, comptype) :: 
  (">=", intcompare op >=, comptype) ::
  ("printu", unaryOp (fn n => (printUTF8 (asInt n); unitVal)), FUNTY ([inttype],
                                                                   unittype)) ::
  eqPrintPrims inttype (fn NUM n => n | _ => raise BugInTypeChecking
                                                        "comparing non-numbers")
(* primitives ((mcl)) 771 *)
local
  val arraypath = PNAME arraymodident
  val arrayarg  = genmodident "Elem"
  val argpath   = PNAME arrayarg
  val resultpath = PAPPLY (arraypath, [argpath])
  val elemtype   = TYNAME (PDOT (argpath, "t"))
  val arraytype  = TYNAME (PDOT (resultpath, "t"))


  fun protect f x = f x
    handle Size      => raise RuntimeError "array too big"
         | Subscript => raise RuntimeError "array index out of bounds"


  fun asArray (ARRAY a) = a
    | asArray _         = raise BugInTypeChecking "non-array value as array"
  fun arrayLeft f (a, x) = f (asArray a, x)
in
  val arrayPrims = 
    ("size", unaryOp (NUM o Array.length o asArray), FUNTY ([arraytype], inttype
                                                                           )) ::
    ("new", binaryOp (fn (NUM n, a) => ARRAY (protect Array.array (n, a))
                       | _ => raise BugInTypeChecking "array sizez not a number"
                                                                              ),
            FUNTY ([inttype, elemtype], arraytype)) ::
    ("at", binaryOp (fn (ARRAY a, NUM i) => protect Array.sub (a, i)
                      | _ => raise BugInTypeChecking "Array.at array or index"),
            FUNTY ([arraytype, inttype], elemtype)) ::
    ("at-put", fn [ARRAY a, NUM i, x] => (protect Array.update (a, i, x);
                                                                        unitVal)
                | _ => raise BugInTypeChecking
                                      "number or types of args to Array.at-put",
            FUNTY ([arraytype, inttype, elemtype], unittype)) ::
    []

  val arraymodtype : modty =
    MTARROW ([(arrayarg, MTEXPORTS [("t", DECABSTY (PDOT (argpath, "t")))]  :
                                                                        modty)],
             MTEXPORTS (("t", DECABSTY (PDOT (resultpath, "t"))) ::
                        ("elem", DECMANTY elemtype) ::
                        map decval arrayPrims) : modty)

  val uarrayPrims = 
    ("new", unaryOp (fn (NUM n) => ARRAY (protect Array.array (n, CONVAL (
                                                          "uninitialized", [])))
                       | _ => raise BugInTypeChecking "array size not a number")
                                                                               ,
            FUNTY ([inttype], arraytype)) ::
    []

  val uarraymodtype : modty =
    MTARROW ([(arrayarg, MTEXPORTS [("t", DECABSTY (PDOT (argpath, "t")))]  :
                                                                        modty)],
             MTEXPORTS (("t", DECABSTY (PDOT (resultpath, "t"))) ::
                        map decval uarrayPrims) : modty)
end
(* primitives ((mcl)) 1361a *)
fun inject_bool x =
      CONVAL (if x then "#t" else "#f", [])
fun project_bool (CONVAL ("#t", [])) = true
  | project_bool (CONVAL ("#f", [])) = false
  | project_bool _ = raise RuntimeError "projected non-boolean"

fun inject_predicate f = fn x => inject_bool (f x)
fun predop f = unaryOp (inject_predicate f)

fun comparison f = binaryOp (inject_predicate f)
fun intcompare f = comparison (
                     fn (NUM n1, NUM n2) => f (n1, n2)
                      | _ => raise BugInTypeChecking "integers expected")
(* type declarations for consistency checking *)
val _ = op inject_bool  : bool -> value
val _ = op project_bool : value -> bool

local
  fun module id primvals : declarable =
    DECMOD (PNAME id,
            MTEXPORTS (("t", DECABSTY (PDOT (PNAME id, "t"))) :: map decval
                                                                      primvals))
in
  val intmod  = module intmodident intPrims
  val symmod  = module symmodident symPrims
  val boolmod = module boolmodident boolPrims
  val unitmod = module unitmodident []
  val arraymod  = DECMOD (PNAME arraymodident, arraymodtype)
  val uarraymod  = DECMOD (PNAME uarraymodident, uarraymodtype)
end

(* utility functions on {\mcl} types 754a *)
fun firstArgType (x, FUNTY (tau :: _, _)) = OK tau
  | firstArgType (x, FUNTY ([], _)) =
      ERROR ("function " ^ x ^
                 " cannot be overloaded because it does not take any arguments")
  | firstArgType (x, _) =
      ERROR (x ^ " cannot be overloaded because it is not a function")

(* utility functions on {\mcl} types 754b *)
fun okOrTypeError (OK a) = a
  | okOrTypeError (ERROR msg) = raise TypeError msg

fun ok a = okOrTypeError a handle _ => raise InternalError
                                                      "overloaded non-function?"
fun resolveOverloaded (f, argty : ty, tys : ty list) : (ty * int) error =
  let fun findAt (tau :: taus, i) = if eqType (argty, ok (firstArgType (f, tau))
                                                                          ) then
                                      OK (tau, i)
                                    else
                                      findAt (taus, i + 1)
        | findAt ([], _) =
            ERROR ("cannot figure out how to resolve overloaded name " ^ f ^
                   " when applied to first argument of type " ^ typeString argty
                                                                               ^
                   " (resolvable: " ^ separate ("", ", ") (map typeString tys) ^
                                                                            ")")
  in  findAt (tys, 0)
  end
(* type declarations for consistency checking *)
val _ = op resolveOverloaded : name * ty * ty list -> (ty * int) error
(* [[typeof]] a {\mcl} expression ((prototype)) 754c *)
fun typeof (e, Gamma) : ty = raise LeftAsExercise "typeof"
(* principal type of a module 756b *)
fun strengthen (p, MTEXPORTS comps) =
      let fun comp (c as (x, dc)) =
            case dc
              of DECABSTY _ => (x, DECMANTY (TYNAME (PDOT (p, x))))
               | DECMOD (p, mt) =>
                   (x, DECMOD (p, strengthen (p, mt)))  (* XXX check me *)
               | DECVAL   _ => c
               | DECMANTY _ => c
               | DECOVLN  _ => c
               | DECMODTY _ => raise BugInTypeChecking
                                                      "module type as component"
      in  MTEXPORTS (map comp comps)
      end
  | strengthen (p, MTALLOF mts) =
      allWithManifest (p, map (fn mt => strengthen (p, mt)) mts)
  | strengthen (p, mt as MTARROW _) =
      mt
(* type declarations for consistency checking *)
val _ = op strengthen : path * modty -> modty
(* elaborate a {\mcl} definition 758a *)
fun declarableResponse c =
      case c
        of DECMODTY mt => mtString mt
         | DECVAL tau => typeString tau
         | DECABSTY _ => "abstract type"
         | DECMANTY _ => "manifest type"
         | DECMOD (_, mt) => mtString mt
         | DECOVLN _ => "overloaded name"
(* elaborate a {\mcl} definition 758c *)
fun printStrings ss _ vs = app print ss
type value_printer = (name -> ty -> value -> unit) -> value list -> unit

fun defResponse (x, c) =
  case c
    of DECVAL tau =>
             (fn printfun => fn [v] => (printfun x tau v; app print [" : ",
                                                                typeString tau])
                              | _ => raise InternalError
                                            "val definition not a single value")
     | DECABSTY path => if pathString path = x then
                          printStrings ["abstract type ", x]
                        else
                          printStrings ["type ", x, " = ", pathString path]
     | DECMANTY tau => printStrings ["type ", x, " = ", typeString tau]
     | DECMOD (_, mt as MTARROW _) => printStrings ["generic module ", x, " : ",
                                                                    mtString mt]
     | DECMOD (_, mt) =>  printStrings ["module ", x, " : ", mtString mt]
     | DECMODTY mt => printStrings ["module type ", x, " = ", mtString mt]
     | DECOVLN [] => raise InternalError "empty overloaded name"
     | DECOVLN (tau :: taus) =>
           printStrings ( "overloaded " :: x :: " : " :: typeString tau ::
                         map (fn t => "\n           " ^ x ^ " : " ^ typeString t
                                                                               )
                         taus)

(* elaborate a {\mcl} definition 759a *)
fun dataDefDeclarables (path, (T, vcons), Gamma) =
  let val tau = TYNAME (PDOT (path, T))
      val declaredty = DECABSTY (PDOT (path, T))
      val Gamma' = bind (T, DECABSTY (PDOT (path, T)), Gamma)
      fun translateVcon (K, tx) = (K, txty (tx, Gamma'))
            handle TypeError msg =>
              raise TypeError ("in type of value constructor " ^ K ^ ", " ^ msg)
      val Ktaus = map translateVcon vcons
      fun validate (K, FUNTY (_, result)) =
            if eqType (result, tau) then
              ()
            else
              raise TypeError ("value constructor " ^ K ^ " should return " ^
                                                                typeString tau ^
                 ", but it returns type " ^ typeString result)
        | validate (K, tau') =
            if eqType (tau', tau) then
              ()
            else
              raise TypeError ("value constructor " ^ K ^ " should have type " ^
                                                                typeString tau ^
                 ", but it has type " ^ typeString tau')
      val ()     = app validate Ktaus

  in  (T, declaredty) :: map (fn (x, tau) => (x, DECVAL tau)) Ktaus
  end
(* elaborate a {\mcl} definition 759b *)
fun addOverload (p, Gamma) =
  let val (tau, first) =
        case pathfind (p, Gamma)
          of DECVAL tau => (tau, okOrTypeError (firstArgType (pathexString p,
                                                                          tau)))
           | c => (* can't overload a [[c]] 765d *)
                  raise TypeError ("only functions can be overloaded, but " ^
                                                              whatcomp c ^ " " ^
                                   pathexString p ^ " is not a function")
      val x = plast p

      val currentTypes =
        (case find (x, Gamma)
           of DECOVLN vals => vals
            | _ => []) handle NotFound _ => []
      val newTypes = tau :: currentTypes
  in  bind (x, DECOVLN newTypes, Gamma)
  end
(* elaborate a {\mcl} definition 760 *)
fun declarable (context, d, Gamma) =
  let fun notcomponent what =
        raise TypeError (what ^ " cannot appear " ^ contextString context)
      (* definition of [[mtypeof]] 762a *)
      fun findModule (px, Gamma) =
        case pathfind (px, Gamma)
          of DECMOD (_, mt) => mt
           | comp => raise TypeError ("looking for a module, but " ^
                                                               pathexString px ^
                                      " is a " ^ whatcomp comp)

      fun mtypeof (path, m, Gamma) =
        let fun ty (MPATH p) = strengthen (txpath (p, Gamma), findModule (p,
                                                                         Gamma))
              | ty (MPATHSEALED (mtx, p)) = sealed (mtx, ty (MPATH p))
              | ty (MUNSEALED defs)       = principal defs
              | ty (MSEALED (mtx, defs))  = sealed (mtx, principal defs)
            and sealed (mtx, mt') =
                  let val mt = txModtype (path, mtx, Gamma)
                  in  case implements (path, mt', mt)
                        of OK () => mt
                         | ERROR msg => raise TypeError msg
                  end
            and principal ds =
              let fun defs ([],    Gamma, seen) = []
                    | defs ((loc, DATA dd)::ds, Gamma, seen) =
                        bindAll (atLoc loc dataDefDeclarables (path, dd, Gamma),
                                                                ds, Gamma, seen)
                    | defs ((loc, OVERLOAD ovls)::ds, Gamma, seen) =
                        let val Gamma' = foldl (atLoc loc addOverload) Gamma
                                                                            ovls
                        in  defs (ds, Gamma', seen)
                        end
                    | defs ((loc, d)::ds, Gamma, seen) =
                        bindAll ([atLoc loc declarable (INMODULE path, d, Gamma)
                                                             ], ds, Gamma, seen)
                  and bindAll (xcs, ds, Gamma, seen) =
                       let fun addBindings ([], Gamma, seen) = defs (ds, Gamma,
                                                                           seen)
                             | addBindings ((x, DECMODTY _) :: xcs, _, _) =
                                 raise TypeError "module type in module"
                             | addBindings ((x, c) :: xcs, Gamma, seen) =
                                 if false andalso member x seen then
                                                     (* maybe this is OK now? *)

                             (* duplicate definition of [[x]] in context 762b *)
                                   raise TypeError ("Redefinition of " ^ x ^
                                                         " in <unknown-module>")
                                 else
                                   (x, c) :: addBindings (xcs, bind (x, c, Gamma
                                                                   ), x :: seen)
                       in  addBindings (xcs, Gamma, seen)
                       end
              in  MTEXPORTS (defs (ds, Gamma, []))
              end
        in  ty m
        end
      (* type declarations for consistency checking *)
      val _ = op mtypeof : path * moddef * declarable env -> modty
  in  case d
        of MODULETYPE (name, mtx) => notcomponent ("a module type (like " ^ name
                                                                          ^ ")")
         | MODULE (name, mx) =>
             (name, DECMOD (contextDot (context, name),
                            mtypeof (contextDot (context, name), mx, Gamma)))
         | GMODULE (f, formals, body) =>
             let val fpath     = contextDot (context, f)
                 val idformals = map (fn (x, mtx) => (genmodident x, (x, mtx)))
                                                                         formals
                 val resultpath = PAPPLY (fpath, map (PNAME o fst) idformals)

                 fun addarg arg (args, res) = (arg :: args, res)

                 fun arrowtype ((mid : modident, (x, mtx)) :: rest, Gamma) =
                       let val mt = txModtype (PNAME mid, mtx, Gamma)
                           val Gamma' = bind (x, DECMOD (PNAME mid, mt), Gamma)
                       in  addarg (mid, mt) (arrowtype (rest, Gamma'))
                       end
                   | arrowtype ([], Gamma) = ([], mtypeof (resultpath, body,
                                                                         Gamma))
             in  (f, DECMOD (fpath, MTARROW (arrowtype (idformals, Gamma))))
             end       
                           
         | EXP e => notcomponent ("an expression (like " ^ expString e ^ ")")
         | QNAME px => notcomponent ("a qualified name (like " ^ pathexString px
                                                                          ^ ")")
         | DEFINE (name, tau, lambda as (formals, body)) =>
             let val funty = FUNTY (map (fn (n, ty) => ty) formals, tau)
             in  declarable (context, VALREC (name, funty, LAMBDA lambda), Gamma
                                                                               )
             end
         | VAL (name, e) =>
             (name, DECVAL (typeof (e, Gamma)))
         | VALREC (name, tau, e as LAMBDA _) =>
             let val tau    = txty (tau, Gamma)
                 val Gamma' = bind (name, DECVAL tau, Gamma)
                 val tau'   = typeof (e, Gamma')
             in  if not (eqType (tau, tau')) then
                   raise TypeError ("identifier " ^ name ^
                                    " is declared to have type " ^
                                    typeString tau ^ " but has actual type " ^
                                    typeString tau')
                 else
                   (name, DECVAL tau)
             end
         | VALREC (name, tau, _) =>
             raise TypeError ("(val-rec [" ^ name ^ " : " ^ tyexString tau ^
                             "] ...) must used (lambda ...) on right-hand side")
         | TYPE (t, tx) =>
             let val tau = txty (tx, Gamma)
             in  (t, DECMANTY tau)
             end
         | DATA _ => raise BugInTypeChecking
                                            "data definition reached declarable"
         | OVERLOAD _ => raise BugInTypeChecking
                                     "overloading definition reached declarable"
  end
(* type declarations for consistency checking *)
val _ = op declarable : context * baredef * declarable env -> name * declarable
(* type declarations for consistency checking *)
val _ = op printStrings : string list -> value_printer
val _ = op defResponse : name * declarable -> value_printer
(* elaborate a {\mcl} definition 761b *)
fun elabdef (d, Gamma) =
  case d
    of EXP e => elabdef (VAL ("it", e), Gamma)
     | MODULETYPE (x, mtx) =>
            let val c = DECMODTY (txModtype (PNAME (MODTYPLACEHOLDER x), mtx,
                                                                         Gamma))
            in  (x, bind (x, c, Gamma), defResponse (x, c))
            end
     | d => let val (x, c) = declarable (TOPLEVEL, d, Gamma)
            in  (x, bind (x, c, Gamma), defResponse (x, c))
            end
(* type declarations for consistency checking *)
type value_printer = value_printer
val _ = op elabdef : baredef * declarable env -> name * declarable env *
                                                                   value_printer


(*****************************************************************)
(*                                                               *)
(*   EVALUATION, TESTING, AND THE READ-EVAL-PRINT LOOP FOR \MCL  *)
(*                                                               *)
(*****************************************************************)

(* evaluation, testing, and the read-eval-print loop for \mcl 767 *)
(* definition of [[namedValueString]] for functional bridge languages 1263a *)
fun namedValueString x v =
  case v of CLOSURE _ => x
          | PRIMITIVE _ => x
          | _ => valueString v
(* type declarations for consistency checking *)
val _ = op namedValueString : name -> value -> string
(* definition of [[namedValueString]] for functional bridge languages 1324b *)
fun namedValueString x v =
  case v of CLOSURE ((_, MODEXP _), _) => "generic module " ^ x
          | CLOSURE _ => x
          | PRIMITIVE _ => x
          | MODVAL _ => "module " ^ x
          | _ => valueString v
(* type declarations for consistency checking *)
val _ = op namedValueString : name -> value -> string
(* definitions of [[matchRef]] and [[Doesn'tMatch]] ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
exception Doesn'tMatch    (* pattern-match failure *)
fun matchRef (CONPAT (k, ps), CONVAL (k', vs)) =
     if k = k' then
       disjointUnion (ListPair.mapEq matchConval (ps, vs))
     else
       raise Doesn'tMatch
  | matchRef (CONPAT _, _) = raise Doesn'tMatch
  | matchRef (WILDCARD, _) = emptyEnv
  | matchRef (PVAR x,   v) = bind (x, ref v, emptyEnv)
and matchConval (PVAR x, vref) = bind (x, vref, emptyEnv)
  | matchConval (p, ref v) = matchRef (p, v)
(* definitions of [[eval]] and [[evaldef]] for {\mcl} 1321b *)
val nullsrc : srcloc = ("translated name in LETRECX", ~1)

fun evalpath (p : pathex, rho) =
  let fun findpath (PNAME (srcloc, x)) = !(find (x, rho))
        | findpath (PDOT (p, x)) =
            (case findpath p
               of MODVAL comps => (!(find (x, comps))
                                   handle NotFound x =>
                                     raise BugInTypeChecking "missing component"
                                                                               )
                | _ => raise BugInTypeChecking "selection from non-module")
        | findpath (PAPPLY (f, args)) = apply (findpath f, map findpath args)
  in  findpath p
  end
and apply (PRIMITIVE prim, vs) = prim vs
  | apply (CLOSURE ((formals, body), rho_c), vs) = 
      (eval (body, bindList (formals, map ref vs, rho_c))
       handle BindListLength => 
         raise BugInTypeChecking ("Wrong number of arguments to closure; " ^
                                  "expected (" ^ spaceSep formals ^ ")"))
  | apply _ = raise BugInTypeChecking "applied non-function"
(* definitions of [[eval]] and [[evaldef]] for {\mcl} 1322a *)
and eval (e, rho : value ref env) =
  let val go = applyCheckingOverflow id in go end (* OMIT *)
  let fun ev (LITERAL n) = n
        (* more alternatives for [[ev]] for {\mcl} 1322b *)
        | ev (VAR p) = evalpath (p, rho)
        | ev (SET (n, e)) = 
            let val v = ev e
            in  find (n, rho) := v;
                unitVal
            end
        (* more alternatives for [[ev]] for {\mcl} 1322c *)
        | ev (VCONX c) = !(find (c, rho))
        | ev (CASE (LITERAL v, (p, e) :: choices)) =
            (let val rho' = matchRef (p, v)
             in  eval (e, extend (rho, rho'))
             end
             handle Doesn'tMatch => ev (CASE (LITERAL v, choices)))
        | ev (CASE (LITERAL v, [])) =
            raise RuntimeError ("'case' does not match " ^ valueString v)
        | ev (CASE (e, choices)) =
            ev (CASE (LITERAL (ev e), choices))
        (* more alternatives for [[ev]] for {\mcl} 1322d *)
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
        (* more alternatives for [[ev]] for {\mcl} 1322e *)
        | ev (LAMBDA (args, body)) = CLOSURE ((map (fn (x, ty) => x) args, body)
                                                                          , rho)
        (* more alternatives for [[ev]] for {\mcl} 1323a *)
        | ev (APPLY (f, args, ref i))  =
           let val fv =
                 if i < 0 then
                   ev f
                 else
                   case ev f
                     of ARRAY a =>
                          (Array.sub (a, i)
                           handle Subscript => raise BugInTypeChecking
                                                             "overloaded index")
                      | _ => raise BugInTypeChecking
                                                  "overloaded name is not array"
           in  case fv
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
           end
        (* more alternatives for [[ev]] for {\mcl} 1323b *)
        | ev (LETX (LET, bs, body)) =
            let val (names, values) = ListPair.unzip bs
            in  eval (body, bindList (names, map (ref o ev) values, rho))
            end
        | ev (LETX (LETSTAR, bs, body)) =
            let fun step ((x, e), rho) = bind (x, ref (eval (e, rho)), rho)
            in  eval (body, foldl step rho bs)
            end
        (* more alternatives for [[ev]] for {\mcl} 1323c *)
        | ev (LETRECX (bs, body)) =
            let val (lhss, values) = ListPair.unzip bs
                val names = map fst lhss
                val _ = errorIfDups ("bound name", names, "letrec")
                fun unspecified () = NUM 42
                val rho' = bindList (names, map (fn _ => ref (unspecified()))
                                                                    values, rho)
                val updates = map (fn (x, e) => (x, eval (e, rho'))) bs
            in  List.app (fn ((x, _), v) => find (x, rho') := v) updates; 
                eval (body, rho')
            end
        (* more alternatives for [[ev]] for {\mcl} 1323d *)
        | ev (MODEXP components) =
            let fun step ((x, e), (results', rho)) =
                  let val loc = ref (eval (e, rho))
                  in  ((x, loc) :: results', bind (x, loc, rho))
                  end
                val (results', _) = foldl step ([], rho) components
            in  MODVAL results'
            end
        (* more alternatives for [[ev]] for {\mcl} 1323e *)
        | ev (ERRORX es) =
            raise RuntimeError (spaceSep (map (valueString o ev) es))
        | ev (EXP_AT (loc, e)) = atLoc loc ev e
(* type declarations for consistency checking *)
val _ = op eval : exp * value ref env -> value
val _ = op ev   : exp                 -> value
  in  ev e
  end
(* definitions of [[eval]] and [[evaldef]] for {\mcl} 1324a *)
and defbindings (VAL (x, e), rho) =
      [(x, ref (eval (e, rho)))]
  | defbindings (VALREC (x, tau, e), rho) =
      let val this = ref (SYM "placedholder for val rec")
(* type declarations for consistency checking *)
val _ = op defbindings : baredef * value ref env -> (name * value ref) list
          val rho' = bind (x, this, rho)
          val v    = eval (e, rho')
          val _    = this := v
      in  [(x, this)]
      end
  | defbindings (EXP e, rho) = 
      defbindings (VAL ("it", e), rho)
  | defbindings (QNAME _, rho) = 
      []
  | defbindings (DEFINE (f, tau, lambda), rho) =
      defbindings (VALREC (f, tau, LAMBDA lambda), rho)
(* definitions of [[eval]] and [[evaldef]] for {\mcl} 1325a *)
  | defbindings (TYPE _, _) =
      []
  | defbindings (DATA (t, typed_vcons), rho) =
      let fun binding (K, tau) =
            let val v = case tau of FUNTY _ => PRIMITIVE (fn vs => CONVAL (K,
                                                                    map ref vs))
                                  | _ => CONVAL (K, [])
            in  (K, ref v)
            end
      in  map binding typed_vcons
      end
  | defbindings (MODULE (x, m), rho) =
      [(x, ref (evalmod (m, rho)))]
  | defbindings (GMODULE (f, formals, body), rho) =
      [(f, ref (CLOSURE ((map fst formals, modexp body), rho)))]
  | defbindings (MODULETYPE (a, _), rho) = 
      []

(* definitions of [[eval]] and [[evaldef]] for {\mcl} 1325b *)
  | defbindings (OVERLOAD ps, rho) = 
      let fun overload (p :: ps, rho) =
                let val x = plast p
                    val v = extendOverloadTable (x, evalpath (p, rho), rho)
                    val loc = ref (ARRAY v)
                in  (x, loc) :: overload (ps, bind (x, loc, rho))
                end
            | overload ([], rho) = []
      in  overload (ps, rho)
      end
(* definitions of [[eval]] and [[evaldef]] for {\mcl} 1325c *)
and extendOverloadTable (x, v, rho) =
  let val currentVals =
        (case find (x, rho)
           of ref (ARRAY a) => a
            | _ => Array.fromList [])
        handle NotFound _ => Array.fromList []
  in  Array.tabulate (1 + Array.length currentVals,
                      fn 0 => v | i => Array.sub (currentVals, i - 1))
  end

(* definitions of [[eval]] and [[evaldef]] for {\mcl} 1326a *)
and defexps (VAL (x, e)) = [(x, e)]
  | defexps (VALREC (x, tau, e)) = [(x, LETRECX ([((x, tau), e)], VAR (PNAME (
                                                                 nullsrc, x))))]
  | defexps (EXP e) =  [("it", e)]
  | defexps (QNAME _) = []
  | defexps (DEFINE (f, tau, lambda)) = defexps (VALREC (f, tau, LAMBDA lambda))
  | defexps (TYPE _) = []
  | defexps (DATA (t, typed_vcons)) = 
      let fun isfuntype (FUNTY _)         = true
            | isfuntype _                 = false
          fun vconExp (K, t) =
            let val v = if isfuntype t then
                          PRIMITIVE (fn vs => CONVAL (K, map ref vs))
                        else
                          CONVAL (K, [])
            in  (K, LITERAL v)
            end
      in  map vconExp typed_vcons
      end
  | defexps (MODULE (x, m)) = [(x, modexp m)]
  | defexps (GMODULE (f, formals, body)) =
      [(f, LAMBDA (map (fn (x, _) => (x, ANYTYPE)) formals, modexp body))]
  | defexps (MODULETYPE (a, _)) = []
  | defexps (OVERLOAD ovls) = unimp "overloadiang within generic module"

(* definitions of [[eval]] and [[evaldef]] for {\mcl} 1326b *)
and modexp (MPATH px)            = VAR px
  | modexp (MPATHSEALED (_, px)) = VAR px
  | modexp (MSEALED (_, defs))   = MODEXP ((List.concat o map (located defexps))
                                                                           defs)
  | modexp (MUNSEALED defs)      = MODEXP ((List.concat o map (located defexps))
                                                                           defs)


(* definitions of [[eval]] and [[evaldef]] for {\mcl} 1326c *)
and evalmod (MSEALED (_, ds), rho) = evalmod (MUNSEALED ds, rho)
  | evalmod (MPATH p, rho) = evalpath (p, rho)
  | evalmod (MPATHSEALED (mtx, p), rho) = evalpath (p, rho)
  | evalmod (MUNSEALED defs, rho) = MODVAL (rev (defsbindings (defs, rho)))

               (* XXX type checker should ensure there are no duplicates here *)
(* definitions of [[eval]] and [[evaldef]] for {\mcl} 1326d *)
and defsbindings ([],   rho) = []
  | defsbindings (d::ds, rho) =
      let val bs   = leftLocated defbindings (d, rho)
          val rho' = foldl (fn ((x, loc), rho) => bind (x, loc, rho)) rho bs
      in  bs @ defsbindings (ds, rho')
      end
(* definitions of [[eval]] and [[evaldef]] for {\mcl} 1327a *)
and evaldef (d, rho) =
  let fun single [(_, loc)] = ! loc
        | single _        = raise InternalError
                                             "wrong number of bindings from def"
(* type declarations for consistency checking *)
val _ = op evaldef : baredef * value ref env -> value ref env * value list
      val bindings = defbindings (d, rho)
      
      fun string (VAL (x, e))         = namedValueString x (single bindings)
        | string (VALREC (x, tau, e)) = namedValueString x (single bindings)
        | string (EXP _)              = valueString (single bindings)
        | string (QNAME px)           = raise InternalError
                                                          "NAME reached evaldef"
        | string (DEFINE (f, _, _))   = namedValueString f (single bindings)
        | string (TYPE (t, tau))      = "type " ^ t
        | string (DATA _) = unimp "DATA definitions"
        | string (GMODULE (f, _, _))= namedValueString f (single bindings)
        | string (MODULE (x, m))      = namedValueString x (single bindings)
        | string (MODULETYPE (a, _))  = "module type " ^ a
        | string (OVERLOAD ps)        = "overloaded names " ^ separate("", " ")
                                                                  (map plast ps)

      val rho' = foldl (fn ((x, loc), rho) => bind (x, loc, rho)) rho bindings
  in  (rho', map (! o snd) bindings)   (* 2nd component was (string d) *)
  end
(* elaboration and evaluation of [[data]] definitions for \mcl 763a *)
fun elabDataDef (context, (T, vcons), Gamma) =
  let val tau    = TYNAME (contextDot (context, T))
      val Gamma' = bind (T, DECMANTY tau, Gamma)
      fun translateVcon (K, tx) =
            (K, txty (tx, Gamma'))
            handle TypeError msg =>
              raise TypeError ("in type of value constructor " ^ K ^ ", " ^ msg)
      val Ktaus = map translateVcon vcons
        
      fun validate (K, FUNTY (_, result)) =
            if eqType (result, tau) then
              ()
            else 

            (* result type of [[K]] should be [[tau]] but is [[result]] 1360b *)
              raise TypeError ("value constructor " ^ K ^ " should return " ^
                                                                typeString tau ^
                               ", but it returns type " ^ typeString result)
        | validate (K, tau') =
            if eqType (tau', tau) then
              ()
            else 
              (* type of [[K]] should be [[tau]] but is [[tau']] 1360c *)
              raise TypeError ("value constructor " ^ K ^ " should have " ^
                                                                typeString tau ^
                              ", but it has type " ^ typeString tau')
      val ()     = app validate Ktaus
      val ()     = ()(*addVcons (mu, Ktaus)*)
                                  (* supports exhaustiveness anal. *) (* OMIT *)
      val Gamma'' = foldl (fn ((K, tau), G) => (bind (K, DECVAL tau, G))) Gamma'
                                                                           Ktaus
  in  (Gamma'', "*" :: map (typeString o snd) Ktaus)
  end
(* type declarations for consistency checking *)
val _ = op elabDataDef : context * data_def * declarable env -> declarable env *
                                                                     string list
(* elaboration and evaluation of [[data]] definitions for \mcl 763b *)
fun evalDataDef ((_, typed_vcons), rho) =
  let fun isfuntype (FUNTY _)         = true
        | isfuntype _                 = false
      fun addVcon ((K, t), rho) =
        let val v = if isfuntype t then
                      PRIMITIVE (fn vs => CONVAL (K, map ref vs))
                    else
                      CONVAL (K, [])
        in  bind (K, ref v, rho)
        end
(* type declarations for consistency checking *)
val _ = op evalDataDef : data_def * value ref env -> value ref env * string list
  in  (foldl addVcon rho typed_vcons, map fst typed_vcons)
  end
(* elaboration and evaluation of [[data]] definitions for \mcl 764a *)
type basis = declarable env * value ref env
fun processDataDef (dd, (Gamma, rho), interactivity) =
  let val (Gamma', tystrings) = elabDataDef (TOPLEVEL, dd, Gamma)
      val (rho', vcons)       = evalDataDef (          dd, rho)
      val _ = if prints interactivity then

       (* print the new type and each of its value constructors for \mcl 764b *)
                let val (T, _) = dd
                    val tau = (case find (T, Gamma')
                                 of DECMANTY tau => tau
                                  | _ => raise Match)
                              handle _ => raise InternalError
                                                        "datatype is not a type"
                    val (kind, vcon_types) =
                      case tystrings of s :: ss => (s, ss)
                                      | [] => let exception NoKindString in
                                                          raise NoKindString end
                in  ( println (typeString tau ^ " :: " ^ kind)
                    ; ListPair.appEq (fn (K, tau) => println (K ^ " : " ^ tau))
                                                             (vcons, vcon_types)
                    )
                end
              else
                ()
  in  (Gamma', rho')
  end
(* type declarations for consistency checking *)
val _ = op processDataDef : data_def * basis * interactivity -> basis
(* definitions of [[basis]] and [[processDef]] for \mcl 765a *)
fun processOverloading (ps, (Gamma, rho), interactivity) =
  let fun next (p, (Gamma, rho)) =
        let val (tau, first) =
              case pathfind (p, Gamma)
                of DECVAL tau => (tau, okOrTypeError (firstArgType (pathexString
                                                                       p, tau)))
                 | c => (* can't overload a [[c]] 765d *)
                        raise TypeError (
                   "only functions can be overloaded, but " ^ whatcomp c ^ " " ^
                                         pathexString p ^ " is not a function")
            val x = plast p

            val currentTypes =
              (case find (x, Gamma)
                 of DECOVLN vals => vals
                  | _ => []) handle NotFound _ => []
            val newTypes = tau :: currentTypes
            val Gamma' = bind (x, DECOVLN newTypes, Gamma)

            (************
            val currentVals =
              if null currentTypes then Array.fromList []
              else case find (x, rho)
                     of ref (ARRAY a) => a
                      | _ => raise BugInTypeChecking
                                                  "overloaded name is not ARRAY"
            val v = evalpath (p, rho)
            val newVals = Array.tabulate (1 + Array.length currentVals,
                                          fn 0 => v | i => Array.sub (
                                                            currentVals, i - 1))
            *****)
            val newVals = extendOverloadTable (x, evalpath (p, rho), rho)
            val rho' = bind (x, ref (ARRAY newVals), rho)

            val _ = if prints interactivity then
                      app print ["overloaded ", x, " : ", typeString tau, "\n"]
                    else
                      ()
        in  (Gamma', rho')
        end
  in  foldl next (Gamma, rho) ps
  end

(* definitions of [[basis]] and [[processDef]] for \mcl 766 *)
type basis = declarable env * value ref env
fun defmarker (MODULETYPE _) = " = "
  | defmarker (DATA _)       = ""
  | defmarker _              = " : "

fun processDef ((loc, DATA dd), (Gamma, rho), interactivity) =
      atLoc loc processDataDef (dd, (Gamma, rho), interactivity)
  | processDef ((loc, QNAME px), (Gamma, rho), interactivity) =
      let val c = pathfind (px, Gamma)
          val x = pathexString px
          val respond = println o concat
          fun response (DECVAL _) = raise InternalError
                                                       "DECVAL reached response"
            | response (DECABSTY path) =
                if pathString path = x then
                  ["abstract type ", x]
                else
                  ["type ", x, " = ", pathString path]
            | response (DECMANTY tau)                = ["type ", x, " = ",
                                                                 typeString tau]
            | response (DECMOD (_, mt as MTARROW _)) = ["generic module ", x,
                                                             " : ", mtString mt]
            | response (DECMOD (_, mt))              = ["module ", x, " : ",
                                                                    mtString mt]
            | response (DECMODTY mt)                 = ["module type ", x, " = "
                                                                  , mtString mt]
            | response (DECOVLN []) = raise InternalError
                                                         "empty overloaded name"
            | response (DECOVLN (tau :: taus)) =
                "overloaded " :: x :: " : " :: typeString tau ::
                map (fn t => "\n           " ^ x ^ " : " ^ typeString t) taus
                  
      val _ = if prints interactivity then
                case c
                  of DECVAL _ =>
                       ignore (processDef ((loc, EXP (VAR px)), (Gamma, rho),
                                                                 interactivity))
                   | _ =>
                       respond (response c)
              else
                ()
      in  (Gamma, rho)
      end
  | processDef ((loc, OVERLOAD ps), (Gamma, rho), interactivity) =
      atLoc loc processOverloading (ps, (Gamma, rho), interactivity)
  | processDef ((loc, d), (Gamma, rho), interactivity) =

    (* (app (fn (x, c) => app print [x, " is ", whatcomp c, "\n"]) Gamma; id) *)
      let val (_, Gamma, printer) = atLoc loc elabdef (d, Gamma)
          val (   rho,   vs)      = atLoc loc evaldef (d, rho)
          
          fun callPrintExp i v =
            APPLY (VAR (PNAME (loc, "print")), [LITERAL v], ref i)

          fun printfun x tau v =
            let val resolved = (case find ("print", Gamma)
                                  of DECOVLN taus => resolveOverloaded ("print",
                                                                      tau, taus)
                                   | _ => ERROR "no printer for tau")
                               handle NotFound _ => ERROR "'print' not found"
            in  case resolved
                  of OK (_, i) => ignore (eval (callPrintExp i v, rho))
                   | ERROR _ =>
                       case d
                         of EXP _ => print (valueString v)
                          | _ => case tau
                                   of FUNTY _ => print x
                                    | _       => print (valueString v)
            end

          val _ = if prints interactivity then
                    (printer printfun vs; print "\n")
                  else
                    ()
      in  (Gamma, rho)
      end
(* type declarations for consistency checking *)
val _ = op processDef : def * basis * interactivity -> basis
fun dump_names (Gamma, rho) = app (println o fst) Gamma (*OMIT*)

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
(*<definition of [[testIsGood]] for \mcl>*)
fun testIsGood _ = unimp "testIsGood"
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
(*   IMPLEMENTATIONS OF \MCL\ PRIMITIVES AND DEFINITION OF [[INITIALBASIS]] *)
(*                                                               *)
(*****************************************************************)

(* implementations of \mcl\ primitives and definition of [[initialBasis]] 768a *)
val intmodenv    = foldl (addValWith (ref o PRIMITIVE)) emptyEnv intPrims
val arraymodenv  = foldl (addValWith (ref o PRIMITIVE)) emptyEnv arrayPrims
val boolmodenv   = foldl (addValWith (ref o PRIMITIVE)) emptyEnv boolPrims
val unitmodenv = emptyEnv : value ref env
val symmodenv  = foldl (addValWith (ref o PRIMITIVE)) emptyEnv symPrims

val modules = 
  [ ("Int",  intmod,  MODVAL intmodenv)
  , ("Bool", boolmod, MODVAL boolmodenv)
  , ("Unit", unitmod, MODVAL unitmodenv)
  , ("Sym",  symmod,  MODVAL symmodenv)
  , (arraymodname,  arraymod,
     CLOSURE ((["Elem"], MODEXP (map (fn (x, f, _) => (x, LITERAL (PRIMITIVE f))
                                                                 ) arrayPrims)),
              emptyEnv))
  , ("UnsafeArray",  uarraymod,
     CLOSURE ((["Elem"], MODEXP (map (fn (x, f, _) => (x, LITERAL (PRIMITIVE f))
                                                                ) uarrayPrims)),
              emptyEnv))
  , ("ArrayCore",  arraymod,
     CLOSURE ((["Elem"], MODEXP (map (fn (x, f, _) => (x, LITERAL (PRIMITIVE f))
                                                                 ) arrayPrims)),
              emptyEnv))

  , ("#t", DECVAL booltype, CONVAL ("#t", []))
  , ("#f", DECVAL booltype, CONVAL ("#f", []))
  ]

fun addmod ((x, dbl, v), (Gamma, rho)) =
  (bind (x, dbl, Gamma), bind (x, ref v, rho))

val initialRho = bind (overloadTable, ref (ARRAY emptyOverloadTable), emptyEnv)

val initialBasis = foldl addmod (emptyEnv, initialRho) modules : basis

val initialBasis =
  let val predefinedTypes =
(* predefined {\mcl} types, functions, and modules, as strings (generated by a script) *)

                             [ "(type int  Int.t)"
                             , "(type bool Bool.t)"
                             , "(type unit Unit.t)"
                             , "(type sym  Sym.t)"
                             , "(module-type ARRAY"
                             , " (exports [abstype t]    ;; an array"
                             ,
                          "          [abstype elem] ;; one element of the array"
                             ,
                         "          [new  : (int elem -> t)]          ; creator"
                             ,
                        "          [size : (t -> int)]               ; observer"
                             ,
                        "          [at     : (t int -> elem)]        ; observer"
                             ,
                         "          [at-put : (t int elem -> unit)])) ; mutator"
                             ,
                "  ;; missing: copy, copy1, similar?, similar1?, print, println"
                             , "  ;;          foreach, foreach-index"
                             , "(module-type GENERIC-ARRAY"
                             , " ([Elem : (exports [abstype t])] --m->"
                             , "     (exports [abstype t]    ;; an array"
                             ,
                  "              [type elem Elem.t] ;; one element of the array"
                             ,
                     "              [new  : (int elem -> t)]          ; creator"
                             ,
                    "              [size : (t -> int)]               ; observer"
                             ,
                    "              [at     : (t int -> elem)]        ; observer"
                             ,
                    "              [at-put : (t int elem -> unit)]))) ; mutator"
                             , "(module IntArray (@m Array Int))"
                             , "(overload Int.+ Int.- Int.* Int./ Int.negated"
                             ,
                              "          Int.= Int.!= Int.< Int.> Int.<= Int.>="
                             , "          Int.print Int.println)"
                             ,
                             "(overload Bool.= Bool.!= Bool.print Bool.println)"
                             , "(overload Sym.= Sym.!= Sym.print Bool.println)"
                             , "(module-type VARIABLE-ARRAY"
                             , "  (exports [abstype t]"
                             , "           [abstype elem]"
                             , "           [new-from : (int -> t)] ; creator"
                             ,
                       "           [size : (t -> int)]               ; observer"
                             ,
                       "           [at     : (t int -> elem)]        ; observer"
                             ,
                          "           [at-put : (t int elem -> unit)] ; mutator"
                             , ""
                             , "           [lo     : (t -> int)]  ; observer"
                             , "           [nexthi : (t -> int)]  ; observer"
                             ,
                              "           [addlo  : (t elem -> unit)] ; mutator"
                             ,
                              "           [addhi  : (t elem -> unit)] ; mutator"
                             , "           [remlo  : (t -> elem)] ; mutator"
                             , "           [remhi  : (t -> elem)] ; mutator"
                             ,
                            "           [setlo  : (t int -> unit)]))  ; mutator"
                             , ""
                             , "(generic-module"
                             ,
"   [VariableArray : ([Elem : (exports [abstype t])] --m-> (allof VARIABLE-ARRAY"
                             ,
"                                                               (exports [type elem Elem.t])))]"
                             , "   (module A (@m Array Elem))"
                             , "   (module U (@m UnsafeArray Elem))"
                             ,
"   (record-module Rep ([elems : A.t] [low-index : int] [population : int] [low-stored : int]))"
                             , "   (type t Rep.t)"
                             , "   (type elem Elem.t)"
                             , ""
                             , "   (define t new-from ([i : int])"
                             , "     (Rep.make (U.new 3) i 0 0))"
                             , ""
                             ,
                             "   (define int size ([a : t]) (Rep.population a))"
                             , ""
                             , "   (define bool in-bounds? ([a : t] [i : int])"
                             , "     (if (>= i (Rep.low-index a))"
                             ,
                       "         (< (- i (Rep.low-index a)) (Rep.population a))"
                             , "         #f))"
                             , ""
                             ,
                             "   (define int internal-index ([a : t] [i : int])"
                             ,
                "     (let* ([k (+ (Rep.low-stored a) (- i (Rep.low-index a)))]"
                             ,
          "            [_ (when (< k 0) (error 'internal-error: 'array-index))]"
                             , "            [n (A.size (Rep.elems a))]"
                             , "            [idx (if (< k n) k (- k n))])"
                             , "       idx))"
                             , ""
                             , "   (define elem at ([a : t] [i : int])"
                             , "     (if (in-bounds? a i)"
                             ,
                            "         (A.at (Rep.elems a) (internal-index a i))"
                             , "         (error 'array-index-out-of-bounds)))"
                             , ""
                             ,
                         "   (define unit at-put ([a : t] [i : int] [v : elem])"
                             , "     (if (in-bounds? a i)"
                             ,
                      "         (A.at-put (Rep.elems a) (internal-index a i) v)"
                             , "         (error 'array-index-out-of-bounds)))"
                             , ""
                             ,
                         "   (define int lo     ([a : t]) (Rep.low-index a))   "
                             ,
     "   (define int nexthi ([a : t]) (+ (Rep.low-index a) (Rep.population a)))"
                             , ""
                             , "   (define unit maybe-grow ([a : t])"
                             , "     (when (>= (size a) (A.size (Rep.elems a)))"
                             , "       (let* ([n  (A.size (Rep.elems a))]"
                             ,
                             "              [n' (if (Int.= n 0) 8 (Int.* 2 n))]"
                             , "              [new-elems (U.new n')]"
                             , "              [start (lo a)]"
                             , "              [limit (nexthi a)]"
                             , "              [i 0]"
                             ,
              "              [_ (while (< start limit)      ; copy the elements"
                             ,
                        "                   (A.at-put new-elems i (at a start))"
                             , "                   (set i (+ i 1))"
                             , "                   (set start (+ start 1)))])"
                             , "         (Rep.set-elems! a new-elems)"
                             , "         (Rep.set-low-stored! a 0))))"
                             , ""
                             , "   (define unit addhi ([a : t] [v : elem])"
                             , "     (maybe-grow a)"
                             , "     (at-put a (nexthi a) v)"
                             ,
                        "     (Rep.set-population! a (+ (Rep.population a) 1)))"
                             , "     "
                             , "   (define unit addlo ([a : t] [v : elem])"
                             , "     (maybe-grow a)"
                             ,
                         "     (Rep.set-population! a (+ (Rep.population a) 1))"
                             ,
                         "     (Rep.set-low-index!  a (- (Rep.low-index a)  1))"
                             ,
                         "     (Rep.set-low-stored! a (- (Rep.low-stored a) 1))"
                             , "     (when (< (Rep.low-stored a) 0)"
                             ,
 "       (Rep.set-low-stored! a (+ (Rep.low-stored a) (A.size (Rep.elems a)))))"
                             , "     (at-put a (Rep.low-index a) v))"
                             , "     "
                             , "   (define elem remhi ([a : t])"
                             , "     (if (<= (Rep.population a) 0)"
                             , "         (error 'removal-from-empty-array)"
                             , "         (let* ([v (at a (- (nexthi a) 1))]"
                             ,
         "                [_ (Rep.set-population! a (- (Rep.population a) 1))])"
                             , "           v)))"
                             , ""
                             , "   (define elem remlo ([a : t])"
                             , "     (if (<= (Rep.population a) 0)"
                             , "         (error 'removal-from-empty-array)"
                             , "         (let* ([v (at a (lo a))]"
                             ,
          "                [_ (Rep.set-population! a (- (Rep.population a) 1))]"
                             ,
                       "                [_ (Rep.set-low-index! a (+ (lo a) 1))]"
                             ,
          "                [_ (Rep.set-low-stored! a (+ (Rep.low-stored a) 1))]"
                             ,
    "                [_ (when (Int.= (Rep.low-stored a) (A.size (Rep.elems a)))"
                             ,
                           "                       (Rep.set-low-stored! a 0))])"
                             , "           v)))"
                             , ""
                             , ""
                             , "   (define unit setlo ([a : t] [i : int])"
                             , "     (Rep.set-low-index! a i))"
                             , ""
                             , ")"
                             , "(val + Int.+)"
                             , "(val - Int.-)"
                             , "(val * Int.*)"
                             , "(val / Int./)"
                             , "(val < Int.<)"
                             , "(val <= Int.<=)"
                             , "(val > Int.>)"
                             , "(val >= Int.>=)"
                             , "(val = Int.=)"
                             , "(val != Int.!=)"
                             ,
                          "(define bool and ([b : bool] [c : bool]) (if b c b))"
                             ,
                          "(define bool or  ([b : bool] [c : bool]) (if b b c))"
                             ,
              "(define bool not ([b : bool])            (if b (= 1 0) (= 0 0)))"
                             ,
                    "(define int mod ([m : int] [n : int]) (- m (* n (/ m n))))"
                             , "(define int negated ([n : int]) (- 0 n))"
                             , "(generic-module"
                             ,
"   [Array : ([M : (exports (abstype t))] --m-> (allof ARRAY (exports (type elem M.t))))]"
                             , "   (module A (@m ArrayCore M))"
                             , "   (type t A.t)"
                             , "   (type elem M.t)"
                             , "   (val new A.new)"
                             , "   (val at A.at)"
                             , "   (val size A.size)"
                             , "   (val at-put A.at-put))"
                             , "(generic-module"
                             , "   [Ref : ([M : (exports (abstype t))] --m->"
                             , "                  (exports [abstype t]"
                             , "                           [new : (M.t -> t)]"
                             , "                           [!   : (t -> M.t)]"
                             ,
                         "                           [:=  : (t M.t -> unit)]))]"
                             , "  (module A (@m ArrayCore M))"
                             , "  (type t A.t)"
                             , "  (define t    new ([x : M.t])  (A.new 1 x))"
                             , "  (define M.t  !   ([cell : t]) (A.at cell 0))"
                             ,
               "  (define unit :=  ([cell : t] [x : M.t]) (A.at-put cell 0 x)))"
                              ] 
      val xdefs = stringsxdefs ("built-in types", predefinedTypes)
  in  readEvalPrintWith predefinedFunctionError (xdefs, initialBasis,
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
