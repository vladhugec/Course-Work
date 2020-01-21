(* Putting the pieces together                  *)
(*                                              *)
(* The uProlog interpreter is composed of these parts: \ *)
(* makenowebnotdef(left as an exercise)         *)
(* <upr.sml>=                                   *)


(*****************************************************************)
(*                                                               *)
(*   \FOOTNOTESIZE SHARED: NAMES, ENVIRONMENTS, STRINGS, ERRORS, PRINTING, INTERACTION, STREAMS, \&\ INITIALIZATION *)
(*                                                               *)
(*****************************************************************)

(* Each interpreter that is written in ML incorporates *)
(* all the following code chunks, some of which are *)
(* defined in \crefmlscheme.chap and some of which are *)
(* defined below.                               *)
(* <\footnotesize shared: names, environments, strings, errors, printing, interaction, streams, \&\ initialization>= *)
(* <for working with curried functions: [[id]], [[fst]], [[snd]], [[pair]], [[curry]], and [[curry3]]>= *)
fun id x = x
fun fst (x, y) = x
fun snd (x, y) = y
fun pair x y = (x, y)
fun curry  f x y   = f (x, y)
fun curry3 f x y z = f (x, y, z)
(* There are a variety of ways to create useful *)
(* functions in the [[f]] position. Many such functions *)
(* are Curried. Here are some of them.          *)
(* <boxed values 81>=                           *)
val _ = op fst    : ('a * 'b) -> 'a
val _ = op snd    : ('a * 'b) -> 'b
val _ = op pair   : 'a -> 'b -> 'a * 'b
val _ = op curry  : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
val _ = op curry3 : ('a * 'b * 'c -> 'd) -> ('a -> 'b -> 'c -> 'd)
(* Interlude: micro-Scheme in ML                *)
(*                                              *)
(* [*] \invisiblelocaltableofcontents[*]        *)
(*                                              *)
(* {epigram}Conversation with David R. Hanson, coauthor *)
(* of A Retargetable C Compiler: Design and     *)
(* Implementation\break\citep                   *)
(* hanson-fraser:retargetable:book.             *)
(*                                              *)
(*  \myblock=\wd0 =0.8=0pt                      *)
(*                                              *)
(*  to \myblock\upshapeHanson: C is a lousy language *)
(*  to write a compiler in.                     *)
(*                                              *)
(* {epigram} The interpreters in Chapters [->] through  *)
(* [->] are written in C. C has many properties to *)
(* recommend it: it is relatively small and simple; *)
(* it is widely known and widely supported; it has a *)
(* perspicuous cost model in which it is easy to *)
(* discover what is happening at the machine level; and *)
(* it provides pointer arithmetic, which makes it a very *)
(* good language in which to write a garbage collector. *)
(* But as we move to more complicated and more ambitious *)
(* bridge languages, C is less than ideal. In this and *)
(* succeeding chapters, I therefore present interpreters *)
(* written in the functional language Standard ML. *)
(*                                              *)
(* Standard ML is particularly well suited to symbolic *)
(* computing, especially functions that operate on *)
(* abstract-syntax trees. And an ML program can *)
(* illustrate connections between language design, *)
(* formal semantics, and implementations much more *)
(* clearly than a C program can. Some of the advantages *)
(* of ML are detailed in the sidebar on \cpageref *)
(* mlscheme.good-ml. In this chapter, and also in \cref *)
(* mlinterps.chap,lazyparse.chap, I develop     *)
(* infrastructure used to write interpreters in ML. *)
(* So that you can focus on the new infrastructure, *)
(* I apply it to a familiar language: I present another *)
(* interpreter for micro-Scheme.                *)
(*                                              *)
(* {sidebar}Helpful properties of the ML family of *)
(* languages [*]                                *)
(*                                              *)
(*  \advance\parsepby -5.5pt \advance\itemsepby *)
(*  -5.5pt                                      *)
(*   • ML is a safe language: there is no such thing as *)
(*  an unchecked run-time error, which means there *)
(*  are no bad behaviors that are entirely up to the *)
(*  programmer to avoid.                        *)
(*   • Like Scheme, ML is naturally polymorphic. *)
(*  Polymorphism simplifies everything. For example, *)
(*  unlike the C code in \crefrange             *)
(*  impcore.chapgcs.chap, our ML code uses one  *)
(*  representation of lists and one length function. *)
(*  The C code in \crefcinterps.chap defines three *)
(*  different types of streams, each with its own *)
(*  [[get]] function; the ML code in \cref      *)
(*  mlinterps.chap defines one type of stream and one *)
(*  [[streamGet]] function. As a final example, *)
(*  because Impcore and micro-Scheme use different *)
(*  environments, each has its own [[process_tests]] *)
(*  function for running unit tests. The ML version *)
(*  in \crefmlinterps.processTests is polymorphic in *)
(*  the environment or environments, and it is shared *)
(*  among all interpreters.                     *)
(*                                              *)
(*  These individual savings in code add up to a lot. *)
(*  More important, when code is polymorphic and is *)
(*  shared among interpreters, you can be certain *)
(*  that whatever that code implements is also the *)
(*  same among languages—polymorphism makes it easier *)
(*  for you to see what is the same and what is *)
(*  different.                                  *)
(*   • Unlike Scheme, ML guarantees internal consistency *)
(*  of polymorphic data structures. For example, *)
(*  if one element of a list is a value, every  *)
(*  element of that list is a value. Such a list can *)
(*  be passed to a polymorphic function like    *)
(*  [[length]], and the same list can be used in a *)
(*  context where only a list of values is      *)
(*  acceptable, like the implementation of function *)
(*  application in [[eval]]. And all of this happens *)
(*  without requiring any variable declarations or *)
(*  type annotations to be written in the code. *)
(*                                              *)
(*  If any of this talk of polymorphism mystifies *)
(*  you, don't worry; polymorphism in programming *)
(*  languages is an important topic in its own right. *)
(*  \creftypesys.chap introduces and defines    *)
(*  polymorphism; \crefml.chap shows how ML provides *)
(*  a limited form of polymorphism without type *)
(*  annotations; and \crefmcl.chap shows a form of *)
(*  polymorphism that makes type annotations    *)
(*  practical.                                  *)
(*   • Like Scheme, ML provides first-class, nested *)
(*  functions, and its initial basis contains useful *)
(*  higher-order functions. These functions help me *)
(*  simplify and clarify the code. For example, in C, *)
(*  running a list of unit tests back-to-front  *)
(*  requires two special-purpose functions: one for *)
(*  Impcore and one for micro-Scheme. In ML, I just *)
(*  use [[foldr]].                              *)
(*   • To detect and signal errors, ML provides *)
(*  exception handlers and exceptions. These    *)
(*  constructs are more flexible and easier to use *)
(*  then C's [[setjmp]] and [[longjmp]].        *)
(*   • Finally, least familiar but most important, *)
(*  ML provides native support for algebraic data *)
(*  types, which I use to represent both abstract *)
(*  syntax and values. These types provide value *)
(*  constructors like the [[IFX]] or [[APPLY]] used *)
(*  in previous chapters, but instead of [[switch]] *)
(*  statements, ML provides pattern matching. Using *)
(*  pattern matching enables me to write function *)
(*  definitions that look a lot like algebraic laws. *)
(*  Such definitions are easier to follow than  *)
(*  C code. A detailed explanation accompanies the *)
(*  definition of function [[valueString]] on \ *)
(*  cpagerefmlscheme.code.valueString.          *)
(*                                              *)
(*  Algebraic data types are explained in detail in \ *)
(*  crefadt.chap, which presents the bridge language *)
(*  uML. uML like micro-Scheme plus algebraic data *)
(*  types. Like all chapters in this book, \cref *)
(*  adt.chap presents both the use and the      *)
(*  implementation of algebraic data types.     *)
(*  To understand the implementation, you will want *)
(*  to understand implementations in \cref      *)
(*  typesys.chap,ml.chap, but to understand how *)
(*  algebraic data types are used, you could jump *)
(*  ahead and look at \crefadt.howto right now. *)
(*                                              *)
(* {sidebar}                                    *)
(*                                              *)
(* The micro-Scheme interpreter in this chapter has the *)
(* same structure as the interpreter in Chapter [->]; as *)
(* before, we have environments, abstract syntax, *)
(* primitives, an evaluator for expressions, an *)
(* evaluator for definitions, and a read-eval-print *)
(* loop. Many details are as similar as I can make them, *)
(* but many are not: I want my ML code to look like ML *)
(* and my C code to look like C; that matters more than *)
(* having either look like the other. You will have an *)
(* easier time reading the ML code if you know my *)
(* programming conventions.                     *)
(*                                              *)
(*   • Names of types are written in lowercase letters *)
(*  with words separated by underscores, like   *)
(*  [[exp]], [[def]], or [[unit_test]]. Names of *)
(*  functions and variables begin with lowercase *)
(*  letters, like [[eval]] or [[evaldef]], but long *)
(*  names may be written in ``camel case'' with a mix *)
(*  of uppercase and lowercase letters, like    *)
(*  [[processTests]] instead of the C-style     *)
(*  [[process_tests]]. (Rarely, I may use an    *)
(*  underscore in the name of a local variable.) *)
(*                                              *)
(*  Names of exceptions are capitalized, like   *)
(*  [[NotFound]] or [[RuntimeError]], and they use *)
(*  camel case. Names of value constructors, which *)
(*  identify alternatives in algebraic data types, *)
(*  are written in all capitals, possibly with  *)
(*  underscores, like [[IFX]], [[APPLY]], or    *)
(*  [[CHECK_EXPECT]] (just like enumeration literals *)
(*  in C).                                      *)
(*                                              *)
(*  These conventions are recommended by the SML'97 *)
(*  Standard Basis Library [cite gansner:basis]. *)
(*   • If you happen to be a seasoned ML programmer, *)
(*  you'll notice something missing: there are no *)
(*  modules. Avoiding modules is poor style, and it *)
(*  makes it impossible to take full advantage of the *)
(*  libraries that come with many implementations *)
(*  of ML, including Standard ML of New Jersey and *)
(*  Moscow ML. And without modules, there's no  *)
(*  sensible way to define abstract types. But the *)
(*  Standard ML module system is overly complicated *)
(*  and burdened with legacy features, and if I don't *)
(*  use it, you don't have to learn it. Avoiding *)
(*  modules gives you a good chance to digest this *)
(*  chapter even if your only previous experience *)
(*  with functional languages is your work with *)
(*  micro-Scheme in Chapter [->].               *)
(*                                              *)
(*  Because I don't use ML modules, I have no formal *)
(*  way to talk about interfaces and to distinguish *)
(*  interfaces from implementations. I work around *)
(*  this problem using a literate-programming trick: *)
(*  I put the types of functions and values, which is *)
(*  mostly what ML interfaces describe, in boxes *)
(*  preceding the implementations. This technique *)
(*  makes it possible to present an interface   *)
(*  formally just before I present its          *)
(*  implementation. The Noweb processor ensures that *)
(*  the material in the boxes is checked by the *)
(*  ML compiler.                                *)
(*                                              *)
(* A final aspect of my ML code is less a matter of *)
(* convention than of necessity: ML is persnickety about *)
(* the order in which definitions appear, and it has *)
(* miserable support for mutual recursion. These *)
(* properties are limitations that I have to work *)
(* around.                                      *)
(*                                              *)
(* The relevant differences between ML and C start with *)
(* syntactic categories: at top level, C has both *)
(* declarations and definitions, but ML has only *)
(* definitions. C's syntactic structure makes it *)
(* possible to be relatively careless about the order in *)
(* which things appear: declare all your structures *)
(* (probably in [[typedef]]s) in any order you like, and *)
(* you can define them in just about any order you like. *)
(* Then declare all your functions in any order you *)
(* like, and you can define them in any order you like. *)
(* Easy. Of course there are drawbacks: only a  *)
(* definition can initialize a variable, and global *)
(* variables can be initialized only in limited ways. *)
(* And it's too easy to define circular structures that *)
(* allow you to chase pointers forever—or into no-man's *)
(* land.                                        *)
(*                                              *)
(* ML's syntactic structure requires you to be careful *)
(* about the order in which things appear: the  *)
(* definition of a name may appear only after the *)
(* definitions of all the other names it refers to. *)
(* Of course there are benefits: every definition *)
(* initializes its name, and initialization may use any *)
(* valid expression, including [[let]] expressions with *)
(* nested definitions. So it is impossible to define *)
(* circular structures that allow you to chase pointers *)
(* forever; as a consequence, any structurally recursive *)
(* function is guaranteed to terminate. ML's designers *)
(* thought this guarantee was more important than the *)
(* convenience of writing definitions in many orders. *)
(* (And to be fair, using ML modules makes it relatively *)
(* convenient to get things in the right order.) In this *)
(* book, groups of related definitions are put into *)
(* Noweb code chunks like [[<<support for names and *)
(* environments>>]]. I carefully stick together larger *)
(* and larger chunks until eventually I wind up with a *)
(* complete interpreter in a chunk like         *)
(* [[]].                                        *)
(*                                              *)
(* What about mutual recursion? Suppose for example, *)
(* that type [[exp]] refers to [[value]] and type *)
(* [[value]] refers to [[exp]]? Mutually recursive *)
(* definitions like [[exp]] and [[value]] are written *)
(* together, adjacent in the source code, connected with *)
(* the keyword [[and]]. (You won't see [[and]] often, *)
(* but when you do, please remember this: [[and]] always *)
(* means mutual recursion, never any kind of Boolean *)
(* operation.) Mutually recursive function definitions *)
(* provide more options: you can join them with [[and]], *)
(* but it is usually more convenient and more idiomatic *)
(* to nest one inside the other using a [[let]] *)
(* binding—you would use [[and]] only when both mutually *)
(* recursive functions need to be called by some third, *)
(* client function. You can also make functions mutually *)
(* recursive by passing one to another as a parameter. [ *)
(* You can use the parameter trick with types as well, *)
(* but the technique, which is called ``two-level *)
(* types,'' \cite{sheard:generic}, is beyond the scope *)
(* of this book.] Wherever I use mutual recursion, I say *)
(* what technique I use. Now, on to the code!   *)
(*                                              *)
(* Names and environments                       *)
(*                                              *)
(* In my C code, names are an abstract type, and two *)
(* names are equal if any only if they are the same *)
(* pointer, so I can compare them using C's built-in [[= *)
(* =]] operator. In ML, strings are immutable and can be *)
(* meaningfully compared using ML's built-in [[=]] *)
(* operator, so I choose to represent names as strings. *)
(* \mlslabelname                                *)
(* <support for names and environments>=        *)
type name = string
(* The [[type]] syntax here is like C's [[typedef]], it *)
(* defines a type by type abbreviation.         *)

(* <support for names and environments>=        *)
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
(* In the code, function [[find]] is closely related to *)
(* the [[find]] from \crefscheme.chap: it returns *)
(* whatever is in the environment, which has type [['a]] *)
(* and not type [[Value *]]. But [[bind]] and   *)
(* [[bindList]] are more loosely related to \cref *)
(* scheme.chap's [[bindalloc]] and [[bindalloclist]]: *)
(* although the ML versions add bindings, they do not *)
(* allocate. \stdbreak (The phrases in the box are *)
(* adapted from declarations that appear in the *)
(* interfaces to ML modules; through some Noweb hackery, *)
(* the declarations are checked by the ML compiler.) \ *)
(* mlsflabelbindList,find,bind \mlslabelenv     *)
(* <boxed values 1>=                            *)
val _ = op emptyEnv : 'a env
val _ = op find     : name * 'a env -> 'a
val _ = op bind     : name      * 'a      * 'a env -> 'a env
val _ = op bindList : name list * 'a list * 'a env -> 'a env
(* <support for names and environments>=        *)
fun duplicatename [] = NONE
  | duplicatename (x::xs) =
      if List.exists (fn x' => x' = x) xs then
        SOME x
      else
        duplicatename xs
(* As in \crefscheme.chap, duplicate names are treated *)
(* as run-time errors. If a name x occurs more than *)
(* twice on a list, function [[duplicatename]] returns \ *)
(* monoSOME x; otherwise it returns [[NONE]].   *)
(* <boxed values 8>=                            *)
val _ = op duplicatename : name list -> name option
(* Error detection and signaling                *)
(*                                              *)
(* Every run-time error is signaled by raising the *)
(* [[RuntimeError]] exception, which carries an error *)
(* message.                                     *)
(* <support for detecting and signaling errors detected at run time>= *)
exception RuntimeError of string (* error message *)
(* <support for detecting and signaling errors detected at run time>= *)
fun errorIfDups (what, xs, context) =
  case duplicatename xs
    of NONE   => ()
     | SOME x => raise RuntimeError (what ^ " " ^ x ^ " appears twice in " ^
                                                                        context)
(* Function [[errorIfDups]] raises the exception if a *)
(* duplicate name is found. Parameter [[what]] says what *)
(* kind of name we're looking at, and [[context]] says *)
(* in what context.                             *)
(* <boxed values 9>=                            *)
val _ = op errorIfDups : string * name list * string -> unit
(* Some errors might be caused not by a fault in *)
(* micro-Scheme code but in my implementation of *)
(* micro-Scheme. For those times, there's the   *)
(* [[InternalError]] exception.                 *)
(* <support for detecting and signaling errors detected at run time>= *)
exception InternalError of string (* bug in the interpreter *)
(* <list functions not provided by \sml's initial basis>= *)
fun zip3 ([], [], []) = []
  | zip3 (x::xs, y::ys, z::zs) = (x, y, z) :: zip3 (xs, ys, zs)
  | zip3 _ = raise ListPair.UnequalLengths

fun unzip3 [] = ([], [], [])
  | unzip3 (trip::trips) =
      let val (x,  y,  z)  = trip
          val (xs, ys, zs) = unzip3 trips
      in  (x::xs, y::ys, z::zs)
      end
(* Standard ML's list-reversal function is called *)
(* [[rev]], but in this book I use [[reverse]]. *)
(* <list functions not provided by \sml's initial basis>= *)
val reverse = rev
(* <list functions not provided by \sml's initial basis>= *)
fun optionList [] = SOME []
  | optionList (NONE :: _) = NONE
  | optionList (SOME x :: rest) =
      (case optionList rest
         of SOME xs => SOME (x :: xs)
          | NONE    => NONE)
(* Reusable utility functions                   *)
(*                                              *)
(* This section includes small utility functions for *)
(* printing, for manipulating automatically generated *)
(* names, and for manipulating sets.            *)
(*                                              *)
(* Utility functions for printing               *)
(*                                              *)
(* For writing values and other information to standard *)
(* output, Standard ML provides a simple [[print]] *)
(* primitive, which writes a string. Anything more *)
(* sophisticated, such as writing to standard error, *)
(* requires using the the [[TextIO]] module, which is *)
(* roughly analogous to C's [[<stdio.h>]]. Using *)
(* [[TextIO]] can be awkward, so I define three *)
(* convenience functions. Function [[println]] is like *)
(* [[print]], but writes a string followed by a newline. *)
(* Functions [[eprint]] and [[eprintln]] are analogous *)
(* to [[print]] and [[println]], but they write to *)
(* standard error. It would be nice to be able to define *)
(* more sophisticated printing functions like the ones *)
(* in \secrefsec:print-interface on page [->], but *)
(* making such functions type-safe requires code that *)
(* beginning ML programmers would find baffling. *)
(* <utility functions for string manipulation and printing>= *)
fun println  s = (print s; print "\n")
fun eprint   s = TextIO.output (TextIO.stdErr, s)
fun eprintln s = (eprint s; eprint "\n")
(* CLOSING IN ON CHECK-PRINT:                   *)
(* <utility functions for string manipulation and printing>= *)
val xprinter = ref print
fun xprint   s = !xprinter s
fun xprintln s = (xprint s; xprint "\n")
(* <utility functions for string manipulation and printing>= *)
fun tryFinally f x post =
  (f x handle e => (post (); raise e)) before post ()

fun withXprinter xp f x =
  let val oxp = !xprinter
      val ()  = xprinter := xp
  in  tryFinally f x (fn () => xprinter := oxp)
  end
(* <utility functions for string manipulation and printing>= *)
fun bprinter () =
  let val buffer = ref []
      fun bprint s = buffer := s :: !buffer
      fun contents () = concat (rev (!buffer))
  in  (bprint, contents)
  end
(* To help you diagnose problems that may arise if you *)
(* decide to implement type checking, type inference, or *)
(* large integers, I also provide a function for *)
(* reporting errors that are detected while reading *)
(* predefined functions. [*]                    *)
(* <utility functions for string manipulation and printing>= *)
fun predefinedFunctionError s = eprintln ("while reading predefined functions, "
                                                                            ^ s)
(* <utility functions for string manipulation and printing>= *)
fun intString n =
  String.map (fn #"~" => #"-" | c => c) (Int.toString n)
(* Plurals!                                     *)
(* <utility functions for string manipulation and printing>= *)
fun plural what [x] = what
  | plural what _   = what ^ "s"

fun countString xs what =
  intString (length xs) ^ " " ^ plural what xs
(* <utility functions for string manipulation and printing>= *)
fun separate (zero, sep) = 
  (* list with separator *)
  let fun s []     = zero
        | s [x]    = x
        | s (h::t) = h ^ sep ^ s t
  in  s
end
val spaceSep = separate ("", " ")   (* list separated by spaces *)
val commaSep = separate ("", ", ")  (* list separated by commas *)
(* Standard ML's built-in support for converting *)
(* integers to strings uses the [[ ]] character as a *)
(* minus sign. We want the hyphen.              *)
(* <boxed values 28>=                           *)
val _ = op intString : int -> string
(* Lists! Functions [[spaceSep]] and [[commaSep]] are *)
(* special cases of the more general function   *)
(* [[separate]].                                *)
(* <boxed values 28>=                           *)
val _ = op spaceSep :                    string list -> string
val _ = op commaSep :                    string list -> string
val _ = op separate : string * string -> string list -> string
(* Here's how we print Unicode characters.      *)
(* <utility functions for string manipulation and printing>= *)
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
(* Utility functions for renaming variables     *)
(*                                              *)
(* In the theory of programming languages, it's fairly *)
(* common to talk about fresh names, where ``fresh'' *)
(* means ``different from any name in the program or its *)
(* environment.'' And if you implement a type checker *)
(* for a polymorphic language like Typed uScheme, or if *)
(* you implement type inference, or if you ever *)
(* implement the lambda calculus, you will need code *)
(* that generates fresh names. \stdbreak You can always *)
(* try names like [[t1]], [[t2]], and so on. But if you *)
(* want to debug, it's usually helpful to relate the *)
(* fresh name to a name already in the program. I like *)
(* to do this by tacking on a numeric suffix; for *)
(* example, to get a fresh name that's like [[x]], *)
(* I might try [[x-1]], [[x-2]], and so on. \stdbreak *)
(* But if the process iterates, I don't want to generate *)
(* a name like [[x-1-1-1]]; I'd much rather generate *)
(* [[x-3]]. This utility function helps by stripping off *)
(* any numeric suffix to recover the original [[x]]. *)
(* <utility functions for string manipulation and printing>= *)
fun stripNumericSuffix s =
      let fun stripPrefix []         = s   (* don't let things get empty *)
            | stripPrefix (#"-"::[]) = s
            | stripPrefix (#"-"::cs) = implode (reverse cs)
            | stripPrefix (c   ::cs) = if Char.isDigit c then stripPrefix cs
                                       else implode (reverse (c::cs))
      in  stripPrefix (reverse (explode s))
      end
(* \stdbreak                                    *)
(*                                              *)
(* Utility functions for sets, collections, and lists *)
(*                                              *)
(* Quite a few analyses of programs, including a type *)
(* checker in \creftypesys.chap and the type inference *)
(* in \crefml.chap, need to manipulate sets of  *)
(* variables. In small programs, such sets are usually *)
(* small, so I provide a simple implementation that *)
(* represents a set using a list with no duplicate *)
(* elements. It's essentially the same implementation *)
(* that you see in micro-Scheme in \crefscheme.chap. [ *)
(* The~\ml~types of the set operations include type *)
(* variables with double primes, like~[[''a]]. The type *)
(* variable~[[''a]] can be instantiated only with an *)
(* ``equality type.'' Equality types include base types *)
(* like strings and integers, as well as user-defined *)
(* types that do not contain functions. Functions \emph *)
(* {cannot} be compared for equality.]          *)

(* Representing error outcomes as values        *)
(*                                              *)
(* When an error occurs, especially during evaluation, *)
(* the best and most convenient thing to do is often to *)
(* raise an ML exception, which can be caught in a *)
(* handler. But it's not always easy to put a handler *)
(* exactly where it's needed to make the control *)
(* transfer work out the way it should. If you need to *)
(* get the code right, sometimes it's better to *)
(* represent an error outcome as a value. Like any other *)
(* value, such a value can be passed and returned until *)
(* it reaches a place where a decision is made. *)
(*                                              *)
(*   • When representing the outcome of a unit test, an *)
(*  error means failure for [[check-expect]] but *)
(*  success for [[check-error]]. Rather than juggle *)
(*  ``exception'' versus ``non-exception,'' I treat *)
(*  both outcomes on the same footing, as values. *)
(*  Successful evaluation to produce bridge-language *)
(*  value v is represented as ML value \monoOK v. *)
(*  Evaluation that signals an error with message m *)
(*  is represented as ML value \monoERROR m.    *)
(*  Constructors [[OK]] and [[ERROR]] are the value *)
(*  constructors of the algebraic data type     *)
(*  [[error]], defined here:                    *)
(* <support for representing errors as \ml\ values>= *)
datatype 'a error = OK of 'a | ERROR of string
(* <support for representing errors as \ml\ values>= *)
infix 1 >>=
fun (OK x)      >>= k  =  k x
  | (ERROR msg) >>= k  =  ERROR msg
(* Sometimes we need to zip together three lists of *)
(* equal length.                                *)
(* <boxed values 32>=                           *)
val _ = op zip3   : 'a list * 'b list * 'c list -> ('a * 'b * 'c) list
val _ = op unzip3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
(* <boxed values 32>=                           *)
val _ = op optionList : 'a option list -> 'a list option
(* What if we have a function [[f]] that could return *)
(* an [['a]] or an error, and another function [[g]] *)
(* that expects an [['a]]? Standard function composition *)
(* and the expression \monoboxg (f x) don't exactly make *)
(* sense, but the idea of composition is good. This form *)
(* of composition poses a standard problem, and it has a *)
(* standard solution. The solution relies on a  *)
(* sequencing operator written [[>>=]], which uses a *)
(* special form of continuation-passing style. (The *)
(* [[>>=]] operator is traditionally called ``bind,'' *)
(* but you might wish to pronounce it ``and then.'') *)
(* The idea is that we apply [[f]] to [[x]], and if the *)
(* result is [[OK y]], we can continue by applying [[g]] *)
(*  to [[y]]. But if the result of applying [[(f x)]] is *)
(* an error, that error is the result of the whole *)
(* computation. The [[>>=]] operator sequences the *)
(* possibly erroneous result [[(f x)]] with the *)
(* continuation [[g]], so where we might wish to write \ *)
(* monoboxg (f x), we instead write             *)
(*                                              *)
(*  [[f x >>= g]].                              *)
(*                                              *)
(* In the definition of [[>>=]], I write the second *)
(* function as [[k]], not [[g]], because [[k]] is *)
(* traditional for a continuation.              *)
(* <boxed values 32>=                           *)
val _ = op >>= : 'a error * ('a -> 'b error) -> 'b error
(* A very common special case occurs when the   *)
(* continuation always succeeds; that is, the   *)
(* continuation [[k']] has type \monobox'a -> 'b instead *)
(* of \monobox'a -> b error. In this case, the execution *)
(* plan is that when [[(f x)]] succeeds, continue by *)
(* applying [[k']] to the result; otherwise propagate *)
(* the error. I know of no standard way to write this *)
(* operator, [Haskell uses [[flip fmap]].] , so I use  *)
(* [[>>=+]], which you might also choose to pronounce *)
(* ``and then.''                                *)

(* <support for representing errors as \ml\ values>= *)
infix 1 >>=+
fun e >>=+ k'  =  e >>= (OK o k')
(* <boxed values 33>=                           *)
val _ = op >>=+ : 'a error * ('a -> 'b) -> 'b error
(* <support for representing errors as \ml\ values>= *)
fun errorList es =
  let fun cons (OK x, OK xs) = OK (x :: xs)
        | cons (ERROR m1, ERROR m2) = ERROR (m1 ^ "; " ^ m2)
        | cons (ERROR m, OK _) = ERROR m
        | cons (OK _, ERROR m) = ERROR m
  in  foldr cons (OK []) es
  end
(* Sometimes we map an error-producing function over a *)
(* list of values to get a list of [['a error]] results. *)
(* Such a list is hard to work with, and the right thing *)
(* to do with it is to convert it to a single value \ *)
(* stdbreak that's either an [['a list]] or an error. *)
(* I call the conversion operation [[errorList]]. [ *)
(* Haskell calls it [[sequence]].] I implement it by *)
(* folding over the list of possibly erroneous results, *)
(* concatenating all error messages.            *)
(* <boxed values 34>=                           *)
val _ = op errorList : 'a error list -> 'a list error
(* A reusable read-eval-print loop              *)
(*                                              *)
(* [*] Functions [[eval]] and [[evaldef]] process *)
(* expressions and true definitions. But an interpreter *)
(* for micro-Scheme also has to process the extended *)
(* definitions [[USE]] and [[TEST]], which need more *)
(* tooling:                                     *)
(*                                              *)
(*   • To process a [[USE]], we must be able to parse *)
(*  definitions from a file and enter a         *)
(*  read-eval-print loop recursively.           *)
(*   • To process a [[TEST]] (like [[check_expect]] or *)
(*  [[check_error]]), we must be able to run tests, *)
(*  and to run a test, we must call [[eval]].   *)
(*                                              *)
(* A lot of the tooling can be shared among more than *)
(* one bridge language. To make sharing easy,   *)
(* I introduce some abstraction.                *)
(*                                              *)
(*   • Type [[basis]], which is different for each *)
(*  bridge language, stands for the collection of *)
(*  environment or environments that are used at top *)
(*  level to evaluate a definition. The name basis *)
(*  comes from The Definition of Standard ML \citep *)
(*  milner:definition-revised.                  *)
(*                                              *)
(*  For micro-Scheme, a [[basis]] is a single   *)
(*  environment that maps each name to a mutable *)
(*  location holding a value. For Impcore, a    *)
(*  [[basis]] would include both global-variable and *)
(*  function environments. And for later languages *)
(*  that have static types, a [[basis]] includes *)
(*  environments that store information about types. *)
(*   • Function [[processDef]], which is different for *)
(*  each bridge language, takes a [[def]] and a *)
(*  [[basis]] and returns an updated [[basis]]. *)
(*  For micro-Scheme, [[processDef]] just evaluates *)
(*  the definition, using [[evaldef]]. For languages *)
(*  that have static types (Typed Impcore, Typed *)
(*  uScheme, and \nml in \creftuscheme.chap,ml.chap, *)
(*  among others), [[processDef]] includes two  *)
(*  phases: type checking followed by evaluation. *)
(*                                              *)
(*  Function [[processDef]] also needs to be told *)
(*  about interaction, which has two dimensions: *)
(*  input and output. On input, an interpreter may or *)
(*  may not prompt:                             *)
(* <type [[interactivity]] plus related functions and value>= *)
datatype input_interactivity = PROMPTING | NOT_PROMPTING
(* On output, an interpreter may or may not show a *)
(* response to each definition.                 *)

(* <type [[interactivity]] plus related functions and value>= *)
datatype output_interactivity = PRINTING | NOT_PRINTING
(* <type [[interactivity]] plus related functions and value>= *)
type interactivity = 
  input_interactivity * output_interactivity
val noninteractive = 
  (NOT_PROMPTING, NOT_PRINTING)
fun prompts (PROMPTING,     _) = true
  | prompts (NOT_PROMPTING, _) = false
fun prints (_, PRINTING)     = true
  | prints (_, NOT_PRINTING) = false
(* <boxed values 20>=                           *)
type interactivity = interactivity
val _ = op noninteractive : interactivity
val _ = op prompts : interactivity -> bool
val _ = op prints  : interactivity -> bool
(* <simple implementations of set operations>=  *)
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
(* <boxed values 29>=                           *)
type 'a set = 'a set
val _ = op emptyset : 'a set
val _ = op member   : ''a -> ''a set -> bool
val _ = op insert   : ''a     * ''a set  -> ''a set
val _ = op union    : ''a set * ''a set  -> ''a set
val _ = op inter    : ''a set * ''a set  -> ''a set
val _ = op diff     : ''a set * ''a set  -> ''a set
(* <collections with mapping and combining functions>= *)
datatype 'a collection = C of 'a set
fun elemsC (C xs) = xs
fun singleC x     = C [x]
val emptyC        = C []
(* [*] In the functions above, a set has the same *)
(* representation as a list, and they can be used *)
(* interchangeably. Sometimes, however, the thing you're *)
(* collecting is itself a set, and you want to  *)
(* distinguish (for an example, see \crefpage   *)
(* adt.ex.exhaustiveness). Here is a type [[collection]] *)
(* that is distinct from the set/list type.     *)
(* <boxed values 30>=                           *)
type 'a collection = 'a collection
val _ = op elemsC  : 'a collection -> 'a set
val _ = op singleC : 'a -> 'a collection
val _ = op emptyC  :       'a collection
(* <collections with mapping and combining functions>= *)
fun joinC     (C xs) = C (List.concat (map elemsC xs))
fun mapC  f   (C xs) = C (map f xs)
fun filterC p (C xs) = C (List.filter p xs)
fun mapC2 f (xc, yc) = joinC (mapC (fn x => mapC (fn y => f (x, y)) yc) xc)
(* Function [[mapC2]] is the most powerful of all—its *)
(* type resembles the type of Standard ML's     *)
(* [[ListPair.map]], but it works quite differently: *)
(* where [[ListPair.map]] takes elements pairwise, *)
(* [[mapC2]] takes all possible combinations.   *)
(* In particular, if you give [[ListPair.map]] two lists *)
(* containing N and M elements respectively, \stdbreak *)
(* the number of elements in the result is min(N,M). If *)
(* you give collections of size N and M to [[mapC2]], *)
(* the resulting collection has size N×M.      *)
(* <boxed values 31>=                           *)
val _ = op joinC   : 'a collection collection -> 'a collection
val _ = op mapC    : ('a -> 'b)      -> ('a collection -> 'b collection)
val _ = op filterC : ('a -> bool)    -> ('a collection -> 'a collection)
val _ = op mapC2   : ('a * 'b -> 'c) -> ('a collection * 'b collection -> 'c
                                                                     collection)
(* <suspensions>=                               *)
datatype 'a action
  = PENDING  of unit -> 'a
  | PRODUCED of 'a

type 'a susp = 'a action ref
(* To implement suspensions, I use a standard   *)
(* combination of imperative and functional code. *)
(* A suspension is a reference to an [[action]], which *)
(* can be pending or can have produced a result. *)
(* <boxed values 40>=                           *)
type 'a susp = 'a susp
(* <suspensions>=                               *)
fun delay f = ref (PENDING f)
fun demand cell =
  case !cell
    of PENDING f =>  let val result = f ()
                     in  (cell := PRODUCED result; result)
                     end
     | PRODUCED v => v
(* Functions [[delay]] and [[demand]] convert to and *)
(* from suspensions.                            *)
(* <boxed values 41>=                           *)
val _ = op delay  : (unit -> 'a) -> 'a susp
val _ = op demand : 'a susp -> 'a
(* The [[SUSPENDED]] constructor represents a stream in *)
(* which the action need to produce the next element may *)
(* not yet have been taken. \stdbreak Getting the *)
(* element requires demanding a value from a suspension, *)
(* and if the action in the suspension is pending, it is *)
(* performed at that time. [*]                  *)
(* <streams>=                                   *)
datatype 'a stream 
  = EOS
  | :::       of 'a * 'a stream
  | SUSPENDED of 'a stream susp
infixr 3 :::
(* <streams>=                                   *)
fun streamGet EOS = NONE
  | streamGet (x ::: xs)    = SOME (x, xs)
  | streamGet (SUSPENDED s) = streamGet (demand s)
(* <streams>=                                   *)
fun streamOfList xs = 
  foldr (op :::) EOS xs
(* Even though its representation uses mutable state *)
(* (the suspension), the stream is an immutable *)
(* abstraction. [To~help with debugging, I~sometimes *)
(* violate the abstraction and look at the state of a *)
(* [[SUSPENDED]] stream.] To observe that abstraction, *)
(* call [[streamGet]]. This function performs whatever *)
(* actions are needed either to produce a pair holding *)
(* an element an a stream (represented as \monoSOME (x, *)
(* xs) or to decide that the stream is empty and no more *)
(* elements can be produced (represented as [[NONE]]). *)
(* <boxed values 42>=                           *)
val _ = op streamGet : 'a stream -> ('a * 'a stream) option
(* The simplest way to create a stream is by using the *)
(* [[:::]] or [[EOS]] constructors. It can also be *)
(* convenient to create a stream from a list. When such *)
(* a stream is read, no new actions are performed. *)
(* <boxed values 42>=                           *)
val _ = op streamOfList : 'a list -> 'a stream
(* <streams>=                                   *)
fun listOfStream xs =
  case streamGet xs
    of NONE => []
     | SOME (x, xs) => x :: listOfStream xs
(* <streams>=                                   *)
fun delayedStream action = 
  SUSPENDED (delay action)
(* Function [[listOfStream]] creates a list from a *)
(* stream. It is useful for debugging.          *)
(* <boxed values 43>=                           *)
val _ = op listOfStream : 'a stream -> 'a list
(* The more interesting streams are those that result *)
(* from actions. To help create such streams, I define *)
(* [[delayedStream]] as a convenience abbreviation for *)
(* creating a stream from one action.           *)
(* <boxed values 43>=                           *)
val _ = op delayedStream : (unit -> 'a stream) -> 'a stream
(* <streams>=                                   *)
fun streamOfEffects action =
  delayedStream (fn () => case action () of NONE   => EOS
                                          | SOME a => a ::: streamOfEffects
                                                                         action)
(* Creating streams using actions and functions *)
(*                                              *)
(* Function [[streamOfEffects]] produces the stream of *)
(* results obtained by repeatedly performing a single *)
(* action (like reading a line of input). \stdbreak The *)
(* action must have type [[unit -> 'a option]]; the *)
(* stream performs the action repeatedly, producing a *)
(* stream of [['a]] values until performing the action *)
(* returns [[NONE]].                            *)
(* <boxed values 44>=                           *)
val _ = op streamOfEffects : (unit -> 'a option) -> 'a stream
(* I use [[streamOfEffects]] to produce a stream of *)
(* lines from an input file:                    *)

(* <streams>=                                   *)
type line = string
fun filelines infile = 
  streamOfEffects (fn () => TextIO.inputLine infile)
(* <boxed values 45>=                           *)
type line = line
val _ = op filelines : TextIO.instream -> line stream
(* <streams>=                                   *)
fun streamRepeat x =
  delayedStream (fn () => x ::: streamRepeat x)
(* Where [[streamOfEffects]] produces the results of *)
(* repeating a single action again and again,   *)
(* [[streamRepeat]] simply repeats a single value again *)
(* and again. This operation might sound useless, but *)
(* here's an example: suppose we read a sequence of *)
(* lines from a file, and for error reporting, we want *)
(* to tag each line with its source location, i.e., file *)
(* name and line number. Well, the file names are all *)
(* the same, and one easy way to associate the same file *)
(* name with every line is to repeat the file name *)
(* indefinitely, then join the two streams using *)
(* [[streamZip]]. Function [[streamRepeat]] creates an *)
(* infinite stream that repeats a value of any type: *)
(* <boxed values 46>=                           *)
val _ = op streamRepeat : 'a -> 'a stream
(* <streams>=                                   *)
fun streamOfUnfold next state =
  delayedStream (fn () => case next state
                            of NONE => EOS
                             | SOME (a, state') => a ::: streamOfUnfold next
                                                                         state')
(* A more sophisticated way to produce a stream is to *)
(* use a function that depends on an evolving state of *)
(* some unknown type [['b]]. The function is applied to *)
(* a state (of type [['b]]) and may produce a pair *)
(* containing a value of type [['a]] and a new state. *)
(* By repeatedly applying the function, we produce a *)
(* sequence of results of type [['a]]. This operation, *)
(* in which a function is used to expand a value into a *)
(* sequence, is the dual of the fold operation, which is *)
(* used to collapse a sequence into a value. The new *)
(* operation is therefore called unfold.        *)
(* <boxed values 47>=                           *)
val _ = op streamOfUnfold : ('b -> ('a * 'b) option) -> 'b -> 'a stream
(* Function [[streamOfUnfold]] can turn any ``get'' *)
(* function into a stream. In fact, the standard unfold *)
(* and get operations should obey the following *)
(* algebraic law:                               *)
(*                                              *)
(*  streamOfUnfold streamGet xs ===xs.          *)
(*                                              *)
(* Another useful ``get'' function is [[(fn n => SOME *)
(* (n, n+1))]]; passing this function to        *)
(* [[streamOfUnfold]] results in an infinite stream of *)
(* increasing integers. [*]                     *)

(* <streams>=                                   *)
val naturals = 
  streamOfUnfold (fn n => SOME (n, n+1)) 0   (* 0 to infinity *)
(* <boxed values 48>=                           *)
val _ = op naturals : int stream
(* (Streams, like lists, support not only unfolding but *)
(* also folding. Function [[streamFold]] is defined *)
(* below in chunk [->].)                        *)

(* <streams>=                                   *)
fun preStream (pre, xs) = 
  streamOfUnfold (fn xs => (pre (); streamGet xs)) xs
(* It's also useful to be able to perform an action *)
(* immediately after getting an element from a stream. *)
(* In [[postStream]], I perform the action only if *)
(* [[streamGet]] succeeds. By performing the [[post]] *)
(* action only when [[streamGet]] succeeds, I make it *)
(* possible to write a [[post]] action that has access *)
(* to the element just gotten. Post-get actions are *)
(* especially useful for debugging.             *)

(* <streams>=                                   *)
fun postStream (xs, post) =
  streamOfUnfold (fn xs => case streamGet xs
                             of NONE => NONE
                              | head as SOME (x, _) => (post x; head)) xs
(* Given an action called [[pre]] and a stream xs, *)
(* I define a stream \monopreStream (pre, xs) that adds *)
(* [[pre ()]] to the action performed by the stream. *)
(* Roughly speaking,                            *)
(*                                              *)
(*  \monostreamGet (preStream (pre, xs)) = \mono(pre *)
(*  (); streamGet xs).                          *)
(*                                              *)
(* (The equivalence is only rough because the pre action *)
(* is performed lazily, only when an action is needed to *)
(* get a value from xs.)                        *)
(* <boxed values 49>=                           *)
val _ = op preStream : (unit -> unit) * 'a stream -> 'a stream
(* <boxed values 49>=                           *)
val _ = op postStream : 'a stream * ('a -> unit) -> 'a stream
(* <streams>=                                   *)
fun streamMap f xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => f x ::: streamMap f xs)
(* Standard list functions ported to streams    *)
(*                                              *)
(* Functions like [[map]], [[filter]], [[fold]], *)
(* [[zip]], and [[concat]] are every bit as useful on *)
(* streams as they are on lists.                *)
(* <boxed values 50>=                           *)
val _ = op streamMap : ('a -> 'b) -> 'a stream -> 'b stream
(* <streams>=                                   *)
fun streamFilter p xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => if p x then x ::: streamFilter p
                                                                              xs
                                               else streamFilter p xs)
(* <boxed values 51>=                           *)
val _ = op streamFilter : ('a -> bool) -> 'a stream -> 'a stream
(* <streams>=                                   *)
fun streamFold f z xs =
  case streamGet xs of NONE => z
                     | SOME (x, xs) => streamFold f (f (x, z)) xs
(* The only sensible order in which to fold the elements *)
(* of a stream is the order in which the actions are *)
(* taken and the results are produced: from left to *)
(* right. [*]                                   *)
(* <boxed values 52>=                           *)
val _ = op streamFold : ('a * 'b -> 'b) -> 'b -> 'a stream -> 'b
(* <streams>=                                   *)
fun streamZip (xs, ys) =
  delayedStream
  (fn () => case (streamGet xs, streamGet ys)
              of (SOME (x, xs), SOME (y, ys)) => (x, y) ::: streamZip (xs, ys)
               | _ => EOS)
(* <streams>=                                   *)
fun streamConcat xss =
  let fun get (xs, xss) =
        case streamGet xs
          of SOME (x, xs) => SOME (x, (xs, xss))
           | NONE => case streamGet xss
                       of SOME (xs, xss) => get (xs, xss)
                        | NONE => NONE
  in  streamOfUnfold get (EOS, xss)
  end
(* <boxed values 53>=                           *)
val _ = op streamZip : 'a stream * 'b stream -> ('a * 'b) stream
(* <boxed values 53>=                           *)
val _ = op streamConcat : 'a stream stream -> 'a stream
(* <streams>=                                   *)
fun streamConcatMap f xs = streamConcat (streamMap f xs)
(* The composition of [[concat]] with [[map f]] is very *)
(* common in list and stream processing, so I give it a *)
(* name.                                        *)
(* <boxed values 54>=                           *)
val _ = op streamConcatMap : ('a -> 'b stream) -> 'a stream -> 'b stream
(* <streams>=                                   *)
infix 5 @@@
fun xs @@@ xs' = streamConcat (streamOfList [xs, xs'])
(* The code used to append two streams is much like the *)
(* code used to concatenate arbitrarily many streams. *)
(* To avoid duplicating the tricky manipulation of *)
(* states, I simply implement append using      *)
(* concatenation.                               *)
(* <boxed values 55>=                           *)
val _ = op @@@ : 'a stream * 'a stream -> 'a stream
(* <streams>=                                   *)
fun streamTake (0, xs) = []
  | streamTake (n, xs) =
      case streamGet xs
        of SOME (x, xs) => x :: streamTake (n-1, xs)
         | NONE => []
(* Whenever I rename bound variables, for example in a *)
(* type \/\ldotsnalpha\alldottau, I have to choose new *)
(* names that don't conflict with existing names in tau *)
(* or in the environment. The easiest way to get good *)
(* names to build an infinite stream of names by using *)
(* [[streamMap]] on [[naturals]], then use      *)
(* [[streamFilter]] to choose only the good ones, and *)
(* finally to take exactly as many good names as I need *)
(* by calling [[streamTake]], which is defined here. *)
(* <boxed values 56>=                           *)
val _ = op streamTake : int * 'a stream -> 'a list
(* <streams>=                                   *)
fun streamDrop (0, xs) = xs
  | streamDrop (n, xs) =
      case streamGet xs
        of SOME (_, xs) => streamDrop (n-1, xs)
         | NONE => EOS
(* If I want ``take,'' sooner or later I'm sure to want *)
(* ``drop'' (\chunkrefmlinterps.chunk.use-streamDrop). *)
(* <boxed values 57>=                           *)
val _ = op streamDrop : int * 'a stream -> 'a stream
(* <streams>=                                   *)
fun every xs () k = streamConcatMap k xs
val run = ()
(* <streams>=                                   *)
fun cartesian xs ys =
  every xs run (fn x => 
    every ys run (fn y =>
      streamOfList [(x, y)]))
(* Function [[solutions]] should be specified by your *)
(* operational semantics, which may include list *)
(* comprehensions. To implement list comprehensions, *)
(* I recommend a variation on [[streamConcatMap]]. *)
(* I sometimes define                           *)
(* <boxed values 75>=                           *)
val _ = op every : 'a stream -> unit -> ('a -> 'b stream) -> 'b stream
(* Using [[every]] and [[run]], the example list *)
(* comprehension for the Cartesian product, \mathbox[(x, *)
(* y) \suchthatx \getsa\listv*x, y \getsa\listv*y], is *)
(* written as                                   *)
(* <boxed values 75>=                           *)
val _ = op cartesian : 'a stream -> 'b stream -> ('a * 'b) stream
(* Your [[solutions]] function should generate solutions *)
(* for uProlog's primitive predicates, but the  *)
(* implementations of those predicates need not change. *)
(* Those implementations expect success and failure *)
(* continuations, but you can get a stream of   *)
(* substitutions using \monoboxstreamOfCPS (p args), *)
(* where [[p]] represents the primitive predicate, *)
(* [[args]] represents its arguments, and       *)
(* [[streamOfCPS]] is defined as follows:       *)
(* <streams>=                                   *)
fun streamOfCPS cpsSource =
  cpsSource (fn theta => fn resume => theta ::: resume ()) (fn () => EOS)
val _ = streamOfCPS : (('a -> (unit->'a stream) -> 'a stream) -> (unit->'a
                                     stream) -> 'a stream) -> 'a stream (*OMIT*)
val _ = streamOfCPS (fn succ => fn fail => succ [("a", 3)] (fn () => EOS)) : (
                                             string * int) list stream  (*OMIT*)
(* <streams>=                                   *)
fun cpsStream answers succ fail =
  case streamGet answers
    of NONE => fail ()
     | SOME (theta, answers) =>
         succ theta (fn () => cpsStream answers succ fail)
(* When [[solutions]] is complete, write a replacement *)
(* [[query]] function that calls [[cpsStream]] on the *)
(* result of [[solutions]], where [[cpsStream]] is *)
(* defined as follows:                          *)
(* <boxed values 76>=                           *)
val _ = op cpsStream : 'subst stream -> ('subst -> (unit->'a) -> 'a) -> (unit->
                                                                       'a) -> 'a
(* <stream transformers and their combinators>= *)
type ('a, 'b) xformer = 
  'a stream -> ('b error * 'a stream) option
(* Stream transformers, which act as parsers    *)
(*                                              *)
(* Our ultimate goal is to turn streams of input lines *)
(* into streams of definitions. Along the way we may *)
(* also have streams of characters, tokens, types, *)
(* expressions, and more. To handle all these different *)
(* kinds of streams using a single set of operators, *)
(* I define a type representing a stream transformer. *)
(* A stream transformer from A to B takes a stream of A *)
(* 's as input and either succeeds, fails, or detects an *)
(* error:                                       *)
(*                                              *)
(*   • If it succeeds, it consumes zero or more A's from *)
(*  the input stream and produces exactly one B. *)
(*  It returns a pair containing [[OK]] B plus  *)
(*  whatever A's were not consumed.             *)
(*   • If it fails, it returns [[NONE]].      *)
(*   • If it detects an error, it returns a pair *)
(*  containing [[ERROR]] m, where m is a message, *)
(*  plus whatever A's were not consumed.        *)
(*                                              *)
(* <boxed values 77>=                           *)
type ('a, 'b) xformer = ('a, 'b) xformer
(* If we apply [[streamOfUnfold]], from \cref   *)
(* mlinterps.streams, to an [[('a, 'b) xformer]], \ *)
(* mdbusemlinterpsstreamOfUnfold we get a function that *)
(* maps a stream of A's to a stream of B's-with-error. *)

(* <stream transformers and their combinators>= *)
fun pure y = fn xs => SOME (OK y, xs)
(* --- #2                                       *)
(* \newskip\myskip \myskip=4pt                  *)
(*                                              *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* \catcode`=\other \catcode`_=\other \catcode`$=\other *)
(*                                              *)
(*  \toprule Stream transformers;               *)
(*  applying functions to                       *)
(*  transformers                                *)
(*  \midrule \type('a, 'b) xformer              *)
(*  \tableboxpure : 'b -> ('a, 'b)              *)
(*  xformer \splitbox<*>('a, 'b ->              *)
(*  'c) xformer * ('a, 'b)                      *)
(*  xformer-> ('a, 'c) xformer \                *)
(*  tablebox<> : ('b -> 'c) * ('a,              *)
(*  'b) xformer -> ('a, 'c) xformer             *)
(*  \tablebox<>? : ('b -> 'c                    *)
(*  option) * ('a, 'b) xformer ->               *)
(*  ('a, 'c) xformer \splitbox<*>!              *)
(*  ('a, 'b -> 'c error) xformer *              *)
(*  ('a, 'b) xformer-> ('a, 'c)                 *)
(*  xformer \tablebox<>! : ('b ->               *)
(*  'c error) * ('a, 'b) xformer ->             *)
(*  ('a, 'c) xformer [8pt] \midrule             *)
(*  Functions useful with [[<>]]                *)
(*  and [[<*>]]                                 *)
(*  \tableboxfst : ('a * 'b) -> 'a              *)
(*  \tableboxsnd : ('a * 'b) -> 'b              *)
(*  \tableboxpair : 'a -> 'b -> 'a              *)
(*  * 'b \tableboxcurry : ('a * 'b              *)
(*  -> 'c) -> ('a -> 'b -> 'c) \                *)
(*  tableboxcurry3 : ('a * 'b * 'c              *)
(*  -> 'd) -> ('a -> 'b -> 'c ->                *)
(*  'd) [8pt] \midrule Combining                *)
(*  transformers in sequence,                   *)
(*  alternation, or conjunction                 *)
(*  \tablebox<* : ('a, 'b) xformer  >]] : ('a, 'b) *)
(*  * ('a, 'c) xformer -> ('a, 'b)  xformer * ('a, *)
(*  xformer \tablebox *> : ('a, 'b) 'c) xformer -> *)
(*  xformer * ('a, 'c) xformer ->   ('a, 'c) xformer *)
(*  ('a, 'c) xformer \tablebox< :   [8pt] \midrule *)
(*  'b * ('a, 'c) xformer -> ('a,   Transformers *)
(*  'b) xformer \tablebox<|> : ('a, useful for both *)
(*  'b) xformer * ('a, 'b) xformer  lexical analysis *)
(*  -> ('a, 'b) xformer \tablebox   and parsing *)
(*  pzero : ('a, 'b) xformer \                  *)
(*  tableboxanyParser : ('a, 'b)                *)
(*  xformer list -> ('a, 'b)                    *)
(*  xformer \tablebox[[<                        *)
(*  \tableboxone : ('a, 'a) xformer             *)
(*  \tableboxeos : ('a, unit)                   *)
(*  xformer \tableboxsat : ('b ->               *)
(*  bool) -> ('a, 'b) xformer ->                *)
(*  ('a, 'b) xformer \tableboxeqx :             *)
(*  ''b -> ('a, ''b) xformer ->                 *)
(*  ('a, ''b) xformer \tablebox                 *)
(*  notFollowedBy : ('a, 'b)                    *)
(*  xformer -> ('a, unit) xformer \             *)
(*  tableboxmany : ('a, 'b) xformer             *)
(*  -> ('a, 'b list) xformer \                  *)
(*  tableboxmany1 : ('a, 'b)                    *)
(*  xformer -> ('a, 'b list)                    *)
(*  xformer \tableboxoptional :                 *)
(*  ('a, 'b) xformer -> ('a, 'b                 *)
(*  option) xformer \tableboxpeek :             *)
(*  ('a, 'b) xformer -> 'a stream               *)
(*  -> 'b option \tableboxrewind :              *)
(*  ('a, 'b) xformer -> ('a, 'b)                *)
(*  xformer \bottomrule                         *)
(*                                              *)
(* Stream transformers and their combinators [*] *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* Error-free transformers and their composition *)
(*                                              *)
(* The [[pure]] combinator takes a value [[h]] of type B *)
(* as argument. It returns an \atob transformer that *)
(* consumes no A's as input and produces [[y]]. *)
(* <boxed values 78>=                           *)
val _ = op pure : 'b -> ('a, 'b) xformer
(* <stream transformers and their combinators>= *)
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
(* For the combination [[tx_f <*> tx_b]] to succeed, *)
(* both [[tx_f]] and [[tx_b]] must succeed. Ensuring *)
(* that two transformers succeed requires a nested case *)
(* analysis.                                    *)
(* <boxed values 79>=                           *)
val _ = op <*> : ('a, 'b -> 'c) xformer * ('a, 'b) xformer -> ('a, 'c) xformer
(* <stream transformers and their combinators>= *)
infixr 4 <$>
fun f <$> p = pure f <*> p
(* The common case of creating [[tx_f]] using [[pure]] *)
(* is normally written using the special operator [[< *)
(* >]], which is also pronounced ``applied to.'' *)
(* It combines a B-to-C function with an \atob  *)
(* transformer to produce an \atoc transformer. *)
(* <boxed values 80>=                           *)
val _ = op <$> : ('b -> 'c) * ('a, 'b) xformer -> ('a, 'c) xformer
(* <stream transformers and their combinators>= *)
infix 1 <|>
fun t1 <|> t2 = (fn xs => case t1 xs of SOME y => SOME y | NONE => t2 xs) 
(* The combinator [[<*>]] creates parsers that read *)
(* things in sequence; but it can't make a choice. *)
(* If any parser in the sequence fails, the whole *)
(* sequence fails. To make a choice, as in ``[[val]] or *)
(* expression or [[define]] or [[use]],'' we use a *)
(* choice operator. The choice operator is written *)
(* [[<|>]] and pronounced ``or.'' If [[t1]] and [[t2]] *)
(* are both \atob transformers, then \monoboxt1 <|> t2 *)
(* is an \atob transformer that first tries [[t1]], then *)
(* tries [[t2]], succeeding if either succeeds, *)
(* detecting an error if either detects an error, and *)
(* failing only if both fail. To assure that the result *)
(* has a predictable type no matter which transformer is *)
(* used, both [[t1]] and [[t2]] have to have the same *)
(* type.                                        *)
(* <boxed values 82>=                           *)
val _ = op <|> : ('a, 'b) xformer * ('a, 'b) xformer -> ('a, 'b) xformer
(* I sometimes want to combine a list of parsers with *)
(* the choice operator. I can do this with a fold *)
(* operator, but I need a ``zero'' parser that always *)
(* fails.                                       *)

(* <stream transformers and their combinators>= *)
fun pzero _ = NONE
(* Because building choices from lists is common, *)
(* I implement this special case as [[anyParser]]. *)

(* <stream transformers and their combinators>= *)
fun anyParser ts = 
  foldr op <|> pzero ts
(* <boxed values 83>=                           *)
val _ = op pzero : ('a, 'b) xformer
(* <boxed values 83>=                           *)
val _ = op anyParser : ('a, 'b) xformer list -> ('a, 'b) xformer
(* <stream transformers and their combinators>= *)
infix 6 <* *>
fun p1 <*  p2 = curry fst <$> p1 <*> p2
fun p1  *> p2 = curry snd <$> p1 <*> p2

infixr 4 <$
fun v <$ p = (fn _ => v) <$> p
(* The abbreviations are formed by modifying the [[<*>]] *)
(* or [[<>]] operator to remove the angle bracket on the *)
(* side containing the result we don't care about. For *)
(* example,                                     *)
(*                                              *)
(*   • Parser [[p1 <* p2]] reads the input of [[p1]] and *)
(*  then the input of [[p2]], but it returns only the *)
(*  result of [[p1]].                           *)
(*   • Parser [[p1 *> p2]] reads the input of [[p1]] and *)
(*  then the input of [[p2]], but it returns only the *)
(*  result of [[p2]].                           *)
(*   • Parser [[v < p]] parses the input the way [[p]] *)
(*   does, but it then ignores [[p]]'s result and *)
(*  instead produces the value [[v]].           *)
(*                                              *)
(* <boxed values 84>=                           *)
val _ = op <*  : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'b) xformer
val _ = op  *> : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'c) xformer
val _ = op <$  : 'b               * ('a, 'c) xformer -> ('a, 'b) xformer
(* <stream transformers and their combinators>= *)
fun one xs = case streamGet xs
               of NONE => NONE
                | SOME (x, xs) => SOME (OK x, xs)
(* The simplest input-inspecting parser is [[one]]. It's *)
(* an \atoa transformer that succeeds if and only if *)
(* there is a value in the input. If there's no value *)
(* input, [[one]] fails; it never signals an error. *)
(* <boxed values 85>=                           *)
val _ = op one : ('a, 'a) xformer
(* <stream transformers and their combinators>= *)
fun eos xs = case streamGet xs
               of NONE => SOME (OK (), EOS)
                | SOME _ => NONE
(* The counterpart of [[one]] is a parser that succeeds *)
(* if and only if there is no input—that is, if we have *)
(* reached the end of a stream. This parser, which is *)
(* called [[eos]], can produce no useful result, so it *)
(* produces the empty tuple, which has type [[unit]]. *)
(* <boxed values 86>=                           *)
val _ = op eos : ('a, unit) xformer
(* Perhaps surprisingly, these are the only two standard *)
(* parsers that look at their input. The only other *)
(* parsing combinator that looks directly at input is *)
(* [[stripAndReportErrors]], which removes [[ERROR]] and *)
(* [[OK]] from error streams.                   *)

(* <stream transformers and their combinators>= *)
fun peek tx xs =
  case tx xs of SOME (OK y, _) => SOME y
              | _ => NONE
(* It is sometimes useful to look at input without *)
(* consuming it. I provide two functions: [[peek]] just *)
(* looks at a transformed stream and maybe produces a *)
(* value, whereas [[rewind]] can change any transformer *)
(* into a transformer that behaves identically, but *)
(* doesn't consume any input. I use these functions *)
(* either to debug, or to find the source-code location *)
(* of the next token in a token stream.         *)
(* <boxed values 87>=                           *)
val _ = op peek : ('a, 'b) xformer -> 'a stream -> 'b option
(* <stream transformers and their combinators>= *)
fun rewind tx xs =
  case tx xs of SOME (ey, _) => SOME (ey, xs)
              | NONE => NONE
(* Given a transformer [[tx]], transformer \monobox *)
(* rewind tx computes the same value as [[tx]], but when *)
(* it's done, it rewinds the input stream back to where *)
(* it was before we ran [[tx]]. The actions performed by *)
(* [[tx]] can't be undone, but the inputs can be read *)
(* again.                                       *)
(* <boxed values 88>=                           *)
val _ = op rewind : ('a, 'b) xformer -> ('a, 'b) xformer
(* <stream transformers and their combinators>= *)
fun sat p tx xs =
  case tx xs
    of answer as SOME (OK y, xs) => if p y then answer else NONE
     | answer => answer
(* Parsers based on conditions                  *)
(*                                              *)
(* Combinator [[sat]] wraps an \atob transformer with a *)
(* B-predicate such that the wrapped transformer *)
(* succeeds only when the underlying transformer *)
(* succeeds and produces a value that satisfies the *)
(* predicate.                                   *)
(* <boxed values 89>=                           *)
val _ = op sat : ('b -> bool) -> ('a, 'b) xformer -> ('a, 'b) xformer
(* <stream transformers and their combinators>= *)
fun eqx y = 
  sat (fn y' => y = y') 
(* Transformer [[eqx b]] is [[sat]] specialized to an *)
(* equality predicate. It is typically used to recognize *)
(* special characters like keywords and minus signs. *)
(* <boxed values 90>=                           *)
val _ = op eqx : ''b -> ('a, ''b) xformer -> ('a, ''b) xformer
(* <stream transformers and their combinators>= *)
infixr 4 <$>?
fun f <$>? tx =
  fn xs => case tx xs
             of NONE => NONE
              | SOME (ERROR msg, xs) => SOME (ERROR msg, xs)
              | SOME (OK y, xs) =>
                  case f y
                    of NONE => NONE
                     | SOME z => SOME (OK z, xs)
(* A more subtle condition is that a partial function *)
(* can turn an input into something we're looking for. *)
(* If we have an \atob transformer, and we compose it *)
(* with a function that given a B, sometimes produces a  *)
(* C, then we get an \atoxC transformer. Because there's *)
(* a close analogy with the application operator [[<>]], *)
(* I notate this partial application operator as [[< *)
(* >?]], with a question mark.                  *)
(* <boxed values 91>=                           *)
val _ = op <$>? : ('b -> 'c option) * ('a, 'b) xformer -> ('a, 'c) xformer
(* <stream transformers and their combinators>= *)
infix 3 <&>
fun t1 <&> t2 = fn xs =>
  case t1 xs
    of SOME (OK _, _) => t2 xs
     | SOME (ERROR _, _) => NONE    
     | NONE => NONE
(* We can run a parser conditional on the success of *)
(* another parser. Parser [[t1 < --- > t2]] succeeds *)
(* only if both [[t1]] and [[t2]] succeed at the same *)
(* point. This parser looks at enough input to decide if *)
(* [[t1]] succeeds, but it does not consume that *)
(* input—it consumes only the input of [[t2]]. *)
(* <boxed values 92>=                           *)
val _ = op <&> : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'c) xformer
(* <stream transformers and their combinators>= *)
fun notFollowedBy t xs =
  case t xs
    of NONE => SOME (OK (), xs)
     | SOME _ => NONE
(* We can also use the success or failure of a parser as *)
(* a condition. Parser \monoboxnotFollowedBy t succeeds *)
(* if and only if [[t]] fails. Parser \monobox  *)
(* notFollowedBy t may look at the input, but it never *)
(* consumes any input. I use [[notFollowedBy]] when *)
(* reading integer literals, to make sure that the *)
(* digits are not followed by a letter or other *)
(* non-delimiting symbol.                       *)
(* <boxed values 93>=                           *)
val _ = op notFollowedBy : ('a, 'b) xformer -> ('a, unit) xformer
(* <stream transformers and their combinators>= *)
fun many t = 
  curry (op ::) <$> t <*> (fn xs => many t xs) <|> pure []
(* Parsers for sequences                        *)
(*                                              *)
(* Inputs are full of sequences. A function takes a *)
(* sequence of arguments, a program is a sequence of *)
(* definitions, and a method definition contains a *)
(* sequence of expressions. To create transformers that *)
(* process sequences, I define functions [[many]] and *)
(* [[many1]]. If [[t]] is an \atob transformer, then \ *)
(* monoboxmany t is an \atoxlist-of-B transformer. *)
(* It runs [[t]] as many times as possible. And even if *)
(* [[t]] fails, \monoboxmany t always succeeds: when *)
(* [[t]] fails, \monoboxmany t returns an empty list of  *)
(* B's.                                         *)
(* <boxed values 94>=                           *)
val _ = op many  : ('a, 'b) xformer -> ('a, 'b list) xformer
(* I'd really like to write that first alternative as *)
(*                                              *)
(*  [[curry (op ::) <> t <*> many t]]           *)
(*                                              *)
(* but that formulation leads to instant death by *)
(* infinite recursion. If you write your own parsers, *)
(* it's a problem to watch out for.             *)

(* <stream transformers and their combinators>= *)
fun many1 t = 
  curry (op ::) <$> t <*> many t
(* Sometimes an empty list isn't acceptable. In that *)
(* case, use \monoboxmany1 t, which succeeds only if *)
(* [[t]] succeeds at least once—in which case it returns *)
(* a nonempty list.                             *)
(* <boxed values 95>=                           *)
val _ = op many1 : ('a, 'b) xformer -> ('a, 'b list) xformer
(* Although \monoboxmany t always succeeds, \monobox *)
(* many1 t can fail.                            *)

(* <stream transformers and their combinators>= *)
fun optional t = 
  SOME <$> t <|> pure NONE
(* Sometimes instead of zero, one, or many B's, we just *)
(* one zero or one; such a B might be called    *)
(* ``optional.'' For example, a numeric literal begins *)
(* with an optional minus sign. Function [[optional]] *)
(* turns an \atob transformer into an \atoxoptional-B *)
(* transformer. Like \monoboxmany t, \monoboxoptional t *)
(* always succeeds.                             *)
(* <boxed values 96>=                           *)
val _ = op optional : ('a, 'b) xformer -> ('a, 'b option) xformer
(* <stream transformers and their combinators>= *)
infix 2 <*>!
fun tx_ef <*>! tx_x =
  fn xs => case (tx_ef <*> tx_x) xs
             of NONE => NONE
              | SOME (OK (OK y),      xs) => SOME (OK y,      xs)
              | SOME (OK (ERROR msg), xs) => SOME (ERROR msg, xs)
              | SOME (ERROR msg,      xs) => SOME (ERROR msg, xs)
infixr 4 <$>!
fun ef <$>! tx_x = pure ef <*>! tx_x
(* Error-detecting transformers and their composition *)
(*                                              *)
(* Sometimes an error is detected not by a parser but by *)
(* a function that is applied to the results of parsing. *)
(* A classic example is a function definition: if the *)
(* formal parameters are syntactically correct but *)
(* contain duplicate name, an error should be signalled. *)
(* We would transform the input into a value of type *)
(* [[name list error]]. But the transformer type already *)
(* includes the possibility of error, and we would *)
(* prefer that errors detected by functions be on the *)
(* same footing as errors detected by parsers, and that *)
(* they be handled by the same mechanisms. To enable *)
(* such handling, I define [[<*>!]] and [[<>!]] *)
(* combinators that merge function-detected errors with *)
(* parser-detected errors.                      *)
(* <boxed values 97>=                           *)
val _ = op <*>! : ('a, 'b -> 'c error) xformer * ('a, 'b) xformer -> ('a, 'c)
                                                                         xformer
val _ = op <$>! : ('b -> 'c error)             * ('a, 'b) xformer -> ('a, 'c)
                                                                         xformer
(* <support for source-code locations and located streams>= *)
type srcloc = string * int
fun srclocString (source, line) =
  source ^ ", line " ^ intString line
(* Source-code locations are useful when reading code *)
(* from a file. When reading code interactively, *)
(* however, a message that says the error occurred ``in *)
(* standard input, line 12,'' is more annoying than *)
(* helpful. As in the C code in \crefpage       *)
(* cinterps.error-format, I use an error format to *)
(* control when error messages include source-code *)
(* locations. The format is initially set to include *)
(* them. [*]                                    *)
(* <support for source-code locations and located streams>= *)
datatype error_format = WITH_LOCATIONS | WITHOUT_LOCATIONS
val toplevel_error_format = ref WITH_LOCATIONS
(* The format is consulted by function [[synerrormsg]], *)
(* which produces the message that accompanies a syntax *)
(* error.                                       *)
(* <support for source-code locations and located streams>= *)
fun synerrormsg (source, line) strings =
  if !toplevel_error_format = WITHOUT_LOCATIONS andalso source =
                                                                "standard input"
  then
    concat ("syntax error: " :: strings)
  else    
    concat ("syntax error in " :: srclocString (source, line) :: ": " :: strings
                                                                               )

(* Source locations are also used at run time. Any *)
(* exception can be marked with a location by converting *)
(* it to the [[Located]] exception:             *)
(* <support for source-code locations and located streams>= *)
exception Located of srcloc * exn
(* <support for source-code locations and located streams>= *)
type 'a located = srcloc * 'a
(* Tracking and reporting source-code locations *)
(*                                              *)
(* An error message is more informative if it says where *)
(* the error occurred. ``Where'' means a source-code *)
(* location. Compilers that take themselves seriously *)
(* report source-code locations right down to the *)
(* individual character: file broken.c, line 12, *)
(* column 17. In production compilers, such precision is *)
(* admirable. But in a pedagogical interpreter, the *)
(* desire for precision has to be balanced against the *)
(* need for simplicity. The best compromise is to track *)
(* only source file and line number. That's good enough *)
(* to help programmers find errors, and it eliminates *)
(* bookkeeping that would otherwise be needed to track *)
(* column numbers.                              *)
(* <boxed values 59>=                           *)
type srcloc = srcloc
val _ = op srclocString : srcloc -> string
(* To keep track of the source location of a line, *)
(* token, expression, or other datum, I put the location *)
(* and the datum together in a pair. To make it easier *)
(* to read the types, I define a type abbreviation which *)
(* says that a value paired with a location is  *)
(* ``located.''                                 *)
(* <boxed values 59>=                           *)
type 'a located = 'a located
(* <support for source-code locations and located streams>= *)
fun atLoc loc f a =
  f a handle e as RuntimeError _ => raise Located (loc, e)
           | e as NotFound _     => raise Located (loc, e)
           (* Here are handlers for more exceptions we recognize. *)
           (* These handlers can be augmented by other,    *)
           (* language-specific handlers.                  *)
           (* <more handlers for [[atLoc]]>=               *)
           | e as IO.Io _   => raise Located (loc, e)
           | e as Div       => raise Located (loc, e)
           | e as Overflow  => raise Located (loc, e)
           | e as Subscript => raise Located (loc, e)
           | e as Size      => raise Located (loc, e)
(* To raise the [[Located]] exception, we use function *)
(* [[atLoc]]. Calling \monoboxatLoc f x applies [[f]] *)
(* to [[x]] within the scope of handlers that convert *)
(* recognized exceptions to the [[Located]] exception: *)
(* <boxed values 60>=                           *)
val _ = op atLoc : srcloc -> ('a -> 'b) -> ('a -> 'b)
(* <support for source-code locations and located streams>= *)
fun located f (loc, a) = atLoc loc f a
fun leftLocated f ((loc, a), b) = atLoc loc f (a, b)
(* And we can call [[atLoc]] easily by using the *)
(* higher-order function [[located]]:           *)
(* <boxed values 61>=                           *)
val _ = op located : ('a -> 'b) -> ('a located -> 'b)
val _ = op leftLocated : ('a * 'b -> 'c) -> ('a located * 'b -> 'c)
(* <support for source-code locations and located streams>= *)
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
(* Once we have a location, we use it to fill in a *)
(* template for an error message. The location replaces *)
(* the string [["<at loc>"]]. The necessary string *)
(* processing is done by [[fillComplaintTemplate]], *)
(* which relies on Standard ML's [[Substring]] module. *)
(* <boxed values 62>=                           *)
val _ = op fillComplaintTemplate : string * srcloc option -> string
(* <support for source-code locations and located streams>= *)
fun errorAt msg loc = 
  ERROR (synerrormsg loc [msg])
(* <support for source-code locations and located streams>= *)
fun locatedStream (streamname, inputs) =
  let val locations = streamZip (streamRepeat streamname, streamDrop (1,
                                                                      naturals))
  in  streamZip (locations, inputs)
  end
(* To signal an error at a given location, code calls *)
(* [[errorAt]]. [*]                             *)
(* <boxed values 63>=                           *)
val _ = op errorAt : string -> srcloc -> 'a error
(* All locations originate in a located stream of lines. *)
(* The locations share a filename, and the line numbers *)
(* are 1, 2, 3, ... and so on. [*]              *)
(* <boxed values 63>=                           *)
val _ = op locatedStream : string * line stream -> line located stream
(* <streams that track line boundaries>=        *)
datatype 'a eol_marked
  = EOL of int (* number of the line that ends here *)
  | INLINE of 'a

fun drainLine EOS = EOS
  | drainLine (SUSPENDED s)     = drainLine (demand s)
  | drainLine (EOL _    ::: xs) = xs
  | drainLine (INLINE _ ::: xs) = drainLine xs
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
(* <boxed values 105>=                          *)
type 'a eol_marked = 'a eol_marked
val _ = op drainLine : 'a eol_marked stream -> 'a eol_marked stream
(* <boxed values 105>=                          *)
val _ = op eol      : ('a eol_marked, int) xformer
val _ = op inline   : ('a eol_marked, 'a)  xformer
val _ = op srcloc   : ('a located eol_marked, srcloc) xformer
(* <support for lexical analysis>=              *)
type 'a lexer = (char, 'a) xformer
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* \catcode`=\other \catcode`_=\other \catcode`$=\other *)
(*                                              *)
(*  \toprule Lexical analyzers; tokens          *)
(*  \midrule \type'a lexer = (char, 'a) xformer \ *)
(*  tableboxisDelim : char -> bool \tablebox    *)
(*  whitespace : char list lexer \tableboxintChars : *)
(*  (char -> bool) -> char list lexer \tablebox *)
(*  intFromChars : char list -> int error \tablebox *)
(*  intToken : (char -> bool) -> int lexer \typetoken *)
(*  \tableboxisLiteral : string -> token -> bool \ *)
(*  tableboxtokenString : token -> string \tablebox *)
(*  lexLineWith : token lexer -> line -> token stream *)
(*  [8pt] \midrule Streams with end-of-line markers *)
(*  \type'a eol_marked \tableboxdrainLine : 'a  *)
(*  eol_marked stream -> 'a eol_marked stream [8pt] \ *)
(*  midrule Parsers                             *)
(*  \type'a parser = (token located eol_marked, 'a) *)
(*  xformer \tableboxeol : ('a eol_marked, int) *)
(*  xformer \tableboxinline : ('a eol_marked, 'a) *)
(*  xformer \tableboxtoken : token parser \tablebox *)
(*  srcloc : srcloc parser \tableboxnoTokens : unit *)
(*  parser \tablebox@@ : 'a parser -> 'a located *)
(*  parser \tablebox<?> : 'a parser * string -> 'a *)
(*  parser \tablebox<!> : 'a parser * string -> 'b *)
(*  parser \tableboxliteral : string -> unit parser \ *)
(*  tablebox>– : string * 'a parser -> 'a parser \ *)
(*  tablebox–< : 'a parser * string -> 'a parser \ *)
(*  tableboxbracket : string * string * 'a parser -> *)
(*  'a parser \splitboxnodupsstring * string -> *)
(*  srcloc * name list-> name list error \tablebox *)
(*  safeTokens : token located eol_marked stream -> *)
(*  token list \tableboxechoTagStream : line stream *)
(*  -> line stream \tableboxstripAndReportErrors : 'a *)
(*  error stream -> 'a stream [8pt] \midrule A  *)
(*  complete, interactive source of abstract syntax *)
(*  \splitboxinteractiveParsedStreamtoken lexer * 'a *)
(*  parser -> string * line stream * prompts -> 'a *)
(*  stream \bottomrule                          *)
(*                                              *)
(* Transformers specialized for lexical analysis or *)
(* parsing [*]                                  *)
(*                                              *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* Lexical analyzers: transformers of characters *)
(*                                              *)
(* The interpreters in this book consume one line at a *)
(* time. But characters within a line may be split into *)
(* multiple tokens. For example, the line       *)
(*                                              *)
(*   (define list1 (x) (cons x '()))            *)
(*                                              *)
(* should be split into the tokens              *)
(*                                              *)
(*                                              *)
(*  (                                           *)
(*  define                                      *)
(*  list1                                       *)
(*  (                                           *)
(*  x                                           *)
(*  )                                           *)
(*  (                                           *)
(*  cons                                        *)
(*  x                                           *)
(*  '                                           *)
(*  (                                           *)
(*  )                                           *)
(*  )                                           *)
(*  )                                           *)
(*                                              *)
(* This section defines reusable transformers that are *)
(* specialized to transform streams of characters into *)
(* something else, usually tokens.              *)
(* <boxed values 98>=                           *)
type 'a lexer = 'a lexer
(* The type [['a lexer]] should be pronounced ``lexer *)
(* returning [['a]].''                          *)

(* <support for lexical analysis>=              *)
fun isDelim c =
  Char.isSpace c orelse Char.contains "()[]{};" c
(* In popular languages, a character like a semicolon or *)
(* comma usually does not join with other tokens to form *)
(* a character. In this book, left and right brackets of *)
(* all shapes keep to themselves and don't group with *)
(* other characters. And in just about every    *)
(* non-esoteric language, blank space separates tokens. *)
(* A character whose presence marks the end of one token *)
(* (and possibly the beginning of the next) is called a *)
(* delimiter. In this book, the main delimiter  *)
(* characters are whitespace and parentheses. The other *)
(* delimiter is the semicolon, which introduces a *)
(* comment. [*]                                 *)
(* <boxed values 99>=                           *)
val _ = op isDelim : char -> bool
(* [[Char.isSpace]] recognizes all whitespace   *)
(* characters. [[Char.contains]] takes a string and a *)
(* character and says if the string contains the *)
(* character. These functions are in the initial basis *)
(* of Standard ML.                              *)

(* <support for lexical analysis>=              *)
val whitespace = many (sat Char.isSpace one)
(* All languages in this book ignore whitespace. Lexer *)
(* [[whitespace]] is typically combined with another *)
(* lexer using the [[*>]] operator.             *)
(* <boxed values 100>=                          *)
val _ = op whitespace : char list lexer
(* <support for lexical analysis>=              *)
fun intChars isDelim = 
  (curry (op ::) <$> eqx #"-" one <|> pure id) <*> many1 (sat Char.isDigit one)
                                                                              <*
  notFollowedBy (sat (not o isDelim) one)
(* The rules for integer literals are as follows: *)
(*                                              *)
(*   • The integer literal may begin with a minus sign. *)
(*   • It continues with one or more digits.  *)
(*   • If it is followed by character, that character *)
(*  must be a delimiter. (In other words, it must not *)
(*  be followed by a non-delimiter.)            *)
(*   • When the sequence of digits is converted to an *)
(*  [[int]], the arithmetic used in the conversion *)
(*  must not overflow.                          *)
(*                                              *)
(* Function [[intChars]] does the lexical analysis to *)
(* grab the characters; [[intFromChars]] handles the *)
(* conversion and its potential overflow, and   *)
(* [[intToken]] puts everything together. Because not *)
(* every language uses the same delimiters, both *)
(* [[intChars]] and [[intToken]] receive a predicate *)
(* that identifies delimiters.                  *)
(* <boxed values 101>=                          *)
val _ = op intChars : (char -> bool) -> char list lexer
(* Function [[Char.isDigit]], like [[Char.isSpace]], is *)
(* part of Standard ML.                         *)

(* <support for lexical analysis>=              *)
fun intFromChars (#"-" :: cs) = 
      intFromChars cs >>=+ Int.~
  | intFromChars cs =
      (OK o valOf o Int.fromString o implode) cs
      handle Overflow => ERROR
                        "this interpreter can't read arbitrarily large integers"
(* Function [[intFromChars]] composes three functions *)
(* from Standard ML's initial basis. Function   *)
(* [[implode]] converts a list of characters to a *)
(* string; [[Int.fromString]] converts a string to an \ *)
(* monoboxint option (raising [[Overflow]] if the *)
(* literal is too big); and [[valOf]] converts an \ *)
(* monoboxint option to an [[int]]. The [[Int. ]] *)
(* function, which is used when we see a minus sign, *)
(* negates an integer. The [[ ]] is meant to resemble a *)
(* ``high minus'' sign, a notational convention that *)
(* goes back at least to \apl.                  *)
(* <boxed values 102>=                          *)
val _ = op intFromChars : char list -> int error
(* <support for lexical analysis>=              *)
fun intToken isDelim =
  intFromChars <$>! intChars isDelim
(* In this book, every language except uProlog can use *)
(* [[intToken]].                                *)
(* <boxed values 103>=                          *)
val _ = op intToken : (char -> bool) -> int lexer
(* <support for lexical analysis>=              *)
datatype bracket_shape = ROUND | SQUARE | CURLY

fun leftString ROUND  = "("
  | leftString SQUARE = "["
  | leftString CURLY  = "{"
fun rightString ROUND  = ")"
  | rightString SQUARE = "]"
  | rightString CURLY = "}"
(* <support for lexical analysis>=              *)
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
(* <boxed values 104>=                          *)
type 'a plus_brackets = 'a plus_brackets
val _ = op bracketLexer : 'a lexer -> 'a plus_brackets lexer
(* The code is divided among these chunks:      *)
(* <common parsing code>=                       *)
(* <combinators and utilities for parsing located streams>= *)
type ('t, 'a) polyparser = ('t located eol_marked, 'a) xformer
(* <combinators and utilities for parsing located streams>= *)
fun token    stream = (snd <$> inline)      stream
fun noTokens stream = (notFollowedBy token) stream
(* The [[EOL]] and [[INLINE]] constructors are essential *)
(* for error recovery, but for parsing, they just get in *)
(* the way. Our first order of business is to define *)
(* analogs of [[one]] and [[eos]] that ignore [[EOL]]. *)
(* Parser [[token]] takes one token; parser [[srcloc]] *)
(* looks at the source-code location of a token, but *)
(* leaves the token in the input; and parser    *)
(* [[noTokens]] succeeds only if there are no tokens *)
(* left in the input. They are built on top of  *)
(* ``utility'' parsers [[eol]] and [[inline]]. The two *)
(* utility parsers have different contracts; [[eol]] *)
(* succeeds only when at [[EOL]], but [[inline]] scans *)
(* past [[EOL]] to look for [[INLINE]].         *)
(* <boxed values 106>=                          *)
val _ = op token    : ('t, 't)   polyparser
val _ = op noTokens : ('t, unit) polyparser
(* <combinators and utilities for parsing located streams>= *)
fun @@ p = pair <$> srcloc <*> p
(* Sometimes the easiest way to keep track of   *)
(* source-code locations is to pair a source-code *)
(* location with a result from a parser. This happens *)
(* just often enough that I find it worth while to *)
(* define the [[@@]] function. (Associate the word *)
(* ``at'' with the idea of ``location.'') The code uses *)
(* a dirty trick: it works because [[srcloc]] looks at *)
(* the input but does not consume any tokens.   *)
(* <boxed values 107>=                          *)
val _ = op @@ : ('t, 'a) polyparser -> ('t, 'a located) polyparser
(* <combinators and utilities for parsing located streams>= *)
infix 0 <?>
fun p <?> what = p <|> errorAt ("expected " ^ what) <$>! srcloc
(* Parsers that report errors                   *)
(*                                              *)
(* Most syntactic forms (expressions, unit tests, *)
(* definitions, and so on) are parsed by trying a set of *)
(* alternatives. When all alternatives fail, I usually *)
(* want to convert the failure into an error. Parser \ *)
(* monoboxp <?> what succeeds when [[p]] succeeds, but *)
(* when [[p]] fails, parser \monoboxp <?> what reports *)
(* an error: it expected [[what]]. The error says what *)
(* the parser was expecting, and it gives the   *)
(* source-code location of the unrecognized token. *)
(* If there is no token, there is no error—at end of *)
(* file, rather than signal an error, a parser made *)
(* using [[<?>]] fails. You can see an example in the *)
(* parser for extended definitions in \chunkref *)
(* mlschemea.chunk.xdef. [*]                    *)
(* <boxed values 108>=                          *)
val _ = op <?> : ('t, 'a) polyparser * string -> ('t, 'a) polyparser
(* The [[<?>]] operator must not be used to define a *)
(* parser that is passed to [[many]], [[many1]], or *)
(* [[optional]] In that context, if parser [[p]] fails, *)
(* it must not signal an error; it must instead *)
(* propagate the failure to [[many]], [[many1]], or *)
(* [[optional]], so those combinators know there is not *)
(* a [[p]] there.                               *)

(* <combinators and utilities for parsing located streams>= *)
infix 4 <!>
fun p <!> msg =
  fn tokens => (case p tokens
                  of SOME (OK _, unread) =>
                       (case peek srcloc tokens
                          of SOME loc => SOME (errorAt msg loc, unread)
                           | NONE => NONE)
                   | _ => NONE)
(* Another common error-detecting technique is to use a *)
(* parser [[p]] to detect some input that shouldn't be *)
(* there. For example, if we're just starting to read a *)
(* definition, the input shouldn't begin with a right *)
(* parenthesis. I can write a parser [[p]] that *)
(* recognizes a right parenthesis, but I can't simply *)
(* combine [[p]] with [[errorAt]] and [[srcloc]] in the *)
(* same way that [[<?>]] does, because I have two goals: *)
(* consume the tokens recognized by [[p]], and also *)
(* report the error at the location of the first of *)
(* those tokens. I can't use [[errorAt]] until after *)
(* [[p]] succeeds, but I have to use [[srcloc]] on the *)
(* input stream as it is before [[p]] is run. I solve *)
(* this problem by defining a special combinator that *)
(* keeps a copy of the tokens inspected by [[p]]. *)
(* If parser [[p]] succeeds, then parser [[p <!> msg]] *)
(* consumes the tokens consumed by [[p]] and reports *)
(* error [[msg]] at the location of [[p]]'s first token. *)
(* <boxed values 109>=                          *)
val _ = op <!> : ('t, 'a) polyparser * string -> ('t, 'b) polyparser
(* <combinators and utilities for parsing located streams>= *)
fun nodups (what, context) (loc, names) =
  let fun dup [] = OK names
        | dup (x::xs) = if List.exists (fn y : string => y = x) xs then
                          errorAt (what ^ " " ^ x ^ " appears twice in " ^
                                                                    context) loc
                        else
                          dup xs
  in  dup names
  end
(* Detection of duplicate names                 *)
(*                                              *)
(* Most of the languages in this book allow you to *)
(* define functions or methods that take formal *)
(* parameters. It is never permissible to use the same *)
(* name for formal parameters in two different  *)
(* positions. There are surprisingly many other places *)
(* where it's not acceptable to have duplicates in a *)
(* list of strings. Function [[nodups]] takes two *)
(* Curried arguments: a pair saying what kind of thing *)
(* might be duplicated and where it appeared, followed *)
(* by a pair containing a list of names and the *)
(* source-code location of the list. If there are no *)
(* duplicates, it returns [[OK]] applied to the list of *)
(* names; otherwise it returns an [[ERROR]].    *)
(* <boxed values 113>=                          *)
val _ = op nodups : string * string -> srcloc * name list -> name list error
(* Function [[List.exists]] is like the micro-Scheme *)
(* [[exists?]]. It is in the initial basis for  *)
(* Standard ML.                                 *)

(* Once the parser sees the opening parenthesis and the *)
(* keyword, failure is impossible: either parser [[p]] *)
(* parses stuff correctly, or there's an error. [*] *)
(* <transformers for interchangeable brackets>= *)
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
(* Parser [[right]] matches a right bracket by itself. *)
(* But quite commonly, we want to wrap another parser  *)
(* [[p]] in matching left and right brackets.   *)
(* If something goes wrong—say the brackets don't *)
(* match—we ought not to try to address the error in the *)
(* right-bracket parser alone; we need to be able to *)
(* report the location of the left bracket as well. *)
(* To be able to issue good error messages, I define *)
(* parser [[matchingRight]], which always succeeds and *)
(* which produces one of three outcomes:        *)
(*                                              *)
(*   • Result \monobox[[FOUND_RIGHT]] (loc, s) says we *)
(*  found a right bracket exactly where we expected *)
(*  to, and its shape and location are s and loc. *)

(* <transformers for interchangeable brackets>= *)
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
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* Function [[matchBrackets]] takes this result, along *)
(* with the left bracket and the parsed result a, and *)
(* knows what to do.                            *)
(* <boxed values 110>=                          *)
type right_result = right_result
val _ = op matchingRight : ('t, right_result) pb_parser
val _ = op scanToClose   : ('t, right_result) pb_parser
val _ = op matchBrackets : string -> bracket_shape located -> 'a -> right_result
                                                                     -> 'a error
(* <transformers for interchangeable brackets>= *)
fun liberalBracket (expected, p) =
  matchBrackets expected <$> sat notCurly left <*> p <*>! matchingRight
fun bracketKeyword (keyword, expected, p) =
  liberalBracket (expected, keyword *> (p <?> expected))
fun bracket (expected, p) =
  liberalBracket (expected, p <?> expected)
fun curlyBracket (expected, p) =
  matchBrackets expected <$> leftCurly <*> (p <?> expected) <*>! matchingRight
(* Story:                                       *)
(*                                              *)
(*   • Parser can fail, right bracket has to match: *)
(*  [[liberalBracket]]                          *)
(*   • Keyword can fail, but if it matches, parser has *)
(*  to match: [[bracketKeyword]]                *)
(*   • Left bracket can fail, but if it matches, parser *)
(*  has to match: [[bracket]], [[curlyBracket]] *)
(*                                              *)
(* <boxed values 111>=                          *)
val _ = op bracketKeyword : ('t, 'keyword) pb_parser * string * ('t, 'a)
                                                 pb_parser -> ('t, 'a) pb_parser
(* <transformers for interchangeable brackets>= *)
fun usageParser keyword =
  let val getkeyword = eqx #"(" one *> (implode <$> many1 (sat (not o isDelim)
                                                                           one))
  in  fn (usage, p) =>
        case getkeyword (streamOfList (explode usage))
          of SOME (OK k, _) => bracketKeyword (keyword k, usage, p)
           | _ => let exception BadUsage of string in raise BadUsage usage end
  end
(* Usually, we want to pull the keyword out of the usage *)
(* string. [*]                                  *)
(* <boxed values 112>=                          *)
val _ = op usageParser : (string -> ('t, string) pb_parser) -> string * ('t, 'a)
                                                 pb_parser -> ('t, 'a) pb_parser
(* Hello, stranger?                             *)
(* <transformers for interchangeable brackets>= *)
fun pretoken stream = ((fn PRETOKEN t => SOME t | _ => NONE) <$>? token) stream
(* <code used to debug parsers>=                *)
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
(* Code used to debug parsers                   *)
(*                                              *)
(* When debugging parsers, I often find it helpful to *)
(* dump out the tokens that a parser is looking at. *)
(* I want to dump all the tokens that are available *)
(* without triggering the action of reading another line *)
(* of input. I believe it's safe to read until I have *)
(* got to both an end-of-line marker and a suspension *)
(* whose value has not yet been demanded.       *)
(* <boxed values 114>=                          *)
val _ = op safeTokens : 'a located eol_marked stream -> 'a list
(* <code used to debug parsers>=                *)
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
(* The [[showErrorInput]] function transforms an *)
(* ordinary parser into a parser that, when it errors, *)
(* shows the input that caused the error. It should be *)
(* applied routinely to every parser you build. *)
(* <boxed values 115>=                          *)
val _ = op showErrorInput : ('t -> string) -> ('t, 'a) polyparser -> ('t, 'a)
                                                                      polyparser
(* <code used to debug parsers>=                *)
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
(* The [[wrapAround]] function can be used to wrap a *)
(* parser; it shows what the parser was looking for, *)
(* what tokens it was looking at, and whether it found *)
(* something.                                   *)
(* <boxed values 116>=                          *)
val _ = op wrapAround : ('t -> string) -> string -> ('t, 'a) polyparser -> ('t,
                                                                  'a) polyparser
(* <streams that issue two forms of prompts>=   *)
fun echoTagStream lines = 
  let fun echoIfTagged line =
        if (String.substring (line, 0, 2) = ";#" handle _ => false) then
          print line
        else
          ()
  in  postStream (lines, echoIfTagged)
  end
(* Testing support                              *)
(*                                              *)
(* Let's get the testing support out of the way first. *)
(* As in the C code, I want to print out any line read *)
(* that begins with the special string [[;#]]. This *)
(* string is a formal comment that helps me test chunks *)
(* marked \LAtranscript\RA. In the ML code, I can do the *)
(* job in a very modular way: I define a post-stream *)
(* action that prints any line meeting the criterion. *)
(* Function [[echoTagStream]] transforms a stream of *)
(* lines to a stream of lines, adding the behavior *)
(* I want.                                      *)
(* <boxed values 117>=                          *)
val _ = op echoTagStream : line stream -> line stream 
(* <streams that issue two forms of prompts>=   *)
fun stripAndReportErrors xs =
  let fun next xs =
        case streamGet xs
          of SOME (ERROR msg, xs) => (eprintln msg; next xs)
           | SOME (OK x, xs) => SOME (x, xs)
           | NONE => NONE
  in  streamOfUnfold next xs
  end
(* Issuing messages for error values            *)
(*                                              *)
(* Function [[stripAndReportErrors]] removes the *)
(* [[ERROR]] and [[OK]] tags from a stream, producing an *)
(* output stream with a simpler type. Values tagged with *)
(* [[OK]] are passed on to the output stream unchanged; *)
(* messages tagged with [[ERROR]] are printed to *)
(* standard error, using [[eprintln]].          *)
(* <boxed values 118>=                          *)
val _ = op stripAndReportErrors : 'a error stream -> 'a stream
(* <streams that issue two forms of prompts>=   *)
fun lexLineWith lexer =
  stripAndReportErrors o streamOfUnfold lexer o streamOfList o explode
(* An error detected during lexical analysis is printed *)
(* without any information about source-code locations. *)
(* That's because, to keep things somewhat simple, *)
(* I've chosen to do lexical analysis on one line at a *)
(* time, and I don't keep track of the line's   *)
(* source-code location.                        *)
(* <boxed values 119>=                          *)
val _ = op lexLineWith : 't lexer -> line -> 't stream
(* <streams that issue two forms of prompts>=   *)
fun parseWithErrors parser =
  let fun adjust (SOME (ERROR msg, tokens)) = SOME (ERROR msg, drainLine tokens)
        | adjust other = other
  in  streamOfUnfold (adjust o parser)
  end
(* When an error occurs during parsing, I drain the rest *)
(* of the tokens on the line where the error occurred. *)
(* I don't strip the errors at this point; errors are *)
(* passed on to the interactive stream because when an *)
(* error is detected, the prompt may need to be changed. *)
(* <boxed values 120>=                          *)
val _ = op parseWithErrors : ('t, 'a) polyparser -> 't located eol_marked stream
                                                              -> 'a error stream
(* <streams that issue two forms of prompts>=   *)
type prompts   = { ps1 : string, ps2 : string }
val stdPrompts = { ps1 = "-> ", ps2 = "   " }
val noPrompts  = { ps1 = "", ps2 = "" }
(* Prompts                                      *)
(*                                              *)
(* All interpreters in the book are built on the Unix *)
(* shell model of having two prompt strings. The first *)
(* prompt string, called [[ps1]], is issued when *)
(* starting to read a definition. The second prompt *)
(* string, called [[ps2]], is issued when in the middle *)
(* of reading a definition. To turn prompting off, we *)
(* set both to the empty string.                *)
(* <boxed values 121>=                          *)
type prompts = prompts
val _ = op stdPrompts : prompts
val _ = op noPrompts  : prompts
(* <streams that issue two forms of prompts>=   *)
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
(* To deliver the right prompt in the right situation, *)
(* I store the current prompt in a mutable cell called *)
(* [[thePrompt]]. The prompt is initially [[ps1]], and *)
(* it stays [[ps1]] until a token is delivered, at which *)
(* point the [[postStream]] action sets the prompt to  *)
(* [[ps2]]. But when we are about to get a new  *)
(* definition, a [[preStream]] action on the syntax *)
(* stream [[xdefs_with_errors]] resets the prompt to  *)
(* [[ps1]]. This combination of pre- and post-stream *)
(* actions, on different streams, makes sure the prompt *)
(* is always appropriate to the state of the parser. [*] *)
(* <boxed values 122>=                          *)
val _ = op interactiveParsedStream : 't lexer * ('t, 'a) polyparser -> string *
                                              line stream * prompts -> 'a stream
val _ = op lexAndDecorate : srcloc * line -> 't located eol_marked stream
  in  
      stripAndReportErrors (preStream (setPrompt ps1, xdefs_with_errors))
  end 
(* The functions defined in this appendix are useful for *)
(* reading all kinds of input, not just computer *)
(* programs, and I encourage you to use them in your own *)
(* projects. But here are two words of caution: with so *)
(* many abstractions in the mix, the parsers are tricky *)
(* to debug. And while some parsers built from  *)
(* combinators are very efficient, mine aren't. *)

(* <common parsing code ((elided))>=            *)
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
(* Supporting code for the ML interpreter for \uscheme *)
(*                                              *)
(* [*] [*] \invisiblelocaltableofcontents[*] This *)
(* appendix describes language-specific code that is *)
(* used to implement micro-Scheme but is not interesting *)
(* enough to include in \crefmlscheme.chap. This code *)
(* includes code for lexical analysis, for parsing, and *)
(* for running unit tests, as does a similar appendix *)
(* for every bridge language that is implemented in ML. *)
(* The code for micro-Scheme also includes an   *)
(* implementation of the ``unspecified'' values in the *)
(* operational semantics.                       *)
(*                                              *)
(* Lexical analysis and parsing                 *)
(*                                              *)
(* Lexical analysis and parsing is implemented by these *)
(* code chunks:                                 *)

(* The reusable function [[setup_error_format]] uses *)
(* interactivity to set the error format, which, as in *)
(* the C versions, determines whether syntax-error *)
(* messages include source-code locations (see functions *)
(* [[errorAt]] and [[synerrormsg]] in \crefpage *)
(* mlinterps.synerrormsg,mlinterps.errorAt).    *)
(* <shared utility functions for initializing interpreters>= *)
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
(* Utility function for limiting the depth of recursion *)
(*                                              *)
(* If there's no other overhead, MLton delivers *)
(* 25 million evals per second. Finding all solutions to *)
(* a Boolean formula requires on the order of 200. *)
(* <function application with overflow checking>= *)
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
(* Utility function for mutual recursion        *)
(*                                              *)
(* In Standard ML, mutually recursive functions are *)
(* typically defined using the [[and]] keyword. But such *)
(* a definition requires that the functions be adjacent *)
(* in the source code. When there are large mutual *)
(* recursions in which many functions participate, it is *)
(* often simpler to implement mutual recursion the way a *)
(* C programmer does: \stdbreak put each function in a *)
(* mutable reference cell and call indirectly through *)
(* the contents of that cell. But how is the cell to be *)
(* initialized? In C, initialization is handled by the *)
(* linker. In ML, we have to initialize the reference *)
(* cell when we create it; \stdbreak the cell doesn't *)
(* get its final value until the function it refers to *)
(* is defined. To initialize such a cell, I use function *)
(* [[forward]] to create an initial function. That *)
(* initial function, if ever called, causes a fatal *)
(* error. [*]                                   *)
(* <function [[forward]], for mutual recursion through mutable reference cells>= *)
fun forward what _ =
  let exception UnresolvedForwardDeclaration of string
  in  raise UnresolvedForwardDeclaration what
  end
(* For an example of [[forward]], see \string\chunkref: *)
(* chunk.first-use-of-forward. (THIS COULD POSSIBLY BE *)
(* ELIMINATED.)                                 *)

exception LeftAsExercise of string


(*****************************************************************)
(*                                                               *)
(*   ABSTRACT SYNTAX FOR \UPROLOG                                *)
(*                                                               *)
(*****************************************************************)

(* Finally, uProlog shares extended definitions with the *)
(* other bridge languages.                      *)
(* <abstract syntax for \uprolog>=              *)
(* Abstract syntax (and no values)              *)
(*                                              *)
(* Of all the languages in this book, Prolog has the *)
(* simplest structure. Unusually, Prolog does not *)
(* distinguish ``values'' from ``abstract syntax''; both *)
(* are represented as terms. A term is a logical *)
(* variable, a literal number, or an application of a *)
(* functor to a list of terms. (An atom is represented *)
(* as the application of a functor to an empty list *)
(* of terms.)                                   *)
(* <definitions of [[term]], [[goal]], and [[clause]] for \uprolog>= *)
datatype term = VAR     of name
              | LITERAL of int
              | APPLY   of name * term list
(* A term can be a functor applied to a list of terms; a *)
(* goal is a predicate applied to a list of terms. Goals *)
(* and applications have identical structure.   *)
(* <definitions of [[term]], [[goal]], and [[clause]] for \uprolog>= *)
type goal = name * term list
(* A clause is a conclusion and a list of premises, all *)
(* of which are goals. If the list of premises is empty, *)
(* the clause is a ``fact''; otherwise it is a ``rule,'' *)
(* but these distinctions are useful only for thinking *)
(* about and organizing programs—the underlying meanings *)
(* are the same. Writing our implementation in ML *)
(* enables us to use the identifier [[:-]] as a value *)
(* constructor for clauses.                     *)
(* <definitions of [[term]], [[goal]], and [[clause]] for \uprolog>= *)
datatype clause = :- of goal * goal list
infix 3 :-
(* At the read-eval-print loop, where a normal language *)
(* can present a true definition, a uProlog program can *)
(* either ask a query or add a clause to the database. *)
(* (The switch between query mode and rule mode is *)
(* hidden from the code in this chapter; the details are *)
(* buried in Section [->].) I group these actions into a *)
(* syntactic category called [[cq]], which is short for *)
(* clause-or-query. It is the Prolog analog of a true *)
(* definition [[def]]. [*]                      *)
(* <definitions of [[def]] and [[unit_test]] for \uprolog>= *)
datatype cq
  = ADD_CLAUSE of clause
  | QUERY      of goal list
type def = cq
(* uProlog includes three unit-test forms.      *)
(* <definitions of [[def]] and [[unit_test]] for \uprolog>= *)
datatype unit_test 
  = CHECK_SATISFIABLE   of goal list
  | CHECK_UNSATISFIABLE of goal list
  | CHECK_SATISFIED     of goal list * (name * term) list
(* <definition of [[xdef]] (shared)>=           *)
datatype xdef = DEF    of def
              | USE    of name
              | TEST   of unit_test
              | DEFS   of def list  (*OMIT*)
(* String conversions                           *)
(*                                              *)
(* This code converts terms, goals, and clauses to *)
(* strings.                                     *)
(* <definitions of [[termString]], [[goalString]], and [[clauseString]]>= *)
fun termString (APPLY ("cons", [car, cdr])) = 
      let fun tail (APPLY ("cons", [car, cdr])) = ", " ^ termString car ^ tail
                                                                             cdr
            | tail (APPLY ("nil",  []))         = "]"
            | tail x                           = "|" ^ termString x ^ "]"
      in  "[" ^ termString car ^ tail cdr
      end
  | termString (APPLY ("nil", [])) = "[]"
  | termString (APPLY (f, []))     = f
  | termString (APPLY (f, [x, y])) =
      if Char.isAlpha (hd (explode f)) then appString f x [y]
      else String.concat ["(", termString x, " ", f, " ", termString y, ")"]
  | termString (APPLY (f, h::t)) = appString f h t
  | termString (VAR v) = v
  | termString (LITERAL n) = intString n
and appString f h t =
      String.concat (f :: "(" :: termString h ::
                     foldr (fn (t, tail) => ", " :: termString t :: tail) [")"]
                                                                              t)
(* <definitions of [[termString]], [[goalString]], and [[clauseString]]>= *)
fun goalString g = termString (APPLY g)
fun clauseString (g :- []) = goalString g
  | clauseString (g :- (h :: t)) =
      String.concat (goalString g :: " :- " :: goalString h ::
                     (foldr (fn (g, tail) => ", " :: goalString g :: tail)) [] t
                                                                               )
(* <definitions of [[termString]], [[goalString]], and [[clauseString]]>= *)
fun substString pairs =
      separate ("no substitution", ", ")
      (map (fn (x, t) => x ^ " = " ^ termString t) pairs)


(*****************************************************************)
(*                                                               *)
(*   SUPPORT FOR TRACING \UPROLOG\ COMPUTATION                   *)
(*                                                               *)
(*****************************************************************)

(* Tracing code is helpful for debugging.       *)
(* <support for tracing \uprolog\ computation>= *)
val tracer = ref (app print)
val _ = tracer := (fn _ => ())
fun trace l = !tracer l
(* Complete implementation of uProlog           *)
(*                                              *)
(* Substitution                                 *)
(*                                              *)



(*****************************************************************)
(*                                                               *)
(*   SUBSTITUTION AND UNIFICATION                                *)
(*                                                               *)
(*****************************************************************)

(* <substitution and unification ((upr))>=      *)
datatype con = ~  of term * term
             | /\ of con  * con
             | TRIVIAL
infix 4 ~
infix 3 /\
(* <free variables of terms, goals, clauses>=   *)
fun termFreevars t =
  let fun f (VAR x,     xs) = insert (x, xs)
        | f (LITERAL _, xs) = xs
        | f (APPLY(_, args), xs) = foldl f xs args
  in  reverse (f (t, []))
  end  
fun goalFreevars goal = termFreevars (APPLY goal)
fun union' (s1, s2) = s1 @ diff (s2, s1)   (* preserves order *)
fun clauseFreevars (c :- ps) =
  foldl (fn (p, f) => union' (goalFreevars p, f)) (goalFreevars c) ps
(* Free variables                               *)
(*                                              *)
(* The function [[termFreevars]] computes the free *)
(* variables of a term. For readability, those free *)
(* variables are ordered by their first appearance in *)
(* the term, when reading from left to right. Similar *)
(* functions compute the free variables of goals and *)
(* clauses.                                     *)
(* <boxed values 66>=                           *)
val _ = op termFreevars   : term   -> name set
val _ = op goalFreevars   : goal   -> name set
val _ = op clauseFreevars : clause -> name set
(* <substitutions for \uprolog>=                *)
type subst = term env
val idsubst = emptyEnv
(* <boxed values 138>=                          *)
type subst = subst
val _ = op idsubst : subst
(* <substitutions for \uprolog>=                *)
fun varsubst theta = 
  (fn x => find (x, theta) handle NotFound _ => VAR x)
(* <boxed values 139>=                          *)
val _ = op varsubst : subst -> (name -> term)
(* <substitutions for \uprolog>=                *)
fun termsubst theta =
  let fun subst (VAR x)         = varsubst theta x
        | subst (LITERAL n)     = LITERAL n
        | subst (APPLY (f, ts)) = APPLY (f, map subst ts)
(* <boxed values 140>=                          *)
val _ = op termsubst : subst -> (term -> term)
  in  subst
  end
(* <substitutions for \uprolog>=                *)
fun goalsubst   theta (f, ts)   = (f, map (termsubst theta) ts)
fun clausesubst theta (c :- ps) = (goalsubst theta c :- map (goalsubst theta) ps
                                                                               )
(* Given the ability to substitute in a term, we may *)
(* also want to substitute in goals and clauses. *)
(* <boxed values 141>=                          *)
val _ = op goalsubst   : subst -> (goal   -> goal)
val _ = op clausesubst : subst -> (clause -> clause)
(* <substitutions for \uprolog>=                *)
fun consubst theta =
  let fun subst (t1 ~  t2) = termsubst theta t1 ~ termsubst theta t2
        | subst (c1 /\ c2) = subst c1 /\ subst c2
        | subst TRIVIAL    = TRIVIAL
  in  subst
  end
(* And we can substitute in constraints.        *)
(* <boxed values 142>=                          *)
val _ = op consubst : subst -> (con -> con)
(* <substitutions for \uprolog>=                *)
infix 7 |-->
fun x |--> (VAR x') = if x = x' then idsubst else bind (x, VAR x', emptyEnv)
  | x |--> t        = if member x (termFreevars t) then
                        raise InternalError "non-idempotent substitution"
                      else
                        bind (x, t, emptyEnv)
(* We create substitutions using the same infix operator *)
(* as in \crefml.chap.                          *)
(* <boxed values 143>=                          *)
val _ = op |--> : name * term -> subst
(* <substitutions for \uprolog>=                *)
fun dom theta = map (fn (a, _) => a) theta
fun compose (theta2, theta1) =
  let val domain  = union (dom theta2, dom theta1)
      val replace = termsubst theta2 o varsubst theta1
  in  map (fn a => (a, replace a)) domain
  end
(* Substitution, free variables, and unification *)
(*                                              *)
(* As part of type inference, \crefml.chap develops a *)
(* representation of substitutions, as well as utility *)
(* functions that apply substitutions to types. Prolog *)
(* uses the same representation, but instead of *)
(* substituting types for type variables, Prolog *)
(* substitutes terms for logical variables. The code, *)
(* which closely resembles the code in \crefml.chap, is *)
(* in \crefupra.substitution. Substitutions are *)
(* discovered by solving equality constraints, which are *)
(* defined here:                                *)
(* <boxed values 65>=                           *)
type subst = subst
val _ = op idsubst : subst
val _ = op |-->    : name * term -> subst
val _ = op varsubst    : subst -> (name   -> term)
val _ = op termsubst   : subst -> (term   -> term)
val _ = op goalsubst   : subst -> (goal   -> goal)
val _ = op clausesubst : subst -> (clause -> clause)
type con = con
val _ = op consubst    : subst -> (con -> con)
(* <substitution and unification ((upr))>=      *)
exception Unsatisfiable
(* <constraint solving ((prototype))>=          *)
fun solve c = raise LeftAsExercise "solve"
(* As in \crefml.chap, you implement the solver. Prolog *)
(* uses the same kind of equality constraints as ML type *)
(* inference, and it uses the same algorithm for the *)
(* solver. If a constraint cannot be solved, [[solve]] *)
(* must raise the [[Unsatisfiable]] exception. [*] *)
(* <boxed values 70>=                           *)
val _ = op solve : con -> subst
fun unify ((f, ts), (f', ts')) =
  solve (APPLY (f, ts) ~ APPLY (f', ts'))
(* Unification by solving equality constraints  *)
(*                                              *)
(* \makenwnotdef(left as exercise)              *)
(*                                              *)
(* To unify a goal with the head of a clause, we solve *)
(* an equality constraint.                      *)
(* <boxed values 69>=                           *)
val _ = op unify : goal * goal -> subst


(*****************************************************************)
(*                                                               *)
(*   RENAMING \UPROLOG\ VARIABLES                                *)
(*                                                               *)
(*****************************************************************)

(* <renaming \uprolog\ variables>=              *)
local
  val n = ref 1
in
  fun freshVar s = VAR ("_" ^ s ^ intString (!n) before n := !n + 1)
(* Renaming variables in clauses: ``Freshening'' *)
(*                                              *)
(* Every time a clause is used, its variables are *)
(* renamed. To rename a variable, I put an underscore in *)
(* front of its name and a unique integer after it. *)
(* Because the parser in Section [->] does not accept *)
(* variables whose names begin with an underscore, these *)
(* names cannot possibly conflict with the names of *)
(* variables that appear in source code.        *)
(* <boxed values 67>=                           *)
val _ = op freshVar : string -> term
end
(* <renaming \uprolog\ variables>=              *)
fun freshen c =
  let val renamings = map (fn x => x |--> freshVar x) (clauseFreevars c)
      val renaming  = foldl compose idsubst renamings
  in  clausesubst renaming c
  end
(* Function [[freshen]] replaces free variables with *)
(* fresh variables. Value [[renaming]] represents a *)
(* renaming \renaming, as in Section [->].      *)
(* <boxed values 68>=                           *)
val _ = op freshen : clause -> clause


(*****************************************************************)
(*                                                               *)
(*   LEXICAL ANALYSIS AND PARSING FOR \UPROLOG, PROVIDING [[CQSTREAM]] *)
(*                                                               *)
(*****************************************************************)

(* Lexical analysis                             *)
(*                                              *)
(* <lexical analysis and parsing for \uprolog, providing [[cqstream]]>= *)
(* <lexical analysis for \uprolog>=             *)
datatype token 
  = UPPER     of string
  | LOWER     of string
  | SYMBOLIC  of string
  | INT_TOKEN of int
  | RESERVED  of string
  | EOF
(* Tokens                                       *)
(*                                              *)
(* uProlog has a more complex lexical structure than *)
(* other languages. We have uppercase, lowercase, and *)
(* symbolic tokens, as well as integers. It simplifies *)
(* the parser if we distinguish reserved words and *)
(* symbols using [[RESERVED]]. Finally, because a *)
(* C-style uProlog comment can span multiple lines, *)
(* we have to be prepared for the lexical analyzer to *)
(* encounter end-of-file. Reading end of file needs to *)
(* be distinguishable from failing to read a token, so *)
(* I represent end of file by its own special token  *)
(* [[EOF]].                                     *)
(* <boxed values 146>=                          *)
type token = token
(* We need to print tokens in error messages.   *)
(* <lexical analysis for \uprolog>=             *)
fun tokenString (UPPER s)     = s
  | tokenString (LOWER s)     = s
  | tokenString (INT_TOKEN n) = intString n
  | tokenString (SYMBOLIC  s) = s
  | tokenString (RESERVED  s) = s
  | tokenString EOF           = "<end-of-file>"
(* Reserved words and anonymous variables       *)
(*                                              *)
(* Tokens formed from symbols or from lower-case letters *)
(* are usually symbolic, but sometimes they are reserved *)
(* words. And because the cut is nullary, not binary, it *)
(* is treated as an ordinary symbol, just like any other *)
(* nullary predicate.                           *)
(* <lexical analysis for \uprolog>=             *)
fun symbolic ":-" = RESERVED ":-"
  | symbolic "."  = RESERVED "."
  | symbolic "|"  = RESERVED "|"
  | symbolic "!"  = LOWER "!"
  | symbolic s    = SYMBOLIC s
fun lower "is" = RESERVED "is"
  | lower "check_satisfiable"   = RESERVED "check_satisfiable"
  | lower "check_unsatisfiable" = RESERVED "check_unsatisfiable"
  | lower "check_satisfied"     = RESERVED "check_satisfied"
  | lower s    = LOWER s
(* A variable consisting of a single underscore gets *)
(* converted to a unique ``anonymous'' variable. *)
(* <lexical analysis for \uprolog>=             *)
fun anonymousVar () =
  case freshVar ""
    of VAR v => UPPER v
     | _ => let exception ThisCan'tHappen in raise ThisCan'tHappen end
(* <lexical analysis for \uprolog>=             *)
local
  (* Classification of characters                 *)
  (*                                              *)
  (* The other languages in this book treat only  *)
  (* parentheses, digits, and semicolons specially. But in *)
  (* Prolog, we distinguish two kinds of names: symbolic *)
  (* and alphanumeric. A symbolic name like [[+]] is used *)
  (* differently from an alphanumeric name like [[add1]]. *)
  (* This difference is founded on a different    *)
  (* classification of characters. In uProlog, every *)
  (* character is either a symbol, an alphanumeric, a *)
  (* space, or a delimiter.                       *)
  (* <character-classification functions for \uprolog>= *)
  val symbols = explode "!%^&*-+:=|~<>/?`$\\"
  fun isSymbol c = List.exists (fn c' => c' = c) symbols
  fun isIdent  c = Char.isAlphaNum c orelse c = #"_"
  fun isDelim  c = not (isIdent c orelse isSymbol c)
  (* <lexical utility functions for \uprolog>=    *)
  fun underscore _ [] = OK (anonymousVar ())
    | underscore c cs = ERROR ("name may not begin with underscore at " ^
                                   implode (c::cs))

  fun int cs [] = intFromChars cs >>=+ INT_TOKEN
    | int cs ids = 
        ERROR ("integer literal " ^ implode cs ^
               " may not be followed by '" ^ implode ids ^ "'")
  (* Utility functions [[underscore]] and [[int]] make *)
  (* sure that an underscore or a sequence of digits, *)
  (* respectively, is never followed by any character that *)
  (* might be part of an alphanumeric identifier. When *)
  (* either of these functions succeeds, it returns an *)
  (* appropriate token.                           *)
  (* <boxed values 147>=                          *)
  val _ = op underscore : char      -> char list -> token error
  val _ = op int        : char list -> char list -> token error
  (* <lexical utility functions for \uprolog>=    *)
  fun unrecognized (ERROR _) = let exception Can'tHappen in raise Can'tHappen
                                                                             end
    | unrecognized (OK cs) =
        case cs
          of []        => NONE
           | #";" :: _ => let exception Can'tHappen in raise Can'tHappen end
           | _ =>
               SOME (ERROR ("invalid initial character in `" ^ implode cs ^ "'")
                                                                          , EOS)
  (* Utility function [[unrecognized]] is called when the *)
  (* lexical analyzer cannot recognize a sequence of *)
  (* characters. If the sequence is empty, it means *)
  (* there's no token. If anything else happens, an error *)
  (* has occurred.                                *)
  (* <boxed values 148>=                          *)
  val _ = op unrecognized : char list error -> ('a error * 'a error stream)
                                                                          option
  (* <lexical utility functions for \uprolog>=    *)
  fun nextline (file, line) = (file, line+1)
  (* When a lexical analyzer runs out of characters on a *)
  (* line, it calls [[nextline]] to compute the location *)
  (* of the next line.                            *)
  (* <boxed values 149>=                          *)
  val _ = op nextline : srcloc -> srcloc
in
  (* <lexical analyzers for for \uprolog>=        *)
  type 'a prolog_lexer = (char eol_marked, 'a) xformer
  fun char chars =
    case streamGet chars
      of SOME (INLINE c, chars) => SOME (OK c, chars) 
       | _ => NONE
  fun eol chars =
    case streamGet chars
      of SOME (EOL _, chars) => SOME (OK (), chars)
       | _ => NONE
  (* <lexical analyzers for for \uprolog>=        *)
  fun manySat p =
    many (sat p char)

  val whitespace =
    manySat Char.isSpace
  val intChars = 
    (curry op :: <$> eqx #"-" char <|> pure id) <*> many1 (sat Char.isDigit char
                                                                               )
  (* <boxed values 150>=                          *)
  type 'a prolog_lexer = 'a prolog_lexer
  val _ = op char : char prolog_lexer
  val _ = op eol  : unit prolog_lexer
  (* uProlog must be aware of the end of an input line. *)
  (* Lexical analyzers [[char]] and [[eol]] recognize a *)
  (* character and the end-of-line marker, respectively. *)

  (* Function [[manySat]] provides a general tool for *)
  (* sequences of characters. Lexers [[whitespace]] and *)
  (* [[intChars]] handle two common cases.        *)
  (* <boxed values 150>=                          *)
  val _ = op manySat    : (char -> bool) -> char list prolog_lexer
  val _ = op whitespace : char list prolog_lexer
  val _ = op intChars   : char list prolog_lexer
  (* <lexical analyzers for for \uprolog>=        *)
  val ordinaryToken =
        underscore            <$> eqx #"_" char <*>! manySat isIdent
    <|> (RESERVED o str)      <$> sat isDelim char                    
    <|> int                   <$> intChars    <*>! manySat isIdent
    <|> (symbolic o implode)  <$> many1 (sat isSymbol char)
    <|> curry (lower o implode o op ::) <$> sat Char.isLower char <*> manySat
                                                                         isIdent
    <|> curry (UPPER o implode o op ::) <$> sat Char.isUpper char <*> manySat
                                                                         isIdent
    <|> unrecognized o fst o valOf o many char
  (* <lexical analyzers for for \uprolog>=        *)
  local
    fun the c = eqx c char
  in
    fun tokenAt loc cs =  (* eta-expanded to avoid infinite regress *)
      (whitespace *> (   the #"/" *> the #"*" *> skipComment loc loc
                     <|> the #";" *> many char *> eol *> tokenAt (nextline loc)
                     <|>                          eol *> tokenAt (nextline loc)
                     <|> (loc, EOF) <$ eos
                     <|> pair loc <$> ordinaryToken
                     )) cs
    and skipComment start loc cs =
      (   the #"*" *> the #"/" *> tokenAt loc
      <|> char *> skipComment start loc
      <|> eol  *> skipComment start (nextline loc)
      <|> id <$>! pure (ERROR ("end of file looking for */ to close comment in "
                                                                               ^
                               srclocString start))
      ) cs
  (* An ordinary token is an underscore, delimiter, *)
  (* integer literal, symbolic name, or alphanumeric name. *)
  (* Uppercase and lowercase names produce different *)
  (* tokens.                                      *)
  (* <boxed values 151>=                          *)
  val _ = op ordinaryToken : token prolog_lexer
  (* We need two main lexical analyzers that keep track of *)
  (* source locations: [[tokenAt]] produces tokens, and *)
  (* [[skipComment]] skips comments. They are mutually *)
  (* recursive, and in order to delay the recursive calls *)
  (* until a stream is supplied, each definition has an *)
  (* explicit [[cs]] argument, which contains a stream of *)
  (* inline characters.                           *)
  (* <boxed values 151>=                          *)
  val _ = op tokenAt     : srcloc -> token located prolog_lexer
  val _ = op skipComment : srcloc -> srcloc -> token located prolog_lexer
  end
end
(* <parsers and streams for \uprolog>=          *)
type 'a parser = (token, 'a) polyparser
val token = token : token parser (* make it monomorphic *)
val symbol = (fn SYMBOLIC  s => SOME s | _ => NONE) <$>? token
val upper  = (fn UPPER     s => SOME s | _ => NONE) <$>? token
val lower  = (fn LOWER     s => SOME s | _ => NONE) <$>? token
val int    = (fn INT_TOKEN n => SOME n | _ => NONE) <$>? token
fun reserved s = eqx s ((fn RESERVED s => SOME s | _ => NONE) <$>? token)
(* Parsing                                      *)
(*                                              *)
(* [*]                                          *)
(*                                              *)
(* Utilities for parsing uProlog                *)
(*                                              *)
(* <boxed values 152>=                          *)
val _ = op symbol : string parser
val _ = op upper  : string parser
val _ = op lower  : string parser
val _ = op int    : int    parser
(* <parsers and streams for \uprolog>=          *)
val notSymbol =
  symbol <!> "arithmetic expressions must be parenthesized" <|>
  pure ()
(* We use these combinators to define the grammar from *)
(* Figure [->]. We use [[notSymbol]] to ensure that a *)
(* term like [[3 + X]] is not followed by another *)
(* symbol. This means we don't parse such terms as [[3 + *)
(* X + Y]].                                     *)
(* <boxed values 153>=                          *)
val _ = op notSymbol : unit parser
(* <parsers and streams for \uprolog>=          *)
fun nilt tokens = pure (APPLY ("nil", [])) tokens
fun cons (x, xs) = APPLY ("cons", [x, xs])
(* Parser [[nilt]] uses the empty list of tokens to *)
(* represent the empty list of terms. It needs an *)
(* explicit type constraint to avoid falling afoul of *)
(* the value restriction on polymorphism. Function *)
(* [[cons]] combines two terms, which is useful for *)
(* parsing lists.                               *)
(* <boxed values 154>=                          *)
val _ = op nilt : term parser
val _ = op cons : term * term -> term
(* <parsers and streams for \uprolog>=          *)
val variable        = upper
val binaryPredicate = symbol
val functr          = lower
fun commas p = 
  curry op :: <$> p <*> many (reserved "," *> p)
(* Here is one utility function [[commas]], plus *)
(* renamings of three other functions.          *)
(* <boxed values 155>=                          *)
val _ = op variable        : string parser
val _ = op binaryPredicate : string parser
val _ = op functr          : string parser
val _ = op commas : 'a parser -> 'a list parser
(* <parsers and streams for \uprolog>=          *)
fun closing bracket = reserved bracket <?> bracket
fun wrap left right p = reserved left *> p <* closing right
local
  fun consElems terms tail = foldr cons tail terms
  fun applyIs a t = APPLY ("is", [a, t])
  fun applyBinary x operator y = APPLY (operator, [x, y])
  fun maybeClause t NONE = t
    | maybeClause t (SOME ts) = APPLY (":-", t :: ts)
in
  fun term tokens = 
    (   applyIs <$> atom <* reserved "is" <*> (term <?> "term")
    <|> applyBinary <$> atom <*> binaryPredicate <*> (atom <?> "atom") <*
                                                                       notSymbol
    <|> atom 
    ) 
    tokens
  and atom tokens = 
    (   curry APPLY <$> functr <*> (wrap "(" ")" (commas (term <?> "term"))
                                   <|> pure []
                                   )
    <|> VAR     <$> variable
    <|> LITERAL <$> int
    <|> wrap "(" ")" (maybeClause <$> term <*> optional (reserved ":-" *> commas
                                                                          term))
    <|> wrap "[" "]" 
            (consElems <$> commas term <*> ( reserved "|" *> (term <?>
                                                                 "list element")
                                          <|> nilt
                                           )
           <|> nilt
            )
    )
    tokens
(* I spell ``functor'' without the ``o'' because in *)
(* Standard ML, [[functor]] is a reserved word. *)
(*                                              *)
(* \stdbreak                                    *)
(*                                              *)
(* Parsing terms, atoms, and goals              *)
(*                                              *)
(* We're now ready to parse uProlog. The grammar is *)
(* based on the grammar from \figrefpageprolog.syntax, *)
(* except that I'm using named function to parse atoms, *)
(* and I use some specialized tricks to organize the *)
(* grammar. Concrete syntax is not for the faint of *)
(* heart.                                       *)
(* <boxed values 156>=                          *)
val _ = op term   : term parser
val _ = op atom   : term parser
val _ = op commas : 'a parser -> 'a list parser
end
(* <parsers and streams for \uprolog>=          *)
fun asGoal _   (APPLY g) = OK g
  | asGoal loc (VAR v)   = 
      errorAt ("Variable " ^ v ^ " cannot be a predicate") loc
  | asGoal loc (LITERAL n) =
      errorAt ("Integer " ^ intString n ^ " cannot be a predicate") loc

val goal = asGoal <$> srcloc <*>! term 
(* <boxed values 157>=                          *)
val _ = op asGoal : srcloc -> term -> goal error
val _ = op goal   : goal parser
(* <parsers and streams for \uprolog>=          *)
datatype concrete
  = BRACKET of string 
  | CLAUSE  of goal * goal list option
  | GOALS   of goal list
  | CTEST   of unit_test
(* <parsers and streams for \uprolog>=          *)
fun checkSatisfied goals =
  let fun split (gs', []) = OK (CHECK_SATISFIED (reverse gs', []))
        | split (gs', rest as ("=", _) :: _) =
             validate ([], rest) >>=+
             (fn subst => CHECK_SATISFIED (reverse gs', subst))
        | split (gs', g :: gs) = split (g :: gs', gs)
      and validate (theta', ("=", [VAR x, t]) :: gs) =
            validate ((x, t) :: theta', gs)
        | validate (theta', ("=", [t1, t2]) :: gs) =
            ERROR ("in check_satisfied, " ^ termString t1 ^ " is set to " ^
                   termString t2 ^ ", but " ^ termString t1 ^
                                                           " is not a variable")
        | validate (theta', g :: gs) =
            ERROR ("in check_satisfied, expected a substitution but got " ^
                   goalString g)
        | validate (theta', []) = OK (reverse theta')
  in  split ([] , goals)
  end
(* Recognizing concrete syntax using modes      *)
(*                                              *)
(* I put together the uProlog parser in three layers. *)
(* The bottom layer is the concrete syntax itself. For a *)
(* moment let's ignore the meaning of uProlog's syntax *)
(* and look only at what can appear. At top level, we *)
(* might see                                    *)
(*                                              *)
(*   • A string in brackets                   *)
(*   • A clause containing a [[:-]] symbol    *)
(*   • A list of one or more goals separated by commas *)
(*   • A unit test                            *)
(*                                              *)
(* The meanings of some of these things can be depend on *)
(* which mode the interpreter is in. So I parse them *)
(* first into a value of type [[concrete]], and I worry *)
(* about the interpretation later.              *)
(* <boxed values 158>=                          *)
type concrete = concrete
(* Among the unit tests, parsing [[check-satisfied]] is *)
(* a bit tricky: we get a list of goals, which must be *)
(* split into ``real'' goals [[gs']] and        *)
(* ``substitution'' goals [[rest]]. A ``substitution'' *)
(* goal is an application of the [[=]] functor. *)
(* <boxed values 158>=                          *)
val _ = op checkSatisfied : goal list -> unit_test error
(* <parsers and streams for \uprolog>=          *)
val unit_test =
     reserved "check_satisfiable" *>
        (wrap "(" ")" (CHECK_SATISFIABLE <$> commas goal)
        <?> "check_satisfiable(goal, ...)")
 <|> reserved "check_unsatisfiable" *>
        (wrap "(" ")" (CHECK_UNSATISFIABLE <$> commas goal)
        <?> "check_unsatisfiable(goal, ...)")
 <|> reserved "check_satisfied" *>
        (wrap "(" ")" (checkSatisfied <$>! commas goal)
         <?> "check_satisfied(goal, ... [, X1 = t1, ...])")
(* The three unit tests are recognized and treated *)
(* specially.                                   *)
(* <boxed values 159>=                          *)
val _ = op unit_test : unit_test parser
(* <parsers and streams for \uprolog>=          *)
val notClosing =
  sat (fn RESERVED "]" => false | _ => true) token
val concrete = 
     (BRACKET o concat o map tokenString) <$> wrap "[" "]" (many notClosing)
 <|> CTEST <$> unit_test
 <|> curry CLAUSE <$> goal <*> reserved ":-" *> (SOME <$> commas goal)
 <|> GOALS <$> commas goal
(* <boxed values 160>=                          *)
val _ = op concrete : concrete parser
(* Compared with unit tests, [[concrete]] values are *)
(* easy to parse.                               *)

(* <parsers and streams for \uprolog>=          *)
datatype mode = QMODE | RMODE
fun mprompt RMODE = "-> "
  | mprompt QMODE = "?- "
(* [*] In most cases, we know what a [[concrete]] value *)
(* is supposed to mean, but there's one case in which we *)
(* don't: a phrase like ``[[color(yellow).]]'' could be *)
(* either a clause or a query. To know which is meant, *)
(* we have to know the mode. In other words, the mode *)
(* distinguishes [[CLAUSE(g, NONE)]] from [[GOALS [g]]]. *)
(* A parser may be in either query mode or rule (clause) *)
(* mode. Each mode has its own prompt.          *)
(* <boxed values 161>=                          *)
type mode = mode
val _ = op mprompt : mode -> string
(* <parsers and streams for \uprolog>=          *)
datatype xdef_or_mode
  = XDEF of xdef
  | NEW_MODE of mode
(* The concrete syntax normally means a clause or query, *)
(* which is denoted by the syntactic nonterminal symbol *)
(* clause-or-query and represented by an ML value of *)
(* type [[cq]] (see chunk [->] in \chaprefprolog). But *)
(* particular concrete syntax, such as ``[[[rule].]]'' *)
(* or ``[[[query].]],'' can be an instruction to change *)
(* to a new mode. The middle layer of uProlog's parser *)
(* produces a value of type [[xdef_or_mode]], which is *)
(* defined as follows:                          *)
(* <boxed values 162>=                          *)
type xdef_or_mode = xdef_or_mode
(* <parsers and streams for \uprolog>=          *)
fun interpretConcrete mode =
  let val (newMode, cq, xdef) = (OK o NEW_MODE, OK o XDEF o DEF, OK o XDEF)
  in  fn c =>
        case (mode, c)
          of (_, BRACKET "rule")     => newMode RMODE
           | (_, BRACKET "fact")     => newMode RMODE
           | (_, BRACKET "user")     => newMode RMODE
           | (_, BRACKET "clause")   => newMode RMODE
           | (_, BRACKET "query")    => newMode QMODE
           | (_, BRACKET s)          => xdef (USE s)
           | (_, CTEST t)            => xdef (TEST t)
           | (RMODE, CLAUSE (g, ps)) => cq (ADD_CLAUSE (g :- getOpt (ps, [])))
           | (RMODE, GOALS [g])      => cq (ADD_CLAUSE (g :- []))
           | (RMODE, GOALS _ ) =>
                 ERROR ("You cannot enter a query in clause mode; " ^
                        "to change modes, type `[query].'")
           | (QMODE, GOALS gs)           => cq (QUERY gs)
           | (QMODE, CLAUSE (g, NONE))   => cq (QUERY [g])
           | (QMODE, CLAUSE (_, SOME _)) => 
                 ERROR ("You cannot enter a new clause in query mode; " ^
                        "to change modes, type `[rule].'")
  end                 
(* The next level of uProlog's parser interpreters a *)
(* [[concrete]] value according to the mode. [[BRACKET]] *)
(* values and unite tests are interpreted in the same *)
(* way regardless of mode, but clauses and especially *)
(* [[GOALS]] are interpreted differently in rule mode *)
(* and in query mode.                           *)
(* <boxed values 163>=                          *)
val _ = op interpretConcrete : mode -> concrete -> xdef_or_mode error
(* <parsers and streams for \uprolog>=          *)
val skippable = 
  (fn SYMBOLIC "." => NONE | EOF => NONE | t => SOME t) <$>? token

fun badConcrete (loc, skipped) last =
  ERROR (srclocString loc ^ ": expected clause or query; skipping" ^
         concat (map (fn t => " " ^ tokenString t) (skipped @ last)))

fun xdef_or_mode mode = interpretConcrete mode <$>!
  (   concrete <* reserved "."
  <|> badConcrete <$> @@ (many  skippable) <*>! ([RESERVED "."] <$ reserved ".")
  <|> badConcrete <$> @@ (many1 skippable) <*>! pure []  (* skip to EOF *)
  )
(* Parser [[xdef_or_mode]] m parses a [[concrete]] *)
(* according to mode m. If it sees something it doesn't *)
(* recognize, it emits an error message and skips ahead *)
(* until it sees a dot or the end of the input. *)
(* Importantly, this parser never fails: it always *)
(* returns either a [[xdef_or_mode]] value or an error *)
(* message.                                     *)
(* <boxed values 164>=                          *)
val _ = op xdef_or_mode : mode -> xdef_or_mode parser
(* <parsers and streams for \uprolog>=          *)
fun xdefsInMode initialMode (name, lines, prompts) =
  let val { ps1, ps2 } = prompts
      val thePrompt = ref (if ps1 = "" then "" else mprompt initialMode)
      val setPrompt = if ps1 = "" then (fn _ => ()) else (fn s => thePrompt := s
                                                                               )

      type read_state = string * mode * token located eol_marked stream
      (* <utility functions for [[cqstream]]>=        *)
      fun startsWithEOF tokens =
        case streamGet tokens
          of SOME (INLINE (_, EOF), _) => true
           | _ => false
      (* Function [[getXdef]] uses [[startsWithEOF]] to check *)
      (* if the input stream has no more tokens.      *)
      (* <boxed values 166>=                          *)
      val _ = op startsWithEOF : token located eol_marked stream -> bool
      (* <utility functions for [[cqstream]]>=        *)
      fun skipPastDot tokens =
        case streamGet tokens
          of SOME (INLINE (_, RESERVED "."), tokens) => tokens
           | SOME (INLINE (_, EOF), tokens) => tokens
           | SOME (_, tokens) => skipPastDot tokens
           | NONE => tokens
      (* If [[getXdef]] detects an error, it skips tokens in *)
      (* the input up to and including the next dot.  *)
      (* <boxed values 167>=                          *)
      val _ = op skipPastDot : token located eol_marked stream -> token located
                                                               eol_marked stream
      (* <utility functions for [[cqstream]]>=        *)
      fun getXdef (ps1, mode, tokens) =
        ( setPrompt ps1
        ; if startsWithEOF tokens then
            NONE
          else
            case xdef_or_mode mode tokens
              of SOME (OK (XDEF d),        tokens) => SOME (d, (ps1, mode,
                                                                        tokens))
               | SOME (OK (NEW_MODE mode), tokens) => getXdef (mprompt mode,
                                                                   mode, tokens)
               | SOME (ERROR msg,          tokens) => 
                                               ( eprintln ("syntax error: " ^
                                                                            msg)
                                               ; getXdef (ps1, mode, skipPastDot
                                                                         tokens)
                                               )
               | NONE =>
                       (* <fail epically with a diagnostic about [[tokens]]>= *)
                         let exception ThisCan'tHappenCqParserFailed
                             val tokensStrings =
                               map (fn t => " " ^ tokenString t) o valOf o peek
                                                                    (many token)
                             val _ = app print (tokensStrings tokens)
                         in  raise ThisCan'tHappenCqParserFailed
                         end
        )                 
      (* Function [[getXdef]] tracks the prompt, the mode, and *)
      (* the remaining unread tokens, which together form the *)
      (* [[read_state]]. It also, when called, sets the *)
      (* prompt.                                      *)
      (* <boxed values 168>=                          *)
      val _ = op getXdef : read_state -> (xdef * read_state) option
      (* Parser [[xdef_or_mode]] is always supposed to return *)
      (* something. If it doesn't, I issue an epic error *)
      (* message.                                     *)


      val lines = preStream (fn () => print (!thePrompt), echoTagStream lines)

      val chars = 
        streamConcatMap
        (fn (loc, s) => streamOfList (map INLINE (explode s) @ [EOL (snd loc)]))
        (locatedStream (name, lines))

      fun getLocatedToken (loc, chars) =
        (case tokenAt loc chars
           of SOME (OK (loc, t), chars) => SOME (OK (loc, t), (loc, chars))
            | SOME (ERROR msg,   chars) => SOME (ERROR msg,   (loc, chars))
            | NONE => NONE
        ) before setPrompt ps2

      val tokens =
        stripAndReportErrors (streamOfUnfold getLocatedToken ((name, 1), chars))

(* Reading clauses and queries while tracking locations *)
(* and modes                                    *)
(*                                              *)
(* To produce a stream of definitions, every other *)
(* language in this book uses the function      *)
(* [[interactiveParsedStream]] from page [->]. uProlog *)
(* can't: [[interactiveParsedStream]] doesn't tag tokens *)
(* with locations, and it doesn't keep track of modes. *)
(* As a replacement, I define a somewhat more complex *)
(* function, [[cqstream]], below. At the core of *)
(* [[cqstream]] is function [[getXdef]].        *)
(* <boxed values 165>=                          *)
val _ = op xdefsInMode : mode -> string * line stream * prompts -> xdef stream
type read_state  = read_state  fun zz__checktyperead_state (x : read_state ) = (
                           x :  string * mode * token located eol_marked stream)
val _ = op getXdef : read_state -> (xdef * read_state) option
  in  streamOfUnfold getXdef (!thePrompt, initialMode, streamMap INLINE tokens)
  end 
(* Using [[INLINE]] may look strange, but many of the *)
(* utility functions from \crefapp:lazyparse expect a *)
(* stream of tokens tagged with [[INLINE]]. Even though *)
(* we don't need [[INLINE]] for uProlog, it is easier to *)
(* use a meaningless [[INLINE]] than it is to rewrite *)
(* big chunks of \crefapp:lazyparse.            *)

val xdefstream = xdefsInMode RMODE
(* <shared definitions of [[filexdefs]] and [[stringsxdefs]]>= *)
fun filexdefs (filename, fd, prompts) = xdefstream (filename, filelines fd,
                                                                        prompts)
fun stringsxdefs (name, strings) = xdefstream (name, streamOfList strings,
                                                                      noPrompts)
(* Streams of extended definitions              *)
(*                                              *)
(* Every language has its own parser, called    *)
(* [[xdefstream]], which converts a stream of lines to a *)
(* stream of [[xdef]]s. But as in \cref         *)
(* cinterps.shared-xdef-streams, the convenience *)
(* functions [[filexdefs]] and [[stringsxdefs]] are *)
(* shared.                                      *)
(* <boxed values 58>=                           *)
val _ = op xdefstream   : string * line stream     * prompts -> xdef stream
val _ = op filexdefs    : string * TextIO.instream * prompts -> xdef stream
val _ = op stringsxdefs : string * string list               -> xdef stream


(*****************************************************************)
(*                                                               *)
(*   EVALUATION, TESTING, AND THE READ-EVAL-PRINT LOOP FOR \UPROLOG *)
(*                                                               *)
(*****************************************************************)

(* The evaluation parts are organized as follows: *)
(* <evaluation, testing, and the read-eval-print loop for \uprolog>= *)
(* <\uprolog's database of clauses>=            *)
type database = clause list
val emptyDatabase = []
fun addClause (r, rs) = rs @ [r] (* must maintain order *)
fun potentialMatches (_, rs) = rs
(* My representation is a list. As a result, I treat *)
(* every clause as a potential match.           *)
(* <boxed values 64>=                           *)
type database = database
val _ = op emptyDatabase    : database
val _ = op addClause        : clause * database -> database
val _ = op potentialMatches : goal * database -> clause list
(* <functions [[eval]], [[is]], and [[compare]], used in primitive predicates>= *)
fun eval (LITERAL n) = n
  | eval (APPLY ("+", [x, y])) = eval x  +  eval y
  | eval (APPLY ("*", [x, y])) = eval x  *  eval y
  | eval (APPLY ("-", [x, y])) = eval x  -  eval y
  | eval (APPLY ("/", [x, y])) = eval x div eval y
  | eval (APPLY ("-", [x]))    = 0 - eval x
  | eval (APPLY (f, _))        = 
      raise RuntimeError (f ^ " is not an arithmetic predicate " ^
                          "or is used with wrong arity")
  | eval (VAR v) = raise RuntimeError ("Used uninstantiated variable " ^ v ^
                                       " in arithmetic expression")
(* Primitive predicate [[is]] requires a very small *)
(* evaluator. Because it works only with integers, never *)
(* with variables, the evaluator doesn't need an *)
(* environment.                                 *)
(* <boxed values 74>=                           *)
val _ = op eval : term -> int
(* Predicate x[[ is ]]e evaluates term e as an integer *)
(* expression and constrains it to equal x.     *)
(* <functions [[eval]], [[is]], and [[compare]], used in primitive predicates>= *)
fun is [x, e] succ fail = (succ (solve (x ~ LITERAL (eval e))) fail
                           handle Unsatisfiable => fail())
  | is _      _    fail = fail ()
(* A comparison predicate is applied to exactly two *)
(* arguments. If these arguments aren't integers, it's a *)
(* run-time error. If they are, ML function [[cmp]] *)
(* determines the success or failure of the predicate. *)
(* <functions [[eval]], [[is]], and [[compare]], used in primitive predicates>= *)
fun compare name cmp [LITERAL n, LITERAL m] succ fail =
      if cmp (n, m) then succ idsubst fail else fail ()
  | compare name _ [_, _] _ _ =
      raise RuntimeError ("Used comparison " ^ name ^ " on non-integer term")
  | compare name _ _ _ _ =
      raise InternalError ("this can't happen---non-binary comparison?!")
(* There are four comparison predicates.        *)

(* [*] Create a tracing version of the interpreter that *)
(* logs every entry to and exit from a Byrd box. Use the *)
(* following functions:                         *)
(* <tracing functions>=                         *)
fun logSucc goal succ theta resume =
  ( app print ["SUCC: ", goalString goal, " becomes ",
               goalString (goalsubst theta goal), "\n"]
  ; succ theta resume
  )
fun logFail goal fail () = 
  ( app print ["FAIL: ", goalString goal, "\n"]
  ; fail ()
  )
fun logResume goal resume () = 
  ( app print ["REDO: ", goalString goal, "\n"]
  ; resume ()
  )
fun logSolve solve goal succ fail = 
  ( app print ["START: ", goalString goal, "\n"]
  ; solve goal succ fail
  )
(* <search ((prototype))>=                      *)
fun 'a query database =
  let val primitives = foldl (fn ((n, p), rho) => bind (n, p, rho))
                       emptyEnv (
                              (* Primitives                                   *)

                              (*                                              *)

                     (* This section describes uProlog's handful of primitive *)

                              (* predicates, starting with [[true]].          *)

                              (* <\uprolog's primitive predicates [[::]]>=    *)
                                 ("true", fn args => fn succ => fn fail =>
                                            if null args then succ idsubst fail
                                                                else fail ()) ::

                     (* Predicate [[atom]] tests to see if its argument is an *)

                              (* atom.                                        *)

                              (* <\uprolog's primitive predicates [[::]]>=    *)
                                 ("atom", fn args => fn succ => fn fail =>
                                             case args of [APPLY(f, [])] => succ
                                                                    idsubst fail
                                                        | _ => fail ()) ::

                      (* Printing a term always succeeds, and it produces the *)

                              (* identity substitution.                       *)

                              (* <\uprolog's primitive predicates [[::]]>=    *)
                                 ("print", fn args => fn succ => fn fail =>
                                             ( app (fn x => (print (termString x
                                                             ); print " ")) args
                                             ; print "\n"
                                             ; succ idsubst fail
                                             )) ::

                              (* <\uprolog's primitive predicates [[::]]>=    *)
                                 ("is", is) ::

                              (* <\uprolog's primitive predicates [[::]]>=    *)
                                 ("<",  compare "<"  op < ) ::
                                 (">",  compare ">"  op > ) ::
                                 ("=<", compare "=<" op <= ) ::
                                 (">=", compare ">=" op >= ) ::

                          (* Each predicate above takes as argument a list of *)

                              (* terms, a success continuation, and a failure *)

                     (* continuation. Two more predicates, [[!]] and [[not]], *)

                     (* cannot be implemented using this technique; they have *)

                        (* to be added directly to the interpreter (Exercises *)

                      (* [->] and [->]). This code ensures that they can't be *)

                              (* used by mistake.                             *)

                              (* <\uprolog's primitive predicates [[::]]>=    *)
                                 ("!",   fn _ => raise RuntimeError
                              "The cut (!) must be added to the interpreter") ::
                                 ("not", fn _ => raise RuntimeError
                      "Predicate `not' must be added to the interpreter") :: [])
      fun solveOne (goal as (predicate, args)) succ fail =
            find (predicate, primitives) args succ fail
            handle NotFound _ =>
              let fun search [] = fail ()
                    | search (clause :: clauses) =  
                        let fun resume () = search clauses
                            val G :- Hs = freshen clause
                            val theta = unify (goal, G)
                        in  solveMany (map (goalsubst theta) Hs) theta succ
                                                                          resume
                        end
                        handle Unsatisfiable => search clauses
(* Here is the code:                            *)
(* <boxed values 71>=                           *)
val _ = op query : database -> goal list  -> (subst -> (unit->'a) -> 'a) -> (
                                                                 unit->'a) -> 'a
val _ = op solveOne  : goal               -> (subst -> (unit->'a) -> 'a) -> (
                                                                 unit->'a) -> 'a
val _ = op solveMany : goal list -> subst -> (subst -> (unit->'a) -> 'a) -> (
                                                                 unit->'a) -> 'a
val _ = op search    : clause list -> 'a
              in  search (potentialMatches (goal, database))
              end
      and solveMany []            theta succ fail = succ theta fail
        | solveMany (goal::goals) theta succ fail =
            solveOne goal
            (fn theta' => fn resume => solveMany (map (goalsubst theta') goals)
                                                 (compose (theta', theta))
                                                 succ
                                                 resume)
            fail
  in  fn gs => solveMany gs idsubst
  end
(* The environment [[primitives]] holds the primitive *)
(* predicates. These predicates are implemented by *)
(* polymorphic ML functions, and as a result, ML's *)
(* ``value restriction'' prevents me from defining *)
(* [[primitives]] at top level. To work around the *)
(* restriction, function [[query]] rebuilds     *)
(* [[primitives]] once per query. Luckily the cost is *)
(* small compared with the cost of the search.  *)

(* <interaction>=                               *)
fun showAndContinue interactivity theta gs =
  let fun varResult x = x ^ " = " ^ termString (varsubst theta x)
      val vars = foldr union' emptyset (map goalFreevars gs)
      val results = separate ("", "\n") (map varResult vars)
  in  if null vars then
        false (* no more solutions possible; don't continue *)
      else
        ( print results
        ; if prompts interactivity then
            case Option.map explode (TextIO.inputLine TextIO.stdIn)
              of SOME (#";" :: _) => (print "\n"; true)
               | _ => false
          else
            (print "\n"; false)
        )
  end
(* To show a solution, we apply the substitution to the *)
(* free varables of the query. If we're prompting, we *)
(* wait for a line of input. If the line begins with a *)
(* semicolon, we continue; otherwise we quit. If we're *)
(* not prompting, we're in batch mode, and we produce at *)
(* most one solution.                           *)
(* <boxed values 73>=                           *)
val _ = op showAndContinue : interactivity -> subst -> goal list -> bool
(* <shared definition of [[withHandlers]]>=     *)
fun withHandlers f a caught =
  f a
  handle RuntimeError msg   => caught ("Run-time error <at loc>: " ^ msg)
       | NotFound x         => caught ("Name " ^ x ^ " not found <at loc>")
       | Located (loc, exn) =>
           withHandlers (fn _ => raise exn) a (fn s => caught (fillAtLoc (s, loc
                                                                             )))
       (* In addition to [[RuntimeError]], [[NotFound]], and *)
       (* [[Located]], [[withHandlers]] catches many exceptions *)
       (* that are predefined ML's Standard Basis Library. *)
       (* These exceptions signal things that can go wrong *)
       (* while evaluating an expression or when reading a *)
       (* file.                                        *)

(* <other handlers that catch non-fatal exceptions and pass messages to [[caught]]>= *)
       | Div                => caught ("Division by zero <at loc>")
       | Overflow           => caught ("Arithmetic overflow <at loc>")
       | Subscript          => caught ("Array index out of bounds <at loc>")
       | Size               => caught (
                                "Array length too large (or negative) <at loc>")
       | IO.Io { name, ...} => caught ("I/O error <at loc>: " ^ name)
       (* I reuse the same exception handlers in later *)
       (* interpreters.                                *)

(* <definitions of [[basis]] and [[processDef]] for \uprolog>= *)
type basis = database
fun processDef (cq, database, interactivity) =
  let fun process (ADD_CLAUSE c) = addClause (c, database)
        | process (QUERY gs) = (
                           (* To issue a query, I provide success and failure *)

                           (* continuations to the [[query]] function defined *)

                              (* above. The success continuation uses         *)

                        (* [[showAndContinue]] to decide between two possible *)

                        (* next steps: resume the search and look for another *)

                              (* solution, or just say ``yes'' and stop.      *)

                              (* <query goals [[gs]] against [[database]]>=   *)
                                query database gs
                                  (fn theta => fn resume =>
                                     if showAndContinue interactivity theta gs
                                              then resume () else print "yes\n")
                                  (fn () => print "no\n"); database)
      fun caught msg = (eprintln (stripAtLoc msg); database)
  in  withHandlers process cq caught
  end
fun dump_names db = println "cannot dump uProlog names"  (*OMIT*)
(* Processing clauses and queries               *)
(*                                              *)
(* uProlog's basis is the database of queries. uProlog *)
(* uses the same generic read-eval-print loop as the *)
(* other interpreters; a ``definition'' is either a *)
(* clause or a query.                           *)
(* <boxed values 72>=                           *)
type basis = basis
val _ = op processDef : cq * database * interactivity -> database
(* Here is the promised [[failtest]].           *)
(* <shared unit-testing utilities>=             *)
fun failtest strings = (app eprint strings; eprint "\n"; false)
(* In each bridge language, test results are reported *)
(* the same way. If there are no tests, there is no *)
(* report. (The report's format is stolen from the *)
(* DrRacket programming environment.)           *)
(* <shared unit-testing utilities>=             *)
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
(* <definition of [[testIsGood]] for \uprolog>= *)
fun testIsGood (test, database) =
  let
(* <definitions of [[checkSatisfiedPasses]] and [[checkUnsatisfiablePasses]]>= *)
      type query = goal list
      val qstring = separate ("?", ", ") o map goalString
      (* If a query fails a test, we print it using function *)
      (* [[qstring]].                                 *)
      (* <boxed values 145>=                          *)
      type query = query
      val _ = op qstring : query -> string
      (* All three unit tests work by passing appropriate *)
      (* success and failure continuations to [[query]]. *)
      (* To pass the [[check-unsatisfiable]] test, the query *)
      (* must be unsatisfiable. If the test fails, the *)
      (* satisfying substitution is shown without logical *)
      (* variables that are introduced by renaming clauses. *)
      (* Such variables begin with underscores, and they are *)
      (* removed by function [[stripSubst]].          *)

(* <definitions of [[checkSatisfiedPasses]] and [[checkUnsatisfiablePasses]]>= *)
      fun stripSubst theta = List.filter (fn (x, _) => String.sub (x, 0) <> #"_"
                                                                         ) theta
      fun checkUnsatisfiablePasses (gs) =
        let fun succ theta' _ =
              failtest ["check_unsatisfiable failed: ", qstring gs,
                          " is satisfiable with ", substString theta']
            fun fail () = true
        in  query database gs (succ o stripSubst) fail
        end
      (* To pass the [[check-satisfiable]] test, the query *)
      (* must be satisfiable.                         *)

(* <definitions of [[checkSatisfiedPasses]] and [[checkUnsatisfiablePasses]]>= *)
      fun checkSatisfiablePasses (gs) =
        let fun succ _ _ = true
            fun fail () = failtest ["check_unsatisfiable failed: ", qstring gs,
                                    " is not satisfiable"]
        in  query database gs succ fail
        end
      (* The [[check-satisfied]] test has an explicit *)
      (* substitution \subsn, and if that substitution has no *)
      (* logical variables, the test passes only if the query *)
      (* \subsn(\listv*g) is satisfied by the identity *)
      (* substitution. (Logical variables introduced by *)
      (* renaming don't count.) If \subsn includes logical *)
      (* variables, \subsn(\listv*g) merely has to be *)
      (* satisfiable.                                 *)

(* <definitions of [[checkSatisfiedPasses]] and [[checkUnsatisfiablePasses]]>= *)
      fun checkSatisfiedPasses (gs, theta) =
        let val thetaVars =
              foldl (fn ((_, t), fv) => union (termFreevars t, fv)) emptyset
                                                                           theta
            val ground = null thetaVars
            val gs' = map (goalsubst theta) gs
            fun succ theta' _ =
              if ground andalso not (null theta') then
                failtest ["check_satisfied failed: ", qstring gs,
                          " required additional substitution ", substString
                                                                         theta']
              else
                true
            fun fail () =
              failtest ["check_satisfied failed: could not prove ", qstring gs']
        in  query database gs' (succ o stripSubst) fail
        end
      fun passes (CHECK_UNSATISFIABLE gs)      = checkUnsatisfiablePasses gs
        | passes (CHECK_SATISFIABLE   gs)      = checkSatisfiablePasses gs
        | passes (CHECK_SATISFIED (gs, theta)) = checkSatisfiedPasses (gs, theta
                                                                               )
(* Substitutions compose just as in \crefml.chap. *)
(* <boxed values 144>=                          *)
val _ = op compose : subst * subst -> subst
(* Unit testing                                 *)
(*                                              *)
(* Unit testing in uProlog is different from any other *)
(* unit testing: we check for satisfiability, or when *)
(* given an explicit substitution, we check that the *)
(* substitution satisfies the given query.      *)
(* <boxed values 144>=                          *)
val _ = op testIsGood : unit_test * basis -> bool
  in  passes test
  end
(* <shared definition of [[processTests]]>=     *)
fun numberOfGoodTests (tests, rho) =
  foldr (fn (t, n) => if testIsGood (t, rho) then n + 1 else n) 0 tests
fun processTests (tests, rho) =
      reportTestResults (numberOfGoodTests (tests, rho), length tests)
(* Function [[processTests]] is shared among all bridge *)
(* languages. For each test, it calls the       *)
(* language-dependent [[testIsGood]], adds up the number *)
(* of good tests, and reports the result. [*]   *)
(* <boxed values 39>=                           *)
val _ = op processTests : unit_test list * basis -> unit
(* <shared read-eval-print loop and [[processPredefined]]>= *)
fun processPredefined (def,basis) = 
  processDef (def, basis, noninteractive)
(* When reading definitions of predefined functions, *)
(* there's no interactivity.                    *)
(* <boxed values 21>=                           *)
val _ = op noninteractive    : interactivity
val _ = op processPredefined : def * basis -> basis
(* <shared read-eval-print loop and [[processPredefined]]>= *)
fun readEvalPrintWith errmsg (xdefs, basis, interactivity) =
  let val unitTests = ref []

(* <definition of [[processXDef]], which can modify [[unitTests]] and call [[errmsg]]>= *)
      fun processXDef (xd, basis) =
        let (* Let's see the generic code that ``processes'' an *)
            (* extended definition. To process a [[USE]] form, *)
            (* we call function [[useFile]], which reads definitions *)
            (* from a file and recursively passes them to   *)
            (* [[readEvalPrintWith]].                       *)
            (* <definition of [[useFile]], to read from a file>= *)
            fun useFile filename =
              let val fd = TextIO.openIn filename
                  val (_, printing) = interactivity
                  val inter' = (NOT_PROMPTING, printing)
              in  readEvalPrintWith errmsg (filexdefs (filename, fd, noPrompts),
                                                                  basis, inter')
                  before TextIO.closeIn fd
              end
            (* To make uProlog more compatible with other   *)
            (* implementations of Prolog, I patch the [[useFile]] *)
            (* function defined in \crefmlscheme.chap. If   *)
            (* [[useFile]] fails with an I/O error, I try adding  *)
            (* ``[[.P]]'' to the name; this is the convention used *)
            (* by XSB Prolog. If adding [[.P]] fails, I try adding  *)
            (* ``[[.pl]]''; this is the convention used by  *)
            (* GNU Prolog and SWI Prolog.                   *)
            (* <definition of [[useFile]], to read from a file>= *)
            val try = useFile
            fun useFile filename = 
              try filename          handle IO.Io _ => 
              try (filename ^ ".P") handle IO.Io _ => 
              try (filename ^ ".pl")
            fun try (USE filename) = useFile filename
              | try (TEST t)       = (unitTests := t :: !unitTests; basis)
              | try (DEF def)      = processDef (def, basis, interactivity)
              | try (DEFS ds)      = foldl processXDef basis (map DEF ds)
                                                                        (*OMIT*)
            fun caught msg = (errmsg (stripAtLoc msg); basis)
            val _ = resetOverflowCheck ()     (* OMIT *)
        in  withHandlers try xd caught
        end 
      (* The extended-definition forms [[USE]] and [[TEST]] *)
      (* are implemented in exactly the same way for every *)
      (* language: internal function [[try]] passes each *)
      (* [[USE]] to [[useFile]], and it adds each [[TEST]] to *)
      (* the mutable list [[unitTests]]—just as in the C code *)
      (* in \secrefpageimpcore.readevalprint. Function [[try]] *)
      (* passes each true definition [[DEF]] to function *)
      (* [[processDef]], which does the language-dependent *)
      (* work.                                        *)
      (* <boxed values 24>=                           *)
      val _ = op errmsg     : string -> unit
      val _ = op processDef : def * basis * interactivity -> basis
      val basis = streamFold processXDef basis xdefs
      val _     = processTests (!unitTests, basis)
(* Function [[readEvalPrintWith]] has a type that *)
(* resembles the type of the C function         *)
(* [[readevalprint]], but the ML version takes an extra *)
(* parameter [[errmsg]]. Using this parameter, I issue a *)
(* special error message when there's a problem in the *)
(* initial basis (see function [[predefinedError]] on \ *)
(* cpagerefmlinterps.predefinedError). \mdbuse  *)
(* mlinterpspredefinedError The special error message *)
(* helps with some of the exercises in \cref    *)
(* typesys.chap,ml.chap, where if something goes wrong *)
(* with the implementation of types, an interpreter *)
(* could fail while trying to read its initial basis. *)
(* (Failure while reading the basis can manifest in *)
(* mystifying ways; the special message demystifies the *)
(* failure.) \mlsflabelreadEvalPrintWith [*]    *)
(* <boxed values 23>=                           *)
val _ = op readEvalPrintWith : (string -> unit) ->                     xdef
                                         stream * basis * interactivity -> basis
val _ = op processXDef       : xdef * basis -> basis
  in  basis
  end
(* Function [[readEvalPrintWith]] executes essentially *)
(* the same imperative actions as the C function *)
(* [[readevalprint]] (\chunkref                 *)
(* scheme.chunk.readevalprint): allocate space for a *)
(* list of pending unit tests; loop through a stream of *)
(* extended definitions, using each one to update the *)
(* environment(s); and process the pending unit tests. *)
(* (The looping action in the ML code is implemented by *)
(* function [[streamFold]], which applies       *)
(* [[processXDef]] to every element of [[xdefs]]. *)
(* Function [[streamFold]] is the stream analog of the *)
(* list function [[foldl]].) Unlike the C       *)
(* [[readevalprint]], which updates the environment *)
(* in place by writing through a pointer, the   *)
(* ML function ends by returning the updated environment *)
(* (s).                                         *)



(*****************************************************************)
(*                                                               *)
(*   FUNCTION [[RUNAS]] FOR \UPROLOG                             *)
(*                                                               *)
(*****************************************************************)

(* <function [[runAs]] for \uprolog>=           *)
fun runAs interactivity = 
  let val _ = setup_error_format interactivity
      val (prompts, prologMode) =
        if prompts interactivity then (stdPrompts, QMODE) else (noPrompts, RMODE
                                                                               )
      val xdefs =
        xdefsInMode prologMode ("standard input", filelines TextIO.stdIn,
                                                                        prompts)
  in  ignore (readEvalPrintWith eprintln (xdefs, emptyDatabase, interactivity))
  end 
(* Command line                                 *)
(*                                              *)
(* uProlog's command-line processor differs from our *)
(* other interpreters, because it has to deal with *)
(* modes. When prompting, it starts in query mode; when *)
(* not prompting, it starts in rule mode.       *)
(* <boxed values 169>=                          *)
val _ = op runAs : interactivity -> unit


(*****************************************************************)
(*                                                               *)
(*   CODE THAT LOOKS AT \UPROLOG'S COMMAND-LINE ARGUMENTS AND CALLS [[RUNAS]] *)
(*                                                               *)
(*****************************************************************)

(* The [[-q]] option is as in other interpreters, and *)
(* the [[-trace]] option turns on tracing.      *)
(* <code that looks at \uprolog's command-line arguments and calls [[runAs]]>= *)
fun runmain ["-q"]          = runAs (NOT_PROMPTING, PRINTING)
  | runmain []              = runAs (PROMPTING,     PRINTING)
  | runmain ("-trace" :: t) = (tracer := app eprint; runmain t)
  | runmain _  =
      TextIO.output (TextIO.stdErr,
                     "Usage: " ^ CommandLine.name() ^ " [trace] [-q]\n")
val _ = runmain (CommandLine.arguments())
