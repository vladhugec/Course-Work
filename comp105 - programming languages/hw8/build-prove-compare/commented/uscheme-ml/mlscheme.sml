(* Pulling the pieces together in the right order *)
(*                                              *)
(* As mentioned in the introduction to this chapter, the *)
(* ML language requires that every type and function be *)
(* defined before it is used. Definitions come not only *)
(* from this chapter but also from \cref        *)
(* lazyparse.chap,mlschemea.chap. To get all the *)
(* definitions in the right order, I use Noweb code *)
(* chunks. The interpreters differ in detail, but each *)
(* is put together along the same lines: shared *)
(* infrastructure; abstract syntax and values, with *)
(* utility functions; lexical analysis and parsing; *)
(* evaluation (including unit testing and the   *)
(* read-eval-print loop); and initialization. As shown *)
(* in the next chapter, interpreters for typed languages *)
(* also have chunks devoted to types and type checking *)
(* (or type inference). [*]                     *)
(* <mlscheme.sml>=                              *)


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
(* <boxed values 68>=                           *)
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
(* <boxed values 64>=                           *)
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
(* <boxed values 65>=                           *)
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
(* <boxed values 66>=                           *)
val _ = op <*> : ('a, 'b -> 'c) xformer * ('a, 'b) xformer -> ('a, 'c) xformer
(* <stream transformers and their combinators>= *)
infixr 4 <$>
fun f <$> p = pure f <*> p
(* The common case of creating [[tx_f]] using [[pure]] *)
(* is normally written using the special operator [[< *)
(* >]], which is also pronounced ``applied to.'' *)
(* It combines a B-to-C function with an \atob  *)
(* transformer to produce an \atoc transformer. *)
(* <boxed values 67>=                           *)
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
(* <boxed values 69>=                           *)
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
(* <boxed values 70>=                           *)
val _ = op pzero : ('a, 'b) xformer
(* <boxed values 70>=                           *)
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
(* <boxed values 71>=                           *)
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
(* <boxed values 72>=                           *)
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
(* <boxed values 73>=                           *)
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
(* <boxed values 74>=                           *)
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
(* <boxed values 75>=                           *)
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
(* <boxed values 76>=                           *)
val _ = op sat : ('b -> bool) -> ('a, 'b) xformer -> ('a, 'b) xformer
(* <stream transformers and their combinators>= *)
fun eqx y = 
  sat (fn y' => y = y') 
(* Transformer [[eqx b]] is [[sat]] specialized to an *)
(* equality predicate. It is typically used to recognize *)
(* special characters like keywords and minus signs. *)
(* <boxed values 77>=                           *)
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
(* <boxed values 78>=                           *)
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
(* <boxed values 79>=                           *)
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
(* <boxed values 80>=                           *)
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
(* <boxed values 81>=                           *)
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
(* <boxed values 82>=                           *)
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
(* <boxed values 83>=                           *)
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
(* <boxed values 84>=                           *)
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
(* <boxed values 92>=                           *)
type 'a eol_marked = 'a eol_marked
val _ = op drainLine : 'a eol_marked stream -> 'a eol_marked stream
(* <boxed values 92>=                           *)
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
(* <boxed values 85>=                           *)
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
(* <boxed values 86>=                           *)
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
(* <boxed values 87>=                           *)
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
(* <boxed values 88>=                           *)
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
(* <boxed values 89>=                           *)
val _ = op intFromChars : char list -> int error
(* <support for lexical analysis>=              *)
fun intToken isDelim =
  intFromChars <$>! intChars isDelim
(* In this book, every language except uProlog can use *)
(* [[intToken]].                                *)
(* <boxed values 90>=                           *)
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
(* <boxed values 91>=                           *)
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
(* <boxed values 93>=                           *)
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
(* <boxed values 94>=                           *)
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
(* <boxed values 95>=                           *)
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
(* <boxed values 96>=                           *)
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
(* <boxed values 100>=                          *)
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
(* <boxed values 97>=                           *)
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
(* <boxed values 98>=                           *)
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
(* <boxed values 99>=                           *)
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
(* <boxed values 101>=                          *)
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
(* <boxed values 102>=                          *)
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
(* <boxed values 103>=                          *)
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
(* <boxed values 104>=                          *)
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
(* <boxed values 105>=                          *)
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
(* <boxed values 106>=                          *)
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
(* <boxed values 107>=                          *)
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
(* <boxed values 108>=                          *)
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
(* <boxed values 109>=                          *)
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
(*   ABSTRACT SYNTAX AND VALUES FOR \USCHEME                     *)
(*                                                               *)
(*****************************************************************)

(* <abstract syntax and values for \uscheme>=   *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* \advanceby 3.5pt                             *)
(*                                              *)
(* \toprule                                     *)
(* Exceptions                                   *)
(* raised at run                                *)
(* time                                         *)
(* \midrule        A name was looked up in an   *)
(* NotFound        environment but not found there. *)
(* BindListLength  A call to [[bindList]] tried to *)
(*              extend an environment, but it passed *)
(*              two lists (names and values) of *)
(*              different lengths.              *)
(* RuntimeError    Something else went wrong during *)
(*              evaluation, i.e., during the    *)
(*              execution of [[eval]].          *)
(* \bottomrule                                  *)
(*                                              *)
(* Exceptions defined especially for this interpreter  *)
(* [*]                                          *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* Abstract syntax and values                   *)
(*                                              *)
(* An abstract-syntax tree can contain a literal value. *)
(* A value, if it is a closure, can contain an  *)
(* abstract-syntax tree. These two types are therefore *)
(* mutually recursive, so I define them together, using *)
(* [[and]].                                     *)
(*                                              *)
(* These particular types use as complicated a nest of *)
(* definitions as you'll ever see. The keyword  *)
(* [[datatype]] defines a new algebraic datatype; the *)
(* keyword [[withtype]] introduces a new type   *)
(* abbreviation that is mutually recursive with the *)
(* [[datatype]]. The first group of [[and]] keywords *)
(* define additional algebraic datatypes, and the second *)
(* group of [[and]] keywords define additional type *)
(* abbreviations. Everything in the whole nest is *)
(* mutually recursive. [*] [*]                  *)
(* <definitions of [[exp]] and [[value]] for \uscheme>= *)
datatype exp = LITERAL of value
             | VAR     of name
             | SET     of name * exp
             | IFX     of exp * exp * exp
             | WHILEX  of exp * exp
             | BEGIN   of exp list
             | APPLY   of exp * exp list
             | LETX    of let_kind * (name * exp) list * exp
             | LAMBDA  of lambda
and let_kind = LET | LETREC | LETSTAR
and    value = NIL
             | BOOLV     of bool   
             | NUM       of int
             | SYM       of name
             | PAIR      of value * value
             | CLOSURE   of lambda * value ref env
             | PRIMITIVE of primitive
withtype primitive = exp * value list -> value (* raises RuntimeError *)
     and lambda    = name list * exp
(* The representations are the same as in C, with these *)
(* exceptions:                                  *)
(*                                              *)
(*   • In a [[LETX]] expression, the bindings are *)
(*  represented by a list of pairs, not a pair of *)
(*  lists—just like environments.             *)
(*   • In the representation of a primitive function, *)
(*  there's no need for an integer tag. As shown in \ *)
(*  crefmlscheme.primitives below, ML's higher-order *)
(*  functions makes it easy to create groups of *)
(*  primitives that share code without having to *)
(*  resort to tags.                             *)
(*   • None of the fields of [[exp]], [[value]], or *)
(*  [[lambda]] is named. Instead of being referred to *)
(*  by name, those fields are referred to by pattern *)
(*  matching.                                   *)
(*                                              *)
(* A primitive function that goes wrong raises the *)
(* [[RuntimeError]] exception, which is the ML  *)
(* equivalent of calling [[runerror]].          *)

(* True definitions, unit tests, and extended   *)
(* definitions are all as in the C code, except again, *)
(* fields are not named. These true definitions are used *)
(* only in micro-Scheme; the unit tests are shared with *)
(* the untyped language uSmalltalk (\crefsmall.chap), *)
(* and the extended definitions are shared with all *)
(* other languages. [*]                         *)
(* <definition of [[def]] for \uscheme>=        *)
datatype def  = VAL    of name * exp
              | EXP    of exp
              | DEFINE of name * lambda
(* [*]                                          *)
(* <definition of [[unit_test]] for untyped languages (shared)>= *)
datatype unit_test = CHECK_EXPECT of exp * exp
                   | CHECK_ASSERT of exp
                   | CHECK_ERROR  of exp
(* <definition of [[xdef]] (shared)>=           *)
datatype xdef = DEF    of def
              | USE    of name
              | TEST   of unit_test
              | DEFS   of def list  (*OMIT*)
(* <definition of [[valueString]] for \uscheme, \tuscheme, and \nml>= *)
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
(* String conversion                            *)
(*                                              *)
(* A micro-Scheme value is an S-expression, and *)
(* converting it to a string is mostly straightforward. *)
(* The only tricky bit is printing lists made up of cons *)
(* cells ([[PAIR]]s); function [[tail]] is mutually *)
(* recursive with [[valueString]], by being defined *)
(* inside [[valueString]], and it implements the same *)
(* list-printing algorithm as function [[printtail]] in *)
(* \crefpageschemea.printtail.imp. (The algorithm goes *)
(* back to McCarthy.) [*]                       *)
(* <boxed values 4>=                            *)
val _ = op valueString : value -> string
(* Function [[valueString]] provides our first  *)
(* comprehensive demonstration of pattern matching over *)
(* the algebraic data type [[value]]. Function  *)
(* [[valueString]] takes one argument and is implemented *)
(* using a case analysis on that argument, but the case *)
(* analysis is defined by pattern matching. There is a *)
(* case for each datatype constructor of the [[value]] *)
(* type; the left-hand side of each case contains a *)
(* pattern match that applies the constructor to a *)
(* variable, to a pair of variables, or to the special *)
(* ``wildcard'' pattern [[_]] (the underscore). All the *)
(* variables in the pattern match, except the wildcard, *)
(* are introduced into the environment and are available *)
(* for use on the right-hand side of the [[=]], just as *)
(* if they had been bound by a micro-Scheme [[let]] or *)
(* ML [[val]].                                  *)

(* To print information about a failed test, we need *)
(* function [[expString]].                      *)
(* <definition of [[expString]] for \uscheme>=  *)
fun expString e =
  let fun bracket s = "(" ^ s ^ ")"
      val bracketSpace = bracket o spaceSep
      fun exps es = map expString es
      fun withBindings (keyword, bs, e) =
        bracket (spaceSep [keyword, bindings bs, expString e])
      and bindings bs = bracket (spaceSep (map binding bs))
      and binding (x, e) = bracket (x ^ " " ^ expString e)
      val letkind = fn LET => "let" | LETSTAR => "let*" | LETREC => "letrec"
  in  case e
        of LITERAL (v as NUM   _) => valueString v
         | LITERAL (v as BOOLV _) => valueString v
         | LITERAL v => "'" ^ valueString v
         | VAR name => name
         | SET (x, e) => bracketSpace ["set", x, expString e]
         | IFX (e1, e2, e3) => bracketSpace ("if" :: exps [e1, e2, e3])
         | WHILEX (cond, body) =>
                         bracketSpace ["while", expString cond, expString body]
         | BEGIN es => bracketSpace ("begin" :: exps es)
         | APPLY (e, es) => bracketSpace (exps (e::es))
         | LETX (lk, bs, e) => bracketSpace [letkind lk, bindings bs, expString
                                                                              e]
         | LAMBDA (xs, body) => bracketSpace ["lambda", bracketSpace xs,
                                                                 expString body]
  end
(* All these type definitions, together with definitions *)
(* of functions [[valueString]] and [[expString]], are *)
(* pulled together in one Noweb code chunk labeled *)
(* [[]].                                        *)
(* <boxed values 2>=                            *)
val _ = op valueString      : value -> string
val _ = op expString        : exp   -> string
(* The string-conversion functions take the place of the *)
(* C code's extensible printer—ML does not provide a *)
(* function like [[printf]], but it provides plenty of *)
(* primitives for creating, manipulating, and combining *)
(* strings.                                     *)



(*****************************************************************)
(*                                                               *)
(*   UTILITY FUNCTIONS ON \USCHEME, \TUSCHEME, AND \NML\ VALUES  *)
(*                                                               *)
(*****************************************************************)

(* <utility functions on \uscheme, \tuscheme, and \nml\ values>= *)
fun embedList []     = NIL
  | embedList (h::t) = PAIR (h, embedList t)
fun embedBool b = BOOLV b
fun bool (BOOLV b) = b
  | bool _         = true
(* Embedding and projection                     *)
(*                                              *)
(* A micro-Scheme S-expression can represent an integer, *)
(* Boolean, name, function, list, etc. We may sometimes *)
(* have an ML Boolean, list, or function that we wish to *)
(* represent as an S-expression, or similarly, an *)
(* S-expression that we wish to represent as a value of *)
(* type [[bool]]. Here we define mappings between type  *)
(* [[value]] and some other ML types. Because the set of *)
(* values representable by an ML value of type [[value]] *)
(* strictly contains each of the sets of values *)
(* representable by these ML types, these mappings are *)
(* called embedding and projection. Because the *)
(* [[value]] type is strictly larger than these *)
(* ML types, no embedding operation ever fails, but a *)
(* projection operation might. [This property is a *)
(* general characteristic of any embedding/projection *)
(* pair. A mathematician would say that an embedding e *)
(* of S into S' is an injection from S-->S'. The *)
(* corresponding projection pi_e is a left inverse of *)
(* the embedding; that is pi_e oe is the identity *)
(* function on S. There is no corresponding guarantee *)
(* for e opi_e; for example, pi_e may be undefined (_|_) *)
(* on some elements of S', or e(pi_e(x)) may not equal x *)
(* . ] For example, although any ML function of type *)
(* [[value -> bool]] can be embedded into [[value]] by *)
(* using the [[PRIMITIVE]] constructor, there are values *)
(* of type [[value]] that cannot be projected into an *)
(* ML function of type [[value -> bool]].       *)
(*                                              *)
(* Lists and Booleans are straightforward. [*] [*] *)
(* <boxed values 3>=                            *)
val _ = op embedBool : bool       -> value
val _ = op embedList : value list -> value
val _ = op bool      : value      -> bool
(* Function [[bool]] is the projection function, mapping *)
(* micro-Scheme values into ML Booleans. Unlike some *)
(* projection functions, [[bool]] is total: it always *)
(* succeeds. The operational semantics of micro-Scheme *)
(* treats any value other than [[#f]] as a true value, *)
(* so by projecting every non-Boolean micro-Scheme value *)
(* to [[true]], [[bool]] reflects the semantics. *)

(* <utility functions on \uscheme, \tuscheme, and \nml\ values>= *)
fun equalatoms (NIL,      NIL    )  = true
  | equalatoms (NUM  n1,  NUM  n2)  = (n1 = n2)
  | equalatoms (SYM  v1,  SYM  v2)  = (v1 = v2)
  | equalatoms (BOOLV b1, BOOLV b2) = (b1 = b2)
  | equalatoms  _                   = false
(* Equality                                     *)
(*                                              *)
(* The interpreter uses equality in two places: in the *)
(* [[=]] primitive and in the [[check-expect]] unit *)
(* test. The primitive version permits only atoms to be *)
(* considered equal.                            *)
(* <boxed values 5>=                            *)
val _ = op equalatoms : value * value -> bool
(* In a unit test written with [[check-expect]], lists *)
(* are compared for equality structurally, the way the *)
(* micro-Scheme function [[equal?]] does.       *)

(* <utility functions on \uscheme, \tuscheme, and \nml\ values>= *)
fun equalpairs (PAIR (car1, cdr1), PAIR (car2, cdr2)) =
      equalpairs (car1, car2) andalso equalpairs (cdr1, cdr2)
  | equalpairs (v1, v2) = equalatoms (v1, v2)
(* <boxed values 6>=                            *)
val _ = op equalpairs : value * value -> bool
(* The testing infrastructure expects this function to *)
(* be called [[testEqual]].                     *)

(* <utility functions on \uscheme, \tuscheme, and \nml\ values>= *)
val testEqual = equalpairs
(* <boxed values 7>=                            *)
val _ = op testEqual : value * value -> bool
(* <utility functions on \uscheme, \tuscheme, and \nml\ values>= *)
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
(* Unspecified values                           *)
(*                                              *)
(* In a [[val]] or [[letrec]] binding, the operational *)
(* semantics of micro-Scheme call for the allocation of *)
(* a location containing an unspecified value. My C code *)
(* chooses a value at random, but the initial basis of *)
(* Standard ML has no random-number generator. So unlike *)
(* the C [[unspecified]] function in \chunkref  *)
(* schemea.chunk.unspecified, the ML version just cycles *)
(* through a few different values. It's enough to *)
(* prevent careless people from assuming that such a *)
(* value is always [[NIL]].                     *)
(* <boxed values 124>=                          *)
val _ = op cycleThrough : 'a list -> (unit -> 'a)
val _ = op unspecified  : unit -> value
(* Scheme, S-expressions, and first-class functions *)
(*                                              *)
(* [*] \invisiblelocaltableofcontents[*]        *)
(*                                              *)
(* \notationgroupmicro-Scheme \makenowebnotdef(generated *)
(* automatically)                               *)
(*                                              *)




(*****************************************************************)
(*                                                               *)
(*   LEXICAL ANALYSIS AND PARSING FOR \USCHEME, PROVIDING [[FILEXDEFS]] AND [[STRINGSXDEFS]] *)
(*                                                               *)
(*****************************************************************)

(* <lexical analysis and parsing for \uscheme, providing [[filexdefs]] and [[stringsxdefs]]>= *)
(* Tokens of the micro-Scheme language          *)
(*                                              *)
(* [*] Our general parsing mechanism from Appendix [->] *)
(* requires a language-specific [[token]] type and two *)
(* functions [[tokenString]] and [[isLiteral]]. *)
(* <lexical analysis for \uscheme\ and related languages>= *)
datatype pretoken = QUOTE
                  | INT     of int
                  | SHARP   of bool
                  | NAME    of string
type token = pretoken plus_brackets
(* I define [[isLiteral]] by comparing the given string  *)
(* [[s]] with the string form of token [[t]].   *)

(* <lexical analysis for \uscheme\ and related languages>= *)
fun pretokenString (QUOTE)     = "'"
  | pretokenString (INT  n)    = intString n
  | pretokenString (SHARP b)   = if b then "#t" else "#f"
  | pretokenString (NAME x)    = x
val tokenString = plusBracketsString pretokenString
(* <lexical analysis for \uscheme\ and related languages>= *)
local
  (* <functions used in all lexers>=              *)
  fun noneIfLineEnds chars =
    case streamGet chars
      of NONE => NONE (* end of line *)
       | SOME (#";", cs) => NONE (* comment *)
       | SOME (c, cs) => 
           let val msg = "invalid initial character in `" ^
                         implode (c::listOfStream cs) ^ "'"
           in  SOME (ERROR msg, EOS)
           end
  (* If the lexer doesn't recognize a bracket, quote mark, *)
  (* integer, or other atom, we're expecting the line to *)
  (* end. The end of the line may present itself as the *)
  (* end of the input stream or as a stream of characters *)
  (* beginning with a semicolon, which marks a comment. *)
  (* If we encounter any other character, something has *)
  (* gone wrong. (The polymorphic type of         *)
  (* [[noneIfLineEnds]] provides a subtle but powerful *)
  (* hint that no token can be produced; the only possible *)
  (* outcomes are that nothing is produced, or the lexer *)
  (* detects an error.) [*]                       *)
  (* <boxed values 111>=                          *)
  val _ = op noneIfLineEnds : 'a lexer
  (* The [[atom]] function identifies the special literals *)
  (* [[#t]] and [[#f]]; all other atoms are names. *)
  (* <functions used in the lexer for \uscheme>=  *)
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
(* Lexical analysis for micro-Scheme            *)
(*                                              *)
(* Before a micro-Scheme token, whitespace is ignored. *)
(* The [[schemeToken]] function tries each alternative *)
(* in turn: the two brackets, a quote mark, an integer *)
(* literal, an atom, or end of line. An atom may be a *)
(* [[SHARP]] name or a normal name. [*]         *)
(* <boxed values 110>=                          *)
val _ = op schemeToken : token lexer
val _ = op atom : string -> pretoken
end
(* Parsers for micro-Scheme expressions         *)
(*                                              *)
(* Usually a parser knows what kind of token it is *)
(* looking for. To make such a parser easier to write, *)
(* I create a special parsing combinator for each kind *)
(* of token. Each one succeeds when given a token of the *)
(* kind it expects; when given any other token, it *)
(* fails.                                       *)
(* <parsers for single \uscheme\ tokens>=       *)
type 'a parser = (token, 'a) polyparser
val pretoken  = (fn (PRETOKEN t)=> SOME t  | _ => NONE) <$>? token : pretoken
                                                                          parser
val quote     = (fn (QUOTE)     => SOME () | _ => NONE) <$>? pretoken
val int       = (fn (INT   n)   => SOME n  | _ => NONE) <$>? pretoken
val booltok   = (fn (SHARP b)   => SOME b  | _ => NONE) <$>? pretoken
val name      = (fn (NAME  n)   => SOME n  | _ => NONE) <$>? pretoken
val any_name  = name
(* <parsers and parser builders for formal parameters and bindings>= *)
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
(* The next step up is syntactic elements used in *)
(* multiple Scheme-like languages. Function [[formals]] *)
(* parses a list of formal parameters. If the formal *)
(* parameters contain duplicates, it's treated as a *)
(* syntax error. Function [[bindings]] produces a list *)
(* of bindings suitable for use in [[let*]] expressions. *)
(* For [[let]] and [[letrec]] expressions, which do not *)
(* permit multiple bindings to the same name, use *)
(* [[distinctBsIn]].                            *)
(* <boxed values 112>=                          *)
val _ = op formalsOf  : string -> name parser -> string -> name list parser
val _ = op bindingsOf : string -> 'x parser -> 'e parser -> ('x * 'e) list
                                                                          parser
val _ = op distinctBsIn : (name * 'e) list parser -> string -> (name * 'e) list
                                                                          parser
(* <parsers and parser builders for formal parameters and bindings>= *)
fun recordFieldsOf name =
  nodups ("record fields", "record definition") <$>!
                                    @@ (bracket ("(field ...)", many name))
(* Record fields also may not contain duplicates. *)
(* <boxed values 113>=                          *)
val _ = op recordFieldsOf : name parser -> name list parser
(* <parsers and parser builders for formal parameters and bindings>= *)
fun kw keyword = 
  eqx keyword any_name
fun usageParsers ps = anyParser (map (usageParser kw) ps)
(* We parse any keyword as the name represented by the *)
(* same string as the keyword. And using the keyword *)
(* parser, we can string together ``usage'' parsers. *)
(* <boxed values 114>=                          *)
val _ = op kw : string -> string parser
val _ = op usageParsers : (string * 'a parser) list -> 'a parser
(* <parsers and parser builders for \scheme-like syntax>= *)
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
(* I'm now ready to parse a quoted S-expression, which *)
(* is a symbol, a number, a Boolean, a list of  *)
(* S-expressions, or a quoted S-expression.     *)
(* <boxed values 115>=                          *)
val _ = op sexp : value parser
(* Full Scheme allows programmers to notate arbitrary *)
(* cons cells using a dot in a quoted S-expression. *)
(* micro-Scheme doesn't support this notation.  *)

(* <parsers and parser builders for \scheme-like syntax>= *)
fun atomicSchemeExpOf name =  VAR                   <$> name
                          <|> LITERAL <$> NUM       <$> int
                          <|> LITERAL <$> embedBool <$> booltok
(* The [[exp]] parser handles atomic expressions, quoted *)
(* S-expressions, the table of bracketed expressions, a *)
(* couple of error cases, and function application, *)
(* which uses parentheses but no keyword.       *)
(* <parsers and parser builders for \scheme-like syntax>= *)
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
(* <parsers and [[xdef]] streams for \uscheme>= *)
fun exptable exp =
  let val bindings = bindingsOf "(x e)" name exp
      val formals  = formalsOf "(x1 x2 ...)" name "lambda"
      val dbs      = distinctBsIn bindings
(* The [[exptable]] itself uses the format described in *)
(* \crefpagelazyparse.code.usageParser: each alternative *)
(* is specified by a pair containing a usage string and *)
(* a parser.                                    *)
(* <boxed values 116>=                          *)
val _ = op exptable  : exp parser -> exp parser
val _ = op exp       : exp parser
val _ = op bindings  : (name * exp) list parser
  in usageParsers
     [ ("(if e1 e2 e3)",            curry3 IFX          <$> exp <*> exp <*> exp)
     , ("(while e1 e2)",            curry  WHILEX       <$> exp  <*> exp)
     , ("(set x e)",                curry  SET          <$> name <*> exp)
     , ("(begin e1 ...)",                  BEGIN        <$> many exp)
     , ("(lambda (names) body)",    curry  LAMBDA       <$> formals      <*> exp
                                                                               )
     , ("(let (bindings) body)",    curry3 LETX LET     <$> dbs "let"    <*> exp
                                                                               )
     , ("(letrec (bindings) body)", curry3 LETX LETREC  <$> dbs "letrec" <*> exp
                                                                               )
     , ("(let* (bindings) body)",   curry3 LETX LETSTAR <$> bindings     <*> exp
                                                                               )
     , ("(quote sexp)",             LITERAL             <$> sexp)

    (* <rows added to ML \uscheme's [[exptable]] in exercises ((prototype))>= *)
     , ("(cond ([q a] ...))",
        let fun desugarCond qas = raise LeftAsExercise "desugar cond expression"
            val qa = bracket ("(question answer)", pair <$> exp <*> exp)
     (* [*] Revisit the material on proofs and derivations in *)
     (* \secrefimpcore.opsem.proof.                  *)
     (*                                              *)
     (*  1. Devise a representation, in Standard ML, of the *)
     (*  judgments of the operational semantics for  *)
     (*  micro-Scheme.                               *)
     (*  2. [*] Devise a representation, in Standard ML, of *)
     (*  derivations that use the operational semantics of *)
     (*  micro-Scheme.                               *)
     (*  3. Change the [[eval]] function of the micro-Scheme *)
     (*  interpreter to return a derivation instead of a *)
     (*  value.                                      *)
     (*                                              *)
     (* [*] Using the representation of derivations in *)
     (* Exercise [<-], write a proof checker that tells *)
     (* whether a given tree represents a valid derivation. *)
     (* [*] \crefpagescheme.cond-sugar describes syntactic *)
     (* sugar for \lisp's original conditional expression: *)
     (* the [[cond]] form. Add a [[cond]] form to    *)
     (* micro-Scheme. Here is a little code to get you *)
     (* started:                                     *)
     (* <boxed values 27>=                           *)
     val _ = op desugarCond : (exp * exp) list -> exp
        in  desugarCond <$> many qa
        end
       )
     (* <rows added to ML \uscheme's [[exptable]] in exercises>= *)
     (* add syntactic sugar here, each row preceded by a comma *)
     ]
  end
(* There is a placeholder for adding more syntax in *)
(* exercises.                                   *)

(* <parsers and [[xdef]] streams for \uscheme>= *)
val exp = fullSchemeExpOf (atomicSchemeExpOf name) exptable
(* <parsers and [[xdef]] streams for \uscheme>= *)
val deftable = usageParsers
  [ ("(define f (args) body)",
        let val formals  = formalsOf "(x1 x2 ...)" name "define"
        in  curry DEFINE <$> name <*> (pair <$> formals <*> exp)
        end)
  , ("(val x e)", curry VAL <$> name <*> exp)
  ]
(* Parsers for micro-Scheme definitions         *)
(*                                              *)
(* I segregate the definition parsers by the ML type of *)
(* definition they produce. Parser [[deftable]] parses *)
(* the true definitions. Function [[define]] is a *)
(* Curried function that creates a [[DEFINE]] node. *)
(* <boxed values 117>=                          *)
val _ = op deftable  : def parser
(* <parsers and [[xdef]] streams for \uscheme>= *)
val testtable = usageParsers
  [ ("(check-expect e1 e2)", curry CHECK_EXPECT <$> exp <*> exp)
  , ("(check-assert e)",           CHECK_ASSERT <$> exp)
  , ("(check-error e)",            CHECK_ERROR  <$> exp)
  ]
(* Parser [[testtable]] parses a unit test.     *)
(* <boxed values 118>=                          *)
val _ = op testtable : unit_test parser
(* <parsers and [[xdef]] streams for \uscheme>= *)
val xdeftable = usageParsers
  [ ("(use filename)", USE <$> name)
  (* <rows added to \uscheme\ [[xdeftable]] in exercises>= *)
  (* add syntactic sugar here, each row preceded by a comma *) 
  ]
(* Parser [[xdeftable]] handles those extended  *)
(* definitions that are not unit tests. It is also where *)
(* you would extend the parser with new syntactic forms *)
(* of definition, like the [[record]] form described in *)
(* \crefpagescheme.record-sugar.                *)
(* <boxed values 119>=                          *)
val _ = op xdeftable : xdef parser
(* <parsers and [[xdef]] streams for \uscheme>= *)
val xdef =  DEF  <$> deftable
        <|> TEST <$> testtable
        <|>          xdeftable
        <|> badRight "unexpected right bracket"
        <|> DEF <$> EXP <$> exp
        <?> "definition"
(* The [[xdef]] parser combines all the types of *)
(* extended [*]                                 *)
(* <boxed values 120>=                          *)
(* definition, plus an error case.              *)

(* <parsers and [[xdef]] streams for \uscheme>= *)
val xdefstream = 
  interactiveParsedStream (schemeToken, xdef)
(* Finally, function [[xdefstream]], which is the *)
(* externally visible interface to the parsing, uses the *)
(* lexer and parser to make a function that converts a *)
(* stream of lines to a stream of extended definitions. *)
(* <boxed values 121>=                          *)
val _ = op xdefstream : string * line stream * prompts -> xdef stream
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
(*   EVALUATION, TESTING, AND THE READ-EVAL-PRINT LOOP FOR \USCHEME *)
(*                                                               *)
(*****************************************************************)

(* <evaluation, testing, and the read-eval-print loop for \uscheme>= *)




(* <definitions of [[eval]], [[evaldef]], [[basis]], and [[processDef]] for \uscheme>= *)
fun eval (e, rho) =
  let val go = applyCheckingOverflow id in go end (* OMIT *)
  let fun ev (LITERAL v) = v
        (* To evaluate [[VAR]] and [[SET]], we need environment *)
        (* lookup. The environment [[rho]] binds each name to a *)
        (* mutable ref cell, which is the ML analogs of a *)
        (* pointer to a location allocated on the heap. In , *)
        (* locations are read and written using special pointer *)
        (* syntax (the [[*]] operator), but in ML, we read and *)
        (* write locations using the functions [[!]] and [[:=]], *)
        (* which are in the initial basis of Standard ML. *)
        (* <more alternatives for [[ev]] for \uscheme>= *)
        | ev (VAR x) = !(find (x, rho))
        | ev (SET (x, e)) = 
            let val v = ev e
            in  find (x, rho) := v;
                v
            end
        (* The right-hand side of [[SET]], here called [[e]], is *)
        (* evaluated in the same environment as the [[SET]], so *)
        (* I evaluate it using [[ev]].                  *)
        (*                                              *)

        (* To evaluate [[IF]] and [[WHILE]], we must interpret a *)
        (* micro-Scheme value as a Boolean. That is the job of *)
        (* the projection function [[bool]].            *)
        (* <more alternatives for [[ev]] for \uscheme>= *)
        | ev (IFX (e1, e2, e3)) = ev (if bool (ev e1) then e2 else e3)
        | ev (WHILEX (guard, body)) = 
            if bool (ev guard) then 
              (ev body; ev (WHILEX (guard, body)))
            else
              NIL
        (* To evaluate [[BEGIN]], I define an auxiliary function *)
        (* [[b]], which uses an accumulating parameter  *)
        (* [[lastval]] to remember the value of the last *)
        (* expression. By initializing [[lastval]] to [[false]], *)
        (* I handle the possibility of an empty [[begin]]. *)
        (* <more alternatives for [[ev]] for \uscheme>= *)
        | ev (BEGIN es) =
            let fun b (e::es, lastval) = b (es, ev e)
                  | b (   [], lastval) = lastval
            in  b (es, BOOLV false)
            end
        (* To evaluate [[LAMBDA]], I capture a closure, which is *)
        (* as simple as in C. [*]                       *)
        (* <more alternatives for [[ev]] for \uscheme>= *)
        | ev (LAMBDA (xs, e)) = ( errorIfDups ("formal parameter", xs, "lambda")
                                ; CLOSURE ((xs, e), rho)
                                )
        (* To evaluate an application, I begin by evaluating the *)
        (* expression [[f]] that is in function position. What's *)
        (* next depends on whether [[f]] evaluates to a *)
        (* primitive, a closure, or something else. To apply a *)
        (* primitive function, as in C, apply it to the syntax  *)
        (* [[e]] and to the values of the arguments.    *)
        (* <more alternatives for [[ev]] for \uscheme>= *)
        | ev (e as APPLY (f, args)) = 
               (case ev f
                  of PRIMITIVE prim => prim (e, map ev args)
                   | CLOSURE clo    =>
                        (* Applying a closure is more interesting. To apply a *)

                          (* micro-Scheme closure correctly, I have to create *)

                          (* fresh locations to hold the values of the actual *)

                       (* parameters. In C, we used the function [[allocate]] *)

                            (* for this purpose; in ML, the built-in function *)

                        (* [[ref]] does the same thing: create a new location *)

                             (* and initialize its contents with a value. The *)

                       (* ML expression \monoboxmap ref actuals does half the *)

                      (* work of C's [[bindalloclist]]; function [[bindList]] *)

                           (* does the other half. \mdbuseschemebindalloclist *)

                         (* <apply closure [[clo]] to [[args]] ((mlscheme))>= *)
                                       let val ((formals, body), savedrho) = clo
                                           val actuals = map ev args
                                       in  eval (body, bindList (formals, map
                                                         ref actuals, savedrho))
                                           handle BindListLength => 
                                               raise RuntimeError (
                                      "Wrong number of arguments to closure; " ^
                                                                   "expected ("
                                                       ^ spaceSep formals ^ ")")
                                       end
                   | v => raise RuntimeError ("Applied non-function " ^
                                                                  valueString v)
               )
        (* This code uses a nice feature of ML pattern matching: *)
        (* in a pattern match, the keyword [[as]] helps name the *)
        (* whole value being matched. The pattern \monoboxe as *)
        (* APPLY (f, args) means ``match this pattern if the *)
        (* given argument is an [[APPLY]] node, call the *)
        (* children of the node [[f]] and [[args]], and call the *)
        (* entire node [[e]].''                         *)

        (* <more alternatives for [[ev]] for \uscheme>= *)
        | ev (LETX (LET, bs, body)) =
            let val (names, values) = ListPair.unzip bs
                val _ = errorIfDups ("bound name", names, "let")
        (* To evaluate [[LET]], it is easiest to unzip the list *)
        (* of pairs [[bs]] into a pair of lists [[(names, *)
        (* values)]]. I then use [[map]] to apply both [[ref]] *)
        (* and [[ev]] to each value. To evaluate [[LETSTAR]], *)
        (* by contrast, it is easier to walk the bindings one *)
        (* pair at a time. The function [[ListPair.unzip]] is *)
        (* from the [[ListPair]] module in the Standard Basis *)
        (* Library.                                     *)
        (* <boxed values 11>=                           *)
        val _ = ListPair.unzip : ('a * 'b) list -> 'a list * 'b list
            in  eval (body, bindList (names, map (ref o ev) values, rho))
            end
        | ev (LETX (LETSTAR, bs, body)) =
            let fun step ((n, e), rho) = bind (n, ref (eval (e, rho)), rho)
            in  eval (body, foldl step rho bs)
            end
        (* <more alternatives for [[ev]] for \uscheme>= *)
        | ev (LETX (LETREC, bs, body)) =
            let val (names, values) = ListPair.unzip bs
                val _ = errorIfDups ("bound name", names, "letrec")
                val rho' = bindList (names, map (fn _ => ref (unspecified()))
                                                                    values, rho)
                val updates = map (fn (n, e) => (n, eval (e, rho'))) bs
        (* To evaluate [[LETREC]], I first operate separately on *)
        (* the names and values to build the new environment *)
        (* [[rho']]. I then use [[map]] on [[bs]] to get a list *)
        (* of updates, use [[List.app]] to mutate one cell for *)
        (* each update, and finally evaluate the body. Function *)
        (* [[List.app]] applies a function for side effect. *)
        (* <boxed values 12>=                           *)
        val _ = List.app : ('a -> unit) -> 'a list -> unit
            in  List.app (fn (n, v) => find (n, rho') := v) updates; 
                eval (body, rho')
            end
(* Evaluation                                   *)
(*                                              *)
(* The machinery above is enough to write the evaluator, *)
(* which takes an expression and an environment and *)
(* produces a value. To make the evaluator easy to *)
(* write, I do most of the work of evaluation in the *)
(* nested function [[ev]], which inherits the   *)
(* environment [[rho]] from the outer function [[eval]]. *)
(* Most AST nodes are evaluated in the same environment *)
(* as their parents, and each such node is evaluated by *)
(* passing it to [[ev]], which lets [[rho]] be implicit. *)
(* The first case of [[ev]] is the evaluation of a *)
(* literal value [[v]], which evaluates to itself. \ *)
(* mlsflabeleval                                *)
(* <boxed values 10>=                           *)
val _ = op eval : exp * value ref env -> value
val _ = op ev   : exp                 -> value
  in  ev e
  end
(* Functions [[eval]] and [[ev]] are mutually recursive. *)

(* <definitions of [[eval]], [[evaldef]], [[basis]], and [[processDef]] for \uscheme>= *)
fun withNameBound (x, rho) =
  (find (x, rho); rho)
  handle NotFound _ => bind (x, ref (unspecified ()), rho)
(* When a definition introduces a new name, that *)
(* definition is evaluated in an environment that *)
(* already includes the name being defined. If the name *)
(* is not already bound, it is bound to a fresh location *)
(* that is initialized with an unspecified value. *)
(* <boxed values 18>=                           *)
val _ = op withNameBound : name * value ref env -> value ref env
(* <definitions of [[eval]], [[evaldef]], [[basis]], and [[processDef]] for \uscheme>= *)
fun evaldef (VAL (x, e), rho) =
      let val rho = withNameBound (x, rho)
          val v   = eval (e, rho)
          val _   = find (x, rho) := v
          val response = case e of LAMBDA _ => x
                                 | _ => valueString v
      in  (rho, response)
      end
  | evaldef (EXP e, rho) =        
      let val v   = eval (e, rho)
          val rho = withNameBound ("it", rho)
          val _   = find ("it", rho) := v
      in  (rho, valueString v)
      end
  | evaldef (DEFINE (f, lambda), rho) =
      let val (xs, e) = lambda
          val _ = errorIfDups ("formal parameter", xs, "definition of function "
                                                                            ^ f)
      in  evaldef (VAL (f, LAMBDA lambda), rho)
      end
(* Given a [[val]] binding to name x, [[evaldef]] first *)
(* uses [[withNameBound]] to make sure x is bound to a *)
(* location in the environment. It then evaluates the *)
(* right-hand side and stores the new value in x's *)
(* location. The interpreter's response is usually the *)
(* value, but if the definition binds a [[lambda]] *)
(* expression, the interpreter instead responds with the *)
(* name x. As in \crefscheme.chap, [[define]] is *)
(* syntactic sugar for [[val]] with [[lambda]]. *)
(*                                              *)
(* The [[EXP]] form doesn't bind a name; [[evaldef]] *)
(* just evaluates the expression, binds the result to  *)
(* [[it]], and responds with the value. \mlsflabel *)
(* evaldef                                      *)
(* <boxed values 19>=                           *)
val _ = op evaldef : def  * value ref env -> value ref env * string
(* The differences between [[VAL]] and [[EXP]] are *)
(* subtle: for [[VAL]], the rules of micro-Scheme *)
(* require that we add the name to environment [[rho]] *)
(* before evaluating expression [[e]]. For [[EXP]], we *)
(* don't bind the name [[it]] until after evaluating the *)
(* first top-level expression.                  *)

(* The language-dependent [[basis]] is, for     *)
(* micro-Scheme, the single environment rho, which maps *)
(* each name to a mutable location that holds a value. *)
(* Function [[processDef]] calls [[evaldef]], prints its *)
(* response, and returns its environment.       *)
(* <definitions of [[eval]], [[evaldef]], [[basis]], and [[processDef]] for \uscheme>= *)
type basis = value ref env
fun processDef (d, rho, interactivity) =
  let val (rho', response) = evaldef (d, rho)
      val _ = if prints interactivity then println response else ()
  in  rho'
  end
fun dump_names basis = app (println o fst) basis  (*OMIT*)
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

(* <definition of [[testIsGood]] for \uscheme>= *)
fun testIsGood (test, rho) =
  let fun outcome e = withHandlers (fn e => OK (eval (e, rho))) e (ERROR o
                                                                     stripAtLoc)
      (* <[[asSyntacticValue]] for \uscheme, \timpcore, \tuscheme, and \nml>= *)
      fun asSyntacticValue (LITERAL v) = SOME v
        | asSyntacticValue _           = NONE
      (* In most languages, the only expressions that are *)
      (* syntactic values are literal expressions.    *)
      (* <boxed values 123>=                          *)
      val _ = op asSyntacticValue : exp -> value option

    (* <shared [[check{Expect,Assert,Error}Passes]], which call [[outcome]]>= *)
      (* <shared [[whatWasExpected]]>=                *)
      fun whatWasExpected (e, outcome) =
        case asSyntacticValue e
          of SOME v => valueString v
           | NONE =>
               case outcome
                 of OK v => valueString v ^ " (from evaluating " ^ expString e ^
                                                                             ")"
                  | ERROR _ =>  "the result of evaluating " ^ expString e
      (* When a [[check-expect]] fails, function      *)
      (* [[whatWasExpected]] reports what was expected. If the *)
      (* thing expected was a syntactic value, I show just the *)
      (* value. Otherwise I show the syntax, plus whatever the *)
      (* syntax evaluated to. The definition of       *)
      (* [[asSyntacticValue]] is language-dependent.  *)
      (* <boxed values 35>=                           *)
      val _ = op whatWasExpected  : exp * value error -> string
      val _ = op asSyntacticValue : exp -> value option
      (* <shared [[checkExpectPassesWith]], which calls [[outcome]]>= *)
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

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
      (*                                              *)
      (*  {combinators} \theaderUnit-testing functions *)
      (*  provided by each language \combinatoroutcomeexp *)
      (*  -> value error \combinatortyexp -> ty error \ *)
      (*  combinatortestEqualvalue * value -> bool \  *)
      (*  combinatorvalueStringvalue -> string \combinator *)
      (*  expStringexp -> string \combinator          *)
      (*  testIsGoodunit_test list * basis -> bool \theader *)
      (*  Shared functions for unit testing \combinator *)
      (*  whatWasExpectedexp * value error -> string \ *)
      (*  combinatorcheckExpectPassesexp * exp -> bool \ *)
      (*  combinatorcheckErrorPassesexp -> bool \combinator *)
      (*  numberOfGoodTestsunit_test list * basis -> int \ *)
      (*  combinatorprocessTestsunit_test list * basis -> *)
      (*  unit {combinators}                          *)
      (*                                              *)
      (* Unit-testing functions                       *)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
      (*                                              *)
      (* Function [[checkExpectPasses]] runs a        *)
      (* [[check-expect]] test and tells if the test passes. *)
      (* If the test does not pass, [[checkExpectPasses]] also *)
      (* writes an error message. Error messages are written *)
      (* using [[failtest]], which, after writing the error *)
      (* message, indicates failure by returning [[false]]. *)
      (* <boxed values 36>=                           *)
      val _ = op checkExpectPassesWith : (value * value -> bool) -> exp * exp ->
                                                                            bool
      val _ = op outcome  : exp -> value error
      val _ = op failtest : string list -> bool

(* <shared [[checkAssertPasses]] and [[checkErrorPasses]], which call [[outcome]]>= *)
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
      (* Function [[checkAssertPasses]] does the analogous job *)
      (* for [[check-assert]].                        *)
      (* <boxed values 37>=                           *)
      val _ = op checkAssertPasses : exp -> bool

(* <shared [[checkAssertPasses]] and [[checkErrorPasses]], which call [[outcome]]>= *)
      val cefailed = "check-error failed: "
      fun checkErrorPasses checkx =
            case outcome checkx
              of ERROR _ => true
               | OK check =>
                   failtest [cefailed, " expected evaluating ", expString checkx
                                                                               ,
                             " to cause an error, but evaluation produced ",
                             valueString check]
      (* Function [[checkErrorPasses]] does the analogous job *)
      (* for [[check-error]].                         *)
      (* <boxed values 38>=                           *)
      val _ = op checkErrorPasses : exp -> bool
      fun checkExpectPasses (cx, ex) = checkExpectPassesWith testEqual (cx, ex)
      fun passes (CHECK_EXPECT (c, e)) = checkExpectPasses (c, e)
        | passes (CHECK_ASSERT c)      = checkAssertPasses c
        | passes (CHECK_ERROR c)       = checkErrorPasses  c
(* In micro-Scheme, a test is good if it passes. *)
(* (In some other languages, tests must also be well *)
(* typed.)                                      *)
(* <boxed values 122>=                          *)
val _ = op testIsGood : unit_test * basis -> bool
val _ = op outcome    : exp -> value error
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

(* If have these pieces, I can define one version of *)
(* [[processTests]] (\crefpagemlinterps.processTests) *)
(* and one read-eval-print loop, each of which is shared *)
(* among many bridge languages. The pieces are organized *)
(* as follows: \mdbusemlinterpsprocessTests     *)
(* <boxed values 22>=                           *)
type basis = basis
val _ = op processDef   : def * basis * interactivity -> basis
val _ = op testIsGood   : unit_test      * basis -> bool
val _ = op processTests : unit_test list * basis -> unit



(*****************************************************************)
(*                                                               *)
(*   IMPLEMENTATIONS OF \USCHEME\ PRIMITIVES AND DEFINITION OF [[INITIALBASIS]] *)
(*                                                               *)
(*****************************************************************)

(* <implementations of \uscheme\ primitives and definition of [[initialBasis]]>= *)
(* <utility functions for building primitives in \uscheme>= *)
fun inExp f = 
  fn (e, vs) => f vs
                handle RuntimeError msg =>
                  raise RuntimeError ("in " ^ expString e ^ ", " ^ msg)
(* Each transition between the bullets of the list above *)
(* is implemented by a lifting function. I start with *)
(* the final transition, at the top of the type tower. *)
(* Given a function [[f]] of type \monoboxvalue list -> *)
(* value, [[inExp f]] is a primitive function that also *)
(* takes an [[exp]] as argument. The new function *)
(* applies [[f]], and if applying [[f]] raises the *)
(* [[RuntimeError]] exception, it adds the expression to *)
(* the error message.                           *)
(* <boxed values 13>=                           *)
val _ = op inExp : (value list -> value) -> (exp * value list -> value)
(* <utility functions for building primitives in \uscheme>= *)
fun arityError n args =
  raise RuntimeError ("expected " ^ intString n ^
                      " but got " ^ intString (length args) ^ " arguments")
fun binaryOp f = (fn [a, b] => f (a, b) | args => arityError 2 args)
fun unaryOp  f = (fn [a]    => f a      | args => arityError 1 args)
(* At the next step down the tower, I need functions of *)
(* type \monoboxvalue list -> value. I make them from *)
(* primitive functions that are either binary or unary *)
(* operators. As in C, I reuse the code that does the *)
(* arity checks. But in ML, I put the arity checks into *)
(* functions [[binaryOp]] and [[unaryOp]], which use *)
(* pattern matching not only to check the number of *)
(* arguments, but also to extract the arguments and pass *)
(* them to an underlying function [[f]]. If a check *)
(* fails, function [[arityError]] raises        *)
(* [[RuntimeError]] with a suitable message. [*] [*] *)
(* <boxed values 14>=                           *)
val _ = op unaryOp  : (value         -> value) -> (value list -> value)
val _ = op binaryOp : (value * value -> value) -> (value list -> value)
(* These higher-order functions encapsulate the ideas of *)
(* ``binary operator'' and ``unary operator'' in a *)
(* general way. As I subdivide the primitives into *)
(* arithmetic, predicates, list primitives, and other, *)
(* I use more higher-order functions to specialize *)
(* things further.                              *)

(* <utility functions for building primitives in \uscheme>= *)
fun arithOp f = binaryOp (fn (NUM n1, NUM n2) => NUM (f (n1, n2)) 
                           | (NUM n, v) =>
                              (* <report [[v]] is not an integer>=            *)
                                           raise RuntimeError (
                                "expected an integer, but got " ^ valueString v)

                       (* Here are the implementations of all the predicates. *)

                        (* Equality comparison uses [[equalatoms]], which, as *)

                       (* required by the semantics of micro-Scheme, succeeds *)

                         (* only on symbols, numbers, Booleans, and the empty *)

                              (* list. The type predicates are all anonymous  *)

                              (* functions.                                   *)

                           | (v, _)     =>
                              (* <report [[v]] is not an integer>=            *)
                                           raise RuntimeError (
                                "expected an integer, but got " ^ valueString v)

                       (* Here are the implementations of all the predicates. *)

                        (* Equality comparison uses [[equalatoms]], which, as *)

                       (* required by the semantics of micro-Scheme, succeeds *)

                         (* only on symbols, numbers, Booleans, and the empty *)

                              (* list. The type predicates are all anonymous  *)

                              (* functions.                                   *)

                         )
(* Arithmetic primitives expect and return integers. As *)
(* in C, I reuse the code that projects two arguments to *)
(* integers, but as above, I do it using higher-order *)
(* functions. Function [[arithOp]], which uses  *)
(* [[binaryOp]], lifts a native ML binary arithmetic *)
(* primitive into a function of type \monoboxvalue list *)
(* -> value.                                    *)
(* <boxed values 15>=                           *)
val _ = op arithOp: (int * int -> int) -> (value list -> value)
(* I can now define primitives by applying first *)
(* [[arithOp]] and then [[inExp]] to ML functions like *)
(* [[+]] and [[*]].                             *)

(* <utility functions for building primitives in \uscheme>= *)
fun predOp f     = unaryOp  (embedBool o f)
fun comparison f = binaryOp (embedBool o f)
fun intcompare f = comparison (fn (NUM n1, NUM n2) => f (n1, n2)
                                | (NUM n, v) =>
                              (* <report [[v]] is not an integer>=            *)
                                                raise RuntimeError (
                                "expected an integer, but got " ^ valueString v)

                       (* Here are the implementations of all the predicates. *)

                        (* Equality comparison uses [[equalatoms]], which, as *)

                       (* required by the semantics of micro-Scheme, succeeds *)

                         (* only on symbols, numbers, Booleans, and the empty *)

                              (* list. The type predicates are all anonymous  *)

                              (* functions.                                   *)

                                | (v, _)     =>
                              (* <report [[v]] is not an integer>=            *)
                                                raise RuntimeError (
                                "expected an integer, but got " ^ valueString v)

                       (* Here are the implementations of all the predicates. *)

                        (* Equality comparison uses [[equalatoms]], which, as *)

                       (* required by the semantics of micro-Scheme, succeeds *)

                         (* only on symbols, numbers, Booleans, and the empty *)

                              (* list. The type predicates are all anonymous  *)

                              (* functions.                                   *)

                              )
(* We have two kinds of predicate: an ordinary predicate *)
(* takes one argument and a comparison takes two. Some *)
(* comparisons apply only to integers. An ML predicate *)
(* returns a [[bool]], but a micro-Scheme predicate *)
(* needs to return a [[value]], so we compose each *)
(* predicate with [[embedBool]].                *)
(* <boxed values 16>=                           *)
val _ = op predOp     : (value         -> bool) -> (value list -> value)
val _ = op comparison : (value * value -> bool) -> (value list -> value)
val _ = op intcompare : (int   * int   -> bool) -> (value list -> value)
(* <utility functions for building primitives in \uscheme>= *)
fun errorPrimitive (_, [v]) = raise RuntimeError (valueString v)
  | errorPrimitive (e, vs)  = inExp (arityError 1) (e, vs)
(* The [[error]] primitive is special because although *)
(* it raises the [[RuntimeError]] exception, this *)
(* behavior is expected, and therefore the context in *)
(* which the exception is raised should not be  *)
(* shown—unless [[error]] is given the wrong number of *)
(* arguments. To maintain such fine control over its *)
(* behavior, [[errorPrimitive]] takes an [[exp]] *)
(* parameter on its own, and it delegates reporting to *)
(* [[inExp]] only in the case of an arity error. *)
(* <boxed values 17>=                           *)
val _ = op errorPrimitive : exp * value list -> value list
val initialBasis =
  let val rho =
        foldl (fn ((name, prim), rho) => bind (name, ref (PRIMITIVE (inExp prim)
                                                                        ), rho))
              emptyEnv ((* I organize my primitives into a list of (name, *)

                      (* function) pairs. To write the list, I use Noweb code *)
                        (* chunk [[]].                                  *)
                        (* Each primitive on the list has type \monoboxvalue *)
                        (* list -> value. In \chunkrefmlscheme.inExp-applied, *)
                        (* I apply [[inExp]] to every primitive on the list, *)

                      (* then use the result to build the initial environment *)
                        (* for the read-eval-print loop. [Actually, the list *)

                     (* contains all the primitives except one. The exception *)
                        (* is [[error]], which should not be wrapped in *)
                        (* [[inExp]].] The list plays the same role as the *)
                        (* unspeakable [[xx]] macros in file prim.h in the *)
                        (* C implementation (chunk [->]). Here are the first *)
                        (* four elements:                               *)
                        (* <primitives for \uscheme\ [[::]]>=           *)
                        ("+", arithOp op +  ) :: 
                        ("-", arithOp op -  ) :: 
                        ("*", arithOp op *  ) :: 
                        ("/", arithOp op div) ::
                        (* The ML keyword [[op]] makes it possible to use an *)

                       (* infix identifier as an ordinary value, so [[arithOp *)

                      (* op +]] passes the value [[+]] (a binary function) to *)
                        (* the function [[arithOp]].                    *)

                        (* <primitives for \uscheme\ [[::]]>=           *)
                        ("<", intcompare op <) :: 
                        (">", intcompare op >) ::
                        ("=", comparison equalatoms) ::
                        ("null?",    predOp (fn (NIL    ) => true | _ => false))
                                                                              ::
                        ("boolean?", predOp (fn (BOOLV _) => true | _ => false))
                                                                              ::
                        ("number?",  predOp (fn (NUM   _) => true | _ => false))
                                                                              ::
                        ("symbol?",  predOp (fn (SYM   _) => true | _ => false))
                                                                              ::
                        ("pair?",    predOp (fn (PAIR  _) => true | _ => false))
                                                                              ::
                        ("procedure?",
                              predOp (fn (PRIMITIVE _) => true | (CLOSURE  _) =>
                                                          true | _ => false)) ::
                        (* The list primitives are also implemented by simple *)
                        (* anonymous functions:                         *)
                        (* <primitives for \uscheme\ [[::]]>=           *)
                        ("cons", binaryOp (fn (a, b) => PAIR (a, b))) ::
                        ("car",  unaryOp  (fn (PAIR (car, _)) => car 
                                            | v => raise RuntimeError
                                                           (
                                "car applied to non-list " ^ valueString v))) ::
                        ("cdr",  unaryOp  (fn (PAIR (_, cdr)) => cdr 
                                            | v => raise RuntimeError
                                                           (
                                "cdr applied to non-list " ^ valueString v))) ::

                       (* The last primitives I can define with type \monobox *)
                        (* value list -> value are the printing primitives. *)
                        (* <primitives for \uscheme\ [[::]]>=           *)
                        ("println", unaryOp (fn v => (print (valueString v ^
                                                                  "\n"); v))) ::
                        ("print",   unaryOp (fn v => (print (valueString v);
                                                                         v))) ::
                        ("printu",  unaryOp (fn NUM n => (printUTF8 n; NUM n)
                                              | v => raise RuntimeError (
                                                                 valueString v ^

                                        " is not a Unicode code point"))) :: [])
      val rho = bind ("error", ref (PRIMITIVE errorPrimitive), rho)
      val fundefs = (* <predefined {\uscheme} functions, as strings>= *)

                     [ "(define caar (xs) (car (car xs)))"
                     , "(define cadr (xs) (car (cdr xs)))"
                     , "(define cdar (xs) (cdr (car xs)))"
                     , "(define list1 (x)     (cons x '()))"
                     , "(define list2 (x y)   (cons x (list1 y)))"
                     , "(define list3 (x y z) (cons x (list2 y z)))"
                     , "(define append (xs ys)"
                     , "  (if (null? xs)"
                     , "     ys"
                     , "     (cons (car xs) (append (cdr xs) ys))))"
                     , "(define revapp (xs ys) ; reverse xs and append ys"
                     , "  (if (null? xs)"
                     , "     ys"
                     , "     (revapp (cdr xs) (cons (car xs) ys))))"
                     , "(define reverse (xs) (revapp xs '()))"
                     , "(define and (b c) (if b  c  b))"
                     , "(define or  (b c) (if b  b  c))"
                     , "(define not (b)   (if b #f #t))"
                     ,
"(define atom? (x) (or (symbol? x) (or (number? x) (or (boolean? x) (null? x)))))"
                     , "(define equal? (sx1 sx2)"
                     , "  (if (atom? sx1)"
                     , "    (= sx1 sx2)"
                     , "    (if (atom? sx2)"
                     , "        #f"
                     , "        (and (equal? (car sx1) (car sx2))"
                     , "             (equal? (cdr sx1) (cdr sx2))))))"
                     , "(define make-alist-pair (k a) (list2 k a))"
                     , "(define alist-pair-key        (pair)  (car  pair))"
                     , "(define alist-pair-attribute  (pair)  (cadr pair))"
                     ,
     "(define alist-first-key       (alist) (alist-pair-key       (car alist)))"
                     ,
     "(define alist-first-attribute (alist) (alist-pair-attribute (car alist)))"
                     , "(define bind (k a alist)"
                     , "  (if (null? alist)"
                     , "    (list1 (make-alist-pair k a))"
                     , "    (if (equal? k (alist-first-key alist))"
                     , "      (cons (make-alist-pair k a) (cdr alist))"
                     , "      (cons (car alist) (bind k a (cdr alist))))))"
                     , "(define find (k alist)"
                     , "  (if (null? alist)"
                     , "    '()"
                     , "    (if (equal? k (alist-first-key alist))"
                     , "      (alist-first-attribute alist)"
                     , "      (find k (cdr alist)))))"
                     , "(define o (f g) (lambda (x) (f (g x))))"
                     , "(define curry   (f) (lambda (x) (lambda (y) (f x y))))"
                     , "(define uncurry (f) (lambda (x y) ((f x) y)))"
                     , "(define filter (p? xs)"
                     , "  (if (null? xs)"
                     , "    '()"
                     , "    (if (p? (car xs))"
                     , "      (cons (car xs) (filter p? (cdr xs)))"
                     , "      (filter p? (cdr xs)))))"
                     , "(define map (f xs)"
                     , "  (if (null? xs)"
                     , "    '()"
                     , "    (cons (f (car xs)) (map f (cdr xs)))))"
                     , "(define app (f xs)"
                     , "  (if (null? xs)"
                     , "    #f"
                     , "    (begin (f (car xs)) (app f (cdr xs)))))"
                     , "(define exists? (p? xs)"
                     , "  (if (null? xs)"
                     , "    #f"
                     , "    (if (p? (car xs)) "
                     , "      #t"
                     , "      (exists? p? (cdr xs)))))"
                     , "(define all? (p? xs)"
                     , "  (if (null? xs)"
                     , "    #t"
                     , "    (if (p? (car xs))"
                     , "      (all? p? (cdr xs))"
                     , "      #f)))"
                     , "(define foldr (op zero xs)"
                     , "  (if (null? xs)"
                     , "    zero"
                     , "    (op (car xs) (foldr op zero (cdr xs)))))"
                     , "(define foldl (op zero xs)"
                     , "  (if (null? xs)"
                     , "    zero"
                     , "    (foldl op (op (car xs) zero) (cdr xs))))"
                     , "(define <= (x y) (not (> x y)))"
                     , "(define >= (x y) (not (< x y)))"
                     , "(define != (x y) (not (= x y)))"
                     , "(define max (x y) (if (> x y) x y))"
                     , "(define min (x y) (if (< x y) x y))"
                     , "(define mod (m n) (- m (* n (/ m n))))"
                     , "(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))"
                     , "(define lcm (m n) (if (= m 0) 0 (* m (/ n (gcd m n)))))"
                     , "(define caar  (sx) (car (car  sx)))"
                     , "(define cdar  (sx) (cdr (car  sx)))"
                     , "(define cadr  (sx) (car (cdr  sx)))"
                     , "(define cddr  (sx) (cdr (cdr  sx)))"
                     , "(define caaar (sx) (car (caar sx)))"
                     , "(define cdaar (sx) (cdr (caar sx)))"
                     , "(define caadr (sx) (car (cadr sx)))"
                     , "(define cdadr (sx) (cdr (cadr sx)))"
                     , "(define cadar (sx) (car (cdar sx)))"
                     , "(define cddar (sx) (cdr (cdar sx)))"
                     , "(define caddr (sx) (car (cddr sx)))"
                     , "(define cdddr (sx) (cdr (cddr sx)))"
                     , "(define list1 (x)               (cons x '()))"
                     , "(define list2 (x y)             (cons x (list1 y)))"
                     , "(define list3 (x y z)           (cons x (list2 y z)))"
                     , "(define list4 (x y z a)         (cons x (list3 y z a)))"
                     ,
                     "(define list5 (x y z a b)       (cons x (list4 y z a b)))"
                     ,
                   "(define list6 (x y z a b c)     (cons x (list5 y z a b c)))"
                     ,
                 "(define list7 (x y z a b c d)   (cons x (list6 y z a b c d)))"
                     ,
               "(define list8 (x y z a b c d e) (cons x (list7 y z a b c d e)))"
                      ]
      val xdefs = stringsxdefs ("predefined functions", fundefs)
  in  readEvalPrintWith predefinedFunctionError (xdefs, rho, noninteractive)
  end
(* The most important exceptions are [[RuntimeError]], *)
(* [[NotFound]], and [[Located]]. Exceptions    *)
(* [[RuntimeError]] and [[NotFound]] are defined above; *)
(* they signal problems with evaluation or with an *)
(* environment, respectively. Exception [[Located]], *)
(* which is defined in \crefmlinterps.chap, is a special *)
(* exception that wraps another exception [[exn]] in a *)
(* source-code location. When [[Located]] is caught, we *)
(* ``re-raise'' exception [[exn]], and we fill in the *)
(* source location in [[exn]]'s error message.  *)
(* <boxed values 25>=                           *)
val _ = op withHandlers : ('a -> 'b) -> 'a -> (string -> 'b) -> 'b
(* Initializing and running the interpreter     *)
(*                                              *)
(* To get a complete interpreter running, what's left to *)
(* do is what's done in C function \scmfunmain: decide *)
(* if the interpreter is interactive, initialize the *)
(* environment and the error format, and start the *)
(* read-eval-print loop on the standard input. \mdbuse *)
(* schememain First, the initial environment.   *)
(*                                              *)
(* A basis for micro-Scheme comprises a single value *)
(* environment. I create the initial basis by starting *)
(* with the empty environment, binding the primitive *)
(* operators, then reading the predefined functions. *)
(* When reading predefined functions, the interpreter *)
(* echoes no responses, and to issue error messages, it *)
(* uses the special function [[predefinedError]]. [*] \ *)
(* makenowebnotdef (from \LAadditions to the    *)
(* micro-Scheme initial basis \upshape[->]\RA)  *)
(* <boxed values 25>=                           *)
val _ = op initialBasis : basis


(*****************************************************************)
(*                                                               *)
(*   FUNCTION [[RUNAS]], WHICH EVALUATES STANDARD INPUT GIVEN [[INITIALBASIS]] *)
(*                                                               *)
(*****************************************************************)

(* <function [[runAs]], which evaluates standard input given [[initialBasis]]>= *)
fun runAs interactivity = 
  let val _ = setup_error_format interactivity
      val prompts = if prompts interactivity then stdPrompts else noPrompts
      val xdefs = filexdefs ("standard input", TextIO.stdIn, prompts)
  in  ignore (readEvalPrintWith eprintln (xdefs, initialBasis, interactivity))
  end 
(* Function [[runAs]] looks at the interactivity mode *)
(* and sets both the error format and the prompts. *)
(* It then starts the read-eval-print loop on standard *)
(* input, with the initial basis.               *)
(* <boxed values 26>=                           *)
val _ = op runAs : interactivity -> unit


(*****************************************************************)
(*                                                               *)
(*   CODE THAT LOOKS AT COMMAND-LINE ARGUMENTS AND CALLS [[RUNAS]] TO RUN THE INTERPRETER *)
(*                                                               *)
(*****************************************************************)

(* To launch the interpreter, I look at command-line *)
(* arguments and call [[runAs]]. The code is executed *)
(* only for its side effect, so I put it on the *)
(* right-hand side of a [[val]] binding with no name. *)
(* Function [[CommandLine.arguments]] returns an *)
(* argument list; [[CommandLine.name]] returns the name *)
(* by which the interpreter was invoked.        *)
(* <code that looks at command-line arguments and calls [[runAs]] to run the interpreter>= *)
val _ = case CommandLine.arguments ()
          of []     => runAs (PROMPTING,     PRINTING)
           | ["-q"] => runAs (NOT_PROMPTING, PRINTING)
           | ["-qq"]=> runAs (NOT_PROMPTING, NOT_PRINTING)   (*OMIT*)
           | ["-names"]=> dump_names initialBasis (*OMIT*)
           | _      => eprintln ("Usage: " ^ CommandLine.name () ^ " [-q]")
