(* Pulling the pieces together                  *)
(*                                              *)
(* The full interpreter shares lots of components with \ *)
(* nml.                                         *)
(* <uml.sml>=                                   *)


(*****************************************************************)
(*                                                               *)
(*   EXCEPTIONS USED IN LANGUAGES WITH TYPE INFERENCE            *)
(*                                                               *)
(*****************************************************************)

(* And all interpreters that implement type inference *)
(* incorporate this code:                       *)
(* <exceptions used in languages with type inference>= *)
exception TypeError of string
exception BugInTypeInference of string


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
(* <boxed values 165>=                          *)
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
(* <support for names and environments>=        *)
fun extend (rho, bindings) =
  foldr (fn ((x, a), rho) => bind (x, a, rho)) rho bindings
(* Extension is an operation we also see in \xlet forms, *)
(* but this is the first interpreter in which I write it *)
(* as a function. \umlflabelextend              *)
(* <boxed values 80>=                           *)
val _ = op extend : 'a env * 'a env -> 'a env
(* <support for names and environments>=        *)
exception DisjointUnionFailed of name
fun disjointUnion envs =
  let val env = List.concat envs
  in  case duplicatename (map fst env)
        of NONE => env
         | SOME x => raise DisjointUnionFailed x
  end
(* <boxed values 82>=                           *)
val _ = op disjointUnion : 'a env list -> 'a env
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
(* <boxed values 125>=                          *)
val _ = op intString : int -> string
(* Lists! Functions [[spaceSep]] and [[commaSep]] are *)
(* special cases of the more general function   *)
(* [[separate]].                                *)
(* <boxed values 125>=                          *)
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
(* <boxed values 129>=                          *)
val _ = op zip3   : 'a list * 'b list * 'c list -> ('a * 'b * 'c) list
val _ = op unzip3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
(* <boxed values 129>=                          *)
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
(* <boxed values 129>=                          *)
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
(* <boxed values 130>=                          *)
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
(* <boxed values 131>=                          *)
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
(* <boxed values 126>=                          *)
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
(* <boxed values 127>=                          *)
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
(* <boxed values 128>=                          *)
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
(* <boxed values 137>=                          *)
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
(* <boxed values 138>=                          *)
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
(* <boxed values 139>=                          *)
val _ = op streamGet : 'a stream -> ('a * 'a stream) option
(* The simplest way to create a stream is by using the *)
(* [[:::]] or [[EOS]] constructors. It can also be *)
(* convenient to create a stream from a list. When such *)
(* a stream is read, no new actions are performed. *)
(* <boxed values 139>=                          *)
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
(* <boxed values 140>=                          *)
val _ = op listOfStream : 'a stream -> 'a list
(* The more interesting streams are those that result *)
(* from actions. To help create such streams, I define *)
(* [[delayedStream]] as a convenience abbreviation for *)
(* creating a stream from one action.           *)
(* <boxed values 140>=                          *)
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
(* <boxed values 141>=                          *)
val _ = op streamOfEffects : (unit -> 'a option) -> 'a stream
(* I use [[streamOfEffects]] to produce a stream of *)
(* lines from an input file:                    *)

(* <streams>=                                   *)
type line = string
fun filelines infile = 
  streamOfEffects (fn () => TextIO.inputLine infile)
(* <boxed values 142>=                          *)
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
(* <boxed values 143>=                          *)
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
(* <boxed values 144>=                          *)
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
(* <boxed values 145>=                          *)
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
(* <boxed values 146>=                          *)
val _ = op preStream : (unit -> unit) * 'a stream -> 'a stream
(* <boxed values 146>=                          *)
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
(* <boxed values 147>=                          *)
val _ = op streamMap : ('a -> 'b) -> 'a stream -> 'b stream
(* <streams>=                                   *)
fun streamFilter p xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => if p x then x ::: streamFilter p
                                                                              xs
                                               else streamFilter p xs)
(* <boxed values 148>=                          *)
val _ = op streamFilter : ('a -> bool) -> 'a stream -> 'a stream
(* <streams>=                                   *)
fun streamFold f z xs =
  case streamGet xs of NONE => z
                     | SOME (x, xs) => streamFold f (f (x, z)) xs
(* The only sensible order in which to fold the elements *)
(* of a stream is the order in which the actions are *)
(* taken and the results are produced: from left to *)
(* right. [*]                                   *)
(* <boxed values 149>=                          *)
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
(* <boxed values 150>=                          *)
val _ = op streamZip : 'a stream * 'b stream -> ('a * 'b) stream
(* <boxed values 150>=                          *)
val _ = op streamConcat : 'a stream stream -> 'a stream
(* <streams>=                                   *)
fun streamConcatMap f xs = streamConcat (streamMap f xs)
(* The composition of [[concat]] with [[map f]] is very *)
(* common in list and stream processing, so I give it a *)
(* name.                                        *)
(* <boxed values 151>=                          *)
val _ = op streamConcatMap : ('a -> 'b stream) -> 'a stream -> 'b stream
(* <streams>=                                   *)
infix 5 @@@
fun xs @@@ xs' = streamConcat (streamOfList [xs, xs'])
(* The code used to append two streams is much like the *)
(* code used to concatenate arbitrarily many streams. *)
(* To avoid duplicating the tricky manipulation of *)
(* states, I simply implement append using      *)
(* concatenation.                               *)
(* <boxed values 152>=                          *)
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
(* <boxed values 153>=                          *)
val _ = op streamTake : int * 'a stream -> 'a list
(* <streams>=                                   *)
fun streamDrop (0, xs) = xs
  | streamDrop (n, xs) =
      case streamGet xs
        of SOME (_, xs) => streamDrop (n-1, xs)
         | NONE => EOS
(* If I want ``take,'' sooner or later I'm sure to want *)
(* ``drop'' (\chunkrefmlinterps.chunk.use-streamDrop). *)
(* <boxed values 154>=                          *)
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
(* <boxed values 161>=                          *)
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
(* <boxed values 162>=                          *)
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
(* <boxed values 163>=                          *)
val _ = op <*> : ('a, 'b -> 'c) xformer * ('a, 'b) xformer -> ('a, 'c) xformer
(* <stream transformers and their combinators>= *)
infixr 4 <$>
fun f <$> p = pure f <*> p
(* The common case of creating [[tx_f]] using [[pure]] *)
(* is normally written using the special operator [[< *)
(* >]], which is also pronounced ``applied to.'' *)
(* It combines a B-to-C function with an \atob  *)
(* transformer to produce an \atoc transformer. *)
(* <boxed values 164>=                          *)
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
(* <boxed values 166>=                          *)
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
(* <boxed values 167>=                          *)
val _ = op pzero : ('a, 'b) xformer
(* <boxed values 167>=                          *)
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
(* <boxed values 168>=                          *)
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
(* <boxed values 169>=                          *)
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
(* <boxed values 170>=                          *)
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
(* <boxed values 171>=                          *)
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
(* <boxed values 172>=                          *)
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
(* <boxed values 173>=                          *)
val _ = op sat : ('b -> bool) -> ('a, 'b) xformer -> ('a, 'b) xformer
(* <stream transformers and their combinators>= *)
fun eqx y = 
  sat (fn y' => y = y') 
(* Transformer [[eqx b]] is [[sat]] specialized to an *)
(* equality predicate. It is typically used to recognize *)
(* special characters like keywords and minus signs. *)
(* <boxed values 174>=                          *)
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
(* <boxed values 175>=                          *)
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
(* <boxed values 176>=                          *)
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
(* <boxed values 177>=                          *)
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
(* <boxed values 178>=                          *)
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
(* <boxed values 179>=                          *)
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
(* <boxed values 180>=                          *)
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
(* <boxed values 181>=                          *)
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
(* <boxed values 156>=                          *)
type srcloc = srcloc
val _ = op srclocString : srcloc -> string
(* To keep track of the source location of a line, *)
(* token, expression, or other datum, I put the location *)
(* and the datum together in a pair. To make it easier *)
(* to read the types, I define a type abbreviation which *)
(* says that a value paired with a location is  *)
(* ``located.''                                 *)
(* <boxed values 156>=                          *)
type 'a located = 'a located
(* <support for source-code locations and located streams>= *)
fun atLoc loc f a =
  f a handle e as RuntimeError _ => raise Located (loc, e)
           | e as NotFound _     => raise Located (loc, e)
           (* <more handlers for [[atLoc]] ((type-inference))>= *)
           | e as TypeError _          => raise Located (loc, e)
           | e as BugInTypeInference _ => raise Located (loc, e)
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
(* <boxed values 157>=                          *)
val _ = op atLoc : srcloc -> ('a -> 'b) -> ('a -> 'b)
(* <support for source-code locations and located streams>= *)
fun located f (loc, a) = atLoc loc f a
fun leftLocated f ((loc, a), b) = atLoc loc f (a, b)
(* <boxed values 158>=                          *)
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
(* <boxed values 159>=                          *)
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
(* <boxed values 160>=                          *)
val _ = op errorAt : string -> srcloc -> 'a error
(* All locations originate in a located stream of lines. *)
(* The locations share a filename, and the line numbers *)
(* are 1, 2, 3, ... and so on. [*]              *)
(* <boxed values 160>=                          *)
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
(* <boxed values 189>=                          *)
type 'a eol_marked = 'a eol_marked
val _ = op drainLine : 'a eol_marked stream -> 'a eol_marked stream
(* <boxed values 189>=                          *)
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
(* <boxed values 182>=                          *)
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
(* <boxed values 183>=                          *)
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
(* <boxed values 184>=                          *)
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
(* <boxed values 185>=                          *)
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
(* <boxed values 186>=                          *)
val _ = op intFromChars : char list -> int error
(* <support for lexical analysis>=              *)
fun intToken isDelim =
  intFromChars <$>! intChars isDelim
(* In this book, every language except uProlog can use *)
(* [[intToken]].                                *)
(* <boxed values 187>=                          *)
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
(* <boxed values 188>=                          *)
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
(* <boxed values 190>=                          *)
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
(* <boxed values 191>=                          *)
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
(* <boxed values 192>=                          *)
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
(* <boxed values 193>=                          *)
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
(* <boxed values 197>=                          *)
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
(* <boxed values 194>=                          *)
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
(* <boxed values 195>=                          *)
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
(* <boxed values 196>=                          *)
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
(* <boxed values 198>=                          *)
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
(* <boxed values 199>=                          *)
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
(* <boxed values 200>=                          *)
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
(* <boxed values 201>=                          *)
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
(* <boxed values 202>=                          *)
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
(* <boxed values 203>=                          *)
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
(* <boxed values 204>=                          *)
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
(* <boxed values 205>=                          *)
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
(* <boxed values 206>=                          *)
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
(*   HINDLEY-MILNER TYPES WITH GENERATED TYPE CONSTRUCTORS       *)
(*                                                               *)
(*****************************************************************)

(* <Hindley-Milner types with generated type constructors>= *)
(* Representing and generating type constructors *)
(*                                              *)
(* [*] The representation of a type constructor must *)
(* solve two problems: for type checking, it should be *)
(* easy to tell if two type constructors are the same. *)
(* And for the [[data]] definition, it should be easy to *)
(* create a type constructor that is distinct from all *)
(* others. I address both problems by assigning each *)
(* type constructor an identity, which I represent by an *)
(* integer.                                     *)
(* <\small [[tycon]], [[freshTycon]], [[eqTycon]], and [[tyconString]] for generated type constructors>= *)
type tycon_identity = int
(* Identifying a type constructor with an integer is *)
(* great for type checking, but it's not so great for *)
(* telling programmers about the types of expressions. *)
(* To make it possible to print an informative  *)
(* representation of any types, I represent a type *)
(* constructor as a record containing not only its *)
(* identity but also a name used to print it. \umllabel *)
(* tycon                                        *)
(* <\small [[tycon]], [[freshTycon]], [[eqTycon]], and [[tyconString]] for generated type constructors>= *)
type tycon = { printName : name, identity : tycon_identity }
(* <\small [[tycon]], [[freshTycon]], [[eqTycon]], and [[tyconString]] for generated type constructors>= *)
fun eqTycon ({ identity = id, printName = _ }, { identity = id', printName = _ }
                                                                             ) =
  id = id'
(* Type constructors are equal if and only if they have *)
(* the same identity.                           *)
(* <boxed values 89>=                           *)
val _ = op eqTycon : tycon * tycon -> bool
(* And a type constructor prints using its      *)
(* [[printName]].                               *)

(* <\small [[tycon]], [[freshTycon]], [[eqTycon]], and [[tyconString]] for generated type constructors>= *)
fun tyconString { identity = _, printName = T } = T
(* <\small [[tycon]], [[freshTycon]], [[eqTycon]], and [[tyconString]] for generated type constructors>= *)
local
  val timesDefined : int env ref = ref emptyEnv
                             (* how many times each tycon is defined *)
in
  fun freshPrintName t =
    let val n = find (t, !timesDefined) handle NotFound _ => 0
        val _ = timesDefined := bind (t, n + 1, !timesDefined)
    in  if n = 0 then t  (* first definition *)
        else t ^ "@{" ^ Int.toString (n+1) ^ "}"
    end
end
(* To choose the [[printName]] of a type constructor, *)
(* I could just use the name in the type constructor's *)
(* definition. But if a constructor is redefined, you *)
(* don't want an error message like ``cannot make *)
(* [[node]] equal to [[node]]'' or ``expected struct *)
(* point but argument is of type struct point.'' [The *)
(* second message is from \texttt{gcc}.] We can do *)
(* better. I define a function [[freshPrintName]] which, *)
(* when given the name of a type constructor, returns a *)
(* [[printName]] that is distinct from prior    *)
(* [[printName]]s. For example, the first time I define *)
(* [[node]], it prints as [[node]]. But the second time *)
(* I define [[node]], it prints as [[node@2]], and *)
(* so on.                                       *)
(* <boxed values 90>=                           *)
val _ = op freshPrintName : string -> string
(* <\small [[tycon]], [[freshTycon]], [[eqTycon]], and [[tyconString]] for generated type constructors>= *)
local
  val nextIdentity = ref 0
  fun freshIdentity () = !nextIdentity before nextIdentity := !nextIdentity + 2
in
  fun freshTycon t = { identity = freshIdentity(), printName = freshPrintName t
                                                                               }
end
(* Every type constructor is created by calling function *)
(* [[freshTycon]], which gives it a fresh [[printName]] *)
(* and a unique [[identity]]. Ordinary type constructors *)
(* have even-numbered identities; odd-numbered  *)
(* identities are reserved for special type constructors *)
(* described in \crefadt.existentials. [*]      *)
(* <boxed values 91>=                           *)
val _ = op freshTycon : name -> tycon
(* The difference between types and type schemes is *)
(* reflected in the abstract syntax: [\Nml\ has only *)
(* four of the five cases found in \tuscheme. The fifth *)
(* case, [[FUNTY]], is represented in \nml\ as a nested *)
(* application of type constructors [[function]] and *)
(* [[args]]. Reducing the number of cases simplifies *)
(* type inference. ] \nmllabeltyvar,ty,type_scheme *)
(* <representation of Hindley-Milner types>=    *)
type tyvar  = name
datatype ty = TYVAR  of tyvar               (* type variable alpha *)
            | TYCON  of tycon               (* type constructor mu *)
            | CONAPP of ty * ty list        (* type-level application *)

datatype type_scheme = FORALL of tyvar list * ty
(* <sets of free type variables in Hindley-Milner types>= *)
fun freetyvars t =
  let fun f (TYVAR v,          ftvs) = insert (v, ftvs)
        | f (TYCON _,          ftvs) = ftvs
        | f (CONAPP (ty, tys), ftvs) = foldl f (f (ty, ftvs)) tys
  in  reverse (f (t, emptyset))
  end  
(* The function [[freetyvars]] computes the free type *)
(* variables of a type. For readability, I ensure that *)
(* type variables appear in the set in the order of *)
(* their first appearance in the type, when reading from *)
(* left to right. \nmlflabelfreetyvars          *)
(* <boxed values 57>=                           *)
val _ = op freetyvars : ty -> name set
(* Primitive type constructors in uML           *)
(*                                              *)
(* In uML, Booleans, lists, pairs, and other algebraic *)
(* data types are predefined using [[data]] definitions. *)
(* Only four type constructors are defined primitively: *)
(*                                              *)
(*   • Integers and symbols, which give types to literal *)
(*  integers and symbols                        *)
(*   • Function and argument type constructors, which *)
(*  give types to functions                     *)
(*                                              *)
(* <type constructors built into \uml\ and \uhaskell>= *)
val inttycon  = freshTycon "int"
val symtycon  = freshTycon "sym"
val funtycon  = freshTycon "function"
val argstycon = freshTycon "arguments"
(* The first two type constructors are used to make the *)
(* [[int]] and [[sym]] types.                   *)

(* <types built into \uml\ and \uhaskell>=      *)
val inttype = TYCON inttycon
val symtype = TYCON symtycon
(* <code to construct and deconstruct function types for \uml>= *)
fun funtype (args, result) = 
  CONAPP (TYCON funtycon, [CONAPP (TYCON argstycon, args), result])

fun asFuntype (CONAPP (TYCON mu, [CONAPP (_, args), result])) =
      if eqTycon (mu, funtycon) then
        SOME (args, result)
      else
        NONE
  | asFuntype _ = NONE
(* The second two are used to make function types, which *)
(* we can construct and deconstruct.            *)
(* <boxed values 92>=                           *)
val _ = op funtype   : ty list * ty -> ty
val _ = op asFuntype : ty -> (ty list * ty) option
(* <definition of [[typeString]] for Hindley-Milner types>= *)
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
(* <shared utility functions on Hindley-Milner types>= *)
type subst = ty env
fun varsubst theta = 
  (fn a => find (a, theta) handle NotFound _ => TYVAR a)
(* Of all the ways we can interpret a substitution, the *)
(* one I use to represent a substitution is a finite map *)
(* from type variables to types. In our code, we have a *)
(* standard representation for such a map:      *)
(* an environment of type [[ty env]]. To interpret a *)
(* substitution as a function from type variables to *)
(* types, we apply [[varsubst]] to it: \nmllabelsubst \ *)
(* nmlflabelvarsubst                            *)
(* <boxed values 49>=                           *)
type subst = subst
val _ = op varsubst : subst -> (name -> ty)
(* As the code shows, the function defined by a *)
(* substitution is total. If a type variable [[a]] is *)
(* not in the domain of the substitution, the function *)
(* leaves [[a]] unchanged.                      *)

(* <shared utility functions on Hindley-Milner types>= *)
fun tysubst theta =
  let fun subst (TYVAR a) = varsubst theta a
        | subst (TYCON c) = TYCON c
        | subst (CONAPP (tau, taus)) = CONAPP (subst tau, map subst taus)
(* The most frequently used interpretation of a *)
(* substitution is as a function from types to types. *)
(* That interpretation is provided by function  *)
(* [[tysubst]]. The code is almost exactly the same as *)
(* the code on \cpagereftuscheme.code.tysubst in \cref *)
(* tuscheme.chap—and there are no pesky quantified types *)
(* to deal with. \nmlflabeltysubst [*]          *)
(* <boxed values 50>=                           *)
val _ = op tysubst : subst -> (ty -> ty)
val _ = op subst   :           ty -> ty
  in  subst
  end
(* <shared utility functions on Hindley-Milner types>= *)
fun dom theta = map (fn (a, _) => a) theta
fun compose (theta2, theta1) =
  let val domain  = union (dom theta2, dom theta1)
      val replace = tysubst theta2 o varsubst theta1
  in  map (fn a => (a, replace a)) domain
  end
(* A function produced by [[tysubst]] has type [[ty -> *)
(* ty]] and so can be composed with any other function *)
(* of the same type, including all functions that *)
(* correspond to substitutions. To be precise, if \subsn *)
(* _1 and \subsn_2 are substitutions, then tysubst \ *)
(* subsn_2 otysubst \subsn_1 is a function from types to *)
(* types (and also corresponds to a substitution). *)
(* Composition is really useful, but a substitution data *)
(* structure \subsn is strictly more useful than the *)
(* corresponding function tysubst \subsn. For one thing, *)
(* we can interrogate \subsn about its domain. To have *)
(* the best of both worlds, I define a function for *)
(* composing substitutions, which obeys the algebraic *)
(* law                                          *)
(*                                              *)
(*  tysubst(compose(\subsn_2, \subsn_1)) = tysubst \ *)
(*  subsn_2 otysubst \subsn_1.                  *)
(*                                              *)
(* Function [[dom]] produces a substitution's domain. \ *)
(* nmlflabeldom,compose                         *)
(* <boxed values 51>=                           *)
val _ = op dom     : subst -> name set
val _ = op compose : subst * subst -> subst
(* <shared utility functions on Hindley-Milner types>= *)
fun instantiate (FORALL (formals, tau), actuals) =
  tysubst (bindList (formals, actuals, emptyEnv)) tau
  handle BindListLength => raise BugInTypeInference
                                              "number of types in instantiation"
(* Instantiation is just as in Chapter [->], except no *)
(* kind environment is needed. Because the system *)
(* ensures everything has the right kind, it is an *)
(* internal error to instantiate with the wrong number *)
(* of arguments. The internal error is signalled by *)
(* raising the exception [[BugInTypeInference]]. *)
(* This exception is raised only when there is a fault *)
(* in the interpreter; a faulty \nml program should *)
(* never trigger this exception. [*] \nmlflabel *)
(* instantiate                                  *)
(* <boxed values 52>=                           *)
val _ = op instantiate : type_scheme * ty list -> ty
(* <shared utility functions on Hindley-Milner types>= *)
val idsubst = emptyEnv
(* <shared utility functions on Hindley-Milner types>= *)
infix 7 |-->
fun a |--> (TYVAR a') = if a = a' then idsubst else bind (a, TYVAR a', emptyEnv)
  | a |--> tau        = if member a (freetyvars tau) then
                          raise BugInTypeInference "non-idempotent substitution"
                        else
                          bind (a, tau, emptyEnv)
(* I finish this section with three more definitions *)
(* related to substitutions. These definitions provide *)
(* the empty substitution [[idsubst]] as well as two *)
(* functions used to create and change substitutions. *)
(* The substitution that maps every type variable to *)
(* itself is sometimes called the empty substitution *)
(* (because its domain is empty) and sometimes the *)
(* identity substitution (because when viewed as a *)
(* function from types to types, it is the identity *)
(* function). In code it is [[idsubst]]; in math it is \ *)
(* idsubst,\notation \idsubstthe identity substitution *)
(* and it obeys the algebraic law \idsubsno\subsn= \ *)
(* subsno\idsubsn= \subsn. \nmlflabelidsubst    *)
(* <boxed values 53>=                           *)
val _ = op idsubst : subst
(* <boxed values 53>=                           *)
val _ = op |--> : name * ty -> subst
(* The [[|–>]] function doesn't accept just any alpha *)
(*  and tau. It produces a substitution \xsubsnalphatau *)
(* only when type variable alpha is not free in tau. *)
(* It's a little hard to motivate this limitation, but *)
(* the idea is to ensure that the [[|–>]] function *)
(* produces only idempotent substitutions. An idempotent *)
(* substitution \subsn has the property that    *)
(*                                              *)
(*  \subsno\subsn= \subsn.                      *)
(*                                              *)
(* Idempotence by itself is not so interesting, but if \ *)
(* subsn= \xsubsnalphatau is idempotent, then we are *)
(* guaranteed the following equality between types: *)
(*                                              *)
(*  \subsnalpha= \subsntau.                     *)
(*                                              *)
(* Because type inference is all about using    *)
(* substitutions to guarantee equality of types, we want *)
(* to be sure that every substitution we create is *)
(* idempotent. (An example of a substitution that is not *)
(* idempotent is \xsubsnalphalistalpha.)        *)

(* <shared utility functions on Hindley-Milner types>= *)
fun typeSchemeString (FORALL ([], tau)) =
      typeString tau
  | typeSchemeString (FORALL (a's, tau)) =
      "(forall (" ^ spaceSep a's ^ ") " ^ typeString tau ^ ")"
(* Functions that print, compare, and create types *)
(*                                              *)
(* Appendix [->] defines a function [[typeString]], *)
(* which we use to print types. We also use this *)
(* function to print type schemes, but when we print a *)
(* true polytype, we make the forall explicit, and *)
(* we show all the quantified variables. [It~is not *)
(* strictly necessary to show the quantified variables, *)
(* because in any top-level type, \emph{all} type *)
(* variables are quantified by the~$\forall$. For this *)
(* reason, Standard~ML leaves out quantifiers and type *)
(* variables. But when you're learning about parametric *)
(* polymorphism, it's better to make the \texttt{forall} *)
(* s explicit. ]                                *)
(* <boxed values 54>=                           *)
val _ = op typeString       : ty          -> string
val _ = op typeSchemeString : type_scheme -> string
(* <shared utility functions on Hindley-Milner types>= *)
fun eqType (TYCON c, TYCON c') = eqTycon (c, c')
  | eqType (CONAPP (tau, taus), CONAPP (tau', taus')) =
      eqType (tau, tau') andalso eqTypes (taus, taus')
  | eqType (TYVAR a, TYVAR a') = a = a'
  | eqType _ = false
and eqTypes (t::taus, t'::taus') = eqType (t, t') andalso eqTypes (taus, taus')
  | eqTypes ([], []) = true
  | eqTypes _ = false
(* Because there are no quantifiers in a type, the *)
(* definition of type equivalence is simpler than the *)
(* corresponding definition for Typed uScheme in chunk  *)
(* [->]. \nmlflabeleqType                       *)
(* <boxed values 55>=                           *)
val _ = op eqType : ty * ty -> bool
(* <shared utility functions on Hindley-Milner types>= *)
local
  val n = ref 1
in
  fun freshtyvar _ = TYVAR ("'t" ^ intString (!n) before n := !n + 1)
(* In our interpreter, fresh type variables come from *)
(* the [[freshtyvar]] function. I use a private mutable *)
(* counter to supply an arbitrary number of type *)
(* variables of the form t n. Because a \nml expression *)
(* or definition does not contain any explicit type *)
(* variables, we need not worry about the names *)
(* colliding with other names.                  *)
(* <boxed values 58>=                           *)
val _ = op freshtyvar : 'a -> ty
end
(* <shared utility functions on Hindley-Milner types>= *)
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
(* Canonical type schemes                       *)
(*                                              *)
(* Type variables like [['t136]] are not suitable for *)
(* use in error messages. A type scheme like \monobox *)
(* (forall ('t136) ((list 't136) -> int)) is unpleasant *)
(* to look at, and it is interchangeable with the *)
(* equivalent type scheme \monobox(forall ('a) ((list *)
(* 'a) -> int)). \stdbreak The two schemes are  *)
(* equivalent because if a type variable is \/-bound, *)
(* its name is irrelevant. For readability, we are *)
(* better off using the names [['a]], [['b]], etc., for *)
(* bound type variables. Function [[canonicalize]] *)
(* renames bound type variables in a type scheme. \ *)
(* stdbreak It replaces each existing bound type *)
(* variable with a canonically named type variable, *)
(* being careful not to use the name of any free type *)
(* variable.                                    *)
(* <boxed values 59>=                           *)
val _ = op canonicalize : type_scheme -> type_scheme
val _ = op newBoundVars : int * name list -> name list
  in  FORALL (newBound, tysubst (bindList (bound, map TYVAR newBound, emptyEnv))
                                                                             ty)
  end
(* The function [[unusedIndex]] finds a name for a bound *)
(* type variable; it ensures that the name is not the *)
(* name of any free type variable.              *)

(* <shared utility functions on Hindley-Milner types>= *)
fun generalize (tau, tyvars) =
  canonicalize (FORALL (diff (freetyvars tau, tyvars), tau))
(* Generalization and instantiation             *)
(*                                              *)
(* Calling [[generalize]](tau, \tyvarset) generalizes *)
(* type tau to a type scheme by closing over type *)
(* variables not in \tyvarset. It also puts the type *)
(* scheme into canonical form.                  *)
(* <boxed values 60>=                           *)
val _ = op generalize : ty * name set -> type_scheme
(* [*]                                          *)

(* <shared utility functions on Hindley-Milner types>= *)
fun freshInstance (FORALL (bound, tau)) =
  instantiate (FORALL (bound, tau), map freshtyvar bound)
(* The dual function, [[instantiate]], is defined in *)
(* chunk [->]. It requires a list of types with which to *)
(* instantiate, but the common case is to instantiate *)
(* with fresh type variables. Function [[freshInstance]] *)
(* implements this case.                        *)
(* <boxed values 61>=                           *)
val _ = op freshInstance : type_scheme -> ty
(* <shared utility functions on Hindley-Milner types>= *)
datatype scheme_shape
  = MONO_FUN of              ty list * ty    (* (tau1 ... tauN -> tau) *)
  | MONO_VAL of              ty              (* tau *)
  | POLY_FUN of tyvar list * ty list * ty
                                         (* (forall (a ...) (tau ... -> tau)) *)
  | POLY_VAL of tyvar list * ty              (* (forall (a ...) tau) *)
(* <shared utility functions on Hindley-Milner types>= *)
fun schemeShape (FORALL (alphas, tau)) =
  case asFuntype tau
    of NONE => if null alphas then MONO_VAL tau
               else                POLY_VAL (alphas, tau)
     | SOME (args, result) =>
               if null alphas then MONO_FUN (args, result)
               else                POLY_FUN (alphas, args, result)
(* <boxed values 96>=                           *)
type scheme_shape = scheme_shape
(* A shape is identified by first looking for a function *)
(* arrow, then checking to see if the list of alpha's is *)
(* empty.                                       *)
(* <boxed values 96>=                           *)
val _ = op schemeShape : type_scheme -> scheme_shape
(* Function [[freetyvarsGamma]] finds the type variables *)
(* free in Gamma, i.e., the type variables free in any  *)
(* sigma in Gamma. We need [[freetyvarsGamma]] to get a *)
(* set of free type variables to use in [[generalize]]; *)
(* when we assign a type scheme to a let-bound variable, *)
(* only those type variables not free in Gamma may be \/ *)
(* -bound. If [[freetyvarsGamma]] were implemented using *)
(* a representation of type \monobox[[type_scheme]] env, *)
(* it would visit every type scheme in every binding in *)
(* the environment. Because most bindings contribute no *)
(* free type variables, most visits would be    *)
(* unnecessary. I therefore implement an optimization: *)
(* with every type environment, I keep a cache of the *)
(* free type variables. A type environment is   *)
(* represented as follows: \nmllabeltype_env    *)
(* <specialized environments for type schemes>= *)
type type_env = type_scheme env * name set
(* <specialized environments for type schemes>= *)
val emptyTypeEnv = 
      (emptyEnv, emptyset)
fun findtyscheme (x, (Gamma, free)) = find (x, Gamma)
(* An empty type environment binds no variables and has *)
(* no free type variables. Looking up a type scheme *)
(* ignores the free variables. \nmlflabelfindtyscheme *)
(* <boxed values 62>=                           *)
val _ = op emptyTypeEnv : type_env
val _ = op findtyscheme : name * type_env -> type_scheme
(* <specialized environments for type schemes>= *)
fun bindtyscheme (x, sigma as FORALL (bound, tau), (Gamma, free)) = 
  (bind (x, sigma, Gamma), union (diff (freetyvars tau, bound), free))
(* <specialized environments for type schemes>= *)
fun freetyvarsGamma (_, free) = free
(* <specialized environments for type schemes>= *)
fun extendTypeEnv (Gamma, bindings) =
  let fun add ((x, sigma), Gamma) = bindtyscheme (x, sigma, Gamma)
  in  foldl add Gamma bindings
  end
(* Because a [[type_env]] has a special representation, *)
(* it needs a special version of [[extend]]. Function *)
(* [[extendTypeEnv]] takes a [[type_env]] on the left *)
(* but a \monoboxtype_scheme env on the right.  *)
(* <boxed values 86>=                           *)
val _ = op extendTypeEnv : type_env * type_scheme env -> type_env
(* <extensions that support existential types>= *)
datatype x_type_scheme
  = FORALL_EXISTS of tyvar list * tyvar list * ty list * ty

fun asExistential (FORALL (alphas_and_betas, tau)) =
  let fun asTyvar (TYVAR a) = a
        | asTyvar _ = let exception GADT in raise GADT end
      fun typeParameters (CONAPP (mu, alphas)) = map asTyvar alphas
        | typeParameters _ = []
  in  case asFuntype tau
        of SOME (args, result) =>
             let val alphas = typeParameters result
                 val betas = diff (alphas_and_betas, alphas)
             in  SOME (FORALL_EXISTS (alphas, betas, args, result))
             end
         | NONE => NONE
  end
(* Before going on with the type theory, here is what we *)
(* have so far, made concrete in code. First, function *)
(* asX. Only a function type can be converted to *)
(* existential. We find the result type by stripping off *)
(* the function arrow. We then look at the result type's *)
(* parameters; those are the \ldotsn alpha. And whatever *)
(* original parameters are left over are the \ldotsmbeta *)
(* .                                            *)
(* <boxed values 103>=                          *)
type x_type_scheme = x_type_scheme
val _ = op asExistential : type_scheme -> x_type_scheme option
(* In order to skolemize an existential type, we have to *)
(* have fresh skolem types. A skolem type is represented *)
(* as a type constructor, but unlike a normal type *)
(* constructor, it has an odd number as its identity. *)
(* (If I were starting from scratch, I would prefer to *)
(* add [[SKOLEM_TYPE]] to the representation of [[ty]], *)
(* but because I have lots of constraint-solving and *)
(* type-inference code leftover from \nml, I prefer a *)
(* representation that permits me to reuse that code.) *)
(* <extensions that support existential types>= *)
fun freshSkolem _ =
  let val { identity = id, printName = T } = freshTycon "skolem type"
  in  TYCON { identity = id + 1, printName = "skolem type " ^ intString (id div
                                                                            2) }
  end

fun isSkolem { identity = n, printName = _ } = (n mod 2 = 1)
(* <extensions that support existential types>= *)
fun addFreeSkolems (TYCON mu, mus) =
      if isSkolem mu then insert (mu, mus) else mus
  | addFreeSkolems (TYVAR _,  mus) =
      mus
  | addFreeSkolems (CONAPP (tau, taus), mus) =
      foldl addFreeSkolems (addFreeSkolems (tau, mus)) taus
(* Using [[addFreeSkolems]], I can find free skolem *)
(* types in a type, in a set of types, or in a list of *)
(* type schemes.                                *)

(* <extensions that support existential types>= *)
fun typeFreeSkolems        tau    = addFreeSkolems (tau, emptyset)
fun typesFreeSkolems       taus   = foldl addFreeSkolems emptyset taus
fun typeSchemesFreeSkolems sigmas =
      typesFreeSkolems (map (fn FORALL (_, tau) => tau) sigmas)
(* I find free skolem types by examining every type *)
(* constructor. I want only every to add a skolem type *)
(* to an existing set, not to allocate multiple sets, so *)
(* I begin with a function that can be passed to *)
(* [[foldl]].                                   *)
(* <boxed values 104>=                          *)
val _ = op addFreeSkolems : ty * tycon set -> tycon set
(* <boxed values 104>=                          *)
val _ = op typeFreeSkolems  : ty     -> tycon set
val _ = op typesFreeSkolems : ty set -> tycon set
val _ = op typeSchemesFreeSkolems : type_scheme list -> tycon set
(* <extensions that support existential types>= *)
fun typeEnvSubst theta Gamma' =
  let fun subst (FORALL ([], tau)) = FORALL ([], tysubst theta tau)
        | subst _ = let exception PolytypeInPattern in raise PolytypeInPattern
                                                                             end
  in  map (fn (x, sigma) => (x, subst sigma)) Gamma'
  end



(*****************************************************************)
(*                                                               *)
(*   ABSTRACT SYNTAX AND VALUES FOR \UML                         *)
(*                                                               *)
(*****************************************************************)

(* The representations defined above are combined with *)
(* representations from other chapters as follows: *)
(* <abstract syntax and values for \uml>=       *)
(* No matter how many type constructors we may add to *)
(* Typed uScheme, these kinding rules tell us everything *)
(* we ever need to know about the formation of types. *)
(* Compare this situation with the situation in Typed *)
(* Impcore. In Typed Impcore, we need the \rulename *)
(* BaseTypes rule for [[int]] and [[bool]]. \stdbreak *)
(* To add arrays we need the \rulenameArrayFormation *)
(* rule. To add lists we would need a list-formation *)
(* rule (\crefpagetypesys.ex.timpcore-lists). And so on. *)
(* Unlike Typed Impcore's type system, Typed uScheme's *)
(* type system can easily be extended with new type *)
(* constructors (\cref                          *)
(* typesys.ex.tuscheme-queues,typesys.ex.tuscheme-pairs,typesys.ex.tuscheme-sums,typesys.ex.polyrefs *)
(* from \cpagereftypesys.ex.tuscheme-queues). \stdbreak *)
(* Similar ideas are used in languages in which *)
(* programmers can define new type constructors, *)
(* including uML (\crefadt.chap) and \mcl (\cref *)
(* mcl.chap).                                   *)
(*                                              *)
(* Implementing kinds                           *)
(*                                              *)
(* A kind is represented using the datatype [[kind]]. \ *)
(* tuslabelkind                                 *)
(* <kinds for typed languages>=                 *)
datatype kind = TYPE                          (* kind of all types *)
              | ARROW of kind list * kind     (* kind of many constructors *)
(* Kinds are equal if and only if they are identical. *)

(* <kinds for typed languages>=                 *)
fun eqKind (TYPE, TYPE) = true
  | eqKind (ARROW (args, result), ARROW (args', result')) =
      eqKinds (args, args') andalso eqKind (result, result')
  | eqKind (_, _) = false
and eqKinds (ks, ks') = ListPair.allEq eqKind (ks, ks')
(* <kinds for typed languages>=                 *)
fun kindString TYPE = "*"
  | kindString (ARROW (ks, k)) =
      "(" ^ spaceSep (map kindString ks @ ["=>", kindString k]) ^ ")"
(* Abstract syntax, including type syntax, and values *)
(*                                              *)
(* We begin our tour of syntax with type expressions: *)
(* a type expression in uML is just like a type *)
(* expression in Typed uScheme \tustypepagetyex. But *)
(* in Typed uScheme, the name of a type (or type *)
(* constructor) identifies it completely, and in uML, a *)
(* type name, has to be translated into a type  *)
(* constructor. The translation transforms syntax t (ML *)
(* type [[tyex]]) into a type scheme sigma      *)
(* ([[type_scheme]]). It is described in \crefpage *)
(* adt.type-translation. [*] \umllabeltyex      *)
(* <definition of [[tyex]] for \uml>=           *)
datatype tyex = TYNAME  of name
                                            (* names type or type constructor *)
              | CONAPPX of tyex * tyex list    (* type-level application *)
              | FORALLX of name list * tyex
              | TYVARX  of name                (* type variable *)
              | FUNTYX  of tyex list * tyex
(* Next, the syntax of patterns. A value constructor is *)
(* represented by its name. Value constructors are used *)
(* in three different representations: a pattern made *)
(* using a value constructor is [[CONPAT]]; a value made *)
(* using a value constructor is [[CONVAL]]; and *)
(* in an expression, a value constructor is [[VCONX]] *)
(* all by itself. \umllabelpat                  *)
(* <definition of [[pat]], for patterns>=       *)
type vcon = name   (* a value constructor *)
datatype pat = WILDCARD
             | PVAR     of name
             | CONPAT   of vcon * pat list
(* Expressions are as in \nml, with the addition of a *)
(* case expression. Strictly speaking, [[case]] makes *)
(* [[if]] redundant, but to enable uML to share code *)
(* with \nml, I keep the [[IFX]] form. \stdbreak I also *)
(* add a [[VCONX]] form, used when the name of a value *)
(* constructor appears in an expression. This form has *)
(* the same type theory and operational semantics as the *)
(* [[VAR]] form, but when something goes wrong, *)
(* it issues a different error message (\chunkref *)
(* adt.chunk.ty-vconx). \umllabelexp            *)
(* <definitions of [[exp]] and [[value]] for \uml>= *)
datatype exp 
  = LITERAL    of value
  | VAR        of name
  | VCONX      of vcon
  | CASE       of exp * (pat * exp) list
  | IFX        of exp * exp * exp (* could be syntactic sugar for CASE *)
  | BEGIN      of exp list
  | APPLY      of exp * exp list
  | LETX       of let_kind * (name * exp) list * exp
  | LAMBDA     of name list * exp
and let_kind = LET | LETREC | LETSTAR
(* We keep \nml's symbols, numbers, closures, and *)
(* primitive functions. \umllabelvalue,vcon     *)
(* <definitions of [[exp]] and [[value]] for \uml>= *)
and value
  = CONVAL of vcon * value list
  | SYM    of name
  | NUM    of int
  | CLOSURE   of lambda * (unit -> value env)
  | PRIMITIVE of primop
 withtype lambda = name list * exp
      and primop = value list -> value
(* <definition of [[def]] for \uml>=            *)
datatype def  = VAL    of name * exp
              | VALREC of name * exp
              | EXP    of exp
              | DEFINE of name * (name list * exp)
              | DATA   of data_def
  withtype data_def = name * kind * (vcon * tyex) list
(* The definitions of uML are the definitions of \nml, *)
(* plus the [[DATA]] form. \umllabeldef         *)
(* <boxed values 78>=                           *)
type data_def = data_def
(* Syntactic sugar for implicit-data            *)
(*                                              *)
(* An implicit data definition gives type parameters, *)
(* the name of the type constructor, and definitions for *)
(* one or more value constructors.              *)
(* <definition of [[implicit_data_def]] for \uml>= *)
datatype implicit_data_def 
  = IMPLICIT_DATA of tyvar list * name * implicit_vcon list
and implicit_vcon 
  = IMPLICIT_VCON of vcon * tyex list
(* Unit tests are like \nml's unit tests, except that *)
(* the type in a [[check-type]] or a            *)
(* [[check-principal-type]] is syntax that has to be *)
(* translated into a [[type_scheme]]. \umllabelunit_test *)
(* <definition of [[unit_test]] for languages with Hindley-Milner types and generated type constructors>= *)
datatype unit_test = CHECK_EXPECT      of exp * exp
                   | CHECK_ASSERT      of exp
                   | CHECK_ERROR       of exp
                   | CHECK_TYPE        of exp * tyex
                   | CHECK_PTYPE       of exp * tyex
                   | CHECK_TYPE_ERROR  of def
(* <definition of [[xdef]] (shared)>=           *)
datatype xdef = DEF    of def
              | USE    of name
              | TEST   of unit_test
              | DEFS   of def list  (*OMIT*)
(* <definition of [[valueString]] for \uml>=    *)

fun valueString (CONVAL ("cons", [v, vs])) = consString (v, vs)
  | valueString (CONVAL ("'()",  []))      = "()"
  | valueString (CONVAL (c, []))  = c
  | valueString (CONVAL (c, vs))  =
      "(" ^ c ^ " " ^ spaceSep (map valueString vs) ^ ")"
  | valueString (NUM n      )   = String.map (fn #"~" => #"-" | c => c) (
                                                                 Int.toString n)
  | valueString (SYM v      )   = v
  | valueString (CLOSURE   _)   = "<procedure>"
  | valueString (PRIMITIVE _)   = "<procedure>"
(* As in other interpreters, we have a special way of *)
(* printing applications of [[cons]].           *)
(* <definition of [[valueString]] for \uml>=    *)
and consString (v, vs) =
      let fun tail (CONVAL ("cons", [v, vs])) = " " ^ valueString v ^ tail vs
            | tail (CONVAL ("'()", []))       = ")"
            | tail _ =
                raise BugInTypeInference
                  "bad list constructor (or cons/'() redefined)"
      in  "(" ^ valueString v ^ tail vs
	  end
(* Printing values, patterns, types, and kinds  *)
(*                                              *)
(* To print a list, we look only at the name of a value *)
(* constructor (we don't have its type). If a user's uML *)
(* program redefines the [[cons]] value constructor, *)
(* chaos will ensue.                            *)
(* <boxed values 122>=                          *)
val _ = op valueString : value -> string
(* <definition of [[patString]] for \uml\ and \uhaskell>= *)
fun patString WILDCARD    = "_"
  | patString (PVAR x)    = x
  | patString (CONPAT (vcon, []))   = vcon
  | patString (CONPAT (vcon, pats)) = "(" ^ spaceSep (vcon :: map patString pats
                                                                         ) ^ ")"
(* Rendering expressions as strings             *)
(*                                              *)
(* <definition of [[expString]] for \nml\ and \uml>= *)
fun expString e =
  let fun bracket s = "(" ^ s ^ ")"
      fun sqbracket s = "[" ^ s ^ "]"
      val bracketSpace = bracket o spaceSep
      fun exps es = map expString es
      fun withBindings (keyword, bs, e) =
        bracket (spaceSep [keyword, bindings bs, expString e])
      and bindings bs = bracket (spaceSep (map binding bs))
      and binding (x, e) = sqbracket (x ^ " " ^ expString e)
      val letkind = fn LET => "let" | LETSTAR => "let*" | LETREC => "letrec"
  in  case e
        of LITERAL v => valueString v
         | VAR name => name
         | IFX (e1, e2, e3) => bracketSpace ("if" :: exps [e1, e2, e3])
         | BEGIN es => bracketSpace ("begin" :: exps es)
         | APPLY (e, es) => bracketSpace (exps (e::es))
         | LETX (lk, bs, e) => bracketSpace [letkind lk, bindings bs, expString
                                                                              e]
         | LAMBDA (xs, body) => bracketSpace ("lambda" :: xs @ [expString body])
         (* <extra cases of [[expString]] for \uml>=     *)
         | VCONX vcon => vcon
         | CASE (e, matches) =>
             let fun matchString (pat, e) = sqbracket (spaceSep [patString pat,
                                                                   expString e])
             in  bracketSpace ("case" :: expString e :: map matchString matches)
             end
         (* <extra cases of [[expString]] for \uml>=     *)
         (* this space is filled in by the uML appendix *)
  end
(* <definitions of [[defString]] and [[defName]] for \nml\ and \uml>= *)
fun defString d =
  let fun bracket s = "(" ^ s ^ ")"
      val bracketSpace = bracket o spaceSep
      fun formal (x, t) = "[" ^ x ^ " : " ^ typeString t ^ "]"
  in  case d
        of EXP e         => expString e
         | VAL    (x, e) => bracketSpace ["val",     x, expString e]
         | VALREC (x, e) => bracketSpace ["val-rec", x, expString e]
         | DEFINE (f, (formals, body)) =>
             bracketSpace ["define", f, bracketSpace formals, expString body]

        (* <cases for [[defString]] for forms found only in \uml ((elided))>= *)
         | DATA (t, kind, _) => bracketSpace ["data", kindString kind, t, "..."]

        (* <cases for [[defString]] for forms found only in \uml ((elided))>= *)
         (*empty*)
  end
fun defName (VAL (x, _))       = x
  | defName (VALREC (x, _)) = x
  | defName (DEFINE (x, _)) = x
  | defName (EXP _) = raise InternalError "asked for name defined by expression"
  (* <clauses for [[defName]] for forms found only in \uml ((elided))>= *)
  | defName (DATA (t, _, _)) = t
  (* <clauses for [[defName]] for forms found only in \uml ((elided))>= *)
  (*empty*)
(* <definition of [[tyexString]] for \uml>=     *)
fun tyexString (TYNAME t) = t
  | tyexString (CONAPPX (tx, txs)) =
      "(" ^ tyexString tx ^ " " ^ spaceSep (map tyexString txs) ^ ")"
  | tyexString (FORALLX (alphas, tx)) =
      "(forall (" ^ spaceSep alphas ^ ") " ^ tyexString tx ^ ")"
  | tyexString (TYVARX a) = a
  | tyexString (FUNTYX (args, result)) =
      "(" ^ spaceSep (map tyexString args) ^ " -> " ^ tyexString result ^ ")"


(*****************************************************************)
(*                                                               *)
(*   UTILITY FUNCTIONS ON \UML\ VALUES                           *)
(*                                                               *)
(*****************************************************************)

(* The first new function we need is the one that *)
(* defines primitive equality. In uML, polymorphic *)
(* equality uses the same rules as in full ML;  *)
(* in particular, identical value constructors applied *)
(* to equal values are considered equal.        *)
(* <utility functions on \uml\ values ((uml))>= *)
fun primitiveEquality (v, v') =
  let fun noFun () = raise RuntimeError "compared functions for equality"
  in  case (v, v')
        of (NUM  n1,  NUM  n2)  => (n1 = n2)
         | (SYM  v1,  SYM  v2)  => (v1 = v2)
         | (CONVAL (vcon, vs), CONVAL (vcon', vs')) =>
             vcon = vcon' andalso ListPair.allEq primitiveEquality (vs, vs')
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
(* <utility functions on \uml\ values ((uml))>= *)
fun embedList []      = CONVAL ("'()", [])
  | embedList (v::vs) = CONVAL ("cons", [v, embedList vs])
(* The parser for literal S-expressions uses    *)
(* [[embedList]] to convert a list of S-expressions into *)
(* an S-expression. The \nml version (\chunkref *)
(* mlscheme.chunk.embedList) uses Standard ML value *)
(* constructors [[PAIR]] and [[NIL]], but the uML *)
(* version uses uML value constructors [[cons]] and [[' *)
(* ()]].                                        *)
(* <boxed values 101>=                          *)
val _ = op embedList : value list -> value
(* <utility functions on \uml\ values>=         *)
fun embedBool b = CONVAL (if b then "#t" else "#f", [])
fun bool (CONVAL ("#t", [])) = true
  | bool _                 = false
(* The operations that convert between \nml Booleans and *)
(* Standard ML Booleans use \nml's [[BOOLV]]. Again, the *)
(* uML versions use uML's value constructors.   *)
(* <boxed values 102>=                          *)
val _ = op bool      : value -> bool
val _ = op embedBool : bool -> value



(*****************************************************************)
(*                                                               *)
(*   LEXICAL ANALYSIS AND PARSING FOR \UML, PROVIDING [[FILEXDEFS]] AND [[STRINGSXDEFS]] *)
(*                                                               *)
(*****************************************************************)

(* <lexical analysis and parsing for \uml, providing [[filexdefs]] and [[stringsxdefs]]>= *)
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
  (* <boxed values 232>=                          *)
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
(* <boxed values 231>=                          *)
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
(* Identifying uML tokens                       *)
(*                                              *)
(* From the implementation of micro-Scheme in \cref *)
(* mlschemea.chap, uML inherits the token parsers *)
(* [[name]], [[booltok]], [[quote]], and [[int]]. Type *)
(* variables are easily recognized. uML has many *)
(* different kinds of names, and I want to be precise *)
(* about which sort of name I mean where. So I rename *)
(* [[name]] to [[any_name]], and I disable [[name]] by *)
(* rebinding it to a useless value.             *)
(* <parsers for \uml\ tokens>=                  *)
val tyvar = quote *> (curry op ^ "'" <$> name <?>
                                               "type variable (got quote mark)")
val any_name = name
val name = () (* don't use me as a parser *)
(* A token that presents as a name is one of the *)
(* following: an arrow, a value constructor, a value *)
(* variable, or a type name. First the predicates: *)
(* <parsers for \uml\ value constructors and value variables>= *)
fun isVcon x =
  let val first = String.sub (x, 0)
  in  x = "cons" orelse x = "'()" orelse
      Char.isUpper first orelse first = #"#" orelse
      String.isPrefix "make-" x
  end
fun isVvar x = x <> "->" andalso not (isVcon x)
(* And now the parsers. A value constructor may be not *)
(* only a suitable name but also a Boolean literal or *)
(* the empty list.                              *)

(* <parsers for \uml\ value constructors and value variables>= *)
val arrow = sat (fn n => n = "->") any_name
val vvar  = sat isVvar any_name
val tyname = vvar
val vcon  = 
  let fun isEmptyList (left, right) = notCurly left andalso snd left = snd right
      val boolcon  = (fn p => if p then "#t" else "#f") <$> booltok
  in  boolcon <|> sat isVcon any_name <|>
      "'()" <$ quote <* sat isEmptyList (pair <$> left <*> right)
  end
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
(* <boxed values 233>=                          *)
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
(* <boxed values 234>=                          *)
val _ = op recordFieldsOf : name parser -> name list parser
(* <parsers and parser builders for formal parameters and bindings>= *)
fun kw keyword = 
  eqx keyword any_name
fun usageParsers ps = anyParser (map (usageParser kw) ps)
(* We parse any keyword as the name represented by the *)
(* same string as the keyword. And using the keyword *)
(* parser, we can string together ``usage'' parsers. *)
(* <boxed values 235>=                          *)
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
(* <boxed values 236>=                          *)
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
(* <parser builders for typed languages>=       *)
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
(* <parser builders for typed languages>=       *)
fun typedFormalOf name colon ty =
      bracket ("[x : ty]", pair <$> name <* colon <*> ty)
fun typedFormalsOf name colon ty context = 
  let val formal = typedFormalOf name colon ty
  in  distinctBsIn (bracket("(... [x : ty] ...)", many formal)) context
  end                            
(* <boxed values 219>=                          *)
val _ = op typedFormalsOf : string parser -> 'b parser -> 'a parser -> string ->
                                                       (string * 'a) list parser
(* <parsers for Hindley-Milner types with generated type constructors>= *)
fun tyex tokens = (
     TYNAME <$> tyname
 <|> TYVARX <$> tyvar
 <|> usageParsers
           [("(forall (tyvars) type)",
             curry FORALLX <$> bracket ("('a ...)", distinctTyvars) <*> tyex)]
 <|> bracket("(ty ty ... -> ty)",
             arrowsOf CONAPPX FUNTYX <$> many tyex <*>! many (arrow *> many tyex
                                                                              ))
) tokens
(* Parsing types and kinds                      *)
(*                                              *)
(* Parsers for types and kinds are as in Typed uScheme, *)
(* except the type parser produces a [[tyex]], not a  *)
(* [[ty]].                                      *)
(* <boxed values 107>=                          *)
val _ = op tyvar : string parser
val _ = op tyex  : tyex   parser
(* <parsers for Hindley-Milner types with generated type constructors>= *)
fun kind tokens = (
  TYPE <$ eqx "*" vvar
  <|> bracket ("arrow kind", curry ARROW <$> many kind <* eqx "=>" vvar <*> kind
                                                                               )
) tokens

val kind = kind <?> "kind"
(* <boxed values 108>=                          *)
val _ = op kind : kind parser
(* <parsers and [[xdef]] streams for \uml>=     *)
fun pattern tokens =  (
                WILDCARD    <$  eqx "_" vvar
      <|>       PVAR        <$> vvar
      <|> curry CONPAT      <$> vcon <*> pure []
      <|> bracket ( "(C x1 x2 ...) in pattern"
                  , curry CONPAT <$> vcon <*> many pattern
                  )
       ) tokens
(* Parsing patterns                             *)
(*                                              *)
(* The distinction between value variable and value *)
(* constructor is most important in patterns.   *)
(* <boxed values 109>=                          *)
val _ = op pattern : pat parser
(* <parsers and [[xdef]] streams for \uml>=     *)
val vvarFormalsIn = formalsOf "(x1 x2 ...)" vvar
val patFormals    = bracket ("(p1 p2 ...)", many pattern)
(* Parsing expressions                          *)
(*                                              *)
(* Parsing is more elaborate then usual because *)
(* I provide for two flavors of each binding construct *)
(* found in \nml: the standard flavor, which binds *)
(* variables, and the ``patterns everywhere'' flavor, *)
(* which binds patterns. (The case expression,  *)
(* of course, binds only patterns.) I begin with parsers *)
(* for formal parameters, which are used to parse both *)
(* expressions and definitions. The [[vvarFormalsIn]] *)
(* parsers takes a string giving the context, because *)
(* the parser may detect duplicate names. The   *)
(* [[patFormals]] parser doesn't take the context, *)
(* because when patterns are used, duplicate names are *)
(* detected during type checking.               *)
(* <boxed values 110>=                          *)
val _ = op vvarFormalsIn : string -> name list parser
val _ = op patFormals    :            pat list parser
(* To parse an expression, I provide two sets of *)
(* parsers, but I provide only the ``expression *)
(* builders'' that work with names. Expression builders *)
(* that work with patterns are left as exercises. *)
(* <parsers and [[xdef]] streams for \uml>=     *)
(* <utility functions that help implement \uml's syntactic sugar ((prototype))>= *)
fun freeIn exp y =
  let fun has_y (CASE (e, choices)) = has_y e orelse (List.exists choice_has_y)
                                                                         choices
        | has_y _ = raise LeftAsExercise "free variable of an expression"
      and choice_has_y (p, e) = not (pat_has_y p) andalso has_y e
      and pat_has_y (PVAR x) = x = y
        | pat_has_y (CONPAT (_, ps)) = List.exists pat_has_y ps
        | pat_has_y WILDCARD = false
  in  has_y exp
  end
(* <utility functions that help implement \uml's syntactic sugar>= *)
val varsupply = 
  streamMap (fn n => "x" ^ intString n) naturals
fun freshVar e =
  case streamGet (streamFilter (not o freeIn e) varsupply)
    of SOME (x, _) => x
     | NONE => let exception EmptyVarSupply in raise EmptyVarSupply end
(* Support for syntactic sugar                  *)
(*                                              *)
(* Some syntactic transformations need to find a *)
(* variable that is not free in a given expression. *)
(* If you have done \crefpagemlscheme.ex.closure-code in *)
(* \crefmlscheme.chap, you're close to having the right *)
(* test. Use that code to complete function [[freeIn]] *)
(* here.                                        *)
(* <boxed values 117>=                          *)
val _ = op freeIn : exp -> name -> bool
(* Once [[freeIn]] is implemented, here are a variety of *)
(* helper functions. Function [[freshVar]] returns a *)
(* variable that is not free in a given expression. *)
(* The supply of variables is infinite, so the exception *)
(* should never be raised.                      *)
(* <boxed values 117>=                          *)
val _ = op varsupply : name stream
val _ = op freshVar  : exp -> name
(* <utility functions that help implement \uml's syntactic sugar>= *)
fun freshVars e xs =
  streamTake (length xs, streamFilter (not o freeIn e) varsupply)
(* Function [[freshVars]] returns as many fresh *)
(* variables as there are elements in [[xs]].   *)
(* <boxed values 118>=                          *)
val _ = op freshVars : exp -> 'a list -> name list
(* <utility functions that help implement \uml's syntactic sugar>= *)
fun tupleVcon xs = case length xs
                     of 2 => "PAIR"
                      | 3 => "TRIPLE"
                      | n => "T" ^ intString n

fun tupleexp [x] = VAR x
  | tupleexp xs  = APPLY (VCONX (tupleVcon xs), map VAR xs)

fun tuplepat [x] = x
  | tuplepat xs  = CONPAT (tupleVcon xs, xs)
(* To support pattern matching in [[lambda]],   *)
(* [[lambda*]], and [[define*]], we turn a sequence of *)
(* names into a single tuple expression, and we turn a *)
(* sequence of patterns into a single tuple pattern. *)
(* Function [[tupleVcon]] gives the name of the value *)
(* constructor for a tuple of the same size as the given *)
(* list.                                        *)
(* <boxed values 119>=                          *)
val _ = op tupleexp  : name list -> exp
val _ = op tuplepat  : pat  list -> pat
val _ = op tupleVcon : 'a   list -> vcon
(* <utility functions that help implement \uml's syntactic sugar>= *)
fun freePatVars (PVAR x)         = insert (x, emptyset)
  | freePatVars (WILDCARD)       = emptyset
  | freePatVars (CONPAT (_, ps)) = foldl union emptyset (map freePatVars ps)
(* Function [[freePatVars]] finds the free variables in *)
(* a pattern.                                   *)
(* <boxed values 120>=                          *)
val _ = op freePatVars : pat -> name set
fun exptable exp =
  let (* parsers used in both flavors *)
      val choice   = bracket ("(pattern exp)", pair <$> pattern <*> exp)
      val letrecBs = distinctBsIn (bindingsOf "(x e)" vvar exp) "letrec"

      (* parsers for bindings to names *)
      val letBs     = distinctBsIn (bindingsOf "(x e)" vvar exp) "let"
      val letstarBs = bindingsOf "(x e)" vvar exp
      val formals   = vvarFormalsIn "lambda"

      (* parsers for bindings to patterns *)
      val patBs       = bindingsOf "(p e)" pattern exp
      val patLetrecBs = map (fn (x, e) => (PVAR x, e)) <$> letrecBs
      val patLetBs =
        let fun patVars (WILDCARD)       = []
              | patVars (PVAR x)         = [x]
              | patVars (CONPAT (_, ps)) = List.concat (map patVars ps)
            fun check (loc, bs) =
              let val xs = List.concat (map (patVars o fst) bs)
              in  nodups ("bound name", "let") (loc, xs) >>=+ (fn _ => bs)
              end
        in  check <$>! @@ patBs
        end
      val patFormals = patFormals (* defined above *)

      (* expression builders that expect to bind names *)
      fun letx letkind bs e = LETX (letkind, bs, e)
      fun lambda xs e = LAMBDA (xs, e)
      fun lambdastar clauses = ERROR "lambda* is left as an exercise"

      (* The rest of the code is for you to write.    *)
      (* <\uml\ expression builders that expect to bind patterns>= *)
      (* you can redefine letx, lambda, and lambdastar here *)
  in  (* The parsers that might change are [[formals]], *)
      (* [[letBs]], and [[letstarBs]]. The expression-builders *)
      (* that might change are [[lambda]], [[lambdastar]], and *)
      (* [[letx]].                                    *)
      (* <parsers for expressions that begin with keywords>= *)
      usageParsers
        [ ("(if e1 e2 e3)",            curry3 IFX    <$> exp  <*> exp <*> exp)
        , ("(begin e1 ...)",                  BEGIN  <$> many exp)
        , ("(lambda (names) body)",           lambda <$> formals <*> exp)
        , ("(lambda* (pats) exp ...)",
             lambdastar <$>!
             many1 (bracket ("[(pat ...) e]",
                             pair <$> (bracket ("(pat ...)", many pattern)) <*>
                                                                          exp)))
        , ("(let (bindings) body)",    letx   LET     <$> letBs     <*> exp)
        , ("(letrec (bindings) body)", letx   LETREC  <$> letrecBs  <*> exp)
        , ("(let* (bindings) body)",   letx   LETSTAR <$> letstarBs <*> exp)
        , ("(case exp (pattern exp) ...)", curry CASE <$> exp <*> many choice)

        , ("(while e1 e2)", exp  *> exp <!>
                                     "uML does not include 'while' expressions")
        , ("(set x e)",     vvar *> exp <!>
                                       "uML does not include 'set' expressions")
        (* <rows added to \uml's [[exptable]] in exercises>= *)
        (* you add this bit *)
        ]
  end
(* <parsers and [[xdef]] streams for \uml>=     *)
val atomicExp =  VAR               <$> vvar
             <|> VCONX             <$> vcon
             <|> (LITERAL o NUM)   <$> int

fun exp tokens = (
     atomicExp
 <|> quote *> (LITERAL <$> sexp)
 <|> exptable exp
 <|> leftCurly <!> "curly brackets are not supported"
 <|> left *> right <!> "empty application"
 <|> bracket ("function application", curry APPLY <$> exp <*> many exp)
) tokens
(* With the keyword expressions defined by [[exptable]], *)
(* here are the atomic expressions and the full *)
(* expressions.                                 *)
(* <boxed values 111>=                          *)
val _ = op atomicExp : exp parser
val _ = op exp       : exp parser
(* <parsers and [[xdef]] streams for \uml>=     *)
(* <definition of [[makeExplicit]], to translate [[implicit-data]] to [[data]]>= *)
fun makeExplicit (IMPLICIT_DATA ([], t, vcons)) =
      let val tx = TYNAME t
          fun convertVcon (IMPLICIT_VCON (K, []))  = (K, tx)
            | convertVcon (IMPLICIT_VCON (K, txs)) = (K, FUNTYX (txs, tx))
      in  (t, TYPE, map convertVcon vcons)
      end
  | makeExplicit (IMPLICIT_DATA (alphas, t, vcons)) =
      let val kind = ARROW (map (fn _ => TYPE) alphas, TYPE)
          val tx   = CONAPPX (TYNAME t, map TYVARX alphas)
          fun close tau = FORALLX (alphas, tau)
          fun vconType (vcon, [])  = tx
            | vconType (vcon, txs) = FUNTYX (txs, tx)
          fun convertVcon (IMPLICIT_VCON (K, []))  = (K, close tx)
            | convertVcon (IMPLICIT_VCON (K, txs)) = (K, close (FUNTYX (txs, tx)
                                                                              ))
  in  (t, kind, map convertVcon vcons)
  end
(* The following code translates an implicit data *)
(* definition into an explicit one. \umlflabel  *)
(* makeExplicit                                 *)
(* <boxed values 124>=                          *)
val _ = op makeExplicit : implicit_data_def -> data_def
val tyvarlist = bracket ("('a ...)", many1 tyvar)
val optionalTyvars = (fn alphas => getOpt (alphas, [])) <$> optional tyvarlist
val implicitData =
  let fun vc c taus = IMPLICIT_VCON (c, taus)
      val vconDef =  vc <$> vcon <*> pure []
                 <|> bracket ("(vcon of ty ...)",
                              vc <$> vcon <* eqx "of" vvar <*> many1 tyex)
  in  usageParsers
      [("(implicit-data [('a ...)] t vcon ... (vcon of ty ...) ...)"
       , (DATA o makeExplicit) <$>
         (curry3 IMPLICIT_DATA <$> optionalTyvars <*> tyname <*> many vconDef)
       )]
  end
(* Parsing definitions                          *)
(*                                              *)
(* I begin with the [[implicit-data]] definition, which *)
(* is parsed here and then transformed to a [[data]] *)
(* definition by function [[makeExplicit]].     *)
(* <boxed values 112>=                          *)
val _ = op implicitData : def parser
(* <parsers and [[xdef]] streams for \uml>=     *)
val def = 
  let (* parser for binding to names *)
      val formals = vvarFormalsIn "define"

      (* parsers for clausal definitions, a.k.a. define* *)
      val lhs = bracket ("(f p1 p2 ...)", pair <$> vvar <*> many pattern)
      val clause =
        bracket ("[(f p1 p2 ...) e]",
                 (fn (f, ps) => fn e => (f, (ps, e))) <$> lhs <*> exp)

      (* definition builders used in all parsers *)
      val Kty = typedFormalOf vcon (kw ":") tyex
      fun data kind name vcons = DATA (name, kind, vcons)

      (* definition builders that expect to bind names *)
      fun define f xs body = DEFINE (f, (xs, body))
      fun definestar _ = ERROR "define* is left as an exercise"


   (* <\uml\ definition builders that expect to bind patterns ((prototype))>= *)
      (* you can redefine 'define' and 'definestar' here *)
  in  usageParsers
      [ ("(define f (args) body)",        define       <$> vvar <*> formals <*>
                                                                            exp)
      , ("(define* (f pats) e ...)",      definestar   <$>! many1 clause)
      , ("(val x e)",                     curry VAL    <$> vvar <*> exp)
      , ("(val-rec x e)",                 curry VALREC <$> vvar <*> exp)
      , ("(data kind t [vcon : type] ...)", data <$> kind <*> tyname <*> many
                                                                            Kty)
      ]
  end
(* Here is the parser for the true definitions. *)
(* <boxed values 113>=                          *)
val _ = op def : def parser
(* <parsers and [[xdef]] streams for \uml>=     *)
val testtable = usageParsers
  [ ("(check-expect e1 e2)",          curry CHECK_EXPECT     <$> exp <*> exp)
  , ("(check-assert e)",                    CHECK_ASSERT     <$> exp)
  , ("(check-error e)",                     CHECK_ERROR      <$> exp)
  , ("(check-type e tau)",            curry CHECK_TYPE       <$> exp <*> tyex)
  , ("(check-principal-type e tau)",  curry CHECK_PTYPE      <$> exp <*> tyex)
  , ("(check-type-error e)",                CHECK_TYPE_ERROR <$> (def <|>
                                                                    implicitData
                                                                 <|> EXP <$> exp
                                                                              ))
  ]
(* The parser for unit tests.                   *)
(* <boxed values 114>=                          *)
val _ = op testtable : unit_test parser
(* <parsers and [[xdef]] streams for \uml>=     *)
val xdeftable = usageParsers
  [ ("(use filename)", USE <$> any_name)
  (* <rows added to \uml's [[xdeftable]] in exercises ((prototype))>= *)
  (* you can add a row for 'val' here *)
  (* <rows added to \uml's [[xdeftable]] in exercises>= *)
  (* you add this bit *)
  ]
(* The parser for other extended definitions.   *)
(* <boxed values 115>=                          *)
val _ = op xdeftable : xdef parser
(* <parsers and [[xdef]] streams for \uml>=     *)
val xdef  =  TEST <$> testtable
         <|>          xdeftable
         <|> DEF  <$> (def <|> implicitData)
         <|> badRight "unexpected right bracket"
         <|> DEF <$> EXP <$> exp
         <?> "definition"
val xdefstream = interactiveParsedStream (schemeToken, xdef)
(* <boxed values 116>=                          *)
val _ = op xdef : xdef parser
(* <parsers and [[xdef]] streams for \uml>=     *)
local
  fun sxb b = CONVAL ("Sx.B", [embedBool b])
  fun sxs s = CONVAL ("Sx.S", [SYM s])
  fun sxn n = CONVAL ("Sx.N", [NUM n])
  fun sxlist sxs = CONVAL("Sx.L", [embedList sxs])

  fun sexp tokens = (
         sxb <$> booltok
     <|> sxs <$> (notDot <$>! @@ any_name)
     <|> sxn <$> int
     <|> leftCurly <!> "curly brackets may not be used in S-expressions"
     <|> (fn v => sxlist [sxs "quote", v]) <$> (quote *> sexp)
     <|> sxlist <$> bracket ("list of S-expressions", many sexp)
    ) tokens
  val sexp = sexp <?> "S-expression"
in
  val sxstream = interactiveParsedStream (schemeToken, sexp)
end
(* We read S-expressions using a little parser. *)
(* <boxed values 121>=                          *)
val _ = op sxstream : string * line stream * prompts -> value stream
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
(* <boxed values 155>=                          *)
val _ = op xdefstream   : string * line stream     * prompts -> xdef stream
val _ = op filexdefs    : string * TextIO.instream * prompts -> xdef stream
val _ = op stringsxdefs : string * string list               -> xdef stream



(*****************************************************************)
(*                                                               *)
(*   DEFINITION OF [[BASIS]] FOR \UML                            *)
(*                                                               *)
(*****************************************************************)

(* A basis for uML                              *)
(*                                              *)
(* A basis is a quadruple <Gamma, Delta, \tyconset, rho> *)
(* . But \tyconset is represented implicitly, by the *)
(* contents of the mutable reference cell       *)
(* [[nextIdentity]], so the representation of a basis *)
(* contains only the components Gamma, Delta, and rho. *)
(* <definition of [[basis]] for \uml>=          *)
type basis = type_env * (ty * kind) env * value env


(*****************************************************************)
(*                                                               *)
(*   TRANSLATION OF {\UML} TYPE SYNTAX INTO TYPES                *)
(*                                                               *)
(*****************************************************************)

(* <translation of {\uml} type syntax into types>= *)
fun txType (TYNAME t, Delta) =
      (find (t, Delta)
       handle NotFound _ => raise TypeError ("unknown type name " ^ t))
  | txType (TYVARX a, Delta) =
      (find (a, Delta)
       handle NotFound _ => raise TypeError ("type variable " ^ a ^
                                                            " is not in scope"))
(* Constructor application must be well-kinded. *)
(*                                              *)
(*  \tyrule KindApp \twoline \txTypet tau\crossdotsn\ *)
(*  kind\karrow\kind \txTypet_i tau_i \kind_i, 1 <=i *)
(*  <=n \txType\mathttbrackett \cdotsnt (\ldotsntau)  *)
(*  tau \kind                                   *)
(*                                              *)
(* <translation of {\uml} type syntax into types>= *)
  | txType (CONAPPX (tx, txs), Delta) =
      let val (tau,  k)  = txType (tx, Delta)
          val (taus, ks) = ListPair.unzip (map (fn tx => txType (tx, Delta)) txs
                                                                               )
      in  case k
            of ARROW (argks, resultk) =>
                 if eqKinds (ks, argks) then
                   (CONAPP (tau, taus), resultk)
                 else
                   (* Error cases for elaboration of type syntax   *)
                   (*                                              *)
                   (* Error messages for bad type syntax are issued here. *)
                   (* <applied type constructor [[tx]] has the wrong kind>= *)
                   if length argks <> length ks then
                     raise TypeError ("type constructor " ^ typeString tau ^
                                                              " is expecting " ^
                                      Int.toString (length argks) ^ " argument"
                                                                               ^
                                      (case argks of [_] => "" | _ => "s") ^
                                                                  ", but got " ^
                                      Int.toString (length taus))
                   else
                     let fun findBad n (k::ks) (k'::ks') =
                               if eqKind (k, k') then
                                 findBad (n+1) ks ks'
                               else
                                 raise TypeError ("argument " ^ Int.toString n ^
                                                       " to type constructor " ^
                                                  typeString tau ^
                                           " should have kind " ^ kindString k ^
                                                  ", but it has kind " ^
                                                                  kindString k')
                           | findBad _ _ _ = raise InternalError
                                                    "undetected length mismatch"
                     in  findBad 1 argks ks
                     end
             | TYPE =>
                   (* <type [[tau]] is not expecting any arguments>= *)
                   raise TypeError ("type " ^ typeString tau ^
                                         " is not a type constructor, but it " ^
                                    "was applied to " ^ Int.toString (length
                                                         taus) ^ " other type" ^
                                    (case taus of [_] => "" | _ => "s"))
                   (* Implementation of uML type inference         *)
                   (*                                              *)
                   (* [*]                                          *)
                   (*                                              *)
                   (* Type inference                               *)
                   (*                                              *)
                   (* The type of the empty list is \/alpha.listalpha, so *)
                   (* to create a fresh instance, we apply [[listtype]] to *)
                   (* a fresh type variable. Literal pairs are treated as *)
                   (* lists. I hope the code for the remaining literals is *)
                   (* more or less obvious.                        *)

      end
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* --- \mono#2 --- #3 --- #4 --- \mono#5        *)
(* --- #3 --- #4                                *)
(*                                              *)
(*  \toprule Syntax                Concept Semantics *)
(*  \midrule \typerows \bottomrule              *)
(*                                              *)
(* Notational correspondence between type syntax and *)
(* types [*]                                    *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* --- #3 --- \mono#5                           *)
(*                                              *)
(*  \toprule Syntax                Concept Semantics *)
(*  \midrule \typerows \bottomrule              *)
(*                                              *)
(* Representational correspondence between type syntax *)
(* and types [*]                                *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* A function type may be formed only when the argument *)
(* and result types have kind [[TYPE]].         *)
(*                                              *)
(*  \tyrule KindFunction \twoquad \txTypet_i tau_i \ *)
(*  ktype, 1 <=i <=n \txTypet tau\ktype \txType\ *)
(*  mathttbracket\cdotsnt -> t \crossdotsntau-->tau \ *)
(*  ktype                                       *)
(*                                              *)
(* <translation of {\uml} type syntax into types>= *)
  | txType (FUNTYX (txs, tx), Delta) =
      let val tks = map (fn tx => txType (tx, Delta)) txs
          val tk  = txType (tx, Delta)
          fun notAType (ty, k) = not (eqKind (k, TYPE))
          fun thetype  (ty, k) = ty
      in  if notAType tk then
            raise TypeError ("in result position, " ^ typeString (thetype tk) ^
                             " is not a type")
          else
            case List.find notAType tks
              of SOME tk =>
                   raise TypeError ("in argument position, " ^
                                    typeString (thetype tk) ^ " is not a type")
               | NONE => (funtype (map thetype tks, thetype tk), TYPE)
      end
(* A [[forall]] quantifier is impermissible in a *)
(* type—this restriction is what makes the type system a *)
(* Hindley-Milner type system.                  *)
(* <translation of {\uml} type syntax into types>= *)
  | txType (FORALLX _, _) =
      raise TypeError ("'forall' is permissible only at top level")
(* Each clause of [[txType]] implements the translation *)
(* rule that corresponds to its syntax. Translation *)
(* rules (\crefadt.fig.kinding) extend Typed uScheme's *)
(* kinding rules. To start, a type name or type variable *)
(* is looked up in the environment Delta. [*] {mathpar} *)
(* \tyrule KindIntroCon \styconin dom Delta     *)
(* Delta(\stycon) = (tau, \akind) \txType\stycontau\ *)
(* akind                                        *)
(*                                              *)
(* \tyrule KindIntroVar alphain dom Delta       *)
(* Delta(alpha) = (tau, \akind) \txTypealphatau\akind *)
(* {mathpar} \umlflabeltxType                   *)
(* <boxed values 93>=                           *)
val _ = op txType : tyex * (ty * kind) env -> ty * kind
(* <translation of {\uml} type syntax into types>= *)
fun txTyScheme (FORALLX (alphas, tx), Delta) =
      let val Delta'   = extend (Delta, map (fn a => (a, (TYVAR a, TYPE)))
                                                                         alphas)
          val (tau, k) = txType (tx, Delta')
      in  if eqKind (k, TYPE) then
            FORALL (alphas, tau)
          else
            raise TypeError ("in " ^ typeSchemeString (FORALL (alphas, tau)) ^
                             ", type " ^ typeString tau ^ " has kind " ^
                                                                   kindString k)
      end
(* <translation of {\uml} type syntax into types>= *)
  | txTyScheme (tx, Delta) =
      case txType (tx, Delta)
        of (tau, TYPE) => FORALL ([], tau)
         | (tau, k) =>
             raise TypeError ("expected a type, but got type constructor " ^
                              typeString tau ^ " of kind " ^ kindString k)
(* In a type scheme, [[forall]] is permitted. Each type *)
(* variable is given kind \ktype. \usetySchemeKindAll  *)
(* [*]                                          *)
(* <boxed values 94>=                           *)
val _ = op txTyScheme : tyex * (ty * kind) env -> type_scheme
(* The distinctness of \ldotsnalpha is guaranteed by the *)
(* parser, so no check is required here.        *)

(* <translation of {\uml} type syntax into types ((elided))>= *)
local
  val known_vcons = ref [] : (tycon * (vcon * type_scheme) list) list ref
in
  fun vconsOf mu =
    case List.find (fn (mu', _) => eqTycon (mu, mu')) (!known_vcons)
      of SOME (_, vcons) => vcons
       | NONE => raise BugInTypeInference "missing vcons"

  fun addVcons (mu, vcons) =
    known_vcons := (mu, vcons) :: !known_vcons
end


(*****************************************************************)
(*                                                               *)
(*   ELABORATION AND EVALUATION OF [[DATA]] DEFINITIONS          *)
(*                                                               *)
(*****************************************************************)

(* <elaboration and evaluation of [[data]] definitions>= *)
fun evalDataDef ((_, _, typed_vcons), rho) =
  let fun isfuntype (FORALLX (_, tau)) = isfuntype tau
        | isfuntype (FUNTYX _)         = true
        | isfuntype _                  = false
      fun addVcon ((K, t), rho) =
        let val v = if isfuntype t then
                      PRIMITIVE (fn vs => CONVAL (K, vs))
                    else
                      CONVAL (K, [])
        in  bind (K, v, rho)
        end
(* Evaluation of datatype definitions           *)
(*                                              *)
(* Evaluating a datatype definition introduces a *)
(* function or value for every value constructor. *)
(* A value constructor with a function type gets a *)
(* function; \stdbreak a value constructor with a *)
(* non-function type is a value by itself. To tell which *)
(* is which, function [[isfuntype]] looks at the type *)
(* syntax. As with \nml, I don't bother formalizing the *)
(* evaluation. \umlflabelevalDataDef            *)
(* <boxed values 83>=                           *)
val _ = op evalDataDef : data_def * value env -> value env * string list
val _ = op isfuntype   : tyex -> bool
  in  (foldl addVcon rho typed_vcons, map fst typed_vcons)
  end
(* <elaboration and evaluation of [[data]] definitions>= *)
fun elabDataDef ((T, k, vcons), Gamma, Delta) =
  let val mu     = freshTycon T
      val Delta' = bind (T, (TYCON mu, k), Delta)
      fun translateVcon (K, tx) =
            (K, txTyScheme (tx, Delta'))
            handle TypeError msg =>
              raise TypeError ("in type of value constructor " ^ K ^ ", " ^ msg)
      val Ksigmas = map translateVcon vcons
      (* <definition of [[validate]], for types of value constructors>= *)
      fun validate (K, sigma as FORALL (alphas, _), mu, k) =
        let (* Function [[appliesMu]] checks if a type is made by *)
            (* applying type constructor [[mu]].            *)
            (* <definitions of [[appliesMu]] and [[validateTypeArguments]]>= *)
            fun appliesMu (CONAPP (tau, _)) = eqType (tau, TYCON mu) 
              | appliesMu _ = false
            (* Function [[validateTypeArguments]] checks to make *)
            (* sure that the arguments to a constructor application *)
            (* are distinct type variables.                 *)
            (* <definitions of [[appliesMu]] and [[validateTypeArguments]]>= *)
            fun validateTypeArguments (CONAPP (_, taus)) =
                  let fun asTyvar (TYVAR a) = a
                        | asTyvar tau =
                            raise TypeError ("in type of " ^ K ^
                                                           ", type parameter " ^
                                             typeString tau ^ " passed to " ^ T
                                                                               ^
                                             " is not a type variable")
                  in  case duplicatename (map asTyvar taus)
                        of NONE => ()
                         | SOME a => 
                             raise TypeError ("in type of " ^ K ^
                                                   ", type parameters to " ^ T ^
                                              " must be distinct, but " ^ a ^
                                              " is passed to " ^ T ^
                                                              " more than once")
                  end
              | validateTypeArguments (TYCON _) =
                  ()  (* happens only when uML is extended with existentials *)
              | validateTypeArguments _ =
                  let exception ImpossibleTypeArguments in raise
                                                     ImpossibleTypeArguments end
            val desiredType = case k of TYPE => "type " ^ tyconString mu
                                      | ARROW _ => "a type made with " ^
                                                                  tyconString mu
            fun validateLengths (alphas, argkinds) =
              if length alphas <> length argkinds then
                (* When validation fails, much of the code that issues *)
                (* error messages is here.                      *)

         (* <for [[K]], complain that [[alphas]] is inconsistent with [[k]]>= *)
                (case k
                   of TYPE =>
                        raise TypeError ("datatype " ^ T ^
                                              " takes no type parameters, so " ^
                                         "value constructor " ^ K ^
                                                     " must not be polymorphic")
                    | ARROW (kinds, _) => 
                          raise TypeError ("datatype constructor " ^ T ^
                                                                   " expects " ^
                                           intString (length kinds) ^
                                                             " type parameter" ^
                                           (case kinds of [_] => "" | _ => "s")
                                                                               ^
                                           ", but value constructor " ^ K ^
                                           (if null alphas then
                                                           " is not polymorphic"
                                            else " expects " ^ Int.toString (
                                                                length alphas) ^
                                                 " type parameter" ^
                                                 (case alphas of [_] => "" | _
                                                                      => "s"))))
              else
                ()
      (* The type-compatibility judgment can fail in unusually *)
      (* many ways. So my implementation has lots of code for *)
      (* detecting bad outcomes and issuing error messages, *)
      (* and it defines several auxiliary functions:  *)
      (*                                              *)
      (*   • Function [[appliesMu]] says if a type is an *)
      (*  application of type constructor µ.         *)
      (*   • Function [[validateTypeArguments]] ensures that *)
      (*  the arguments in a constructor application are *)
      (*  distinct type variables; it is defined only on *)
      (*  constructor applications.                   *)
      (*   • Function [[validateLengths]] checks that the *)
      (*  number of type variables in a \/ is the same as *)
      (*  the number of type parameters specified by µ's *)
      (*  kind.                                       *)
      (*                                              *)
      (* [*]                                          *)
      (* <boxed values 97>=                           *)
      val _ = op appliesMu             : ty -> bool
      val _ = op validateTypeArguments : ty -> unit
      val _ = op validateLengths       : tyvar list * kind list -> unit
        in  (* The case analysis includes one case per rule. *)
            (* In addition, there is a catchall case that matches *)
            (* when the shape of the type scheme doesn't match the *)
            (* kind of µ.                                  *)

         (* <validation by case analysis on [[schemeShape shape]] and [[k]]>= *)
            case (schemeShape sigma, k)
              of (MONO_VAL tau, TYPE) =>
                   if eqType (tau, TYCON mu) then
                     ()
                   else

               (* <type of [[K]] should be [[desiredType]] but is [[sigma]]>= *)
                     raise TypeError ("value constructor " ^ K ^ " should have "
                                                                 ^ desiredType ^
                                      ", but it has type " ^ typeSchemeString
                                                                          sigma)
               | (MONO_FUN (_, result), TYPE) =>
                   if eqType (result, TYCON mu) then
                     ()
                   else

       (* <result type of [[K]] should be [[desiredType]] but is [[result]]>= *)
                     raise TypeError ("value constructor " ^ K ^
                                               " should return " ^ desiredType ^
                                      ", but it returns type " ^ typeString
                                                                         result)
               | (POLY_VAL (alphas, tau), ARROW (argkinds, _)) => 
                   if appliesMu tau then
                     ( validateLengths (alphas, argkinds)
                     ; validateTypeArguments tau
                     )
                   else

               (* <type of [[K]] should be [[desiredType]] but is [[sigma]]>= *)
                     raise TypeError ("value constructor " ^ K ^ " should have "
                                                                 ^ desiredType ^
                                      ", but it has type " ^ typeSchemeString
                                                                          sigma)
               | (POLY_FUN (alphas, _, result), ARROW (argkinds, _)) => 
                   if appliesMu result then
                     ( validateLengths (alphas, argkinds)
                     ; validateTypeArguments result
                     )
                   else

       (* <result type of [[K]] should be [[desiredType]] but is [[result]]>= *)
                     raise TypeError ("value constructor " ^ K ^
                                               " should return " ^ desiredType ^
                                      ", but it returns type " ^ typeString
                                                                         result)
               | _ =>
                   (* When validation fails, much of the code that issues *)
                   (* error messages is here.                      *)

         (* <for [[K]], complain that [[alphas]] is inconsistent with [[k]]>= *)
                   (case k
                      of TYPE =>
                           raise TypeError ("datatype " ^ T ^
                                              " takes no type parameters, so " ^
                                            "value constructor " ^ K ^
                                                     " must not be polymorphic")
                       | ARROW (kinds, _) => 
                             raise TypeError ("datatype constructor " ^ T ^
                                                                   " expects " ^
                                              intString (length kinds) ^
                                                             " type parameter" ^
                                              (case kinds of [_] => "" | _ =>
                                                                          "s") ^
                                              ", but value constructor " ^ K ^
                                              (if null alphas then
                                                           " is not polymorphic"
                                               else " expects " ^ Int.toString (
                                                                length alphas) ^
                                                    " type parameter" ^
                                                    (case alphas of [_] => "" |
                                                                    _ => "s"))))
        end                   
      val ()     = app (fn (K, sigma) => validate (K, sigma, mu, k)) Ksigmas
      val ()     = addVcons (mu, Ksigmas) (* OMIT *)
      val Gamma' = extendTypeEnv (Gamma, Ksigmas)
  in  (Gamma', Delta', kindString k :: map (typeSchemeString o snd) Ksigmas)
  end
(* Elaborating datatype definitions             *)
(*                                              *)
(* In uML's [[data]] form, the type of every value *)
(* constructor is explicit in the syntax. Writing all *)
(* that syntax may not be much fun, but it makes the *)
(* definition easy to elaborate: bind type name \stycon *)
(* to a fresh type constructor, translate the type of *)
(* each value constructor, check that all the types are *)
(* compatible, and return the extended environments. [*] *)
(* \tyruleData \multiline µ\notin\atyconset\qquad\ *)
(* atyconset' = \atyconset\cup{µ} Delta' = Delta{\stycon *)
(* |->(µ, \akind) } \txtyscheme[Delta'] t_i sigma_i, 1 *)
(* <=i <=n \vconcompatsigma_i µ\akind, 1 <=i <=n Gamma' *)
(* = Gamma{\avcon_1 : sigma_1, ..., \avcon_n : sigma_n } *)
(* \umlelabdef \astData(\stycon:: \akind, \avcon_1 : *)
(* t_1, ..., \avcon_n : t_n) Gamma',Delta',\atyconset' \ *)
(* stdbreak This rule is implemented by function *)
(* [[elabDataDef]]. The function returns Gamma', Delta', *)
(* and a list of strings: the name \stycon followed by *)
(* names [\ldotsnK]. \umlflabelelabDataDef      *)
(* <boxed values 95>=                           *)
val _ = op elabDataDef : data_def * type_env * (ty * kind) env -> type_env * (ty
                                                       * kind) env * string list
(* <elaboration and evaluation of [[data]] definitions>= *)
fun processDataDef (dd, (Gamma, Delta, rho), interactivity) =
  let val (Gamma', Delta', tystrings) = elabDataDef (dd, Gamma, Delta)
      val (rho', vcons)               = evalDataDef (dd, rho)
      val _ = if prints interactivity then
                (* The name of the new type constructor is printed with *)
                (* its kind, and the name of each value constructor is *)
                (* printed with its type.                       *)
                (* <print the new type and each of its value constructors>= *)
                let val (T, _, _) = dd
                    val (mu, _)   = find (T, Delta')
                    val (kind, vcon_types) =
                      case tystrings of s :: ss => (s, ss)
                                      | [] => let exception NoKindString in
                                                          raise NoKindString end
                in  ( println (typeString mu ^ " :: " ^ kind)
                    ; ListPair.appEq (fn (K, tau) => println (K ^ " : " ^ tau))
                                                             (vcons, vcon_types)
                    )
                end
              else
                ()
  in  (Gamma', Delta', rho')
  end
(* To process a data definition, use [[elabDataDef]] and *)
(* [[evalDataDef]].                             *)
(* <boxed values 99>=                           *)
val _ = op processDataDef : data_def * basis * interactivity -> basis


(*****************************************************************)
(*                                                               *)
(*   DEFINITIONS OF [[EMPTYBASIS]], [[PREDEFINEDTYPEBASIS]], [[BOOLTYPE]], [[LISTTYPE]], AND [[UNITTYPE]] *)
(*                                                               *)
(*****************************************************************)

(* <definitions of [[emptyBasis]], [[predefinedTypeBasis]], [[booltype]], [[listtype]], and [[unittype]]>= *)
val emptyBasis = (emptyTypeEnv, emptyEnv, emptyEnv)
fun addTycon ((t, tycon, kind), (Gamma, Delta, rho)) =
  (Gamma, bind (t, (TYCON tycon, kind), Delta), rho)
val primTyconBasis : basis = 
  foldl addTycon emptyBasis (
                           (* Specifications of primitive types and functions *)
                             (*                                              *)

                      (* Like Typed uScheme, uML has both primitive types and *)

                     (* primitive values. Primitive types [[int]] and [[sym]] *)

                       (* are bound into the kinding environment Delta. Other *)

                      (* built-in types are either defined in user code, like *)

                     (* [[list]] and [[bool]], or they don't have names, like *)
                             (* the function type.                           *)

                           (* <primitive type constructors for \uml\ [[::]]>= *)
                             ("int", inttycon, TYPE) :: 
                             ("sym", symtycon, TYPE) :: nil)
(* <definitions of [[emptyBasis]], [[predefinedTypeBasis]], [[booltype]], [[listtype]], and [[unittype]]>= *)
val predefinedTypeBasis =
  let val predefinedTypes = (* <predefined {\uml} types, as strings>=       *)

                             [ "(data (* => *) option"
                             , "  [SOME : (forall ('a) ('a -> (option 'a)))]"
                             , "  [NONE : (forall ('a) (option 'a))])"
                             , "(data * bool"
                             , "  [#t : bool]"
                             , "  [#f : bool])"
                             , "(data (* => *) list"
                             , "  ['()  : (forall ('a) (list 'a))]"
                             ,
                         "  [cons : (forall ('a) ('a (list 'a) -> (list 'a)))])"
                             , "(data * unit [UNIT : unit])"
                             , "(data (* * * => *) triple"
                             ,
             "  [TRIPLE : (forall ('a 'b 'c) ('a 'b 'c -> (triple 'a 'b 'c)))])"
                             , "(implicit-data ('a1 'a2 'a3 'a4) 4-tuple"
                             , "         [T4 of 'a1 'a2 'a3 'a4])"
                             , "(implicit-data ('a1 'a2 'a3 'a4 'a5) 5-tuple"
                             , "         [T5 of 'a1 'a2 'a3 'a4 'a5])"
                             ,
                              "(implicit-data ('a1 'a2 'a3 'a4 'a5 'a6) 6-tuple"
                             , "         [T6 of 'a1 'a2 'a3 'a4 'a5 'a6])"
                             ,
                          "(implicit-data ('a1 'a2 'a3 'a4 'a5 'a6 'a7) 7-tuple"
                             , "         [T7 of 'a1 'a2 'a3 'a4 'a5 'a6 'a7])"
                             ,
                      "(implicit-data ('a1 'a2 'a3 'a4 'a5 'a6 'a7 'a8) 8-tuple"
                             ,
                             "         [T8 of 'a1 'a2 'a3 'a4 'a5 'a6 'a7 'a8])"
                             ,
                  "(implicit-data ('a1 'a2 'a3 'a4 'a5 'a6 'a7 'a8 'a9) 9-tuple"
                             ,
                         "         [T9 of 'a1 'a2 'a3 'a4 'a5 'a6 'a7 'a8 'a9])"
                             ,
            "(implicit-data ('a1 'a2 'a3 'a4 'a5 'a6 'a7 'a8 'a9 'a10) 10-tuple"
                             ,
                    "        [T10 of 'a1 'a2 'a3 'a4 'a5 'a6 'a7 'a8 'a9 'a10])"
                             , "(implicit-data order LESS EQUAL GREATER)"
                             , "(data * sx"
                             , "   [Sx.B : (bool -> sx)]"
                             , "   [Sx.S : (sym  -> sx)]"
                             , "   [Sx.N : (int  -> sx)]"
                             , "   [Sx.L : ((list sx)  -> sx)])"
                              ] 
      val xdefs = stringsxdefs ("built-in types", predefinedTypes)
      fun process (DEF (DATA dd), b) = processDataDef (dd, b, noninteractive)
        | process _ = raise InternalError "predefined definition is not DATA"
  in  streamFold process primTyconBasis xdefs
  end
(* Building the initial basis: predefined types, *)
(* primitives, predefined functions             *)
(*                                              *)
(* Other interpreters build an initial basis by starting *)
(* with an empty basis, adding primitives, and adding *)
(* predefined functions. But the initial basis for the *)
(* uML interpreter has to be built in five stages, not *)
(* three:                                       *)
(*                                              *)
(*  1. Start with an empty basis                *)
(*  2. Add the primitive type constructors [[int]] and *)
(*  [[sym]], producing [[primTyconBasis]]       *)
(*  3. [*] Add the predefined types, producing  *)
(*  [[predefinedTypeBasis]]                     *)
(*                                              *)
(*  (At this point, it is possible to implement type *)
(*  inference, which uses the predefined types  *)
(*  [[list]] and [[bool]] to infer the types of list *)
(*  literals and Boolean literals.)             *)
(*  4. Add the primitives, some of whose types refer to *)
(*  predefined types, producing [[primFunBasis]] *)
(*  5. Add the predefined functions, some of whose *)
(*  bodies refer to primitives, producing       *)
(*  [[initialBasis]]                            *)
(*                                              *)
(* After step [<-], the predefined types [[list]] and *)
(* [[bool]] need to be exposed to the type-inference *)
(* engine, and all the predefined types need to be *)
(* exposed to the implementations of the primitives. *)
(* The basis holding the predefined types is called *)
(* [[predefinedTypeBasis]], and the code for the first *)
(* two steps is implemented here. First, the primitive *)
(* type constructors:                           *)
(* <boxed values 100>=                          *)
val _ = op emptyBasis : basis
(* Next, the predefined types. Internal function *)
(* [[process]] accepts only [[data]] definitions, which *)
(* can be elaborated without type inference. \  *)
(* makenowebnotdef (from \LApredefined uML types \ *)
(* upshape[->]\RA) We add primitive values and user *)
(* code.                                        *)
(* <boxed values 100>=                          *)
val _ = op predefinedTypeBasis : basis
(* The [[predefinedTypeBasis]] is used to define *)
(* [[booltype]], which is used in type inference, which *)
(* is used in [[elabdef]], which is used in     *)
(* [[processDef]]. So when [[predefinedTypeBasis]] is *)
(* defined, [[processDef]] is not yet available. *)
(* I therefore define internal function [[process]], *)
(* which processes only data definitions. Luckily, *)
(* [[elabDataDef]] does not require type inference. *)

(* Internal access to predefined types          *)
(*                                              *)
(* Types [[bool]], [[list]], [[unit]], and so on are *)
(* used not only in the basis, but also inside the *)
(* interpreter: they are used to infer types, to define *)
(* primitive functions, or both. I extract them from *)
(* [[predefinedTypeBasis]]. I also define types *)
(* [[alpha]] and [[beta]], which are used to write the *)
(* types of polymorphic primitives.             *)
(* <definitions of [[emptyBasis]], [[predefinedTypeBasis]], [[booltype]], [[listtype]], and [[unittype]]>= *)
local
  val (_, Delta, _) = predefinedTypeBasis
  fun predefined t = fst (find (t, Delta))
  val listtycon = predefined "list"
in
  val booltype     = predefined "bool"
  fun listtype tau = CONAPP (listtycon, [tau])
  val unittype     = predefined "unit"
  val sxtype       = predefined "sx"
  val alpha = TYVAR "'a"
  val beta  = TYVAR "'b"
end


(*****************************************************************)
(*                                                               *)
(*   TYPE INFERENCE FOR \NML\ AND \UML                           *)
(*                                                               *)
(*****************************************************************)

(* <type inference for \nml\ and \uml>=         *)
(* Constraints and constraint solving           *)
(*                                              *)
(* To highlight the relationship between the code and *)
(* the math, I use a representation that's close to the *)
(* math: [Experienced type-system hackers might prefer a *)
(* list of pairs of types, but a list of pairs is easy *)
(* to work with only if you already understand what's *)
(* going on.] the \eqty operator is [[ ]]; the \land *)
(*  operator is [[/                             *)
(* ]; and the \trivc constraint is [[TRIVIAL]]. \ *)
(* nmllabelcon \nmlflabeltilde,cand,TRIVIAL [*] *)
(* <representation of type constraints>=        *)
datatype con = ~  of ty  * ty
             | /\ of con * con
             | TRIVIAL
infix 4 ~
infix 3 /\
(* (If you have programmed in ML before, you may know *)
(* [[ ]] as the unary minus operator. My code redefines  *)
(* [[ ]], but the original version can still be used by *)
(* its qualified name [[Int. ]].)               *)

(* Utility functions on constraints             *)
(*                                              *)
(* Many of the utility functions defined on types have *)
(* counterparts on constraints. For example, we can find *)
(* free type variables in a constraint, and we can *)
(* substitute for free type variables.\nmlflabel *)
(* freetyvarsConstraint                         *)
(* <utility functions on type constraints>=     *)
fun freetyvarsConstraint (t ~  t') = union (freetyvars t, freetyvars t')
  | freetyvarsConstraint (c /\ c') = union (freetyvarsConstraint c,
                                             freetyvarsConstraint c')
  | freetyvarsConstraint TRIVIAL    = emptyset
(* <utility functions on type constraints>=     *)
fun consubst theta =
  let fun subst (tau1 ~ tau2) = tysubst theta tau1 ~ tysubst theta tau2
        | subst (c1 /\ c2)    = subst c1 /\ subst c2
        | subst TRIVIAL       = TRIVIAL
  in  subst
  end
(* When adding a new binding, I update the set of free *)
(* type variables in Gamma. I take the union of the *)
(* existing free type variables with the free type *)
(* variables of the new type scheme sigma. \nmlflabel *)
(* bindtyscheme                                 *)
(* <boxed values 63>=                           *)
val _ = op bindtyscheme : name * type_scheme * type_env -> type_env
(* Finally, when we want the free type variables, *)
(* we just take them from the pair. \nmlflabel  *)
(* freetyvarsGamma                              *)
(* <boxed values 63>=                           *)
val _ = op freetyvarsGamma : type_env -> name set
(* A substitution is applied to a constraint using the *)
(* following rules: {mathpar} \subsn(tau_1\eqtytau_2) = *)
(* \subsntau_1 \eqty\subsntau_2                 *)
(*                                              *)
(* \subsn(\tyc_1 \land\tyc_2) = \subsn\tyc_1 \land\subsn *)
(* \tyc_2                                       *)
(*                                              *)
(* \subsn\trivc= \trivc {mathpar} The code resembles the *)
(* code for [[tysubst]] in chunk [->]. [*] \nmlflabel *)
(* consubst                                     *)
(* <boxed values 63>=                           *)
val _ = op consubst : subst -> con -> con
(* <utility functions on type constraints>=     *)
fun conjoinConstraints []      = TRIVIAL
  | conjoinConstraints [c]     = c
  | conjoinConstraints (c::cs) = c /\ conjoinConstraints cs
(* I implement the \bigwedge{ ...} operator using the *)
(* ML function [[conjoinConstraints]]. Because I want to *)
(* preserve the number and order of sub-constraints, *)
(* I avoid using [[foldl]] or [[foldr]]. \nmlflabel *)
(* conjoinConstraints                           *)
(* <boxed values 64>=                           *)
val _ = op conjoinConstraints : con list -> con
(* <utility functions on type constraints>=     *)
(* A constraint can be printed in full, but it's easier *)
(* to read if its first passed to [[untriviate]], which *)
(* removes as many [[TRIVIAL]] sub-constraints as *)
(* possible.                                    *)
(* <definitions of [[constraintString]] and [[untriviate]]>= *)
fun constraintString (c /\ c') = constraintString c ^ " /\\ " ^ constraintString
                                                                              c'
  | constraintString (t ~  t') = typeString t ^ " ~ " ^ typeString t'
  | constraintString TRIVIAL = "TRIVIAL"

fun untriviate (c /\ c') = (case (untriviate c, untriviate c')
                              of (TRIVIAL, c) => c
                               | (c, TRIVIAL) => c
                               | (c, c') => c /\ c')
  | untriviate atomic = atomic
(* Two more utility functions are defined in \cref *)
(* app:ml. Function [[constraintString]] can be used to *)
(* print constraints, and function [[untriviate]] *)
(* removes trivial conjuncts from a constraint. *)
(* <boxed values 65>=                           *)
val _ = op constraintString : con -> string
val _ = op untriviate       : con -> con
(* <utility functions on type constraints>=     *)
fun isSolved TRIVIAL = true
  | isSolved (tau ~ tau') = eqType (tau, tau')
  | isSolved (c /\ c') = isSolved c andalso isSolved c'
fun solves (theta, c) = isSolved (consubst theta c)
(* For debugging, it can be useful to see if a  *)
(* substitution solves a constraint. \nmlflabelisSolved *)
(* <boxed values 67>=                           *)
val _ = op isSolved : con -> bool
val _ = op solves : subst * con -> bool
(* Constraint solving                           *)
(*                                              *)
(* Our type-inference algorithm is built on a constraint *)
(* solver, which given a constraint \tyc produces a *)
(* substitution \subsn such that \subsn\tyc is  *)
(* satisfied. But if the algorithm is given an ill-typed *)
(* program, it produces an unsolvable constraint: one *)
(* for which no such \subsn exists. Examples of *)
(* unsolvable constraints include int\eqtybool and list *)
(* alpha\eqtyalpha. When we discover an unsolvable *)
(* constraint, we want to issue a readable error *)
(* message, which shouldn't be full of machine-generated *)
(* fresh type variables. To do so, take the pair of *)
(* types that can't be made equal, and put the pair into *)
(* canonical form. Function [[unsatisfiableEquality]] *)
(* does just that. [*]                          *)
(* <constraint solving>=                        *)
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
(* The implementation of [[unsatisfiableEquality]] is *)
(* more than a little weird. To make a single type out *)
(* of tau_1 and tau_2, so their variables can be *)
(* canonicalized together, I make the type tau_1 -->tau *)
(* _2. What's weird is that there's no real function *)
(* here—it's just a device to make one type out of two. *)
(* As soon as I get the canonical version, I immediately *)
(* take it apart, getting back canonical types [[t1']] *)
(*  and [[t2']].                                *)

(* <constraint solving ((prototype))>=          *)
fun solve c = raise LeftAsExercise "solve"
(* I'm hoping you will implement the solver. \nmlflabel *)
(* solve                                        *)
(* <boxed values 66>=                           *)
val _ = op solve : con -> subst
(* At the end of the iteration, if the set of values not *)
(* yet matched is nonempty, the pattern match is not *)
(* exhaustive. There's only one snag: for interesting *)
(* types, the sets are infinite. To represent such sets, *)
(* I define a ``simple value set'' to be one of two *)
(* choices:                                     *)
(*                                              *)
(*   • All values of type tau, for a given tau *)
(*   • A given value constructor K, applied to a list of *)
(*  zero or more simple value sets              *)
(*                                              *)
(* A full set of values is a collection of simple sets, *)
(* using the collections defined in \crefpage   *)
(* mlinterps.collection.                        *)
(* <exhaustiveness analysis for {\uml}>=        *)
datatype simple_vset = ALL of ty
                     | ONE of vcon * simple_vset list
type vset = simple_vset collection
(* <exhaustiveness analysis for {\uml} ((elided))>= *)
fun vsetString (ALL tau) = "_" (* (ALL : " ^ typeString tau ^ ")" *)
  | vsetString (ONE (K, [])) = K
  | vsetString (ONE (K, vsets)) = "(" ^ K ^ " " ^ spaceSep (map vsetString vsets
                                                                         ) ^ ")"
(* <exhaustiveness analysis for {\uml} ((elided))>= *)
fun hasAll (ALL _) = true
  | hasAll (ONE (_, vs)) = List.exists hasAll vs
(* <exhaustiveness analysis for {\uml} ((prototype))>= *)
fun classifyVset   p vs = joinC (mapC (classifySimple p) vs)
and classifySimple p vs = raise LeftAsExercise "pattern-matching classification"
(* Classification takes a single value set as input, but *)
(* it always produces a collection as output. Here's how *)
(* you should use a pattern p to classify a simple value *)
(* set \vset:                                   *)
(*                                              *)
(*   • If the pattern is a wildcard or a variable, it *)
(*  always matches, and the result is a singleton *)
(*  collection containing \monobox(true, vs).   *)

(* <exhaustiveness analysis for {\uml}>=        *)
fun unroll tau =
  let val mu = case tau of TYCON mu => mu
                         | CONAPP (TYCON mu, _) => mu
                         | _ => raise BugInTypeInference "not ADT"
      fun ofVcon (name, sigma) =
        let val tau'   = freshInstance sigma
            val argTys = case asFuntype tau'
                           of SOME (args, res) =>
                                let val theta = solve (res ~ tau)
                                in  map (tysubst theta) args
                                end
                            | NONE => []
        in  ONE (name, map ALL argTys) : simple_vset
        end
  in  C (map ofVcon (vconsOf mu))
  end
(* Function call \monoboxvconsOf mu returns the name and *)
(* type scheme of each value constructor associated *)
(* with [[mu]]. Function [[vconsOf]] is omitted from *)
(* this book.                                   *)

(* <exhaustiveness analysis for {\uml} ((prototype))>= *)
fun exhaustivenessCheck (ps, tau) =
  eprintln "(Case expression not checked for exhaustiveness.)"
(* <exhaustiveness analysis for {\uml}>=        *)
(* filled in when implementing uML *)
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

(* <definitions of [[typeof]] and [[elabdef]] for \nml\ and \uml>= *)
fun typeof (e, Gamma) =
  let (* Calling typesof(<\ldotsne>, Gamma) returns (<\ldotsn *)
      (* tau>, \tyc) such that \typeisc\tyce_i tau_i for *)
      (* every i with 1<=i <=n. The base case is trivial; the *)
      (* induction step uses this rule from Section [->]:[*] \ *)
      (* usetyTypesOfC                                *)

(* <shared definition of [[typesof]], to infer the types of a list of expressions>= *)
      fun typesof ([],    Gamma) = ([], TRIVIAL)
        | typesof (e::es, Gamma) =
            let val (tau,  c)  = typeof  (e,  Gamma)
                val (taus, c') = typesof (es, Gamma)
            in  (tau :: taus, c /\ c')
            end
      (* Computing the type of a literal value is left as part *)
      (* of Exercise [->].                            *)

(* <function [[literal]], to infer the type of a literal constant ((prototype))>= *)
      fun literal _ = raise LeftAsExercise "literal"
      (* To infer the type of a literal value, we call *)
      (* [[literal]]. To infer the type of a variable, we use *)
      (* fresh type variables to create a most general *)
      (* instance of the variable's type scheme in Gamma. *)
      (* No constraint is needed.                     *)

(* <function [[ty]], to infer the type of a \nml\ expression, given [[Gamma]]>= *)
      fun ty (LITERAL n) = literal n
        | ty (VAR x) = (freshInstance (findtyscheme (x, Gamma)), TRIVIAL)
        (* Section [->] shows how to rewrite a type rule to *)
        (* introduce explicit substitutions; here is the rule *)
        (* for application: \usetyApplyC To implement this rule, *)
        (* we let [[funty]] stand for \tau, [[actualtypes]] *)
        (* stand for \ldotsntau, and [[rettype]] stand for alpha *)
        (* . The first premise is implemented by a call to *)
        (* [[typesof]] and the second by a call to      *)
        (* [[freshtyvar]]. The constraint is represented just as *)
        (* written in the rule.                         *)
        (* <more alternatives for [[ty]]>=              *)
        | ty (APPLY (f, actuals)) = 
             (case typesof (f :: actuals, Gamma)
                of ([], _) => let exception ThisCan'tHappen in raise
                                                             ThisCan'tHappen end
                 | (funty :: actualtypes, c) =>
                      let val rettype = freshtyvar ()
                      in  (rettype, c /\ (funty ~ funtype (actualtypes, rettype)
                                                                              ))
                      end)
        (* The remaining cases for [[ty]] are left as exercises, *)
        (* except we provide syntactic sugar for [[LETSTAR]]. *)
        (* <more alternatives for [[ty]]>=              *)
        | ty (LETX (LETSTAR, [], body)) = ty body
        | ty (LETX (LETSTAR, (b :: bs), body)) = 
            ty (LETX (LET, [b], LETX (LETSTAR, bs, body)))
        (* <more alternatives for [[ty]] ((prototype))>= *)
        | ty (IFX (e1, e2, e3))        = raise LeftAsExercise "type for IFX"
        | ty (BEGIN es)                = raise LeftAsExercise "type for BEGIN"
        | ty (LAMBDA (formals, body))  = raise LeftAsExercise "type for LAMBDA"
        | ty (LETX (LET, bs, body))    = raise LeftAsExercise "type for LET"
        | ty (LETX (LETREC, bs, body)) = raise LeftAsExercise "type for LETREC"
        (* <more alternatives for [[ty]]>=              *)
        | ty (CASE (e, choices)) = 
            let (* We get the type of a value constructor in the same *)
                (* way as we get the type of a variable: instantiate its *)
                (* type scheme with fresh type variables.       *)
                (* <definition of function [[pvconType]]>=      *)
                fun pvconType (K, Gamma) =
                  freshInstance (findtyscheme (K, Gamma))
                  handle NotFound x => raise TypeError (
                                              "no value constructor named " ^ x)
                (* <definition of function [[pattype]]>=        *)
                fun pattype (WILDCARD, _) =
                      (emptyEnv, freshtyvar(), TRIVIAL)
                (* A variable also has a fresh type, produces a type *)
                (* environment which binds that type to itself, and *)
                (* introduces no constraints. \usetyPatVar As usual, *)
                (* a type environment binds a name to a type scheme, not *)
                (* a type, and the binding \mathbox{x |->alpha} is *)
                (* shorthand for \mathbox{x |->\/.alpha}.       *)
                (* <definition of function [[pattype]]>=        *)
                  | pattype (PVAR x, _) =
                      let val alpha = freshtyvar ()
                (* I begin [[pattype]] with the easiest cases.  *)
                (* A wildcard has a fresh type, produces an empty type *)
                (* environment, and introduces no constraints. \usety *)
                (* PatWildcard \umlflabelpattype                *)
                (* <boxed values 87>=                           *)
                      in  (bind (x, FORALL ([], alpha), emptyEnv), alpha,
                                                                        TRIVIAL)
                      end
                (* A bare value constructor has the type and constraint *)
                (* that come from inferring its type. It produces an *)
                (* empty type environment. \usetyPatBareVcon    *)
                (* <definition of function [[pattype]]>=        *)
                  | pattype (CONPAT (K, []), Gamma) =
                      (emptyEnv, pvconType (K, Gamma), TRIVIAL)
                (* <definition of function [[pattype]]>=        *)
                  | pattype(p as CONPAT (vcon, pats), Gamma) = 
                      let val vcon_tau = pvconType (vcon, Gamma)
                          val (Gamma'_is, tau_is, c_is) = pattypes (pats, Gamma)
                          val alpha  = freshtyvar ()
                          val c      = vcon_tau ~ funtype (tau_is, alpha)
                          val c'     = conjoinConstraints c_is
                          val Gamma' = disjointUnion Gamma'_is
                                       handle DisjointUnionFailed x =>
                                         raise TypeError ("name " ^ x ^
                                                         " is bound multiple " ^
                                                          "times in pattern " ^
                                                                    patString p)
                      in  (Gamma', alpha, c /\ c')
                      end
                (* <definition of function [[pattype]]>=        *)
                and pattypes (ps, Gamma) = unzip3 (map (fn p => pattype (p,
                                                                     Gamma)) ps)
                (* \typesystemuml-constraints The big case involves a *)
                (* value constructor applied to nested sub-patterns. \ *)
                (* usetyPatVcon The code discharges one premise after *)
                (* another until it has everything needed in the result. *)
                (* Typechecking of sub-patterns, in the second premise *)
                (* of the rule, is delegated to [[pattypes]].   *)
                (* <boxed values 88>=                           *)
                val _ = op pattypes : pat list * type_env -> type_scheme env
                                                       list * ty list * con list
                (* I implement [[pattypes]] using [[map]] and   *)
                (* [[pattype]]. Function [[unzip3]] is defined in \cref *)
                (* mlinterps.chap.                              *)
                (* <boxed values 88>=                           *)
                val _ = op pattypes : pat list * type_env -> type_scheme env
                                                       list * ty list * con list
                (* <definition of function [[choicetype]]>=     *)
                fun choicetype ((p, e), Gamma) =
                      let val (Gamma', tau, c) = pattype (p, Gamma)
                          val (tau', c') = typeof (e, extendTypeEnv (Gamma,
                                                                        Gamma'))
                          val (ty, con) = (funtype ([tau], tau'), c /\ c')
                          val _ =
                              (* Finally, vanilla uML, which doesn't support  *)

                      (* existential types for value constructors, implements *)

                              (* the escaping-skolem check by doing nothing.  *)

(* <check [[p]], [[e]], [[Gamma']], [[Gamma]], [[ty]], and [[con]] for escaping skolem types>= *)
                                  ()          
                (* The rule for a choice infers a type for the pattern *)
                (* and the expression: \usetyChoice The combination *)
                (* Gamma+Gamma' is implemented by function      *)
                (* [[extendTypeEnv]]. \umlflabelchoicetype \umlflabel *)
                (* disjointUnion                                *)
                (* <boxed values 85>=                           *)
                val _ = op choicetype : (pat * exp) * type_env -> ty * con
                      in  (ty, con)
                      end
                val (tau, c_e) = typeof (e, Gamma)
                val (tau_i's, c_i's) =
                      ListPair.unzip (map (fn choice => choicetype (choice,
                                                                Gamma)) choices)
                val alpha = freshtyvar ()
                fun constrainArrow tau_i = tau_i ~ funtype ([tau], alpha)
                val c' = conjoinConstraints (map constrainArrow tau_i's)
                val c = c_e /\ c' /\ conjoinConstraints c_i's
                val () = exhaustivenessCheck (map fst choices, tysubst (solve c)
                                                                 tau) (* OMIT *)
        (* Type-inference code for case expressions, choices, *)
        (* and patterns                                 *)
        (*                                              *)
        (* \typesystemuml-constraints                   *)
        (*                                              *)
        (* The constraint-based typing rule for case expressions *)
        (* is as follows: \usetyCase To implement the judgment *)
        (* form \typeisc\tyc_i \choicep_i e_i tau_i, I define *)
        (* function [[choicetype]] below. Although tau_i always *)
        (* has the form of an arrow type, I choose to represent *)
        (* it as an ordinary type, then use internal function *)
        (* [[constrainArrow]] to ensure that the argument and *)
        (* result of each arrow are as they should be. \ *)
        (* umlflabelty [*]                              *)
        (* <boxed values 84>=                           *)
        val _ = op ty         : exp                    -> ty * con
        val _ = op typeof     : exp         * type_env -> ty * con
        val _ = op choicetype : (pat * exp) * type_env -> ty * con
            in  (alpha, c)
            end
        (* Function [[ListPair.unzip]] converts a list of pairs *)
        (* to a pair of lists.                          *)

        (* The implementations of [[pattype]] and [[pattypes]] *)
        (* complete type inference for case expressions. And *)
        (* inference code for the other expressions, except for *)
        (* value constructors, is shared with \nml. Type *)
        (* inference for a value constructor instantiates its *)
        (* type scheme with fresh variables: [*]        *)
        (* <more alternatives for [[ty]]>=              *)
        | ty (VCONX vcon) =
            let val tau =
                  freshInstance (findtyscheme (vcon, Gamma))
                  handle NotFound _ => raise TypeError (
                                           "no value constructor named " ^ vcon)
            in  (tau, TRIVIAL)
            end
(* <boxed values 69>=                           *)
val _ = op typeof  : exp      * type_env -> ty      * con
val _ = op typesof : exp list * type_env -> ty list * con
val _ = op literal : value -> ty * con
val _ = op ty      : exp   -> ty * con
  in  ty e
  end
(* <definitions of [[typeof]] and [[elabdef]] for \nml\ and \uml>= *)
fun elabdef (d, Gamma) =
  case d
    of VAL    (x, e)      =>
                        (* The cases for [[VAL]] and [[VALREC]] resemble each *)

                       (* other. We begin with [[VAL]], which computes a type *)
                             (* and generalizes it. \usetyValC               *)

                     (* <infer and bind type for [[VAL    (x, e)]] for \nml>= *)
                             let val (tau, c) = typeof (e, Gamma)
                                 val theta    = solve c
                                 val sigma    = generalize (tysubst theta tau,
                                                          freetyvarsGamma Gamma)
                             in  (bindtyscheme (x, sigma, Gamma),
                                                         typeSchemeString sigma)
                             end

                     (* This code takes a big shortcut: we just assume that \ *)

                           (* subsnGamma=Gamma. How can we get away with this *)

                        (* assumption? Because we can prove that a top-level  *)

                           (* Gamma never contains a free type variable. This *)

                      (* property guarantees that \subsnGamma=Gamma for any \ *)

                      (* subsn. You can prove this property for yourself in \ *)
                             (* exrefpageml.ex.no-free-tyvars-at-top-level.  *)

     | VALREC (x, e)      =>
                        (* The code for [[VALREC]] is a bit more complicated. *)

                        (* We need an environment that binds x to tau, but we *)

                          (* don't yet know tau. The original rule looks like *)

                     (* this: \usetyValRec Here's a version with constraints: *)

                         (* \tyrule[ValRecC]ValRec \upshapewith constraints \ *)

                     (* threeline \typeisc[{x |->alpha}] \tyce tau\qquadalpha *)

                       (* is fresh \twoquad\trivsat\subsn(\tyc\landalpha\eqty *)

                      (* tau) \subsnGamma=Gamma sigma= \generalize\subsnalpha *)

                         (* \ftv(Gamma) \nmltopt\xvalrec(x, e) -->Gamma{x |-> *)
                             (* sigma}                                       *)
                             (*                                              *)

                     (* As usual, we introduce a fresh type variable to stand *)

                        (* for tau, then constrain it to be equal to the type *)
                             (* of e.                                        *)

                     (* <infer and bind type for [[VALREC (x, e)]] for \nml>= *)
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
     | EXP e              => elabdef (VAL ("it", e), Gamma)
     | DEFINE (x, lambda) => elabdef (VALREC (x, LAMBDA lambda), Gamma)
     (* Support for datatype definitions             *)
     (*                                              *)
     (* Cases for elaboration and evaluation of definitions *)
     (*                                              *)
     (* In uML, the [[DATA]] definition is handled by *)
     (* function [[processDef]] (\chunkref           *)
     (* adt.chunk.processDef). Functions [[elabdef]] and *)
     (* [[evaldef]] are reused from \nml, with these extra *)
     (* cases which should never be executed.        *)
     (* <extra case for [[elabdef]] used only in \uml>= *)
     | DATA _ => raise InternalError "DATA reached elabdef"
     (* Cases and code for Chapter \adtchapnum       *)
     (*                                              *)
     (* uML (\crefadt.chap) is built on \nml, with additional *)
     (* cases for pattern matching and algebraic data types. *)
     (* The following code chunks are placeholders for code *)
     (* that is added in \crefadt.chap.              *)
     (* <extra case for [[elabdef]] used only in \uml>= *)
     (* filled in when implementing uML *)
(* Elaboration and type inference for definitions *)
(*                                              *)
(* Given a definition, we extend the top-level type *)
(* environment. We infer the type of the thing defined, *)
(* generalize the type to a type scheme, and add a *)
(* binding to the environment. This step is called *)
(* elaboration. To report to a user, we also return a *)
(* string suitable for printing. \nmlflabelelabdef *)
(* <boxed values 70>=                           *)
val _ = op elabdef : def * type_env -> type_env * string
(* Forms [[EXP]] and [[DEFINE]] are syntactic sugar. *)

(* Type inference                               *)
(*                                              *)
(* Type inference builds on constraint solving. With *)
(* constraints in place, all we need is [[typeof]], *)
(* which implements the typing rules for expressions, *)
(* and [[elabdef]], which implements the rules for *)
(* definitions.                                 *)
(* <boxed values 68>=                           *)
val _ = op typeof  : exp * type_env -> ty * con
val _ = op elabdef : def * type_env -> type_env * string



(*****************************************************************)
(*                                                               *)
(*   EVALUATION, TESTING, AND THE READ-EVAL-PRINT LOOP FOR \UML  *)
(*                                                               *)
(*****************************************************************)

(* <evaluation, testing, and the read-eval-print loop for \uml>= *)
(* <definition of [[namedValueString]] for functional bridge languages>= *)
fun namedValueString x v =
  case v of CLOSURE _ => x
          | PRIMITIVE _ => x
          | _ => valueString v
(* The string returned by [[evaldef]] is the value, *)
(* unless the value is a named procedure, in which case *)
(* it is the name.                              *)
(* <boxed values 45>=                           *)
val _ = op namedValueString : name -> value -> string
(* <definitions of [[match]] and [[Doesn'tMatch]]>= *)
exception Doesn'tMatch    (* pattern-match failure *)
fun match (CONPAT (k, ps), CONVAL (k', vs)) =
     if k = k' then
       disjointUnion (ListPair.mapEq match (ps, vs))
     else
       raise Doesn'tMatch
  | match (CONPAT _, _) = raise Doesn'tMatch
  | match (WILDCARD, _) = emptyEnv
  | match (PVAR x,   v) = bind (x, v, emptyEnv)
(* The rules are implemented by function [[match]], *)
(* which implements judgment \mathbox\patmatchesp v rho' *)
(* by returning rho'. And it implements judgment \ *)
(* patfailsp v by raising the ML exception      *)
(* [[Doesn'tMatch]]. \umlflabelmatch [*] [*]    *)
(* <boxed values 81>=                           *)
val _ = op match : pat * value -> value env (* or raises Doesn'tMatch *)
(* If patterns [[ps]] and values [[vs]] were lists of *)
(* different lengths, function [[ListPair.mapEq]] would *)
(* raise an exception, but the type system ensures that *)
(* this can't happen.                           *)

(* <definitions of [[eval]] and [[evaldef]] for \nml\ and \uml>= *)
fun eval (e, rho) =
  let val go = applyCheckingOverflow id in go end (* OMIT *)
  let fun ev (LITERAL v)        = v
        | ev (VAR x)            = find (x, rho)
        | ev (IFX (e1, e2, e3)) = ev (if bool (ev e1) then e2 else e3)
        | ev (LAMBDA l)         = CLOSURE (l, fn _ => rho)
        | ev (BEGIN es) =
            let fun b (e::es, lastval) = b (es, ev e)
                  | b (   [], lastval) = lastval
            in  b (es, embedBool false)
            end
        | ev (APPLY (f, args)) = 
           (case ev f
              of PRIMITIVE prim => prim (map ev args)
               | CLOSURE clo =>
                             (* To apply a closure, we bind formal parameters *)

                       (* directly to the values of actual parameters, not to *)

                              (* mutable cells.                               *)

                              (* <apply closure [[clo]] to [[args]] ((ml))>=  *)
                                let val ((formals, body), mkRho) = clo
                                    val actuals = map ev args
                                in  eval (body, bindList (formals, actuals,
                                                                      mkRho ()))
                                    handle BindListLength => 
                                        raise BugInTypeInference
                                          "Wrong number of arguments to closure"
                                end
               | _ => raise BugInTypeInference "Applied non-function"
               )
        (* \xlet evaluates all right-hand sides in rho, then *)
        (* extends rho to evaluate the body.            *)
        (* <more alternatives for [[ev]] for \nml\ and \uml>= *)
        | ev (LETX (LET, bs, body)) =
            let val (names, values) = ListPair.unzip bs
            in  eval (body, bindList (names, map ev values, rho))
            end
        (* \xletstar evaluates pairs in sequence, adding a *)
        (* binding to rho after each evaluation.        *)
        (* <more alternatives for [[ev]] for \nml\ and \uml>= *)
        | ev (LETX (LETSTAR, bs, body)) =
            let fun step ((x, e), rho) = bind (x, eval (e, rho), rho)
            in  eval (body, foldl step rho bs)
            end
        (* \xletrec is the most interesting case. Function *)
        (* [[makeRho']] builds an environment in which each *)
        (* right-hand side stands for a closure. Each closure's *)
        (* captured environment is the one built by     *)
        (* [[makeRho']]. The recursion is OK because the *)
        (* environment is built lazily, so [[makeRho']] always *)
        (* terminates. The right-hand sides must be lambda *)
        (* abstractions. [*]                            *)
        (* <more alternatives for [[ev]] for \nml\ and \uml>= *)
        | ev (LETX (LETREC, bs, body)) =
            let fun makeRho' () =
                  let fun step ((x, e), rho) =
                            (case e
                               of LAMBDA l => bind (x, CLOSURE (l, makeRho'),
                                                                            rho)
                                | _ => raise RuntimeError "non-lambda in letrec"
                                                                               )
                  in  foldl step rho bs
                  end
            in  eval (body, makeRho'())
            end
        (* uML also has special syntax for a value-constructor *)
        (* expression, but it isn't interesting: like a value *)
        (* variable, a value constructor is evaluated by looking *)
        (* it up in the environment:                    *)
        (* <more alternatives for [[ev]] for \nml\ and \uml>= *)
        | ev (VCONX vcon) = find (vcon, rho)
        (* <more alternatives for [[ev]] for \nml\ and \uml>= *)
        | ev (CASE (LITERAL v, (p, e) :: choices)) =
            (let val rho' = match (p, v)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
        (* \newskip\myskip \myskip=6pt                  *)
        (*                                              *)
        (*      \toprule     Concept     Interpreter    *)
        (*     Semantics                                *)
        (*     \midrule e    Expression  \umltypeexp    *)
        (*         x         Variable    \mlstypename   *)
        (*       \avcon      Value       \umltypevcon   *)
        (*                   constructor                *)
        (*         p         Pattern     \umltypepat    *)
        (*         v         Value       \umltypevalue  *)
        (*  [\myskip] rho+   Extension   \umlfunextend  *)
        (*        rho'                                  *)
        (*  rho_1 \uplusrho  Disjoint    \umlfundisjointUnion *)
        (*         _2        union                      *)
        (*    [\myskip] \    Pattern     match(p, v) = rho' \ *)
        (*   patmatchesp v   matches     umlfunpagematch *)
        (*        rho'                                  *)
        (*    \patfailsp v   Pattern     match(p, v) raises *)
        (*                   match fails [[Doesn'tMatch]] *)
        (*    \bottomrule                               *)
        (*                                              *)
        (* [*]                                          *)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
        (*                                              *)
        (* In the code, rules for \xcase(\xliteral(v), cs) must *)
        (* precede the rule for \xcase(e, cs). And function *)
        (* [[match]] implements the matching judgment:  *)
        (* on success, it returns rho', and on failure, *)
        (* it raises the exception [[Doesn'tMatch]]. \umlflabel *)
        (* ev                                           *)
        (* <boxed values 79>=                           *)
        val _ = op match  : pat * value -> value env
        val _ = op extend : 'a env * 'a env -> 'a env
             in  eval (e, extend (rho, rho'))
             end
             handle Doesn'tMatch => ev (CASE (LITERAL v, choices)))
        | ev (CASE (LITERAL v, [])) =
            raise RuntimeError ("'case' does not match " ^ valueString v)
        | ev (CASE (e, choices)) =
            ev (CASE (LITERAL (ev e), choices))
  in  ev e
  end
(* Evaluation of expressions                    *)
(*                                              *)
(* Because the abstract syntax of \nml is a subset of *)
(* micro-Scheme, the evaluator is almost a subset of the *)
(* micro-Scheme evaluator. One difference is that *)
(* because \nml doesn't have mutation, environments map *)
(* names to values, instead of mapping them to mutable *)
(* cells. Another is that type inference should *)
(* eliminate most potential errors. If one of those *)
(* errors occurs anyway, we raise the exception *)
(* [[BugInTypeInference]]. \nmlflabeleval       *)
(* <boxed values 71>=                           *)
val _ = op eval : exp * value env -> value
(* <definitions of [[eval]] and [[evaldef]] for \nml\ and \uml>= *)
fun evaldef (VAL (x, e), rho) =
      let val v   = eval (e, rho)
          val rho = bind (x, v, rho)
      in  (rho, namedValueString x v)
      end
  | evaldef (VALREC (f, LAMBDA lambda), rho) =
      let fun makeRho' () = bind (f, CLOSURE (lambda, makeRho'), rho)
          val v           = CLOSURE (lambda, makeRho')
      in  (makeRho'(), f)
      end
  | evaldef (VALREC _, rho) =
      raise RuntimeError "expression in val-rec must be lambda"
  | evaldef (EXP e, rho) = 
      let val v   = eval (e, rho)
          val rho = bind ("it", v, rho)
      in  (rho, valueString v)
      end
(* <definitions of [[eval]] and [[evaldef]] for \nml\ and \uml>= *)
  | evaldef (DEFINE (f, lambda), rho) =
      evaldef (VALREC (f, LAMBDA lambda), rho)
  (* uML, which is the subject of \crefadt.chap, is like \ *)
  (* nml but with one additional definition form, for *)
  (* defining an algebraic data type. \Nml lacks that *)
  (* form, so the corresponding clause in [[evaldef]] is *)
  (* empty.                                       *)
  (* <clause for [[evaldef]] for datatype definition (\uml\ only)>= *)
  (* code goes here in Chapter 11 *)
  (* <clause for [[evaldef]] for datatype definition (\uml\ only)>= *)
  | evaldef (DATA _, _) = raise InternalError "DATA reached evaldef"
(* Evaluating definitions                       *)
(*                                              *)
(* Evaluating a definition can produce a new    *)
(* environment. Function [[evaldef]] also returns a *)
(* string that gives the name or value being defined.  *)
(* [*] \nmlflabelevaldef                        *)
(* <boxed values 72>=                           *)
val _ = op evaldef : def * value env -> value env * string
(* <definition of [[processDef]] for \uml>=     *)
fun processDef (DATA dd, basis, interactivity) =
      processDataDef (dd, basis, interactivity)
  | processDef (d, (Gamma, Delta, rho), interactivity) =
      let val (Gamma', tystring)  = elabdef (d, Gamma)
          val (rho',   valstring) = evaldef (d, rho)
          val _ =
            if prints interactivity then
              println (valstring ^ " : " ^ tystring)
            else
              ()
(* Processing definitions                       *)
(*                                              *)
(* As in other interpreters for statically typed *)
(* languages, [[processDef]] first elaborates a *)
(* definition, then evaluates it. A data definition is *)
(* handled by function [[processDataDef]] below. All *)
(* other definitions are handled by the versions of *)
(* [[elabdef]] and [[evaldef]] defined for \nml in \cref *)
(* ml.chap. In the formal type system, we delegate to *)
(* [[elabdef]] using this rule: \tyrule ReuseDefinition *)
(* \nmlelabdefd Gamma' \umlelabdefd Gamma',Delta,\ *)
(* atyconset                                    *)
(*                                              *)
(* [*]                                          *)
(* <boxed values 98>=                           *)
val _ = op processDef : def * basis * interactivity -> basis
      in  (Gamma', Delta, rho')
      end
fun dump_names (_, _, values) = app (println o fst) values  (*OMIT*)
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

       (* The read-eval-print loop                     *)
       (*                                              *)
       (* The read-eval-print loop is almost identical to the *)
       (* read-eval-print loop for Typed uScheme; the only *)
       (* difference is that instead of a handler for  *)
       (* [[BugInTypeChecking]], we have a handler for *)
       (* [[BugInTypeInference]].                      *)

(* <other handlers that catch non-fatal exceptions and pass messages to [[caught]] ((type-inference))>= *)
       | TypeError          msg => caught ("type error <at loc>: " ^ msg)
       | BugInTypeInference msg => caught ("bug in type inference: " ^ msg)
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
(* Unit testing                                 *)
(*                                              *)
(* Unit testing is as in \nml, except that types in the *)
(* syntax have to be translated.                *)
(* <definition of [[testIsGood]] for \uml>=     *)
(* <definition of [[skolemTypes]] for languages with generated type constructors>= *)
val skolemTypes =
  streamOfEffects (fn () => SOME (TYCON (freshTycon "skolem type")))
(* <shared definitions of [[typeSchemeIsAscribable]] and [[typeSchemeIsEquivalent]]>= *)
fun asGeneralAs (sigma_g, sigma_i as FORALL (a's, tau)) =
  let val theta = bindList (a's, streamTake (length a's, skolemTypes), emptyEnv)
                                                                                
      val skolemized = tysubst theta tau
      val tau_g = freshInstance sigma_g
  in  (solve (tau_g ~ skolemized); true) handle _ => false
  end
(* Two type schemes are equivalent if each is as general *)
(* as the other. (Notice that equivalent type schemes *)
(* have the same instances.)                    *)
(* <shared definitions of [[typeSchemeIsAscribable]] and [[typeSchemeIsEquivalent]]>= *)
fun eqTypeScheme (sigma1, sigma2) =
  asGeneralAs (sigma1, sigma2) andalso asGeneralAs (sigma2, sigma1)
(* <boxed values 230>=                          *)
val _ = op xdef : xdef parser
(* Checking types against type schemes          *)
(*                                              *)
(* [*] The instance property is not so easy to check *)
(* directly—searching for permutations is tedious—but *)
(* the idea is simple: no matter what types are used to *)
(* instantiate sigma_i, sigma_g can be instantiated to *)
(* the same type. To implement this idea, I create a *)
(* supply of skolem types that cannot possibly be part *)
(* of any type in any \nml program.             *)
(* <boxed values 230>=                          *)
val _ = op skolemTypes  : ty stream
(* I use skolem types to create an ``arbitrary'' *)
(* instance of sigma_i. If that instance can be made *)
(* equal to a fresh instance of sigma_g, then sigma_g is *)
(* as general as sigma_i.                       *)
(* <boxed values 230>=                          *)
val _ = op asGeneralAs  : type_scheme * type_scheme -> bool
(* With [[asGeneralAs]] and [[eqTypeScheme]] in hand, *)
(* we can implement the unit tests. The [[check-type]] *)
(* checks to see if the type of [[e]] is as general as *)
(* the type being claimed for [[e]].            *)
(* <shared definitions of [[typeSchemeIsAscribable]] and [[typeSchemeIsEquivalent]]>= *)
fun typeSchemeIsAscribable (e, sigma_e, sigma) =
  if asGeneralAs (sigma_e, sigma) then
    true
  else
    failtest ["check-type failed: expected ", expString e, " to have type ",
              typeSchemeString sigma, ", but it has type ", typeSchemeString
                                                                        sigma_e]
(* And [[check-principal-type]] checks for equivalence. *)
(* <shared definitions of [[typeSchemeIsAscribable]] and [[typeSchemeIsEquivalent]]>= *)
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
fun testIsGood (test, (Gamma, Delta, rho)) =
  let fun ty e = typeof (e, Gamma)
                 handle NotFound x => raise TypeError ("name " ^ x ^
                                                              " is not defined")
      fun ddtystring dd =
        case elabDataDef (dd, Gamma, Delta)
          of (_, _, kind :: _) => kind
           | _ => "???"
      fun deftystring d =
        (case d of DATA dd => ddtystring dd
                 | _ => snd (elabdef (d, Gamma)))
        handle NotFound x => raise TypeError ("name " ^ x ^ " is not defined")

(* <definitions of [[check{Expect,Assert,Error}Checks]] that use type inference>= *)
      fun checkExpectChecks (e1, e2) = 
        let val (tau1, c1) = ty e1
            val (tau2, c2) = ty e2
            val c = tau1 ~ tau2
            val theta = solve (c1 /\ c2 /\ c)
        in  true
        end handle TypeError msg =>
            failtest ["In (check-expect ", expString e1, " ", expString e2,
                                                                     "), ", msg]

(* <definitions of [[check{Expect,Assert,Error}Checks]] that use type inference>= *)
      fun checkExpChecksIn what e =
        let val (tau, c) = ty e
            val theta = solve c
        in  true
        end handle TypeError msg =>
            failtest ["In (", what, " ", expString e, "), ", msg]
      val checkAssertChecks = checkExpChecksIn "check-assert"
      val checkErrorChecks  = checkExpChecksIn "check-error"
      (* <definition of [[checkTypeChecks]] using type inference>= *)
      fun checkTypeChecks form (e, sigma) = 
        let val (tau, c) = ty e
            val theta  = solve c
        in  true
        end handle TypeError msg =>
            failtest ["In (", form, " ", expString e, " " ^ typeSchemeString
                                                                   sigma, "), ",
                      msg]

      fun withTranslatedSigma check form (e, sigmax) =
        check (e, txTyScheme (sigmax, Delta))
        handle TypeError msg =>
          failtest ["In (", form, " ", expString e, " ", tyexString sigmax,
                                                                     "), ", msg]

      val checkTxTypeChecks =
        withTranslatedSigma (checkTypeChecks "check-type") "check-type"
      val checkTxPtypeChecks =
        withTranslatedSigma (checkTypeChecks "check-principal-type")
                            "check-principal-type"
             
      fun checks (CHECK_EXPECT (e1, e2))    = checkExpectChecks (e1, e2)
        | checks (CHECK_ASSERT e)           = checkAssertChecks e
        | checks (CHECK_ERROR e)            = checkErrorChecks  e
        | checks (CHECK_TYPE  (e, sigmax))  = checkTxTypeChecks (e, sigmax)
        | checks (CHECK_PTYPE (e, sigmax))  = checkTxPtypeChecks (e, sigmax)
        | checks (CHECK_TYPE_ERROR e)       = true

      fun outcome e = withHandlers (fn () => OK (eval (e, rho))) () (ERROR o
                                                                     stripAtLoc)
      (* <[[asSyntacticValue]] for \uml>=             *)
      fun asSyntacticValue (LITERAL v) = SOME v
        | asSyntacticValue (VCONX c)   = SOME (CONVAL (c, []))
        | asSyntacticValue (APPLY (e, es)) =
            (case (asSyntacticValue e, optionList (map asSyntacticValue es))
               of (SOME (CONVAL (c, [])), SOME vs) => SOME (CONVAL (c, vs))
                | _ => NONE)
        | asSyntacticValue _ = NONE
      (* A syntactic value is either a literal or a value *)
      (* constructor applied to zero or more syntactic values. *)
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
      (* <boxed values 132>=                          *)
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
      (* <boxed values 133>=                          *)
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
      (* <boxed values 134>=                          *)
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
      (* <boxed values 135>=                          *)
      val _ = op checkErrorPasses : exp -> bool
      fun checkExpectPasses (cx, ex) = checkExpectPassesWith testEqual (cx, ex)
      (* <definitions of [[check*Type*Passes]] using type inference>= *)
      fun checkTypePasses (e, sigma) =
        let val (tau, c) = ty e
            val theta    = solve c
            val sigma_e  = generalize (tysubst theta tau, freetyvarsGamma Gamma)
        in  typeSchemeIsAscribable (e, sigma_e, sigma)
        end handle TypeError msg =>
            failtest ["In (check-type ", expString e, " ", typeSchemeString
                                                              sigma, "), ", msg]
      (* <definitions of [[check*Type*Passes]] using type inference>= *)
      fun checkPrincipalTypePasses (e, sigma) =
        let val (tau, c) = ty e
            val theta    = solve c
            val sigma_e  = generalize (tysubst theta tau, freetyvarsGamma Gamma)
        in  typeSchemeIsEquivalent (e, sigma_e, sigma)
        end handle TypeError msg =>
            failtest ["In (check-principal-type ", expString e, " ",
                      typeSchemeString sigma, "), ", msg]
      (* <definitions of [[check*Type*Passes]] using type inference>= *)
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

      val checkTxTypePasses =
        withTranslatedSigma checkTypePasses          "check-type"
      val checkTxPtypePasses =
        withTranslatedSigma checkPrincipalTypePasses "check-principal-type"

      fun passes (CHECK_EXPECT (c, e))     = checkExpectPasses    (c, e)
        | passes (CHECK_ASSERT c)          = checkAssertPasses    c
        | passes (CHECK_ERROR c)           = checkErrorPasses     c
        | passes (CHECK_TYPE (c, sigmax))  = checkTxTypePasses    (c, sigmax)
        | passes (CHECK_PTYPE (c, sigmax)) = checkTxPtypePasses   (c, sigmax)
        | passes (CHECK_TYPE_ERROR d)      = checkTypeErrorPasses d

  in  checks test andalso passes test
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
(* <boxed values 136>=                          *)
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




(*****************************************************************)
(*                                                               *)
(*   IMPLEMENTATIONS OF \UML\ PRIMITIVES AND DEFINITION OF [[INITIALBASIS]] *)
(*                                                               *)
(*****************************************************************)

(* The next step is to add the primitive functions. \ *)
(* makenowebnotdef (from \LApredefined uML functions \ *)
(* upshape[->]\RA)                              *)
(* <implementations of \uml\ primitives and definition of [[initialBasis]]>= *)
(* <shared utility functions for building primitives in languages with type inference>= *)
fun binaryOp f = (fn [a, b] => f (a, b) | _ => raise BugInTypeInference
                                                                      "arity 2")
fun unaryOp  f = (fn [a]    => f  a     | _ => raise BugInTypeInference
                                                                      "arity 1")
(* Primitives                                   *)
(*                                              *)
(* Here are the primitives. All are either binary or *)
(* unary operators. Type inference should guarantee that *)
(* operators are used with the correct arity.   *)
(* <boxed values 73>=                           *)
val _ = op unaryOp  : (value         -> value) -> (value list -> value)
val _ = op binaryOp : (value * value -> value) -> (value list -> value)
(* <shared utility functions for building primitives in languages with type inference>= *)
fun arithOp f =
      binaryOp (fn (NUM n1, NUM n2) => NUM (f (n1, n2)) 
                 | _ => raise BugInTypeInference "arithmetic on non-numbers")
val arithtype = funtype ([inttype, inttype], inttype)
(* Arithmetic primitives expect and return integers. *)
(* Each primitive operation must be associated with a *)
(* type scheme in the initial environment. It is easier, *)
(* however, to associate a type with each primitive and *)
(* to generalize them all at one go when we create the *)
(* initial environment.                         *)
(* <boxed values 74>=                           *)
val _ = op arithOp   : (int * int -> int) -> (value list -> value)
val _ = op arithtype : ty
(* Complete implementation of uML type inference *)
(*                                              *)
(* [*] This Appendix presents the solutions to most of *)
(* the implementation problems in Chapter [->]. *)
(*                                              *)
(* Type inference                               *)
(*                                              *)
(* The type of the empty list is \/alpha.listalpha, so *)
(* to create a fresh instance, we apply [[listtype]] to *)
(* a fresh type variable. Literal pairs are treated as *)
(* lists. I hope the code for the remaining literals is *)
(* more or less obvious.                        *)

(* <utility functions for building \nml\ primitives>= *)
fun predOp f     = unaryOp  (embedBool o f)
fun comparison f = binaryOp (embedBool o f)
fun intcompare f = 
      comparison (fn (NUM n1, NUM n2) => f (n1, n2)
                   | _ => raise BugInTypeInference "comparing non-numbers")
fun predtype x = funtype ([x],    booltype)
fun comptype x = funtype ([x, x], booltype)
(* <boxed values 75>=                           *)
val _ = op predOp     : (value         -> bool) -> (value list -> value)
val _ = op comparison : (value * value -> bool) -> (value list -> value)
val _ = op intcompare : (int   * int   -> bool) -> (value list -> value)
val _ = op predtype   : ty -> ty
val _ = op comptype   : ty -> ty
(* The predicates are similar to micro-Scheme   *)
(* predicates. As in micro-Scheme, values of any type *)
(* can be compared for equality. Equality has type alpha *)
(* *alpha-->bool, which gets generalized to type scheme *)
(* \/alpha\alldotalpha*alpha-->bool. In full ML, values *)
(* of function types may not be compared for equality. *)

val primFunBasis =
  let fun addPrim ((name, prim, tau), (Gamma, Delta, rho)) = 
        ( bindtyscheme (name, generalize (tau, freetyvarsGamma Gamma), Gamma)
        , Delta
        , bind (name, PRIMITIVE prim, rho)
        )
  in  foldl addPrim predefinedTypeBasis (
                              (* <primitives for \nml\ and \uml\ [[::]]>=     *)
                                         ("+", arithOp op +,   arithtype) :: 
                                         ("-", arithOp op -,   arithtype) :: 
                                         ("*", arithOp op *,   arithtype) :: 
                                         ("/", arithOp op div, arithtype) ::

                              (* <primitives for \nml\ and \uml\ [[::]]>=     *)
                                         ("<", intcompare op <,
                                                            comptype inttype) ::
                                         (">", intcompare op >,
                                                            comptype inttype) ::
                                         ("=", comparison primitiveEquality,
                                                              comptype alpha) ::

                     (* The last primitives are the printing primitives, plus *)

                              (* [[error]].                                   *)

                              (* <primitives for \nml\ and \uml\ [[::]]>=     *)
                                         ("println", unaryOp (fn v => (print (
                                                     valueString v ^ "\n"); v)),
                                                        funtype ([alpha],
                                                                   unittype)) ::
                                         ("print",   unaryOp (fn v => (print (
                                                     valueString v);        v)),
                                                        funtype ([alpha],
                                                                   unittype)) ::
                                         ("printu",  unaryOp (fn NUM n => (
                                                             printUTF8 n; NUM n)
                                                               | _ => raise
                                     BugInTypeInference "printu of non-number"),
                                                        funtype ([inttype],
                                                                   unittype)) ::
                                         ("error", unaryOp (fn v => raise
                                                  RuntimeError (valueString v)),
                                                        funtype ([alpha], beta))
                                                                              ::

                       (* The type of [[error]], which is \/alpha,beta\alldot *)

                        (* alpha-->beta, tells us something interesting about *)

                        (* its behavior. The type suggests that [[error]] can *)

                     (* produce an arbitrary beta without ever consuming one. *)

                      (* Such a miracle is impossible; what the type tells us *)

                              (* is that the [[error]] function never returns *)

                     (* normally. In \nml this type means it must either halt *)

                       (* the interpreter or fail to terminate; in full ML, a *)

                      (* function of this type could also raise an exception. *)


                       (* The [[read]] primitive uses the parser to produce a *)

                              (* list of S-expressions stored in a file.      *)

                              (* <primitives for \nml\ and \uml\ [[::]]>=     *)
                                         ("read", unaryOp (fn (SYM s) =>
                                                                let val fd =
                                                                 TextIO.openIn s
                                                                      handle _
                                 => raise RuntimeError ("Cannot read file " ^ s)
                                                                    val sxs =
                                           sxstream (s, filelines fd, noPrompts)
                                                                in  embedList (
                                                               listOfStream sxs)
                                                                    before
                                                               TextIO.closeIn fd
                                                                end
                                                             | _ => raise
                                       BugInTypeInference "read got non-symbol")
                                                , funtype ([symtype], listtype
                                                                sxtype)) :: nil)
  end
(* And the final step is to add the predefined  *)
(* functions. Here we have access to all of type *)
(* inference and evaluation, in the form of function *)
(* [[readEvalPrintWith]].                       *)
(* <implementations of \uml\ primitives and definition of [[initialBasis]]>= *)
val initialBasis =
  let val predefinedFuns =
        (* <predefined {\uml} functions, as strings>=   *)

         [ "(define null? (xs)"
         , "   (case xs"
         , "      [(cons y ys) #t]"
         , "      ['()         #f]))"
         , "(data (* * => *) pair"
         , "  [PAIR : (forall ('a 'b) ('a 'b -> (pair 'a 'b)))])"
         , "(val pair PAIR)"
         , "(define fst (p)"
         , "   (case p [(PAIR x _) x]))"
         , "(define snd (p)"
         , "   (case p [(PAIR _ y) y]))"
         , "(define compare-ints (n1 n2)"
         , "  (if (< n1 n2) LESS"
         , "      (if (< n2 n1) GREATER"
         , "          EQUAL)))"
         , "(define null? (xs)"
         , "   (case xs ['()        #t]"
         , "            [(cons _ _) #f]))"
         , "(define car (xs)"
         , "   (case xs ['()        (error 'car-of-empty-list)]"
         , "            [(cons y _) y]))"
         , "(define cdr (xs)"
         , "   (case xs ['()         (error 'cdr-of-empty-list)]"
         , "            [(cons _ ys) ys]))"
         , "(define append (xs ys)"
         , "  (case xs"
         , "     ['()         ys]"
         , "     [(cons z zs) (cons z (append zs ys))]))"
         , ""
         , "(define revapp (xs ys)"
         , "  (case xs"
         , "     ['()         ys]"
         , "     [(cons z zs) (revapp zs (cons z ys))]))"
         , "(define list1 (x) (cons x '()))"
         , "(define bind (x y alist)"
         , "  (case alist"
         , "     ['() (list1 (pair x y))]"
         , "     [(cons p ps)"
         , "        (if (= x (fst p))"
         , "            (cons (pair x y) ps)"
         , "            (cons p (bind x y ps)))]))"
         , "(define find (x alist)"
         , "  (case alist"
         , "       ['()   NONE]"
         , "       [(cons (PAIR key value) pairs)"
         , "          (if (= x key)"
         , "              (SOME value)"
         , "              (find x pairs))]))"
         , "(define isbound? (x alist)"
         , "  (case (find x alist)"
         , "     [(SOME _) #t]"
         , "     [NONE     #f]))"
         , "(define and (b c) (if b  c  b))"
         , "(define or  (b c) (if b  b  c))"
         , "(define not (b)   (if b #f #t))"
         , "(define o (f g) (lambda (x) (f (g x))))"
         , "(define curry   (f) (lambda (x) (lambda (y) (f x y))))"
         , "(define uncurry (f) (lambda (x y) ((f x) y)))"
         , "(define caar (xs) (car (car xs)))"
         , "(define cadr (xs) (car (cdr xs)))"
         , "(define cdar (xs) (cdr (car xs)))"
         , "(define length (xs)"
         , "  (case xs"
         , "     ('() 0)"
         , "     ((cons _ ys) (+ 1 (length ys)))))"
         , "(define filter (p? xs)"
         , "  (case xs"
         , "     ('()   '())"
         , "     ((cons y ys)  (if (p? y) (cons y (filter p? ys))"
         , "                              (filter p? ys)))))"
         , "(define map (f xs)"
         , "  (case xs"
         , "     ('() '())"
         , "     ((cons y ys) (cons (f y) (map f ys)))))"
         , "(define app (f xs)"
         , "  (case xs"
         , "     ('() UNIT)"
         , "     ((cons y ys) (begin (f y) (app f ys)))))"
         , "(define reverse (xs) (revapp xs '()))"
         , "(define exists? (p? xs)"
         , "  (case xs"
         , "     ('() #f)"
         , "     ((cons y ys) (if (p? y) #t (exists? p? ys)))))"
         , "(define all? (p? xs)"
         , "  (case xs"
         , "     ('() #t)"
         , "     ((cons y ys) (if (p? y) (all? p? ys) #f))))"
         , "(define foldr (op zero xs)"
         , "  (case xs"
         , "     ('() zero)"
         , "     ((cons y ys) (op y (foldr op zero ys)))))"
         , "(define foldl (op zero xs)"
         , "  (case xs"
         , "     ('() zero)"
         , "     ((cons y ys) (foldl op (op y zero) ys))))"
         , "(define <= (x y) (not (> x y)))"
         , "(define >= (x y) (not (< x y)))"
         , "(define != (x y) (not (= x y)))"
         , "(define max (m n) (if (> m n) m n))"
         , "(define min (m n) (if (< m n) m n))"
         , "(define mod (m n) (- m (* n (/ m n))))"
         , "(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))"
         , "(define lcm (m n) (* m (/ n (gcd m n))))"
         , "(define min* (xs) (foldr min (car xs) (cdr xs)))"
         , "(define max* (xs) (foldr max (car xs) (cdr xs)))"
         , "(define gcd* (xs) (foldr gcd (car xs) (cdr xs)))"
         , "(define lcm* (xs) (foldr lcm (car xs) (cdr xs)))"
         , "(define list1 (x)               (cons x '()))"
         , "(define list2 (x y)             (cons x (list1 y)))"
         , "(define list3 (x y z)           (cons x (list2 y z)))"
         , "(define list4 (x y z a)         (cons x (list3 y z a)))"
         , "(define list5 (x y z a b)       (cons x (list4 y z a b)))"
         , "(define list6 (x y z a b c)     (cons x (list5 y z a b c)))"
         , "(define list7 (x y z a b c d)   (cons x (list6 y z a b c d)))"
         , "(define list8 (x y z a b c d e) (cons x (list7 y z a b c d e)))"
         , "(define takewhile (p? xs)"
         , "  (case xs"
         , "     ('() '())"
         , "     ((cons y ys)"
         , "        (if (p? y)"
         , "            (cons y (takewhile p? ys))"
         , "            '()))))"
         , "(define dropwhile (p? xs)"
         , "  (case xs"
         , "     ('() '())"
         , "     ((cons y ys)"
         , "        (if (p? y)"
         , "            (dropwhile p? ys)"
         , "            xs))))"
          ]
      val xdefs = stringsxdefs ("predefined functions", predefinedFuns)
  in  readEvalPrintWith predefinedFunctionError (xdefs, primFunBasis,
                                                                 noninteractive)
  end


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
