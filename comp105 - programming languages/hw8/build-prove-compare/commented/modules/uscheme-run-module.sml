(* <uscheme-run-module.sml>=                    *)
functor MkUschemeRun( (*val basis : string list*)
                       (* would like basis to be a parameter, but the noweb
                          is not yet set up to make this work *)
                      structure Stream : STREAM
                        where type 'a stream = 'a UschemeLanguage.stream
                      structure Env : ENV where type name = string
                                            and type 'a env = 'a
                                                               UschemeSyntax.env
                      structure Parser : USCHEME_PARSER
                        where type xdef = UschemeSyntax.Xdef.xdef
                        where type prompts = UschemeLanguage.prompts
                      structure Eval : EVAL
                        where type exp = UschemeSyntax.exp
                          and type def = UschemeSyntax.def
                          and type value = UschemeSyntax.value
                          and type basis = UschemeSyntax.value ref Env.env
                    )
  :> sig include INIT include RUN end
       where type basis = UschemeSyntax.value ref Env.env
  
=struct
  structure ReadEvalPrint = MkReadEvalPrint(structure Language = UschemeLanguage
                                            structure Handlers = UschemeHandlers
                                            structure Stream = Stream
                                            val noPrompts = Parser.noPrompts
                                           )

  structure MyStreams = MkLanguageStreams(structure L = UschemeLanguage
                                          structure S = Stream
                                          val noPrompts = Parser.noPrompts)

  open Stream
  open Env
  open Parser
  open Eval
  open MyStreams

  open ReadEvalPrint

  open RuntimeError
  open Stringutil
  open UschemeHandlers
  open UschemeSyntax
  open UschemeValUtils

  type basis = value ref env
  structure TestGood =
    MkUschemeUnitTestGood(structure Eval = Eval)

  structure Tests = MkUnitTests(structure Eval = Eval
                                structure Test = TestGood)
  open Tests

  open Interactivity
  open Srcloc

  

(*****************************************************************)
(*                                                               *)
(*   UTILITY FUNCTIONS FOR BUILDING PRIMITIVES IN \USCHEME       *)
(*                                                               *)
(*****************************************************************)

(* Each transition between the bullets of the list above *)
(* is implemented by a lifting function. I start with *)
(* the final transition, at the top of the type tower. *)
(* Given a function [[f]] of type \monoboxvalue list -> *)
(* value, [[inExp f]] is a primitive function that also *)
(* takes an [[exp]] as argument. The new function *)
(* applies [[f]], and if applying [[f]] raises the *)
(* [[RuntimeError]] exception, it adds the expression to *)
(* the error message.                           *)
(* <utility functions for building primitives in \uscheme>= *)
fun inExp f = 
  fn (e, vs) => f vs
                handle RuntimeError msg =>
                  raise RuntimeError ("in " ^ expString e ^ ", " ^ msg)
(*unboxval*)
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
(* <utility functions for building primitives in \uscheme>= *)
fun arityError n args =
  raise RuntimeError ("expected " ^ intString n ^
                      " but got " ^ intString (length args) ^ " arguments")
fun binaryOp f = (fn [a, b] => f (a, b) | args => arityError 2 args)
fun unaryOp  f = (fn [a]    => f a      | args => arityError 1 args)
(*unboxval*)
(* These higher-order functions encapsulate the ideas of *)
(* ``binary operator'' and ``unary operator'' in a *)
(* general way. As I subdivide the primitives into *)
(* arithmetic, predicates, list primitives, and other, *)
(* I use more higher-order functions to specialize *)
(* things further.                              *)

(* Arithmetic primitives expect and return integers. As *)
(* in C, I reuse the code that projects two arguments to *)
(* integers, but as above, I do it using higher-order *)
(* functions. Function [[arithOp]], which uses  *)
(* [[binaryOp]], lifts a native ML binary arithmetic *)
(* primitive into a function of type \monoboxvalue list *)
(* -> value.                                    *)
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
(*unboxval*)
(* I can now define primitives by applying first *)
(* [[arithOp]] and then [[inExp]] to ML functions like *)
(* [[+]] and [[*]].                             *)

(* We have two kinds of predicate: an ordinary predicate *)
(* takes one argument and a comparison takes two. Some *)
(* comparisons apply only to integers. An ML predicate *)
(* returns a [[bool]], but a micro-Scheme predicate *)
(* needs to return a [[value]], so we compose each *)
(* predicate with [[embedBool]].                *)
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
(*unboxval*)
(* The [[error]] primitive is special because although *)
(* it raises the [[RuntimeError]] exception, this *)
(* behavior is expected, and therefore the context in *)
(* which the exception is raised should not be  *)
(* shown—unless [[error]] is given the wrong number of *)
(* arguments. To maintain such fine control over its *)
(* behavior, [[errorPrimitive]] takes an [[exp]] *)
(* parameter on its own, and it delegates reporting to *)
(* [[inExp]] only in the case of an arity error. *)
(* <utility functions for building primitives in \uscheme>= *)
fun errorPrimitive (_, [v]) = raise RuntimeError (valueString v)
  | errorPrimitive (e, vs)  = inExp (arityError 1) (e, vs)
(*unboxval*)
  

(*****************************************************************)
(*                                                               *)
(*   SHARED UTILITY FUNCTIONS FOR INITIALIZING INTERPRETERS      *)
(*                                                               *)
(*****************************************************************)

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
  

(*****************************************************************)
(*                                                               *)
(*   IMPLEMENTATIONS OF \USCHEME\ PRIMITIVES AND DEFINITION OF [[INITIALBASIS]] *)
(*                                                               *)
(*****************************************************************)

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
(* <implementations of \uscheme\ primitives and definition of [[initialBasis]]>= *)
(* Each transition between the bullets of the list above *)
(* is implemented by a lifting function. I start with *)
(* the final transition, at the top of the type tower. *)
(* Given a function [[f]] of type \monoboxvalue list -> *)
(* value, [[inExp f]] is a primitive function that also *)
(* takes an [[exp]] as argument. The new function *)
(* applies [[f]], and if applying [[f]] raises the *)
(* [[RuntimeError]] exception, it adds the expression to *)
(* the error message.                           *)
(* <utility functions for building primitives in \uscheme>= *)
fun inExp f = 
  fn (e, vs) => f vs
                handle RuntimeError msg =>
                  raise RuntimeError ("in " ^ expString e ^ ", " ^ msg)
(*unboxval*)
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
(* <utility functions for building primitives in \uscheme>= *)
fun arityError n args =
  raise RuntimeError ("expected " ^ intString n ^
                      " but got " ^ intString (length args) ^ " arguments")
fun binaryOp f = (fn [a, b] => f (a, b) | args => arityError 2 args)
fun unaryOp  f = (fn [a]    => f a      | args => arityError 1 args)
(*unboxval*)
(* These higher-order functions encapsulate the ideas of *)
(* ``binary operator'' and ``unary operator'' in a *)
(* general way. As I subdivide the primitives into *)
(* arithmetic, predicates, list primitives, and other, *)
(* I use more higher-order functions to specialize *)
(* things further.                              *)

(* Arithmetic primitives expect and return integers. As *)
(* in C, I reuse the code that projects two arguments to *)
(* integers, but as above, I do it using higher-order *)
(* functions. Function [[arithOp]], which uses  *)
(* [[binaryOp]], lifts a native ML binary arithmetic *)
(* primitive into a function of type \monoboxvalue list *)
(* -> value.                                    *)
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
(*unboxval*)
(* I can now define primitives by applying first *)
(* [[arithOp]] and then [[inExp]] to ML functions like *)
(* [[+]] and [[*]].                             *)

(* We have two kinds of predicate: an ordinary predicate *)
(* takes one argument and a comparison takes two. Some *)
(* comparisons apply only to integers. An ML predicate *)
(* returns a [[bool]], but a micro-Scheme predicate *)
(* needs to return a [[value]], so we compose each *)
(* predicate with [[embedBool]].                *)
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
(*unboxval*)
(* The [[error]] primitive is special because although *)
(* it raises the [[RuntimeError]] exception, this *)
(* behavior is expected, and therefore the context in *)
(* which the exception is raised should not be  *)
(* shown—unless [[error]] is given the wrong number of *)
(* arguments. To maintain such fine control over its *)
(* behavior, [[errorPrimitive]] takes an [[exp]] *)
(* parameter on its own, and it delegates reporting to *)
(* [[inExp]] only in the case of an arity error. *)
(* <utility functions for building primitives in \uscheme>= *)
fun errorPrimitive (_, [v]) = raise RuntimeError (valueString v)
  | errorPrimitive (e, vs)  = inExp (arityError 1) (e, vs)
(*unboxval*)
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
                     , "(define length (xs)"
                     , "  (if (null? xs) 0"
                     , "    (+ 1 (length (cdr xs)))))"
                     , "(define and (b c) (if b  c  b))"
                     , "(define or  (b c) (if b  b  c))"
                     , "(define not (b)   (if b #f #t))"
                     ,
"(define atom? (x) (or (number? x) (or (symbol? x) (or (boolean? x) (null? x)))))"
                     , "(define equal? (s1 s2)"
                     , "  (if (or (atom? s1) (atom? s2))"
                     , "    (= s1 s2)"
                     ,
             "    (and (equal? (car s1) (car s2)) (equal? (cdr s1) (cdr s2)))))"
                     , "(define append (xs ys)"
                     , "  (if (null? xs)"
                     , "     ys"
                     , "     (cons (car xs) (append (cdr xs) ys))))"
                     , "(define revapp (xs ys) ; reverse xs and append ys"
                     , "  (if (null? xs)"
                     , "     ys"
                     , "     (revapp (cdr xs) (cons (car xs) ys))))"
                     , "(define reverse (xs) (revapp xs '()))"
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
                     , "  (if (null? alist) '()"
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
  

(*****************************************************************)
(*                                                               *)
(*   FUNCTION [[RUNAS]], WHICH EVALUATES STANDARD INPUT GIVEN [[INITIALBASIS]] *)
(*                                                               *)
(*****************************************************************)

(* Function [[runAs]] looks at the interactivity mode *)
(* and sets both the error format and the prompts. *)
(* It then starts the read-eval-print loop on standard *)
(* input, with the initial basis.               *)
(* <function [[runAs]], which evaluates standard input given [[initialBasis]]>= *)
fun runAs interactivity = 
  let val _ = setup_error_format interactivity
      val prompts = if prompts interactivity then stdPrompts else noPrompts
      val xdefs = filexdefs ("standard input", TextIO.stdIn, prompts)
  in  ignore (readEvalPrintWith eprintln (xdefs, initialBasis, interactivity))
  end 
(*unboxval*)
end
