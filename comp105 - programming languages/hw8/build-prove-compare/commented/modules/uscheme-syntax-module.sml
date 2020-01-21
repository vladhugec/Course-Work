(* <uscheme-syntax-module.sml>=                 *)
structure UschemeSyntax = struct
  type name = string
  type 'a env = 'a Env.env
  open Stringutil
  

(*****************************************************************)
(*                                                               *)
(*   ABSTRACT SYNTAX AND VALUES FOR \USCHEME                     *)
(*                                                               *)
(*****************************************************************)

(* All these type definitions, together with definitions *)
(* of functions [[valueString]] and [[expString]], are *)
(* pulled together in one Noweb code chunk labeled [[ *)
(*                                              *)
(* ( ***************************************************************** ) *)
(* ( * * ) ( * ABSTRACT SYNTAX AND VALUES FOR \USCHEME* ) ( * *)
(* * )                                          *)
(* ( ***************************************************************** ) *)
(*                                              *)
(* ]].                                          *)
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
(*unboxval*)
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
(*unboxval*)
(* The string-conversion functions take the place of the *)
(* C code's extensible printer—ML does not provide a *)
(* function like [[printf]], but it provides plenty of *)
(* primitives for creating, manipulating, and combining *)
(* strings.                                     *)

  structure Xdef = MkXdef(type def = def
                          type unit_test = unit_test)
end
