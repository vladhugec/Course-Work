(* uscheme-unit-test-good-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
functor MkUschemeUnitTestGood(structure Eval : EVAL
                                where type exp = UschemeSyntax.exp
                                  and type def = UschemeSyntax.def
                                  and type value = UschemeSyntax.value
                                  and type basis = UschemeSyntax.value ref
                                                                         Env.env
                            )
  :> UNIT_TEST_GOOD where type basis = UschemeSyntax.value ref Env.env
                     and type unit_test = UschemeSyntax.unit_test
  =
struct
  open Eval

  open Error
  open Srcloc
  open UnitTestReport
  open UschemeHandlers
  open UschemeSyntax
  open UschemeValUtils
  

(*****************************************************************)
(*                                                               *)
(*   DEFINITION OF [[TESTISGOOD]] FOR \USCHEME                   *)
(*                                                               *)
(*****************************************************************)

(* definition of [[testIsGood]] for \uscheme 1123a *)
fun testIsGood (test, rho) =
  let fun outcome e = withHandlers (fn e => OK (eval (e, rho))) e (ERROR o
                                                                     stripAtLoc)

   (* [[asSyntacticValue]] for \uscheme, \timpcore, \tuscheme, and \nml 1123b *)
      fun asSyntacticValue (LITERAL v) = SOME v
        | asSyntacticValue _           = NONE
      (*unboxval*)

 (* shared [[check{Expect,Assert,Error}Passes]], which call [[outcome]] 1012c *)
      (* shared [[whatWasExpected]] 1010b *)
      fun whatWasExpected (e, outcome) =
        case asSyntacticValue e
          of SOME v => valueString v
           | NONE =>
               case outcome
                 of OK v => valueString v ^ " (from evaluating " ^ expString e ^
                                                                             ")"
                  | ERROR _ =>  "the result of evaluating " ^ expString e
      (*unboxval*)
      (* shared [[checkExpectPassesWith]], which calls [[outcome]] 1011 *)
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
      (*unboxval*)

(* shared [[checkAssertPasses]] and [[checkErrorPasses]], which call [[outcome]] 1012a *)
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
      (*unboxval*)

(* shared [[checkAssertPasses]] and [[checkErrorPasses]], which call [[outcome]] 1012b *)
      val cefailed = "check-error failed: "
      fun checkErrorPasses checkx =
            case outcome checkx
              of ERROR _ => true
               | OK check =>
                   failtest [cefailed, " expected evaluating ", expString checkx
                                                                               ,
                             " to cause an error, but evaluation produced ",
                             valueString check]
      (*unboxval*)
      fun checkExpectPasses (cx, ex) = checkExpectPassesWith testEqual (cx, ex)
      fun passes (CHECK_EXPECT (c, e)) = checkExpectPasses (c, e)
        | passes (CHECK_ASSERT c)      = checkAssertPasses c
        | passes (CHECK_ERROR c)       = checkErrorPasses  c
(*unboxval*)
  in  passes test
  end

end
