(* <run-module.sml>=                            *)
functor MkRun(structure REPL : READ_EVAL_PRINT
                where type 'a stream = 'a Std.stream
              structure L    : LANGUAGE
                where type 'a stream = 'a Std.stream
              structure LS   : LANGUAGE_STREAMS
                where type 'a stream = 'a Std.stream
              structure R    : READER
                where type 'a stream = 'a Std.stream
              val initialBasis : L.basis
              sharing type L.prompts = LS.prompts = R.prompts
              sharing type REPL.basis = L.basis
              sharing type REPL.Xdef.xdef = L.Xdef.xdef = LS.xdef
              sharing type REPL.Xdef.unit_test = L.Xdef.unit_test
             ) :> RUN
  =
struct
  open REPL
  open L
  open LS
  open R
  open Initutil
  open Stringutil
  open Interactivity
  

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
