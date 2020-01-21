(* read-eval-print-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
functor MkReadEvalPrint(structure Language : LANGUAGE
                        structure Stream : STREAM
                          sharing type Language.stream = Stream.stream
                        structure Handlers : HANDLERS
                        val noPrompts : Language.prompts  (* UGH *)
                       )
  :> READ_EVAL_PRINT where type 'a stream = 'a Stream.stream
                       and type Xdef.def = Language.Xdef.def
                       and type Xdef.xdef = Language.Xdef.xdef
                       and type Xdef.unit_test = Language.Xdef.unit_test
                       and type basis = Language.basis
  =
struct
  structure LS = MkLanguageStreams(structure L = Language
                                   structure S = Stream
                                   val noPrompts = noPrompts
                                  )
  open LS
  open Language
  open Xdef
  open Stream
  open Env (* for NotFound *)
  open Interactivity
  open RuntimeError
  open Srcloc
  open DepthCheck
  open Handlers
  type 'a stream = 'a Stream.stream
  

(*****************************************************************)
(*                                                               *)
(*   SHARED READ-EVAL-PRINT LOOP AND [[PROCESSPREDEFINED]]       *)
(*                                                               *)
(*****************************************************************)

(* shared read-eval-print loop and [[processPredefined]] 371c *)
fun processPredefined (def,basis) = 
  processDef (def, basis, noninteractive)
(*unboxval*)
(* shared read-eval-print loop and [[processPredefined]] 372a *)
fun readEvalPrintWith errmsg (xdefs, basis, interactivity) =
  let val unitTests = ref []

(* definition of [[processXDef]], which can modify [[unitTests]] and call [[errmsg]] 372c *)
      fun processXDef (xd, basis) =
        let (* definition of [[useFile]], to read from a file 372b *)
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
            val caught = caught o (fn x => (resetOverflowCheck (); x))
                                                                        (*OMIT*)
        in  withHandlers try xd caught
        end 
      (*unboxval*)
      val basis = streamFold processXDef basis xdefs
      val _     = processTests (!unitTests, basis)
(*unboxval*)
  in  basis
  end
end
