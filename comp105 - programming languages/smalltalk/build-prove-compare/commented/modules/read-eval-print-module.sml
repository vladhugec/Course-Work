(* <read-eval-print-module.sml>=                *)
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

(* When reading definitions of predefined functions, *)
(* there's no interactivity.                    *)
(* <shared read-eval-print loop and [[processPredefined]]>= *)
fun processPredefined (def,basis) = 
  processDef (def, basis, noninteractive)
(*unboxval*)
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
(* failure.) \mlsflabelreadEvalPrintWith [*] \  *)
(* nwaddsplitboxreadEvalPrintWith(string -> unit) -> *)
(* xdef stream * basis * interactivity -> basis *)
(* <shared read-eval-print loop and [[processPredefined]]>= *)
fun readEvalPrintWith errmsg (xdefs, basis, interactivity) =
  let val unitTests = ref []
      (* The extended-definition forms [[USE]] and [[TEST]] *)
      (* are implemented in exactly the same way for every *)
      (* language: internal function [[try]] passes each *)
      (* [[USE]] to [[useFile]], and it adds each [[TEST]] to *)
      (* the mutable list [[unitTests]]—just as in the C code *)
      (* in \secrefpageimpcore.readevalprint. Function [[try]] *)
      (* passes each true definition [[DEF]] to function *)
      (* [[processDef]], which does the language-dependent *)
      (* work.                                        *)

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

end
