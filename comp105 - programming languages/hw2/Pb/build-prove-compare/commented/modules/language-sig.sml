(* <language.sig>=                              *)
signature LANGUAGE = sig (* parser, typechecker, and evaluator *)
  structure Xdef : XDEF

  type prompts
  type 'a stream
  type line = string
  val xdefstream : string * line stream * prompts -> Xdef.xdef stream

  (* JS alert: functions below assume interaction on stdout/stderr *)
  type basis
  val processDef : Xdef.def * basis * Interactivity.interactivity -> basis
  val processTests : Xdef.unit_test list * basis -> unit

  (* processTests uses some shared code? *)
end
