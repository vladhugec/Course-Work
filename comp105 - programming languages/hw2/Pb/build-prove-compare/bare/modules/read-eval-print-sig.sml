(* read-eval-print.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
signature READ_EVAL_PRINT = sig
  structure Xdef : XDEF
  type 'a stream
  type basis
  val readEvalPrintWith :
    (string -> unit) -> Xdef.xdef stream * basis * Interactivity.interactivity
                                                                        -> basis
end
