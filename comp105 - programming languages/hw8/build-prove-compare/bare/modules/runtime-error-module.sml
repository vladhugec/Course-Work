(* runtime-error-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
structure RuntimeError :> RUNTIME_ERROR = struct
  type name = string
  val duplicatename = Env.duplicatename
  

(*****************************************************************)
(*                                                               *)
(*   SUPPORT FOR DETECTING AND SIGNALING ERRORS DETECTED AT RUN TIME *)
(*                                                               *)
(*****************************************************************)

(* support for detecting and signaling errors detected at run time 363d *)
exception RuntimeError of string (* error message *)
(* support for detecting and signaling errors detected at run time 363f *)
fun errorIfDups (what, xs, context) =
  case duplicatename xs
    of NONE   => ()
     | SOME x => raise RuntimeError (what ^ " " ^ x ^ " appears twice in " ^
                                                                        context)
(*unboxval*)
(* support for detecting and signaling errors detected at run time 363g *)
exception InternalError of string (* bug in the interpreter *)
end
