(* initutil-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
structure Initutil :> INITUTIL = struct
  open Srcloc
  open Interactivity
  

(*****************************************************************)
(*                                                               *)
(*   SHARED UTILITY FUNCTIONS FOR INITIALIZING INTERPRETERS      *)
(*                                                               *)
(*****************************************************************)

(* shared utility functions for initializing interpreters 374c *)
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
end
