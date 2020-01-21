(* interactivity-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
structure Interactivity :> INTERACTIVITY = struct
  

(*****************************************************************)
(*                                                               *)
(*   TYPE [[INTERACTIVITY]] PLUS RELATED FUNCTIONS AND VALUE     *)
(*                                                               *)
(*****************************************************************)

(* type [[interactivity]] plus related functions and value 370 *)
datatype input_interactivity = PROMPTING | NOT_PROMPTING
(* type [[interactivity]] plus related functions and value 371a *)
datatype output_interactivity = PRINTING | NOT_PRINTING
(* type [[interactivity]] plus related functions and value 371b *)
type interactivity = 
  input_interactivity * output_interactivity
val noninteractive = 
  (NOT_PROMPTING, NOT_PRINTING)
fun prompts (PROMPTING,     _) = true
  | prompts (NOT_PROMPTING, _) = false
fun prints (_, PRINTING)     = true
  | prints (_, NOT_PRINTING) = false
(*unboxval*)
end
