(* command-line-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
functor MkCommandLine(structure Run : RUN) :> sig 
  val run_with_command_line : unit -> unit
    (* run the interpreter after processing command-line args *)
end =
struct
  open Run
  open Stringutil
  open Interactivity
  fun run_with_command_line () =
    let 

(*****************************************************************)
(*                                                               *)
(*   CODE THAT LOOKS AT COMMAND-LINE ARGUMENTS AND CALLS [[RUNAS]] TO RUN THE INTERPRETER *)
(*                                                               *)
(*****************************************************************)

(* code that looks at command-line arguments and calls [[runAs]] to run the interpreter 375b *)
val _ = case CommandLine.arguments ()
          of []     => runAs (PROMPTING,     PRINTING)
           | ["-q"] => runAs (NOT_PROMPTING, PRINTING)
           | ["-qq"]=> runAs (NOT_PROMPTING, NOT_PRINTING)   (*OMIT*)
           | _      => eprintln ("Usage: " ^ CommandLine.name () ^ " [-q]")
    in  ()
    end
end
