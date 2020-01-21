(* <uscheme-handlers-module.sml>=               *)
structure UschemeHandlers :> HANDLERS = struct
  open Env
  open RuntimeError
  open Srcloc
  

(*****************************************************************)
(*                                                               *)
(*   SHARED DEFINITION OF [[WITHHANDLERS]]                       *)
(*                                                               *)
(*****************************************************************)

(* The most important exceptions are [[RuntimeError]], *)
(* [[NotFound]], and [[Located]]. Exceptions    *)
(* [[RuntimeError]] and [[NotFound]] are defined above; *)
(* they signal problems with evaluation or with an *)
(* environment, respectively. Exception [[Located]], *)
(* which is defined in \crefmlinterps.chap, is a special *)
(* exception that wraps another exception [[exn]] in a *)
(* source-code location. When [[Located]] is caught, we *)
(* ``re-raise'' exception [[exn]], and we fill in the *)
(* source location in [[exn]]'s error message.  *)
(* <shared definition of [[withHandlers]]>=     *)
fun withHandlers f a caught =
  f a
  handle RuntimeError msg   => caught ("Run-time error <at loc>: " ^ msg)
       | NotFound x         => caught ("Name " ^ x ^ " not found <at loc>")
       | Located (loc, exn) =>
           withHandlers (fn _ => raise exn) a (fn s => caught (fillAtLoc (s, loc
                                                                             )))
       (* In addition to [[RuntimeError]], [[NotFound]], and *)
       (* [[Located]], [[withHandlers]] catches many exceptions *)
       (* that are predefined ML's Standard Basis Library. *)
       (* These exceptions signal things that can go wrong *)
       (* while evaluating an expression or when reading a *)
       (* file.                                        *)

(* <other handlers that catch non-fatal exceptions and pass messages to [[caught]]>= *)
       | Div                => caught ("Division by zero <at loc>")
       | Overflow           => caught ("Arithmetic overflow <at loc>")
       | Subscript          => caught ("Array index out of bounds <at loc>")
       | Size               => caught (
                                "Array length too large (or negative) <at loc>")
       | IO.Io { name, ...} => caught ("I/O error <at loc>: " ^ name)
       (* I reuse the same exception handlers in later *)
       (* interpreters.                                *)

end
