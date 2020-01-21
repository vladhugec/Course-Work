(* uscheme-handlers-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
structure UschemeHandlers :> HANDLERS = struct
  open Env
  open RuntimeError
  open Srcloc
  

(*****************************************************************)
(*                                                               *)
(*   SHARED DEFINITION OF [[WITHHANDLERS]]                       *)
(*                                                               *)
(*****************************************************************)

(* shared definition of [[withHandlers]] 373b *)
fun withHandlers f a caught =
  f a
  handle RuntimeError msg   => caught ("Run-time error <at loc>: " ^ msg)
       | NotFound x         => caught ("Name " ^ x ^ " not found <at loc>")
       | Located (loc, exn) =>
           withHandlers (fn _ => raise exn) a (fn s => caught (fillAtLoc (s, loc
                                                                             )))

(* other handlers that catch non-fatal exceptions and pass messages to [[caught]] 374a *)
       | Div                => caught ("Division by zero <at loc>")
       | Overflow           => caught ("Arithmetic overflow <at loc>")
       | Subscript          => caught ("Array index out of bounds <at loc>")
       | Size               => caught (
                                "Array length too large (or negative) <at loc>")
       | IO.Io { name, ...} => caught ("I/O error <at loc>: " ^ name)
end
