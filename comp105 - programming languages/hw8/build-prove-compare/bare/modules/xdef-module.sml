(* xdef-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
functor MkXdef(type def
               type unit_test) :> XDEF where type def = def
                                         and type unit_test = unit_test
                                       =
struct
  type def = def
  type name = string
  type unit_test = unit_test
  

(*****************************************************************)
(*                                                               *)
(*   DEFINITION OF [[XDEF]] (SHARED)                             *)
(*                                                               *)
(*****************************************************************)

(* definition of [[xdef]] (shared) 361c *)
datatype xdef = DEF    of def
              | USE    of name
              | TEST   of unit_test
              | DEFS   of def list  (*OMIT*)
end