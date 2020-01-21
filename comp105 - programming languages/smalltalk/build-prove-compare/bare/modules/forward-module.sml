(* forward-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
structure Forward = struct
  

(*****************************************************************)
(*                                                               *)
(*   FUNCTION [[FORWARD]], FOR MUTUAL RECURSION THROUGH MUTABLE REFERENCE CELLS *)
(*                                                               *)
(*****************************************************************)

(* function [[forward]], for mutual recursion through mutable reference cells 1008a *)
fun forward what _ =
  let exception UnresolvedForwardDeclaration of string
  in  raise UnresolvedForwardDeclaration what
  end
end
