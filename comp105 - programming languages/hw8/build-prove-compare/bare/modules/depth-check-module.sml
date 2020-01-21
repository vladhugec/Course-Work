(* depth-check-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
structure DepthCheck = struct
  open RuntimeError
  

(*****************************************************************)
(*                                                               *)
(*   FUNCTION APPLICATION WITH OVERFLOW CHECKING                 *)
(*                                                               *)
(*****************************************************************)

(* function application with overflow checking 1007e *)
local
  val recursionLimit = ref 10000
in
  fun applyCheckingOverflow f =
    if !recursionLimit <= 0 then
      raise RuntimeError "recursion too deep"
    else
      let val _ = recursionLimit := !recursionLimit - 1
      in  fn arg => f arg before (recursionLimit := !recursionLimit + 1)
      end
  fun resetOverflowCheck () = recursionLimit := 10000
end
end
