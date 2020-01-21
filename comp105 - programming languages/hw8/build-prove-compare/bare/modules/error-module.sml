(* error-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
structure Error :> ERROR = struct
  

(*****************************************************************)
(*                                                               *)
(*   SUPPORT FOR REPRESENTING ERRORS AS \ML\ VALUES              *)
(*                                                               *)
(*****************************************************************)

(* support for representing errors as \ml\ values 1008b *)
datatype 'a error = OK of 'a | ERROR of string
(* support for representing errors as \ml\ values 1009a *)
infix 1 >>=
fun (OK x)      >>= k  =  k x
  | (ERROR msg) >>= k  =  ERROR msg
(*unboxval*)
(* support for representing errors as \ml\ values 1009b *)
infix 1 >>=+
fun e >>=+ k'  =  e >>= (OK o k')
(*unboxval*)
(* support for representing errors as \ml\ values 1010a *)
fun errorList es =
  let fun cons (OK x, OK xs) = OK (x :: xs)
        | cons (ERROR m1, ERROR m2) = ERROR (m1 ^ "; " ^ m2)
        | cons (ERROR m, OK _) = ERROR m
        | cons (OK _, ERROR m) = ERROR m
  in  foldr cons (OK []) es
  end
(*unboxval*)
end
