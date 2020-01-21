(* env-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
structure ListEnv :> ENV where type name = string
                           and type 'a env = (string * 'a) list =
struct
  

(*****************************************************************)
(*                                                               *)
(*   SUPPORT FOR NAMES AND ENVIRONMENTS                          *)
(*                                                               *)
(*****************************************************************)

(* support for names and environments 358a *)
type name = string
(* support for names and environments 358b *)
type 'a env = (name * 'a) list
val emptyEnv = []

(* lookup and check of existing bindings *)
exception NotFound of name
fun find (name, []) = raise NotFound name
  | find (name, (n, v)::tail) = if name = n then v else find (name, tail)

(* adding new bindings *)
exception BindListLength
fun bind (name, v, rho) = (name, v) :: rho
fun bindList (n::vars, v::vals, rho) = bindList (vars, vals, bind (n, v, rho))
  | bindList ([], [], rho) = rho
  | bindList _ = raise BindListLength
(*unboxval*)
(* support for names and environments 363e *)
fun duplicatename [] = NONE
  | duplicatename (x::xs) =
      if List.exists (fn x' => x' = x) xs then
        SOME x
      else
        duplicatename xs
(*unboxval*)
end
                                                                
structure Env :> ENV where type name = string = struct
  

(*****************************************************************)
(*                                                               *)
(*   SUPPORT FOR NAMES AND ENVIRONMENTS                          *)
(*                                                               *)
(*****************************************************************)

(* support for names and environments 358a *)
type name = string
(* support for names and environments 358b *)
type 'a env = (name * 'a) list
val emptyEnv = []

(* lookup and check of existing bindings *)
exception NotFound of name
fun find (name, []) = raise NotFound name
  | find (name, (n, v)::tail) = if name = n then v else find (name, tail)

(* adding new bindings *)
exception BindListLength
fun bind (name, v, rho) = (name, v) :: rho
fun bindList (n::vars, v::vals, rho) = bindList (vars, vals, bind (n, v, rho))
  | bindList ([], [], rho) = rho
  | bindList _ = raise BindListLength
(*unboxval*)
(* support for names and environments 363e *)
fun duplicatename [] = NONE
  | duplicatename (x::xs) =
      if List.exists (fn x' => x' = x) xs then
        SOME x
      else
        duplicatename xs
(*unboxval*)
end
