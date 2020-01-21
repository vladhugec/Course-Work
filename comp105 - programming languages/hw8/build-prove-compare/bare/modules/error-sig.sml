(* error.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
signature ERROR = sig
  datatype 'a error = OK of 'a | ERROR of string
  val >>= : 'a error * ('a -> 'b error) -> 'b error
  val >>=+ : 'a error * ('a -> 'b) -> 'b error
  val errorList : 'a error list -> 'a list error
end
