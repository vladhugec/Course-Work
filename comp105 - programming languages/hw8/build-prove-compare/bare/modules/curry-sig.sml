(* curry.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
signature CURRY = sig
  val id     : 'a -> 'a
  val fst    : ('a * 'b) -> 'a
  val snd    : ('a * 'b) -> 'b
  val pair   : 'a -> 'b -> 'a * 'b
  val curry  : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
  val curry3 : ('a * 'b * 'c -> 'd) -> ('a -> 'b -> 'c -> 'd)
end
