(* <listutil.sig>=                              *)
signature LISTUTIL = sig
  val zip3   : 'a list * 'b list * 'c list -> ('a * 'b * 'c) list
  val unzip3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
  val reverse : 'a list -> 'a list
  val optionList : 'a option list -> 'a list option
end
