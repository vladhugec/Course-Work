(* handlers.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
signature HANDLERS = sig
  (* XXX maybe migrate me into LANGUAGE *)
  val withHandlers : ('a -> 'b) -> 'a -> (string -> 'b) -> 'b
  (* language-dependent *)
end
