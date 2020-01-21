(* <handlers.sig>=                              *)
signature HANDLERS = sig
  (* XXX maybe migrate me into LANGUAGE *)
  val withHandlers : ('a -> 'b) -> 'a -> (string -> 'b) -> 'b
  (* language-dependent *)
end
