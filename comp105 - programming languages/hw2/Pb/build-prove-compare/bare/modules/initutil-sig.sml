(* initutil.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
signature INITUTIL = sig
  val override_if_testing : unit -> unit
  val setup_error_format : Interactivity.interactivity -> unit
end
