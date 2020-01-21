(* <initutil.sig>=                              *)
signature INITUTIL = sig
  val override_if_testing : unit -> unit
  val setup_error_format : Interactivity.interactivity -> unit
end
