(* uscheme-go-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
structure USchemeGo = struct
  structure C = MkCommandLine(structure Run = UschemeParts.Interpreter)
  val _ = C.run_with_command_line ()
end
