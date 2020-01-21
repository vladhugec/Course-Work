(* <uscheme-go-module.sml>=                     *)
structure USchemeGo = struct
  structure C = MkCommandLine(structure Run = UschemeParts.Interpreter)
  val _ = C.run_with_command_line ()
end
