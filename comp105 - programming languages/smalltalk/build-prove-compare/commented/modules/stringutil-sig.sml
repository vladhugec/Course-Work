(* <stringutil.sig>=                            *)
signature STRINGUTIL = sig
  val println    : string -> unit
  val eprint     : string -> unit
  val eprintln   : string -> unit
  val printUTF8  : int -> unit
  val intString : int -> string
  val spaceSep :                    string list -> string
  val commaSep :                    string list -> string
  val separate : string * string -> string list -> string

  val predefinedFunctionError : string -> unit (* XXX rename me *)
end
