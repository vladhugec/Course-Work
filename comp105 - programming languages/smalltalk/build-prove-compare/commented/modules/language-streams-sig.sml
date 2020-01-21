(* <language-streams.sig>=                      *)
signature LANGUAGE_STREAMS = sig
  type prompts
  type xdef
  type 'a stream
  val filexdefs    : string * TextIO.instream * prompts -> xdef stream
  val stringsxdefs : string * string list               -> xdef stream
end
