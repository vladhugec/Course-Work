(* <uscheme-parser.sig>=                        *)
signature USCHEME_PARSER = sig
  type prompts
  val stdPrompts : prompts
  val noPrompts  : prompts

  type 'a stream = 'a Std.stream
  type line = string
  type xdef
  val xdefstream : string * line stream * prompts -> xdef stream
end
