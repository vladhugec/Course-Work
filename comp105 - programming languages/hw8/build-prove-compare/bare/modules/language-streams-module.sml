(* language-streams-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
functor MkLanguageStreams(structure L : LANGUAGE
                          structure S : STREAM
                            sharing type L.stream = S.stream
                          val noPrompts : L.prompts
                         ) :> LANGUAGE_STREAMS
  where type prompts = L.prompts
    and type xdef    = L.Xdef.xdef
    and type 'a stream = 'a L.stream
  =
struct
  open S
  open L
  open Xdef
  

(*****************************************************************)
(*                                                               *)
(*   SHARED DEFINITIONS OF [[FILEXDEFS]] AND [[STRINGSXDEFS]]    *)
(*                                                               *)
(*****************************************************************)

(* shared definitions of [[filexdefs]] and [[stringsxdefs]] 1020c *)
fun filexdefs (filename, fd, prompts) = xdefstream (filename, filelines fd,
                                                                        prompts)
fun stringsxdefs (name, strings) = xdefstream (name, streamOfList strings,
                                                                      noPrompts)
(*unboxval*)
end
