(* <language-streams-module.sml>=               *)
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

(* Streams of extended definitions              *)
(*                                              *)
(* Every language has its own parser, called    *)
(* [[xdefstream]], which converts a stream of lines to a *)
(* stream of [[xdef]]s. But as in \cref         *)
(* cinterps.shared-xdef-streams, the convenience *)
(* functions [[filexdefs]] and [[stringsxdefs]] are *)
(* shared.                                      *)
(* <shared definitions of [[filexdefs]] and [[stringsxdefs]]>= *)
fun filexdefs (filename, fd, prompts) = xdefstream (filename, filelines fd,
                                                                        prompts)
fun stringsxdefs (name, strings) = xdefstream (name, streamOfList strings,
                                                                      noPrompts)
(*unboxval*)
end
