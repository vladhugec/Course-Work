(* <srcloc.sig>=                                *)
signature SRCLOC = sig
  type srcloc
  type 'a stream
  type 'a error = 'a Error.error

  exception Located of srcloc * exn

  val srclocString : srcloc -> string
  val atLoc : srcloc -> ('a -> 'b) -> ('a -> 'b)
  val fillComplaintTemplate : string * srcloc option -> string
  val errorAt : string -> srcloc -> 'a error

  type 'a located = srcloc * 'a
  type line = string
  val locatedStream : string * line stream -> line located stream
  val fillAtLoc  : string * srcloc -> string
  val stripAtLoc : string -> string

  datatype error_format = WITH_LOCATIONS | WITHOUT_LOCATIONS
  val toplevel_error_format : error_format ref
end
