(* eol-marks.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
  (* XXX was called PARSING_STREAM_UTILS, which might be better *)
signature EOL_MARKS = sig
  datatype 'a eol_marked
    = EOL of int (* number of the line that ends here *)
    | INLINE of 'a
  type 'a stream
  type ('a, 'b) xformer
  type srcloc = Srcloc.srcloc
  type 'a located = 'a Srcloc.located
  val drainLine : 'a eol_marked stream -> 'a eol_marked stream
  val eol      : ('a eol_marked, int) xformer
  val inline   : ('a eol_marked, 'a)  xformer
  val srcloc   : ('a located eol_marked, srcloc) xformer
end
