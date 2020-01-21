(* stdstreams-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
structure Std = struct
  structure StreamRep = MkStreamRep(Suspension)
  structure Stream    = MkStream(StreamRep)
  structure Xformer   = MkXformer(Stream)
  type 'a stream = 'a Stream.stream
  type ('a, 'b) xformer = ('a, 'b) Xformer.xformer
end
