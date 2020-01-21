(* <stream.sig>=                                *)
signature STREAM = sig
  type 'a stream
  type line = string
  val EOS : 'a stream
  val streamGet : 'a stream -> ('a * 'a stream) option
  val streamDrop : int * 'a stream -> 'a stream
  val streamOfList : 'a list -> 'a stream
  val listOfStream : 'a stream -> 'a list
  val delayedStream : (unit -> 'a stream) -> 'a stream
  val streamOfEffects : (unit -> 'a option) -> 'a stream
  val filelines : TextIO.instream -> line stream
  val streamRepeat : 'a -> 'a stream
  val streamOfUnfold : ('b -> ('a * 'b) option) -> 'b -> 'a stream
  val preStream : (unit -> unit) * 'a stream -> 'a stream
  val postStream : 'a stream * ('a -> unit) -> 'a stream
  val streamMap : ('a -> 'b) -> 'a stream -> 'b stream
  val streamFilter : ('a -> bool) -> 'a stream -> 'a stream
  val streamFold : ('a * 'b -> 'b) -> 'b -> 'a stream -> 'b
  val streamZip : 'a stream * 'b stream -> ('a * 'b) stream
  val streamConcat : 'a stream stream -> 'a stream
  val streamConcatMap : ('a -> 'b stream) -> 'a stream -> 'b stream
  val @@@ : 'a stream * 'a stream -> 'a stream
  val naturals : int stream
end

signature STREAM_REP = sig
  type 'a susp
  datatype 'a stream 
    = EOS
    | :::       of 'a * 'a stream
    | SUSPENDED of 'a stream susp
  type line = string
  val streamGet : 'a stream -> ('a * 'a stream) option
  val streamDrop : int * 'a stream -> 'a stream
  val streamOfList : 'a list -> 'a stream
  val listOfStream : 'a stream -> 'a list
  val delayedStream : (unit -> 'a stream) -> 'a stream
  val streamOfEffects : (unit -> 'a option) -> 'a stream
  val filelines : TextIO.instream -> line stream
  val streamRepeat : 'a -> 'a stream
  val streamOfUnfold : ('b -> ('a * 'b) option) -> 'b -> 'a stream
  val preStream : (unit -> unit) * 'a stream -> 'a stream
  val postStream : 'a stream * ('a -> unit) -> 'a stream
  val streamMap : ('a -> 'b) -> 'a stream -> 'b stream
  val streamFilter : ('a -> bool) -> 'a stream -> 'a stream
  val streamFold : ('a * 'b -> 'b) -> 'b -> 'a stream -> 'b
  val streamZip : 'a stream * 'b stream -> ('a * 'b) stream
  val streamConcat : 'a stream stream -> 'a stream
  val streamConcatMap : ('a -> 'b stream) -> 'a stream -> 'b stream
  val @@@ : 'a stream * 'a stream -> 'a stream
  val naturals : int stream
end
