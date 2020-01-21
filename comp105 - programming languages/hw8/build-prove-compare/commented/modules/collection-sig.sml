(* <collection.sig>=                            *)
signature COLLECTION = sig
  type 'a collection
  val elemsC  : 'a collection -> 'a Listsets.set
  val singleC : 'a -> 'a collection
  val emptyC  :       'a collection
  val joinC   : 'a collection collection -> 'a collection
  val mapC    : ('a -> 'b)      -> ('a collection -> 'b collection)
  val filterC : ('a -> bool)    -> ('a collection -> 'a collection)
  val mapC2   : ('a * 'b -> 'c) -> ('a collection * 'b collection -> 'c
                                                                     collection)
end
