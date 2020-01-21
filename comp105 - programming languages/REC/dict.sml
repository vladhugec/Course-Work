functor DictFn (structure Key : KEY) :> DICT where type key = Key.key = 
struct
	type key = Key.key
	type 'a dict = (key * 'a) list

	val empty = []

	exception NotFound of key

	fun find (k,d) = case (List.find (fn (p1,p2) => Key.eqKey(k,p1)) d) of
							 SOME(a,b) => b
							| _ => raise NotFound k
    fun bind (k,v,d) = 	let
    						val the_p = find(k,d)

    						val removedMap = List.filter (fn (p1,p2) => not (Key.eqKey(k,p1))) d
    					in
    						(k,v)::removedMap
    					end
    					handle NotFound k => (k,v)::d



	  (*exception NotFound of key*)
(*
  val empty : 'a dict
  val find  : key * 'a dict -> 'a    may raise NotFound 
  val bind  : key * 'a * 'a dict -> 'a dict*)
	(* Body *)
end