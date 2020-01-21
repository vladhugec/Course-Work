(* <simplesets.sig>=                            *)
signature SIMPLESETS = sig
  type 'a set
  val emptyset : 'a set
  val member   : ''a -> ''a set -> bool
  val insert   : ''a     * ''a set  -> ''a set
  val union    : ''a set * ''a set  -> ''a set
  val inter    : ''a set * ''a set  -> ''a set
  val diff     : ''a set * ''a set  -> ''a set
end
