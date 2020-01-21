(* xformer.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
signature XFORMER = sig
  type 'a stream
  type 'a error = 'a Error.error
  type ('a, 'b) xformer =
    'a stream -> ('b error * 'a stream) option
  val pure : 'b -> ('a, 'b) xformer
  val <*> : ('a, 'b -> 'c) xformer * ('a, 'b) xformer -> ('a, 'c) xformer
  val <$> : ('b -> 'c) * ('a, 'b) xformer -> ('a, 'c) xformer
  val <|> : ('a, 'b) xformer * ('a, 'b) xformer -> ('a, 'b) xformer
  val <*  : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'b) xformer
  val  *> : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'c) xformer
  val <$  : 'b               * ('a, 'c) xformer -> ('a, 'b) xformer
  val one : ('a, 'a) xformer
  val eos : ('a, unit) xformer
  val peek : ('a, 'b) xformer -> 'a stream -> 'b option
  val rewind : ('a, 'b) xformer -> ('a, 'b) xformer
  val sat : ('b -> bool) -> ('a, 'b) xformer -> ('a, 'b) xformer
  val eqx : ''b -> ('a, ''b) xformer -> ('a, ''b) xformer
  val <$>? : ('b -> 'c option) * ('a, 'b) xformer -> ('a, 'c) xformer
  val <&> : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'c) xformer
  val notFollowedBy : ('a, 'b) xformer -> ('a, unit) xformer
  val many  : ('a, 'b) xformer -> ('a, 'b list) xformer
  val many1 : ('a, 'b) xformer -> ('a, 'b list) xformer
  val optional : ('a, 'b) xformer -> ('a, 'b option) xformer
  val <*>! : ('a, 'b -> 'c error) xformer * ('a, 'b) xformer -> ('a, 'c) xformer
  val <$>! : ('b -> 'c error)             * ('a, 'b) xformer -> ('a, 'c) xformer

  val anyParser : ('a, 'b) xformer list -> ('a, 'b) xformer

  val id : 'a -> 'a
  val fst : 'a * 'b -> 'a
  val snd : 'a * 'b -> 'b
  val pair : 'a -> 'b -> 'a * 'b

  val curry  : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
  val curry3 : ('a * 'b * 'c -> 'd) -> ('a -> 'b -> 'c -> 'd)
end
