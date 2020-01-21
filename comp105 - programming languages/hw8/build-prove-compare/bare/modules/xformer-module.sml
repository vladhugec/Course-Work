(* xformer-module.sml (THIS CAN'T HAPPEN -- claimed code was not used) *)
functor MkXformer(Stream : STREAM) :> XFORMER where type 'a stream = 'a
                                                                 Stream.stream =
struct
  open Curry
  open Stream
  open Error
  

(*****************************************************************)
(*                                                               *)
(*   STREAM TRANSFORMERS AND THEIR COMBINATORS                   *)
(*                                                               *)
(*****************************************************************)

(* stream transformers and their combinators 1027a *)
type ('a, 'b) xformer = 
  'a stream -> ('b error * 'a stream) option
(*unboxval*)
(* stream transformers and their combinators 1027b *)
fun pure y = fn xs => SOME (OK y, xs)
(*unboxval*)
(* stream transformers and their combinators 1029a *)
infix 3 <*>
fun tx_f <*> tx_b =
  fn xs => case tx_f xs
             of NONE => NONE
              | SOME (ERROR msg, xs) => SOME (ERROR msg, xs)
              | SOME (OK f, xs) =>
                  case tx_b xs
                    of NONE => NONE
                     | SOME (ERROR msg, xs) => SOME (ERROR msg, xs)
                     | SOME (OK y, xs) => SOME (OK (f y), xs)
(*unboxval*)
(* stream transformers and their combinators 1029b *)
infixr 4 <$>
fun f <$> p = pure f <*> p
(*unboxval*)
(* stream transformers and their combinators 1030a *)
infix 1 <|>
fun t1 <|> t2 = (fn xs => case t1 xs of SOME y => SOME y | NONE => t2 xs) 
(*unboxval*)
(* stream transformers and their combinators 1030b *)
fun pzero _ = NONE
(* stream transformers and their combinators 1030c *)
fun anyParser ts = 
  foldr op <|> pzero ts
(*unboxval*)
(* stream transformers and their combinators 1031a *)
infix 6 <* *>
fun p1 <*  p2 = curry fst <$> p1 <*> p2
fun p1  *> p2 = curry snd <$> p1 <*> p2

infixr 4 <$
fun v <$ p = (fn _ => v) <$> p
(*unboxval*)
(* stream transformers and their combinators 1031b *)
fun one xs = case streamGet xs
               of NONE => NONE
                | SOME (x, xs) => SOME (OK x, xs)
(*unboxval*)
(* stream transformers and their combinators 1031c *)
fun eos xs = case streamGet xs
               of NONE => SOME (OK (), EOS)
                | SOME _ => NONE
(*unboxval*)
(* stream transformers and their combinators 1032a *)
fun peek tx xs =
  case tx xs of SOME (OK y, _) => SOME y
              | _ => NONE
(*unboxval*)
(* stream transformers and their combinators 1032b *)
fun rewind tx xs =
  case tx xs of SOME (ey, _) => SOME (ey, xs)
              | NONE => NONE
(*unboxval*)
(* stream transformers and their combinators 1032c *)
fun sat p tx xs =
  case tx xs
    of answer as SOME (OK y, xs) => if p y then answer else NONE
     | answer => answer
(*unboxval*)
(* stream transformers and their combinators 1032d *)
fun eqx y = 
  sat (fn y' => y = y') 
(*unboxval*)
(* stream transformers and their combinators 1033a *)
infixr 4 <$>?
fun f <$>? tx =
  fn xs => case tx xs
             of NONE => NONE
              | SOME (ERROR msg, xs) => SOME (ERROR msg, xs)
              | SOME (OK y, xs) =>
                  case f y
                    of NONE => NONE
                     | SOME z => SOME (OK z, xs)
(*unboxval*)
(* stream transformers and their combinators 1033b *)
infix 3 <&>
fun t1 <&> t2 = fn xs =>
  case t1 xs
    of SOME (OK _, _) => t2 xs
     | SOME (ERROR _, _) => NONE    
     | NONE => NONE
(*unboxval*)
(* stream transformers and their combinators 1033c *)
fun notFollowedBy t xs =
  case t xs
    of NONE => SOME (OK (), xs)
     | SOME _ => NONE
(*unboxval*)
(* stream transformers and their combinators 1033d *)
fun many t = 
  curry (op ::) <$> t <*> (fn xs => many t xs) <|> pure []
(*unboxval*)
(* stream transformers and their combinators 1034a *)
fun many1 t = 
  curry (op ::) <$> t <*> many t
(*unboxval*)
(* stream transformers and their combinators 1034b *)
fun optional t = 
  SOME <$> t <|> pure NONE
(*unboxval*)
(* stream transformers and their combinators 1035a *)
infix 2 <*>!
fun tx_ef <*>! tx_x =
  fn xs => case (tx_ef <*> tx_x) xs
             of NONE => NONE
              | SOME (OK (OK y),      xs) => SOME (OK y,      xs)
              | SOME (OK (ERROR msg), xs) => SOME (ERROR msg, xs)
              | SOME (ERROR msg,      xs) => SOME (ERROR msg, xs)
infixr 4 <$>!
fun ef <$>! tx_x = pure ef <*>! tx_x
(*unboxval*)
end
