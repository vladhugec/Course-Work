(*
    Vladimir Hugec
    comp105 hw6
    March 5, 2019
*)


(***** Problem A *****)

fun mynull [] = true
  | mynull [_] = false
  | mynull _ = false
  
val () = Unit.checkExpectWith Bool.toString "[] is true" 
         (fn () => mynull []) 
         true
val () = Unit.checkExpectWith Bool.toString "[1] is false" 
         (fn () => mynull [1]) 
         false
   
(***** Problem B *****)

fun firstVowel nil = false
  | firstVowel (c::xs) = (#"a" = c) 
                           orelse (#"e" = c)
                           orelse (#"i" = c)
                           orelse (#"o" = c)
                           orelse (#"u" = c)
  
val () = Unit.checkExpectWith Bool.toString "a is true" 
         (fn () => firstVowel [#"a",#"b",#"c",#"d"]) 
         true
val () = Unit.checkExpectWith Bool.toString "e is true" 
         (fn () => firstVowel [#"e",#"b",#"c",#"d"]) 
         true
val () = Unit.checkExpectWith Bool.toString "i is true" 
         (fn () => firstVowel [#"i",#"b",#"c",#"d"]) 
         true
val () = Unit.checkExpectWith Bool.toString "o is true" 
         (fn () => firstVowel [#"o",#"b",#"c",#"d"]) 
         true
val () = Unit.checkExpectWith Bool.toString "u is true" 
         (fn () => firstVowel [#"u",#"b",#"c",#"d"]) 
         true
val () = Unit.checkExpectWith Bool.toString "b is false" 
         (fn () => firstVowel [#"b",#"c",#"d"])
         false      
val () = Unit.checkExpectWith Bool.toString "nil is false" 
         (fn () => firstVowel []) 
         false

(***** Problem C *****)

fun reverse nil = nil
  | reverse x = List.foldr (fn (y,z) => z @ [y]) [] x

val () = Unit.checkExpectWith (Unit.listString Int.toString) "reverse"
         (fn () => reverse [1,2,3,4]) 
         [4,3,2,1]

exception Invalid_input
fun minlist (a::b) = 
      List.foldl (fn (y,z) => if y > z then z else y) a b
 |  minlist _ = raise Invalid_input
  
val () = Unit.checkExpectWith Int.toString "min of list 1"
         (fn () => minlist [4,3,2,1]) 
         1

val () = Unit.checkExpectWith Int.toString "min of list 3"
         (fn () => minlist [4,3,5,7]) 
         3

(***** Problem D *****)

exception Mismatch of string
fun zip (nil, nil) = nil
  | zip ((a::b),(c::d)) = (a,c)::zip (b, d)
  | zip (_,_) = raise Mismatch "List lengths not equal"
  
val () = Unit.checkExpectWith 
         (Unit.listString (Unit.pairString Int.toString Int.toString)) "zip 123"
         (fn () => zip ([1,2,3,4],[5,6,7,8]))
         [(1, 5), (2, 6), (3, 7), (4, 8)]

val () = Unit.checkExpectWith 
      (Unit.listString (Unit.pairString String.toString Int.toString)) "zip abc"
         (fn () => zip (["a","b","c","d"],[1,2,3,4]))
         [("a", 1), ("b", 2), ("c", 3), ("d", 4)]
         
val () = Unit.checkExnWith 
       (Unit.listString (Unit.pairString Int.toString Int.toString)) "zip error"
         (fn () => zip ([1,2,3],[5,6,7,8]))

(***** Problem E *****)
    
fun pairfoldrEq f e ((a::b),(c::d)) = f(a,c, pairfoldrEq f e (b, d))
  | pairfoldrEq f e (nil,nil) = e
  | pairfoldrEq f e (_,_) = raise Mismatch "List lengths not equal"

fun plus (a, b, c) = a+b+c
val () = Unit.checkExpectWith Int.toString "pair foldr"
         (fn () => pairfoldrEq plus 0 ([5,6,7,8],[1,2,3,4]))
         36
         
fun ziptoo (x, y) = pairfoldrEq (fn (a, b, c) => (a, b)::c) [] (x, y)

(*val () = Unit.checkExpectWith 
      (Unit.listString (Unit.pairString String.toString Int.toString)) "ziptoo"
         (fn () => ziptoo (["a","b","c","d"],[1,2,3,4]))
         [("a", 1), ("b", 2), ("c", 3), ("d", 4)] *)
         
val () = Unit.checkExpectWith 
         (Unit.listString (Unit.pairString Int.toString Int.toString)) "zip 123"
         (fn () => ziptoo ([1,2,3,4],[5,6,7,8]))
         [(1, 5), (2, 6), (3, 7), (4, 8)]

(***** Problem F *****)

fun concat ((x)::(y)) = x @ concat y
  | concat nil = nil
  
val () = Unit.checkExpectWith (Unit.listString Int.toString) "concat"
         (fn () => concat [[1], [2, 3, 4], [], [5, 6]])
         [1, 2, 3, 4, 5, 6]

(***** Problem G *****)

type 'a env = string -> 'a
exception NotFound of string

val emptyEnv = fn (s) => raise NotFound s

val bindVar : string * 'a * 'a env -> 'a env = 
    fn (var, v, env) => fn (s) => if var = s then v else env s
    
val lookup : string * 'a env -> 'a = 
    fn (var, env) => env var
    
val isBound : string * 'a env -> bool = 
    fn (var, env) => let val f = env var in true end
    handle NotFound (var) => false
    
val env = bindVar ("Five", 5, emptyEnv)
val env = bindVar ("Two", 2, env)
    
val () = Unit.checkExpectWith Int.toString "lookup 5"
         (fn () => lookup ("Five", env))
         5
val () = Unit.checkExpectWith Bool.toString "bound t"
         (fn () => isBound ("Five", env))
         true
val () = Unit.checkExpectWith Bool.toString "bound f"
         (fn () => isBound ("Six", env))
         false
val () = Unit.checkExpectWith Bool.toString "look error"
         (fn () => isBound ("Two", env))
         true
val () = Unit.checkExpectWith Int.toString "look error"
         (fn () => lookup ("Two", env))
         2
val () = Unit.checkExnWith Int.toString "look error"
         (fn () => lookup ("Six", env))
         
(***** Problem H *****)

(* GIVENS *)

datatype nat = ZERO
      | TIMES10PLUS of nat * int

fun times10plus (ZERO, 0) = ZERO
  | times10plus (m, d)    = TIMES10PLUS (m, d)
  
fun times10 n = times10plus (n, 0)

fun natOfDigit d = times10plus (ZERO, d)

fun flip f (x, y) = f (y, x)

fun natOfDigits ds = foldl (flip times10plus) ZERO ds

(* END GIVENS *)

fun intOfNat (TIMES10PLUS (m, d)) = 10 * intOfNat m + d
  | intOfNat ZERO = 0

val () = Unit.checkExpectWith Int.toString "intOfNat"
         (fn () => intOfNat (natOfDigits [1, 2, 3]))
         123

fun natString ZERO = "0"
  | natString (TIMES10PLUS (ZERO, d)) = Int.toString d
  | natString (TIMES10PLUS (m, d)) = natString m ^ Int.toString d
  
val () = Unit.checkExpectWith
         String.toString "natstring"
         (fn () => natString (natOfDigits [3, 2, 1, 0]))
         "3210"

val () = Unit.checkExpectWith
         String.toString "natstring"
         (fn () => natString (natOfDigits [3, 2, 1]))
         "321"

val () = Unit.checkExpectWith
         String.toString "natstring"
         (fn () => natString (natOfDigits [0]))
         "0"
         
fun natOfInt 0 = ZERO
  | natOfInt x = times10plus (natOfInt (x div 10), x mod 10)
 
val () = Unit.checkExpectWith
         Int.toString "NatofInt"
         (fn () => intOfNat (natOfInt 2018))
         2018
         
val () = Unit.checkExpectWith
         natString "NatofInt"
         (fn () => natOfInt 2018)
         (TIMES10PLUS(TIMES10PLUS(TIMES10PLUS(TIMES10PLUS(ZERO, 2), 0), 1), 8))
         
(***** Problem I *****)

fun carryIntoNat (x, 0) = x
  | carryIntoNat (ZERO, c) = natOfInt c
  | carryIntoNat (TIMES10PLUS (m, d), c) =
      times10plus (carryIntoNat (m, (d + c) div 10), (d + c) mod 10)
      
val () = Unit.checkExpectWith
         Int.toString "natstring"
         (fn () => intOfNat (carryIntoNat (natOfInt 99, 1)))
         100
         
val () = Unit.checkExpectWith
         Int.toString "natstring"
         (fn () => intOfNat (carryIntoNat (natOfInt 99, 0)))
         99
         
val () = Unit.checkExpectWith
         Int.toString "natstring"
         (fn () => intOfNat (carryIntoNat (ZERO, 1)))
         1

fun addWithCarry (n1, ZERO, c) = carryIntoNat (n1, c)
  | addWithCarry (ZERO, n2, c) = carryIntoNat (n2, c)
  | addWithCarry (TIMES10PLUS (m1, d1), TIMES10PLUS (m2, d2) , c) =
      let val d = (d1 + d2 + c) mod 10 
          val c'= (d1 + d2 + c) div 10
        in  times10plus (addWithCarry (m1, m2, c'), d)
        end

val () = Unit.checkExpectWith
         Int.toString "natstring"
         (fn () => intOfNat (addWithCarry (natOfInt 5, natOfInt 6, 1)))
         12

val () = Unit.checkExpectWith
         Int.toString "natstring"
         (fn () => intOfNat (addWithCarry (natOfInt 5, ZERO, 1)))
         6

val () = Unit.checkExpectWith
         Int.toString "natstring"
         (fn () => intOfNat (addWithCarry (ZERO, natOfInt 5, 1)))
         6
         
fun addNats (n1, n2) = addWithCarry (n1, n2, 0)

val () = Unit.checkExpectWith
         Int.toString "natstring"
         (fn () => intOfNat (addNats (natOfInt 5, natOfInt 5)))
         10

exception Negative
fun borrowFromNat (n, 0) = n
  | borrowFromNat (TIMES10PLUS (m, 0), 1) = 
                  times10plus (borrowFromNat (m, 1), 9)
  | borrowFromNat (TIMES10PLUS (m, d), 1) = 
                  if d > 0 then times10plus (m, d-1) else raise Negative
  | borrowFromNat (ZERO, 1) = raise Negative
  | borrowFromNat _ = raise Negative
  
val () = Unit.checkExpectWith
         Int.toString "borrowFromNat"
         (fn () => intOfNat (borrowFromNat (natOfInt 5, 1)))
         4
         
val () = Unit.checkExpectWith
         Int.toString "borrowFromNat"
         (fn () => intOfNat (borrowFromNat (natOfInt 50, 1)))
         49
         
val () = Unit.checkExnWith
         Int.toString "borrowFromNat"
         (fn () => intOfNat (borrowFromNat (natOfInt ~1, 1)))

val () = Unit.checkExnWith
         Int.toString "borrowFromNat"
         (fn () => intOfNat (borrowFromNat (ZERO, 1)))
         

fun subWithBorrow (n1, ZERO, b) = borrowFromNat (n1, b)
  | subWithBorrow (ZERO, n2, b) = borrowFromNat (n2, b)
  | subWithBorrow (TIMES10PLUS (m1, d1), TIMES10PLUS (m2, d2), b) =
        let val d = (d1 - d2 - b) mod 10
            val b' = if (d1 - d2 - b) < 0 then 1 else 0
        in  times10plus (subWithBorrow (m1, m2, b'), d)
        end

val () = Unit.checkExpectWith
         Int.toString "subWithBorrow"
         (fn () => intOfNat (subWithBorrow (natOfInt 5, natOfInt 4, 1)))
         0
         
val () = Unit.checkExnWith natString "subWithBorrow"
         (fn () => subWithBorrow (natOfInt 5, natOfInt 5, 1))

val () = Unit.checkExpectWith
         Int.toString "subWithBorrow"
         (fn () => intOfNat (subWithBorrow (natOfInt 5, ZERO, 1)))
         4

fun subNats (n1, n2) = subWithBorrow (n1, n2, 0)

val () = Unit.checkExnSatisfiesWith natString "1 - 5"
       (fn () => subNats (natOfDigits [1], natOfDigits [5]))
       ("Negative", fn Negative => true | _ => false)


(***** Problem J *****)

type 'a ilist = int * 'a list

(* EXPLAINATION 
    I choose to represent the indicator list as a double where the first element
    is the position of the indicator in the list starting with 1 and the second 
    element is 'a list. So for example (3, [a,b,c,d]) the indicator is the 3rd
    element of [a,b,c,d] so the indicator is at c in the list.
    So this means that moving the indicator around simply adds or subtracts from
    int indicator in the first element of the tuple.
    This also mean that ifoldr and ifoldl wouldnt have to be reimplemented, and
    I would be able to define ifoldl/r in terms of foldl/r 
*)

val singletonOf : 'a -> 'a ilist =
    fn (x) => (1, x::[])
    
fun indicated (pos, l) = List.nth (l, pos-1)

val () = Unit.checkExpectWith Int.toString "indicated"
         (fn () => indicated (4, [1,2,3,4,5,6]))
         4
    
val indicatorLeft : 'a ilist -> 'a ilist =
    fn (1, l) => raise Subscript
     | (pos, l) => (pos - 1, l)

(* I used length here because for the life of me I could get the types of the
   function to line up correctly, please have mercy on me *)
val indicatorRight: 'a ilist -> 'a ilist =
    fn (pos, l) => if pos < List.length l then (pos+1, l) else raise Subscript

val deleteLeft  : 'a ilist -> 'a ilist =
   fn (pos, l) => (pos-1, (List.take (l, pos - 2)) @ (List.drop (l, pos - 1)))

val deleteRight : 'a ilist -> 'a ilist =
    fn (pos, l) => (pos, (List.take (l, pos)) @ (List.drop (l, pos + 1)))
    
val insertLeft  : 'a * 'a ilist -> 'a ilist =
    fn (x, (pos, l)) => (pos+1, (List.take (l, pos-1)) @ [x] @
                                (List.drop (l, pos-1)))

val insertRight : 'a * 'a ilist -> 'a ilist =
    fn (x, (pos, l)) => (pos, (List.take (l, pos)) @ [x] @ (List.drop (l, pos)))
    
fun ifoldl f e (pos, l) = foldl f e l
val ifoldl : ('a * 'b -> 'b) -> 'b -> 'a ilist -> 'b = ifoldl

fun ifoldr f e (pos, l) = foldr f e l
val ifoldr : ('a * 'b -> 'b) -> 'b -> 'a ilist -> 'b = ifoldr


val prop1 = fn (x, xs) => deleteLeft (insertLeft (x, xs)) = xs

val prop2 = fn (x, xs) => indicatorLeft (insertLeft (x, xs)) =
            indicatorRight (insertRight (x, indicatorLeft xs))


val test = singletonOf 3
val test = insertLeft  (1, test)
val test = insertLeft  (2, test)
val test = insertRight (4, test)
val test = indicatorRight test
val test = insertRight (5, test)

val () = Unit.checkExpectWith 
       (Unit.pairString Int.toString (Unit.listString Int.toString)) "12345test"
         (fn () => test)
         (4, [1,2,3,4,5])
         
val () = Unit.checkExpectWith 
      (Unit.pairString Int.toString (Unit.listString String.toString)) "insertL"
         (fn () => insertLeft ("a", (1, ["b", "c", "d"])))
         (2, ["a", "b", "c", "d"])
         
val () = Unit.checkExpectWith 
      (Unit.pairString Int.toString (Unit.listString String.toString)) "insertR"
         (fn () => insertRight ("a", (1, ["b", "c", "d"])))
         (1, ["b", "a", "c", "d"])
         
val () = Unit.checkExnWith 
      (Unit.pairString Int.toString (Unit.listString String.toString)) "indicR"
         (fn () => indicatorRight (3, ["b", "c", "d"]))
         
val () = Unit.checkExpectWith 
      (Unit.pairString Int.toString (Unit.listString String.toString)) "indicR"
         (fn () => indicatorRight (2, ["b", "c", "d"]))
         (3, ["b", "c", "d"]) 

val () = Unit.checkExpectWith Bool.toString "prop1"
         (fn () => prop1 (6, (4, [1,2,3,4,5,6])))
         true

val () = Unit.checkExpectWith Bool.toString "prop2"
         (fn () => prop2 (6, (4, [1,2,3,4,5,6])))
         true

val () = Unit.checkExnWith Bool.toString "prop2"
         (fn () => prop2 (6, (1, [1,2,3,4,5,6])))
         
val () = Unit.checkExpectWith Int.toString "ifoldr+"
         (fn () => ifoldr (op +) 0 (1, [1,2,3,4,5]))
         15

val () = Unit.checkExpectWith Int.toString "ifoldl+"
         (fn () => ifoldl (op +) 0 (1, [1,2,3,4,5]))
         15
         
val () = Unit.checkExpectWith Int.toString "ifoldl-"
         (fn () => ifoldl (op -) 0 (1, [1,2,3,4,5]))
         3
         
val () = Unit.checkExpectWith Int.toString "ifoldr-"
         (fn () => ifoldr (op -) 0 (1, [1,2,3,4,5]))
         3

val () = Unit.checkExpectWith Int.toString "ifoldl-"
         (fn () => foldl (op -) 0 [1,2,3,4,5])
         3
         
val () = Unit.checkExpectWith Int.toString "ifoldr-"
         (fn () => foldr (op -) 0 [1,2,3,4,5])
         3
                  
val () = Unit.reportWhenFailures () (* put me at the _end_ *)