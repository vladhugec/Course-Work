fun duplicates (x::y::xs) = if x = y
                               then true
                               else (duplicates xs) 
  | duplicates (x::[]) = false
  | duplicates ([]) = false
  
val () = Unit.checkExpectWith Bool.toString "true condition" 
         (fn () => duplicates [1, 2, 3, 4, 5, 5]) 
         true
         
val () = Unit.checkExpectWith Bool.toString "false condition" 
         (fn () => duplicates [1, 2, 3, 4, 5, 6]) 
         false
         
(* function that given a list of int, tells the number that are greater than an input number*)

fun xLargerThan (x::xs, y) = let 
                                val Llist = List.filter (fn (z) => z > y) (x::xs)
                                fun length (x::xs) = 1 + length xs
                                  | length ([]) = 0
                              in 
                                 length Llist
                              end
  | xLargerThan (_,_) = 0 
                              
val () = Unit.checkExpectWith Int.toString "false condition" 
         (fn () => xLargerThan ([1, 2, 3, 4, 5, 6, 7], 3)) 
         4
         
(* swap values at even positions with neighbor*)

fun swapEven (x::y::xs) = y :: x :: swapEven xs
  | swapEven (x::[]) = x :: []
  | swapEven nil = nil
  
val () = Unit.checkExpectWith (Unit.listString Int.toString) "swap"
         (fn () => swapEven [1,2,3,4]) 
         [2,1,4,3]
         
val () = Unit.checkExpectWith (Unit.listString Int.toString) "swap"
         (fn () => swapEven [1,2,3,4,5]) 
         [2,1,4,3,5]
         

(* find super digit *)

fun getDigList (x) = 
                  let
                      val last_dig = x mod 10
                      val rest_digs = x div 10
                  in
                      if (rest_digs mod 10) = 0
                          then rest_digs :: last_dig :: nil
                          else (getDigList rest_digs) @ last_dig :: nil
                  end
                  
val () = Unit.checkExpectWith (Unit.listString Int.toString) "swap"
         (fn () => getDigList 15) 
         [1,5]

val () = Unit.checkExpectWith (Unit.listString Int.toString) "swap"
         (fn () => getDigList 9875) 
         [9,8,7,5]
         
fun sumDigs (x) = List.foldr (op +) 0 (getDigList x)

val () = Unit.checkExpectWith Int.toString "sumdigs 15" 
         (fn () => sumDigs 15)
         6
val () = Unit.checkExpectWith Int.toString "sumdigs 9875" 
         (fn () => sumDigs 9875)
         29

fun superDig (x) = let
                      val digSum = sumDigs x
                   in
                      if (digSum div 10) = 0
                          then digSum
                          else (superDig digSum)
                   end
                    
                    
val () = Unit.checkExpectWith Int.toString "super 15" 
         (fn () => superDig 15)
         6

val () = Unit.checkExpectWith Int.toString "super 9875" 
         (fn () => superDig 9875)
         2
         
         


                           
                            
  