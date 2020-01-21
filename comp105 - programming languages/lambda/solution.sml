(* Lambda calculus solution template, COMP 105 *)
structure OpenSolution = struct

  (******************* EVALUATION BASICS ********************)

  datatype void = VOID of void  (* a type with no values *)

  datatype term =   VAR of string
                  | LAMBDA of (string * term)
                  | APP of (term * term)

  exception LeftAsExercise of string

  val var : string -> term         = fn x => VAR x
  val app : term   -> term -> term = fn x => fn y => APP(x, y) 
  val lam : string -> term -> term = fn x => fn y => LAMBDA(x, y)

  val cpsLambda :
      term -> 
      (string -> term -> 'a) -> 
      (term -> term -> 'a) -> 
      (string -> 'a) -> 
      'a
    = fn term => fn l => fn a => fn v => (case term of VAR(x) => v x
                                                     | APP(x, y) => a x y
                                                     | LAMBDA(x, y) => l x y)
                                                     
  (********************** SUBSTITUTION **********************)

  val freeIn   : string -> term -> bool        = 
                      fn s => fn t => (case t of 
                                          VAR(x) => s = x
                                        | LAMBDA(x, y) => (s = x)
                                        | APP(x, y) => t = x orelse t = y)
                                                                          
  val freeVars : term -> string list           = 
                      fn t => (case t of 
                                        VAR(x) => x :: nil
                                      | APP(x, y) => nil
                                      | LAMBDA(x, y) => nil)
                                                        

  val subst    : string * term -> term -> term = 
                      fn (s, te) =>
                             fn t => (case t of 
                                        VAR(x) => if x = s then te else var x
                                      | APP(x, y) => app x y
                                      | LAMBDA(x, y) => lam x y)
          
  (****************** REDUCTION STRATEGIES ******************)

  val reduceN : term Reduction.reducer = fn _ => raise LeftAsExercise "reduceN"
  val reduceA : term Reduction.reducer = fn _ => raise LeftAsExercise "reduceA"

end


structure SealedSolution :> SOLUTION = OpenSolution
