;; Vladimir Hugec
;; Feb 19, 2019
;; HW4 - comp105

;;
;; PROBLEM - 14
;;

;; PART B - max*
;;
;; max* finds the maximum of a non-empty list of integers
;;
(define max* (x)
    (foldr (lambda (y z) 
                (if (> y z) 
                    y 
                    z)) 
            (car x) (cdr x)))
               
(check-expect (max* '(1 2 3 4 5 6 7)) 7)

;; PART C - gcd*
;;
;; gcd* finds the greatest common denominator of a non-empty list of integers
;;
(define gcd* (x)
    (foldr (lambda (y z)
                   (gcd y z))
            (car x) (cdr x)))
           
(check-expect (gcd* '(10 5 20 30)) 5)

;; PART D - lcm*
;;
;; lcm* finds the least common denominator of a non-empty list of integers
;;
(define lcm* (x)
    (foldr (lambda (y z)
                   (lcm y z))
            (car x) (cdr x)))
           
(check-expect (lcm* '(1 2 3 4)) 12)
(check-expect (lcm* '(3 6 8)) 24)


;; PART E - sum
;;
;; Finds the sum of a non-empty list of integers
;;
(define sum (x)
    (foldr + 0 x))
    
(check-expect (sum '(1 2 3 4 5)) 15)


;; PART F - product
;;
;; Finds the product of a non-empty list of integers
;;
(define product (x)
    (foldr * 1 x))
    
(check-expect (product '(1 2 3 4)) 24)


;; PART H - append
;;
;; Appends two lists together 
;;
(define append (x y)
    (foldr cons y x))
    
(check-expect (append '(a b c) '(1 2 3)) '(a b c 1 2 3)) 


;; PART J - reverse
;;
;; reverses a list 
;;
(define reverse (x)
    (foldl cons '() x))
    
(check-expect (reverse '(1 2 3 4)) '(4 3 2 1))
(check-expect (reverse '(1 (2 3) 4)) '(4 (2 3) 1))



;;
;; PROBLEM - 15
;;


;; length
;;
;; returns the number of elements present in a non-empty list
;;
(define length (x)
    (foldr (lambda (y z) (+ z 1)) 0 x))
    
(check-expect (length '(1 2 3 4)) 4)
(check-expect (length '(1)) 1)


;; map (f x)
;;
;; applies function f to each element in list x 
;;
(define map (f x)
    (foldr (lambda (y z) (cons (f y) z)) '() x))
    
;; TESTER FUNCTION
;; adds 1 to input
(define add1 (x)
    (+ x 1))
(check-expect (map add1 '(1 2 3 4)) '(2 3 4 5))


;; filter (p? x)
;;
;; applies function x to each element in y and returns new list of elements
;; which satisfied function x 
;;
(define filter (p? x)
    (foldr (lambda (y z) 
               (if (p? y) 
                   (cons y z) 
                   z)) 
            '() x))
            
;; even [TESTER FUNCTION]
;; returns #t if even
(define even? (x)
    (= (mod x 2) 0))
(check-expect (even? 2) #t)
(check-expect (even? 3) #f)
(check-expect (filter even? '(1 2 3 4 5 6 7)) '(2 4 6))

;; exsists? (p? x)
;;
;; returns true if there exists an element in list x that satisfies function p?
;;
(define exists? (p? x)
    (foldr (lambda (y z) (or (p? y) z)) #f x))
            
(check-expect (exists? even? '(1 3 5 7)) #f)
(check-expect (exists? even? '(1 3 4 5 7)) #t)

;; all? (p? y)
;;
;; returns true if all elements in list x satisfy the function p?
;;
(define all? (p? x)
    (foldr (lambda (y z) (and (p? y) z)) #t x))

(check-expect (all? even? '(1 3 5 7)) #f)
(check-expect (all? even? '(1 3 4 5 7)) #f)
(check-expect (all? even? '(4 2 6 8)) #t)

;;
;; PROBLEM - 19
;;

;;GIVENS
(val emptyset (lambda (x) #f))
(define member? (x s) (s x))

;; evens
;;
;; set that contains all evens
;;
;; LAWS
;;   anything even mod 2 is 0        
(val evens (lambda (x) (= (mod x 2) 0)))
        
(check-expect (evens 2) #t)
(check-expect (evens 3) #f)
(check-expect (evens 10) #t)

;; two-digit
;;
;; set that contains all two-digit (positive) numbers
;;
;; LAWS
;;   numbers greater than 9 and less than 100 are two-digits
(val two-digit (lambda (x) (= (> x 9) (< x 100))))
        
(check-expect (two-digit 0) #f)
(check-expect (two-digit 9) #f)
(check-expect (two-digit 99) #t)
(check-expect (two-digit 101) #f)

;; add-element
;;
;; add an element x to a set s
;;
;; LAWS
;;     
(define add-element (x s)
        (lambda (y) (or (equal? x y) (member? x s))))
        
(check-expect (member? 1 (add-element 1 emptyset)) #t)
(check-expect (member? 3 (add-element 2 emptyset)) #f) 

;; union
;;
;; computes the union of two sets 
;;
;; LAWS
;;    xE{s1 U s2}
(define union (s1 s2)
        (lambda (x) (or (s1 x) (s2 x))))
        
(check-expect (member? 2 (union evens two-digit)) #t)
(check-expect (member? 3 (union evens two-digit)) #f)
(check-expect (member? 11 (union evens two-digit)) #t)

;; inter
;;
;; computes the intersection of two sets
;;
;; LAWS
;;    xE{s1 n s2}
(define inter (s1 s2)
        (lambda (x) (and (s1 x) (s2 x))))
        
(check-expect (member? 2 (inter evens two-digit)) #f)
(check-expect (member? 12 (inter evens two-digit)) #t)
(check-expect (member? 13 (inter evens two-digit)) #f)

;; diff s1 s2
;; 
;; computes the set that contains every element of s1 that is not also in s2
;;
;; LAWS
;;    xE {s1 - s2}
(define diff (s1 s2)
        (lambda (x) (and (s1 x) (not (s2 x)))))
        
(check-expect (member? 2 (diff evens two-digit)) #t)
(check-expect (member? 10 (diff evens two-digit)) #f)
(check-expect (member? 97 (diff evens two-digit)) #f)


;; make-set-ops (..)
;;
;; Make 6 inputs into a list those 6 inputs
;;
(define make-set-ops (a b c d e f)        
        (list6 a b c d e f))

;; set-ops-from (eq?)
;;
;; creates set functions and stores as local vars then calls make-set-ops
;; on those vars
;;
(define set-ops-from (eq?)
        (let ([empty (lambda (x) #f)]
              [member? (lambda (x s) (s x))]
              [add (lambda (x s) (lambda (y) (or (y s) (eq? x y))))]
              [union (lambda (x s1 s2) (or (s1 x) (s2 x)))]
              [inter (lambda (x s1 s2) (and (s1 x) (s2 x)))]
              [diff (lambda (x s1 s2) (and (s1 x) (not (s2 x))))])
              
          (make-set-ops empty member? add union inter diff)))

;;GIVEN DEFINITIONS
(record set-ops (empty member? add-element union inter diff))
(val atom-set-ops (set-ops-from =))
(val atom-emptyset      (set-ops-empty atom-set-ops))
(val atom-member?      (set-ops-member? atom-set-ops))
(val atom-add-element  (set-ops-add-element atom-set-ops))
(val atom-union (set-ops-union atom-set-ops))
(val atom-inter (set-ops-inter atom-set-ops))
(val atom-diff (set-ops-diff atom-set-ops))
;;GIVEN TESTS
(check-assert (procedure? set-ops-from))
(check-assert (set-ops? (set-ops-from =)))


;;
;; PROBLEM: A
;;
;; f-functional
;;
;; built to resemble f-imperative in Impcore
;; Function takes input y and tests local var x with y as inputs to p?
;; if true, then x and y are passed through g and the result is tested again
;; until (p? x y) is false at which point it calls h passing x and y
;;
(define f-functional (y)
        (letrec ((x e)
                 (test (lambda (x) 
                           (if (p? x y) 
                               (test (g x y)) 
                               (h x y)))))
                 (test x)))

;;
;; PROBLEM: F
;;                 
;; flip (f)
;;
;; flips the function
;;
;; LAWS
;;  (3 > 2) ->flip-> (2 > 3)
;;  (a > b) ->flip-> (b > a) or (a < b)
(define flip (f)
        (lambda (a b) (f b a)))
        
(check-expect ((flip <) 3 4) #f)
(check-expect ((flip <=) 3 4) #f)
(check-expect ((flip append) '(a b c) '(1 2 3)) '(1 2 3 a b c))


;; INPUT VALIDATION
;;
;; GIVEN FUNCTIONS:
(define bound-in? (key pairs)
       (if (null? pairs)
           #f
           (|| (= key (alist-first-key pairs))
               (bound-in? key (cdr pairs)))))

;; V1 [Contract taken from the Guideline]
;; faults/none
;; 
;; an analyzer that always returns the empty list of faults, 
;; no matter what the response.
;;
(val faults/none (lambda (R) '()))

(check-expect (faults/none 'R) '())

;; V2
;; faults/always 
;;
;; takes one argument (a fault F), and it returns an analyzer that finds
;; fault with every response. 
;;   No matter what the response, the analyzer returns a singleton list
;;   containing the fault F
;;
(define faults/always (F)
        (lambda (R) (cons F '())))
        
(check-expect (procedure? (faults/always 'nobutton)) #t)
(check-expect ((faults/always 'nobutton) '()) '(nobutton))

;; V3
;; faults/equal 
;;
;; takes two arguments, a key k and a value v, and it returns an analyzer that
;; finds fault if the response binds k to v. 
;;    When given a response R, the analyzer returns an empty set of faults 
;;    unless key k is bound to value v in R. 
;;    If key k is bound to value v, the analyzer returns a singleton list
;;    containing the fault k.
;;
;; LAWS
;;    if k->v --> '(k)
(define faults/equal (k v)
        (lambda (R) (if (equal? (find k R) v)
                        (cons k '())
                        '())))
                        
(check-expect (bound-in? 'nobutton '((nobutton 4) (nobutton 5))) #t)
(check-expect (find 'nobutton '((nobutton 5))) 5)
(check-expect (equal? (find 'nobutton '((nobutton 5))) 5) #t)
(check-expect ((faults/equal 'nobutton 'x) '((nobutton y) (xy a))) '())
(check-expect ((faults/equal 'nobutton 'x) '((button z) (nobutton x))) 
                                                          '(nobutton))

;; V4
;; faults/both 
;;
;; takes two analyzers a1 and a2 as arguments. It returns an analyzer that, 
;; when applied to a response, finds all the faults found by analyzer a1
;; and also all the faults found by analyzer a2, returning them together
;; in a single set.
;;
(define faults/both (a1 a2)
        (lambda (R) (append (a1 R) (a2 R))))
        
(check-expect ((faults/both (faults/equal 'badsubmit_asst '...)
                            (faults/equal 'info #f)) 
                   '((xyz abc) (badsubmit_asst ...) (info #f))) 
                    '(badsubmit_asst info))

;; V5 
;; faults/switch 
;;
;; key k determines which field of the response is used to make a decision. 
;; When the analyzer is given a response, the value associated with key k
;; in the response is looked up in the analyzer table, and the resulting 
;; analyzer is used to find faults in the response.
;;
(define faults/switch (k T)
        (lambda (R) ((find (find k R) T) R)))


;;REGRADE GIVEN VALIDATOR
(val regrade-analyzer
  (faults/switch 'why
    (bind         'photo
                  faults/none
      (bind       'badsubmit
                   (faults/both (faults/equal 'badsubmit_asst '...)
                                 (faults/equal 'info #f))
        (bind     'badgrade
                  (faults/both
                      (faults/equal 'badgrade_asst '...)
                      (faults/both
                         (faults/equal 'info #f)
                         (faults/equal 'problem #f)))
            (bind '#f
                  (faults/always 'nobutton)
                  '()))))))
                          
;;REGRADE TESTERS
(check-expect (regrade-analyzer '([why photo])) '())

(check-expect (regrade-analyzer '([why badsubmit]
                                  [info wrong-pdf]
                                  [badsubmit_asst opsem]))
              '())

(check-expect (regrade-analyzer '([why badsubmit]
                                  [badsubmit_asst opsem]
                                  [info #f]))
              '(info))

(check-expect (regrade-analyzer '([why badsubmit]
                                  [info wrong-pdf]
                                  [badsubmit_asst ...]))
              '(badsubmit_asst))

(check-expect (regrade-analyzer '([why badsubmit]
                                  [info #f]
                                  [problem #f]
                                  [badsubmit_asst ...]))
              '(badsubmit_asst info))

(check-expect (regrade-analyzer '([why badgrade]
                                  [info #f]
                                  [problem #f]
                                  [badgrade_asst impcore]))
              '(info problem))

;; V6
;; travel-validator
;; 
;; takes a travel plan and returns the field at fault if an error is discovered
;;
(val travel-validator
        (faults/switch 'type 
            (bind 'round-trip 
                    (faults/both 
                            (faults/equal 'out_date #f) 
                             (faults/equal 'return_date #f))
             (bind 'one-way 
                     (faults/equal 'out_date #f)
              (bind '#f 
                    (faults/always 'type) 
                    '())))))
            
(check-expect
   (travel-validator '([type round-trip]
                       [out_date 4/11/2019]
                       [return_date #f]))
   '(return_date))

(check-expect
   (travel-validator '([type #f]
                       [out_date 4/11/2019]
                       [return_date #f]))
   '(type))
   
(check-expect
   (travel-validator '([type round-trip]
                       [out_date #f]
                       [return_date xxx]))
   '(out_date))
   
(check-expect
   (travel-validator '([type one-way]
                       [out_date #f]
                       [return_date #f]))
   '(out_date))
   
;;
;; PROBLEM - O
;;
;; ordered-by?
;;
;; Function takes one argument, a comparison function c, and returns a predicate
;; that tells if a list of values is totally ordered by that relation.
;;
;; LAWS
;;    (f < '(x)) = #t
;;    (f < '(1 2) = (1 < 2)
;;        (f < x) = ((car x) < (cadr x))
;;    (f < '(1 2 3)) = (1 < 2) and (2 < 3)
;;        (f < x) = ((car x) < (cadr x)) and (f < (cdr x))
(define ordered-by? (c)
        (letrec ([sorted (lambda (x) 
                            (if (null? (cdr x))
                                #t
                                (if (pair? (cddr x))
                                    (and (c (car x) (cadr x)) (sorted (cdr x)))
                                    (c (car x) (cadr x)))))])

                sorted))
                
(check-expect ((ordered-by? <) '(1 2 3 4 5 6 7 8 9 10)) #t)
(check-expect ((ordered-by? <=) '(1 2 3)) #t)
(check-expect ((ordered-by? <) '(3 2 1)) #f)
(check-expect ((ordered-by? >=) '(3 2 1)) #t)
(check-expect ((ordered-by? >=) '(3 3 3)) #t)
(check-expect ((ordered-by? =) '(3 3 3)) #t)
(check-expect ((ordered-by? >) '(4 3 4)) #f)