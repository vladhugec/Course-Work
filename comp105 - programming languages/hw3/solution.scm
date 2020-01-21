;;
;; VLADIMIR HUGEC
;; comp105 - HW3
;;

;; count (x s)
;;
;; Count compares x to each element in the list s
;;     count DOES NOT compare x to sublist elements present in s
;; Returns the # of instances of x in list s
;;
;;
;; LAWS FOR count
;;
;; (count x '() = 0
;; (count x s) = (count x (car x)) + (count x (cdr s))
;;
(define count (x s)
    (if (null? s)
        0
        (if (= x (car s))
            (+ 1 (count x (cdr s)))
            (count x (cdr s)))))
            
;; test for count
(check-expect (count 'a '(1 b a a a(c a))) 3)

;; countall (x s)
;;
;; countall x to each element in the list s and each element in
;; sublists of s
;; Returns the # of instances of x in list s and s's sublists
;;
;;
;; LAWS FOR countall
;; 
;; (countall x '()) = 0
;; (countall x s) = (countall x (car s)) + (countall x (cdr s))
;;
(define countall (x s)
    (if (null? s)
        0
        (if (pair? (car s))
            (+ (countall x (car s)) (countall x (cdr s)))
            (if (= x (car s))
                (+ 1 (countall x (cdr s)))
                (countall x (cdr s))))))
                
;; test for countall
(check-expect (countall 'a '(1 b a (c a) a (b a b a))) 5)
(check-expect (countall 'a '((a a) (b a) (b b))) 3)


;; mirror (s)
;; 
;; given list s, mirror returns the same list but in
;; reverse, this includes all sublist of s as well
;; e.g (1 (2 3) 4) -> (4 (3 2) 1)
;;
;;
;; LAWS FOR mirror
;;
;; (mirror '()) = '()
;; (mirror a) = (append (mirror (cdr a)) (car a))
;; (mirror ((a)) = (append (mirror (cdr a)) (mirror (car a)))
;;
(define mirror (s)
    (if (null? s)
        s
        (if (pair? (car s))
            (append (mirror (cdr s)) (cons (mirror (car s)) '()))
            (append (mirror (cdr s)) (list1 (car s))))))
        
;; test for mirror
(check-expect (mirror '(1 2 3 4 5)) '(5 4 3 2 1))
(check-expect (mirror '((a (b 5)) (c d) e)) '(e (d c) ((5 b) a)))



;; flatten (s)
;;
;; flatten collapses all sublists of s into one list and returns
;;
;;
;; LAWS FOR flatten
;;
;; (flatten '()) = '()
;; (flatten '(a) = (append (car a) (flatten (cdr a)))
;; (flatten '((a)) = (append (flatten (car a)) (flatten (cdr a)))
;;
(define flatten (s)
    (if (null? s)
        s
        (if (pair? (car s))
            (append (flatten (car s)) (flatten (cdr s)))
            (append (list1 (car s)) (flatten (cdr s))))))

;; tests for flatten:
(check-expect (flatten '((I Ching) (U Thant) (E Coli))) 
                             '(I Ching U Thant E Coli))            
(check-expect (flatten '(((((a)))))) '(a))
(check-expect (flatten '((a b) ((c d) e))) '(a b c d e))



;;contig-list (x y) [HELPER FUNCTION]
;;
;; contig-list checks if the remaining elements of x are equivilent
;; to the corrisponding elements of y
;;
;;
;; LAWS FOR contig-list
;;
;; (contig-list '() y) = #t
;; (contig-list x y) = ((car x) = (car y)) & (contig-list (cdr x) (cdr y)
;;
(define contig-list (x y)
    (if (null? x)
        #t
        (if (= (car x) (car y))
            (contig-list (cdr x) (cdr y))
            #f)))
            
;;contig-sublist? (x y)
;;
;; contig-sublist? checks if x is a continuous segment of y
;;   returns #t if x is found in y
;;           #f other wise
;;    calls contig-list when the first element in x is found in y
(define contig-sublist? (x y)
    (if (= (car x) (car y))
        (contig-list (cdr x) (cdr y))
        (contig-sublist? x (cdr y))))
                
;; test for contig-sublist?
(check-expect (contig-sublist? '(a b c) '(x a y b z c)) #f)
(check-expect (contig-sublist? '(a y b) '(x a y b z c)) #t)
(check-expect (contig-sublist? '(x) '(x a y b z c)) #t)

;;sublist? (x y)
;;
;; sublist? checks whether elements of x are pre in the same order
;; in y but regardless of whether or not they are continuous
;;
;;
;; LAWS FOR sublist
;;
;; (sublist? '() y) = #t
;; (sublist? x '()) = #f
;; (sublist? x y) = [(equal? (car x) (car y)) AND (sublist? (cdr x) (cdr y))]
;; (sublist? x y) = [{not (equal? (car x) (car y))} AND (sublist? x (cdr y))]
;;
(define sublist? (x y)
    (if (null? x)
        #t
        (if (null? y)
            #f
            (if (= (car x) (car y))
                (sublist? (cdr x) (cdr y))
                (sublist? x (cdr y))))))

;; test for sublist?
(check-expect (sublist? '(a b c) '(x a y b z c)) #t)
(check-expect (sublist? '(a y b) '(x a y b z c)) #t)
(check-expect (sublist? '(a z b) '(x a y b z c)) #f)
(check-expect (sublist? '(x y z) '(x a y b z c)) #t)


;; PROBLEM: 10

;; takewhile (x y)
;;
;; returns the subset of y that continuosly evalute to #t
;; when inputted into the function x?
;;
;; LAWS FOR takewhile
;; 
;; (takewhile x y) = (car y) + (takewhile x (cdr y))
(define takewhile (x y)
    (if (x (car y))
        (cons (car y) (takewhile x (cdr y)))
        '()))
        
(define even? (x) (= (mod x 2) 0))
(check-expect (takewhile even? '(2 4 6 7 8 10 12)) '(2 4 6))

;; dropwhile (x y)
;;
;; the opposite of takewhile
;; returns the subset of y left over after x? returns false
;;
;; LAWS FOR
;;
;; (dropwhile x y) = (dropwhile x (cdr y))
(define dropwhile (x y)
    (if (x (car y))
        (dropwhile x (cdr y))
        (append y '())))
        
(check-expect (dropwhile even? '(2 4 6 7 8 10 12)) '(7 8 10 12))


;; PROBLEM: B

;; take (n xs)
;;
;; take returns the longest prefix of xs that contains at most n elements.
;; 
;; LAWS FOR take
;; 
;; (take 0 xs) = '()
;; (take n xs) = '((car xs) (take (n-1) (cdr xs)))
;;
(define take (n xs)
    (if (= n 0)
        '()
        (cons (car xs) (take (- n 1) (cdr xs)))))

;; drop (n xs)
;;
;; drop returns a list with n elements from the front of the list removed
;;
;; LAWS FOR drop
;;
;; (drop 0 xs) = xs
;; (drop n xs) = (drop (n-1) (cdr xs))
;;
(define drop (n xs)
    (if (= n 0)
        xs
        (drop (- n 1) (cdr xs))))
        
(check-expect (drop 5 '(2 4 6 7 8 10 12)) '(10 12))
(check-expect (take 5 '(2 4 6 7 8 10 12)) '(2 4 6 7 8))
(check-expect (append (take 5 '(2 4 6 7 8 10 12)) (drop 5 '(2 4 6 7 8 10 12))) 
                                                           '(2 4 6 7 8 10 12))
    
    
;; PROBLEM: C

;; zip (x y)
;;
;; zip returns a list where the elements of x are paired with their
;; corresponding elements in y
;; e.g x=(1 2) & y =(a b) -> ((1 a)(2 b))
;;
;; LAWS FOR zip
;;
;; (zip '() y) = '()
;; (zip x y) = '( [(car x)(car y)] [(zip (cdr x) (cdr y))] )
;;
(define zip (x y)
    (if (null? x)
        '()
        (cons (cons (car x) (cons (car y) '())) (zip (cdr x) (cdr y)))))
            
(check-expect (zip '(1 2 3) '(a b c)) '((1 a) (2 b) (3 c)))

;; first-values [HELPER FUNCTION]
;;
;; constructs a list of all the first values of each sublist of x
;;
;; LAWS FOR first-values
;; 
;; (first-values '()) = '()
;; (first-values x) = '((caar x) (first-values (cdr x)))
;;
(define first-values (x)
    (if (null? x)
        '()
        (cons (caar x) (first-values (cdr x)))))

;; second-values [HELPER FUNCTION]
;;
;; constructs a list of all the second values of each sublist of x
;;
;; LAWS FOR second-values
;; 
;; (second-values '()) = '()
;; (second-values x) = '((cdar x) (second-values (cdr x)))
;;
(define second-values (x)
    (if (null? x)
        '()
        (append (cdar x) (second-values (cdr x)))))

;;unzip (x)
;;
;; given a list of lists, unzip collapses the lists into
;; lists of corresponging values
;; e.g ((A a)(B b)) -> ((A B) (a b)) 
;;
;; LAWS FOR unzip
;;
;; (unzip '()) = '()
;; (unzip '((x)(y)) = '( [(first-values x)] [(second-values x)] )
;;
(define unzip (x)
    (if (null? x)
        '()
        (append (cons (first-values x) '()) (cons (second-values x) '()))))

(check-expect (unzip '((I Magnin) (U Thant) (E Coli))) 
                      '((I U E) (Magnin Thant Coli)))


;; PROBLEM: LP

;; singleton? (x)
;;
;; returns #t if x is a list containing only a single element
(define singleton? (x)
    (null? (cdr x)))
    
(check-expect (singleton? '(1 2 3 4 5 6 7 8 9 11 11 1 1 1 1 1 1 1 1 1 1)) #f)
(check-expect (singleton? '(1)) #t)

;; has-n-elements? (xs n)
;;
;; checks if the list xs has exactly n elements
;; returns #t if the length of xs == n
;;         #f otherwise
;;
;; LAWS FOR has-n-elements
;;
;; (has-n-elements xs 0) = (null? xs)
;; (has-n-elements xs n) = (has-n-elements (cdr xs) (n-1))
;;
(define has-n-elements? (xs n)
    (if (= n 0)
        (null? xs)
        (has-n-elements? (cdr xs) (- n 1))))
        
(check-expect (has-n-elements? '(1 2 3 4 5) 5) #t)
(check-expect (has-n-elements? '(1 2 3 4 5 6 7 8 9 10 11 12) 12) #t)
(check-expect (has-n-elements? '(1 2 3 4 5) 2) #f)

;; nearly-same-lengths? (xs ys)
;;
;; returns #t if the lengths of xs and ys differ by at most 1 values
;;
;; LAWS FOR nearly-same-lengths
;;
;; (nearly-same-lengths? '() '()) = #t
;; (nearly-same-lengths? '() y) = (null? (cdr ys))
;; (nearly-same-lengths? x '()) = (null? (cdr xs))
;; (nearly-same-lengths? x y) = (nearly-same-lengths? (cdr xs) (cdr ys))
;;
(define nearly-same-lengths? (xs ys)
        (if (null? xs)
            (if (null? ys)
                #t
                (null? (cdr ys)))
            (if (null? ys)
                 (null? (cdr xs))
                 (nearly-same-lengths? (cdr xs) (cdr ys)))))
                 
(check-expect (nearly-same-lengths? '(1 2 3) '(1)) #f)
(check-expect (nearly-same-lengths? '(a b) '(c d e f)) #f)
(check-expect (nearly-same-lengths? '(1) '(1 2 3)) #f)
(check-expect (nearly-same-lengths? '(1 2) '(1 2 3)) #t)
(check-expect (nearly-same-lengths? '(1 2 3 4) '(1 2)) #f)
(check-expect (nearly-same-lengths? '(1 2 3 4 5 6) '(1 2 3 4 5)) #t)

;; PROBLEM: N

;; 1)
;;    Define, LIST(A) = {'()} U {(cons a as) | a∈A AND as∈LIST(A)}
;;        Then define LIST1(A) as a LIST(A) containing only one element
;;        where 'a' can either be a list itself (a∈LIST(A)) or it can be a
;;        single element (let S represent a single element):
;;    
;;     LIST1(A) = LIST(A) INTERSECT {(cons a as) | (a∈LIST(A) OR a=S) & as='()}
;; 
;;        or in other words, LIST1(A) is the set of all LIST(A)'s where
;;        'as' is the empty list '() and a is a single element or a single
;;        list.
;; 2)
;;    If LIST1(A) denotes a single element list, let V be a sigle element
;;    and let S be the empty set '() and Z be a list of arbitrary length
;;    Then:
;;         V = {ATOM} U {(cons a as)| (a=Z OR a=ATOM) AND as=s)
;;    Therefore we know that:
;;            (cons V S)∈LIST1(A)
;;    Since V is any signle element and S is the empty set
;;    then all LIST1(A) can be defined by the set of all lists
;;    that can be constructed by V and S.
;;    LIST1(A) = {(cons a as | a∈V AND as∈S}


;; PROBLEM: G

;; has? (sx a) [HELPER FUNCTION TAKEN FROM TEXTBOOK]
;;
;; returns #t if a is present in sx
(define has? (sx a)
    (if (atom? sx)
        (= sx a)
        (or (has? (car sx) a) (has? (cdr sx ) a))))

;; check-each-against (x y) [HELPER FUNCTION]
;;
;; returns #t if each element of x is found in y
;;
;; LAWS FOR check-each-against
;;
;; (check-each-against '() y z) = #t
;; (check-each-against x y z) = (check-each-against (cdr x) y z)
;;
(define check-each-against (x y z)
    (if (null? x)
        #t
        (if (even? (countall (car x) z))
            (if (has? y (car x))
                (check-each-against (cdr x) y z)
                #f)
             #f)))

;; permutation? (x y)
;;
;; checks if x is a permutation of y
;;    returns #t if so
;;            #f otherwise
;;
;; LAWS FOR permutation
;;
;; == (check-each-against (x y z)) (check-each-against (y x z))
;;
(define permutation? (x y)
    (and (check-each-against x y (zip x y)) (check-each-against y x (zip x y))))
    
(check-expect (permutation? '(a b c) '(c b a)) #t)
(check-expect (permutation? '(a b b) '(a a b)) #f)
(check-expect (permutation? '(a b c) '(c b a d)) #f)
(check-expect (permutation? '(a a b) '(b a a)) #t)


;; PROBLEM: F

;; remove-one-copy (x y)
;;
;; removes a single instance of element x present in list y
;;
;; LAWS FOR remove-one-copy
;; [+ in here will mean "appended to"]
;;
;; (remove-one-copy x y) = (remove-one-copy x (cdr y)) + (car y)
;; (remove-one-copy x x) = (cdr x)
;;
(define remove-one-copy (x y)
    (if (has? y x)
        (if (equal? (car y) x)
            (cdr y)
            (append (remove-one-copy x (cdr y)) (cons (car y) '())))
        (error 'removed-an-absent-item)))
        
(check-expect (remove-one-copy 'a '(a b c)) '(b c))
(check-expect (remove-one-copy 'a '(a a b b c c)) '(a b b c c))
(check-error (remove-one-copy 'a '(x y z)))
(check-expect (remove-one-copy 'y '(y x z)) '(x z))

;; PROBLEM: D

;; arg-max (x y)
;;
;; arg-max returns the largest result of applying x to each element in y
;;
;; LAWS FOR arg-max
;; 
;; (arg-max x '()) = '(0)
;; (arg-max x y) = (arg-max x (cdr y))
;; 
(define arg-max (x y)
  (if (null? y)
      (list1 0)
      (if (> (x (car y)) (x (arg-max x (cdr y))))
          (car y)
          (arg-max x (cdr y)))))
          
(check-expect (arg-max car '((105 PL) (160 Algorithms) 
                             (170 Theory))) '(170 Theory))
    
;; PROBLEM: E

;; ALL FUNCTIONS TAKEN FROM TEXTBOOK NEEDED TO WRITE rightmost-point FOR PART E)
(define make-point (x y) (cons 'make-point (cons x (cons y '()))))
(define point? (p)
    (if (pair? p)
        (= 'make-point (car p))
        #f))
(define point-x (p)
    (if (point? p)
        (car (cdr p))
        (error (list2 p 'is-not-a-point-record))))
(define point-y (p)
    (if (point? p)
        (car (cdr (cdr p)))
        (error (list2 p 'is-not-a-point-record))))
;; END FUNCTIONS TAKEN FROM BOOK


;; drop-point
;;
;; deletes point from list unless it is the rightmost point,
;; if it is the rightmost point, return the x coordinate
;; e.g (1 2 3 4 5 6) -> (3 4 5 6) -> (5 6) -> 5
;; 
;; LAWS FOR drop-point
;; 
;; (drop-point '()) = 0
;;
(define drop-point (x)
    (if (null? x)
        0
        (if (equal? (max* x) (car x))
            (car x)
            (if (equal? (max* x) (cadr x))
                (car x)
                (drop-point (drop 2 x))))))


;; rightmost-point
;; 
;; given a list of points x, function returns the largest x coordinate
;; found in the list of points
;; 
;; LAWS FOR rightmost-point
;;
;; x = '((make-point 1 2) (make-point 5 6) (make-point 4 3)))
;; (unzip x) = '((make-point...)(1 2 3 4 5 6))
;; (cadr (unzip x)) = '(1 2 3 4 5 6)
;; (drop-point (cadr (unzip x))) = 5
;;
(define rightmost-point (x)
    (drop-point (cadr (unzip x))))
    
(check-expect (rightmost-point '((make-point 1 2) 
                                 (make-point 5 6) (make-point 4 3))) 5)
        
        
;; PROBLEM: H

;; split (x) [HELPER FUNCTION]
;;
;; split continuously removes one element from the second list in x
;; and adds it to the end of the first list until the two lists have
;; lengths that differ by at most 1.
;;
;; LAWS FOR split
;;
;;   let (car x) = z & (car x) = y
;; (split x) = x if length(z) =~= length(y)
;;           = '( [z (car y)] [(cdr y)] )
;; 
(define split (x)
    (if (nearly-same-lengths? (car x) (flatten (cdr x)))
        x
        (split (list2 (append (car x) (cons (car (flatten (cdr x))) '()))
                      (cdr (flatten(cdr x)))))))
            
;; split-list (x)
;;
;; function splits the list x into a list of 2 lists down the middle
;; if x has odd # of elements, function will return list of 2 lists whose
;; lengths differ by at most 1 element
;;
;; LAWS FOR split-list
;; 
;; (split-list '()) = '( () () )
;; (split-list x) = '( (car x) (cdr x) )
;; (split-list x) = '( [(car x) (cadr x)] (cddr x) ) 
(define split-list (x)
    (if (null? x)
        (list2 '() '())
        (if (has-n-elements? x 2)
            (list2 (cons (car x) '()) (cdr x))
            (split (list2 (cons (car x) (cons (cadr x) '())) (cddr x))))))


(check-expect (split-list '()) '(() ()))
(check-expect (split-list '(a b)) '((a) (b)))
(check-expect (split-list '(a b c d)) '((a b) (c d)))
(check-expect (split-list '(a b c d e)) '((a b) (c d e)))
(check-expect (split-list '(a b c d e f)) '((a b c) (d e f))) 
(check-expect (split-list '(a b c d e f g)) '((a b c) (d e f g)))