;; tell if list has 2 consecutive elements that are duplicates

(define duplicates? (x)
    (if (null? (cdr x))
        #f
        (if (equal? (car x) (cadr x))
                #t
                (duplicates? (cdr x)))))
        
            
(check-expect (duplicates? '(1, 2, 3, 4, 5, 5, 6, 7, 8)) #t)
(check-expect (duplicates? '(1, 1, 2)) #t)
(check-expect (duplicates? '(a, b, c, d, e, f, f, f, g, h)) #t)
(check-expect (duplicates? '(1, 2, 3, 4, 5, 6, 7, 8)) #f)


