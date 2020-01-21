;;
;; VLADIMIR HUGEC
;; COMP105 HW5
;; Feb 23, 2019
;;
;; solution.scm
;;


;;
;; PROBLEM: L
;;

;; GIVEN DEFINITIONS
(define value? (_) #t) ;; tell if the argument is a value
(define conjoin (p? q?) (lambda (x) (if (p? x) (q? x) #f)))

;; list-of?
;;
;; returns #t if v is a list of value and each of which satisfy A?
;; Otherwise, (list-of? A? v) returns #f.
;;
;; LAWS
;;    (list-of? A? 5) == #f
;;    (list-of? A? '()) == #t
;;    (list-of? A? '(() 5) == #f
;;    (list-of? A? v) == (A? (car v)) and (list-of A? (cdr v))
;;        
(define list-of? (A? v)
    (if (number? v)
        #f
        (if (null? v)
            #t
            (and (A? (car v)) (list-of? A? (cdr v))))))
;; even        
;; Tester Helper function
(define even? (x) (= (mod x 2) 0))

(check-expect (list-of? even? '(2 4 6 8)) #t)
(check-expect (list-of? even? '(2 4 6 9)) #f)
(check-expect (list-of? even? '(2)) #t)
(check-expect (list-of? even? 2) #f)
(check-expect (list-of? number? '(2 3 4)) #t)
(check-expect (list-of? symbol? '(x y z)) #t)
(check-expect (list-of? symbol? '(x y 5)) #f)
(check-expect (list-of? boolean? '(#f #f 2)) #f)
(check-expect (list-of? number? '()) #t)
(check-expect (list-of? even? '()) #t)
(check-expect (list-of? (conjoin number? even?) (cons '() 4)) #f)

;;
;; PROBLEM: F
;;

;; GIVEN DEFINITIONS
(record not [arg])
(record and [args])
(record or [args])

;; formula?
;;
;; given an arbitrary uScheme value, returns #t if the value
;; represents a Boolean formula and #f otherwise
;;
;; LAWS
;;    (formula? sym) == #t
;;    (formula? (make-or f)) == (formula? (car (cadr v)))
;;    (formula? (make-and f)) == (formula? (car (cadr v)))
;;    (formula? (make-not f)) == (formula? (cadr v))
;;
(define formula? (v)
    (if (symbol? v)
        #t
        (if (|| (or? v) (and? v))
            (if (pair? (cadr v))
                (formula? (car (cadr v)))
                #t)
            (&& (not? v) (formula? (cadr v))))))
              
              
(check-expect (formula? 'POP) #t)
(check-expect (formula? '()) #f)
(check-expect (formula? (make-and '())) #t)
(check-expect (formula? (make-and '(x))) #t)
(check-expect (formula? (make-and 'x)) #t)
(check-expect (list-of? symbol? (cadr (make-and (list2 'x 'y)))) #t)
(check-expect (formula? (make-and (list2 'x 'y))) #t)
(check-expect (formula? (make-and (list3 (make-or 
                                          (list2 (make-not 'x) 'x)) 'y 'z))) #t)
(check-expect (formula? (make-and 
                            (list2 (make-or (list2 'x (make-not 'x))) 'y))) #t)
(check-expect (formula? (make-and (list2 'y 
                                    (make-or (list2 'x (make-not 'x)))))) #t)
(check-expect (formula? (make-and (list2 5 'x))) #f)
(check-expect (formula? '(x y z)) #f)

;;
;; PROBLEM: E
;;

;; eval-formula
;;
;; Given a formula and an enviornment, if the formula is satisfied
;; in the given environment -> returns #t 
;; otherwise it returns #f
;; NOTE: function may be called only if all variables in f are bound in env
;;
;; LAWS:
;;    (eval-formula (make-or f)) == (|| (eval-formula (car (or-args f)) env) 
;;                                    (eval-formula (cadr (or-args f)) env))
;;    (eval-formula (make-and f)) == (&& (eval-formula (car (and-args f)) env) 
;;                                     (eval-formula (cadr (and-args f)) env))
;;    (eval-formula (make-not f)) == (not (find (not-arg f) env))
;;    (eval-formula f) == (find f env)
;;
(define eval-formula (f env)
    (if (formula? f)
        (if (or? f)
            (|| (eval-formula (car (or-args f)) env) 
                (eval-formula (cadr (or-args f)) env)) 
            (if (and? f)
                (&& (eval-formula (car (and-args f)) env) 
                    (eval-formula (cadr (and-args f)) env))
                (if (not? f)
                    (not (find (not-arg f) env))
                    (find f env))))
         #f))
    
    
(check-expect (eval-formula (make-and (list2 'x 'y)) '((x #t) (y #t))) #t)
(check-expect (eval-formula (make-and (list2 
                                        (make-or (list2 (make-not 'x) 'x)) 'y)) 
                 '((x #t) (y #f))) #f)

(check-expect (eval-formula (make-and 
                                (list2 (make-or (list2 (make-not 'x) 'x))
                                                             'y)) 
                 '((x #t) (y #t))) #t)
(check-expect (eval-formula (make-and (list2 (make-not 'x) 'y)) 
                                                    '((x #f) (y #t))) #t)
(check-expect (eval-formula (make-or (list2 (make-not 'x) 'x)) 
                                                    '((x #t) (y #f))) #t)
(check-expect (eval-formula (make-or (list2 (make-and 
                                                (list2 (make-not 'x) 'y)) 'x)) 
                 '((x #f) (y #t))) #t)

(check-expect (eval-formula (make-and (list3 (make-or (list3 'x 'y 'z)) 
                                         (make-or (list3 (make-not 'x) 
                                                         (make-not 'y)
                                                         (make-not 'z))) 
                                         (make-or (list3 'x 'y 
                                                    (make-not 'z))))) 
                 '((x #t) (y #f))) #t)

(check-expect (eval-formula (make-and (list2 
                                        (make-and (list2 (make-not 'x) 
                                                         (make-not 'y))) 
                                        (make-or (list2 'x 'y)))) 
                 '((x #f) (y #f))) #f)

;;
;; PROBLEM: S
;;

;; GIVEN FUNCTIONS:
(define bound-in? (key pairs)
       (if (null? pairs)
           #f
           (|| (= key (alist-first-key pairs))
               (bound-in? key (cdr pairs)))))

;; find-formula-true-asst

(define find-formula-true-asst (f fail succ)
    (letrec
        ;; find-formula-asst
        ;;
        ;; extends assignment cur to find an assignment that
        ;; makes the single formula equal to bool.
        ;;
        ;; LAWS:
        ;;
        ;; (find-formula-asst x bool cur fail succeed) == 
        ;;    == (find-formula-symbol x bool cur fail succeed),
        ;;                                            where x is a symbol
        ;;        ;; (find-formula-asst (make-not f)  bool cur fail succeed) ==
        ;;    == (find-formula-asst) (cadr formula) (not bool) cur fail succeed)
        ;;        ;; (find-formula-asst (make-or  fs) #t cur fail succeed) ==
        ;;    == (find-any-asst (cdr formula) bool cur fail succeed)
        ;; 
        ;; (find-formula-asst (make-or  fs) #f cur fail succeed) ==
        ;;    == (find-all-asst (cdr formula) bool cur fail succeed)
        ;;
        ;; (find-formula-asst (make-and fs) #t cur fail succeed) ==
        ;;    == (find-all-asst (cdr formula) bool cur fail succeed)
        ;;
        ;; (find-formula-asst (make-and fs) #f cur fail succeed) ==
        ;;    (find-any-asst (cdr formula) bool cur fail succeed)
        ;;
        ((find-formula-asst 
          (lambda (formula bool cur fail succeed)
            (if (symbol? formula)
                (find-formula-symbol formula bool cur fail succeed)
                (if (not? formula)
                    (find-formula-asst 
                                (cadr formula) (not bool) cur fail succeed)
                    (if (or? formula) 
                        (if (= bool #t)
                           (find-any-asst (cadr formula) bool cur fail succeed)
                           (find-all-asst (cadr formula) bool cur fail succeed))
                        (if (and? formula)
                            (if (= bool #f)
                                (find-any-asst 
                                      (cadr formula) bool cur fail succeed)
                                (find-all-asst 
                                      (cadr formula) bool cur fail succeed))
                            (find-formula-asst 
                                   (car formula) bool cur fail succeed)))))))
        ;; find-all-asst
        ;;
        ;; extends cur to find an assignment that makes every
        ;; formula in the list formulas equal to bool.
        ;;
        ;; LAWS:
        ;;
        ;; (find-all-asst '() bool cur fail succeed) == (succeed cur fail) 
        ;; (find-all-asst (cons f fs) bool cur fail succeed) ==
        ;;    == (find-formula-asst f bool cur fail (lambda (cur resume)
        ;;                         (find-all-asst fs bool cur resume succeed)))
        ;; 
         (find-all-asst 
          (lambda (formulas bool cur fail succeed)
            (if (not (null? formulas))
                ;; all have to be true, if one is true check the rest of list
                (find-formula-asst (car formulas) bool cur fail
                 (lambda (cur resume) (find-all-asst 
                                       (cdr formulas) bool cur resume succeed)))
                ;; all are true in list
                (succeed cur fail))))
        
        ;; find-any-asst
        ;;
        ;; extends cur to find an assignment that makes any
        ;; one of the formulas equal to bool.
        ;;
        ;; LAWS:
        ;;
        ;; (find-any-asst '() bool cur fail succeed) == (fail)
        ;; (find-any-asst (cons f fs) bool cur fail succeed) ==
        ;;    == (find-formula-asst f bool cur 
        ;;         (lambda () (find-any-asst fs bool cur fail succeed)) succeed)
        ;;
         (find-any-asst 
          (lambda (formulas bool cur fail succeed)
            (if (not (null? formulas))
                (find-formula-asst (car formulas) bool cur
                                    ;; fail continuation checks rest of formula
                             (lambda () (find-any-asst 
                                         (cdr formulas) bool cur fail succeed))
                                     succeed)
                ;; no true found in entire list
                (fail))))

        ;; find-formula-symbol
        ;;
        ;; If x is bound to bool in cur, function succeeds with environment
        ;;     cur and resume continuation fail
        ;; If x is bound to (not bool) in cur, function fails
        ;; If x is not bound in cur, fucntion extends cur with a binding 
        ;;    of x to bool, then succeeds with the extended environment and
        ;;    resume continuation fail
        ;;
        ;; LAWS:
        ;; 
        ;;     where x is not bound in cur
        ;; (find-formula-symbol x bool cur fail succeed) ==
        ;;    == (succeed (bind x bool cur) fail)
        ;; 
        ;;     where x is bool in cur
        ;; (find-formula-symbol x bool cur fail succeed) ==
        ;;    == (succeed cur fail)
        ;;
        ;;     where x is (not bool) in cur
        ;; (find-formula-symbol x bool cur fail succeed) == 
        ;;    == (fail)
         (find-formula-symbol 
          (lambda (x bool cur fail succeed)
            (if (bound-in? x cur)
                (if (= (find x cur) bool)
                    (succeed cur fail)
                    (fail))
                (succeed (bind x bool cur) fail)))))
        (find-formula-asst f #t '() fail succ)))
        
;; GIVEN TESTERS
(check-assert (procedure? find-formula-true-asst))
(check-error (find-formula-true-asst))
(check-error (find-formula-true-asst 'x))
(check-error (find-formula-true-asst 'x (lambda () 'fail))) ;not2args 
(check-error
 (find-formula-true-asst 'x (lambda () 'fail) (lambda (c r) 'succeed) 'z))
 
(check-error (find-formula-true-asst 'x (lambda () 'fail) (lambda () 'succeed)))
;; success continuation expects 2 arguments, not 0
(check-error (find-formula-true-asst 'x (lambda () 'fail) 
                                        (lambda (_) 'succeed)))
    ; success continuation expects 2 arguments, not 1
(check-error (find-formula-true-asst
                   (make-and (list2 'x (make-not 'x)))
                   (lambda (_) 'fail)
                   (lambda (_) 'succeed)))
                   
(check-expect   ; x can be solved
   (find-formula-true-asst 'x
                           (lambda () 'fail)
                           (lambda (cur resume) 'succeed))
    'succeed)
    
(check-expect   ; x is solved by '((x #t))
   (find-formula-true-asst 'x
                           (lambda () 'fail)
                           (lambda (cur resume) (find 'x cur)))
    #t)
(check-expect   ; (make-not 'x) can be solved
   (find-formula-true-asst (make-not 'x)
                           (lambda () 'fail)
                           (lambda (cur resume) 'succeed))
    'succeed)
(check-expect   ; (make-not 'x) is solved by '((x #f))
   (find-formula-true-asst (make-not 'x)
                           (lambda () 'fail)
                           (lambda (cur resume) (find 'x cur)))
    #f)

(check-expect   ; (make-and (list2 'x (make-not 'x))) cannot be solved
   (find-formula-true-asst (make-and (list2 'x (make-not 'x)))
                                       (lambda () 'fail)
                                       (lambda (cur resume) 'succeed))
    'fail)


;; PERSONAL TEST
(check-expect
    (find-formula-true-asst (make-and (list2 
                                        (make-or (list2 (make-not 'x) 'x)) 'y))
                             (lambda () 'fail) (lambda (cur resume) cur))
        '((x #f) (y #t)))

(check-expect
    (find-formula-true-asst (make-or (list3 'z 'y (make-and (list2 'a 'b))))
                             (lambda () 'fail) (lambda (cur resume) cur))
        '((z #t)))

(check-expect
    (find-formula-true-asst (make-and (list2 (make-and 
                                              (list2 (make-not 'x)
                                                     (make-not 'y))) 
                                          (make-or (list2 'x 'y))))
                             (lambda () 'fail) (lambda (cur resume) cur))
        'fail)