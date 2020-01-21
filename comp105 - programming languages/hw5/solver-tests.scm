;;
;; VLADIMIR HUGEC
;; COMP105 HW5
;; Feb 23, 2019
;;
;; solver-tests.scm
;;


;;
;; PROBLEM: T
;;

(record not [arg])   ;; OK if these are duplicates
(record or  [args])
(record and [args])


; (~X v X) n Y
; This case tests simple functional correctness of all three operators
(val f1 (make-and (list2 (make-or (list2 (make-not 'x) 'x)) 'y)))
(val s1 '((x #f) (y #t)))

; Z v Y v (A n B)
; This case tests longer inputs and nested operators
(val f2 (make-or (list3 'z 'y (make-and (list2 'a 'b)))))
(val s2 '((z #t)))

; (~X n ~Y) n (X v Y)
; This further tests composition of logical operators and correct execution
(val f3 (make-and (list2 (make-and (list2 (make-not 'x) (make-not 'y)))
                             (make-or (list2 'x 'y)))))
(val s3 'no-solution)


