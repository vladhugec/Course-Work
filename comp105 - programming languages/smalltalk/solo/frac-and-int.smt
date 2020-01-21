;;******************************************************************************
;; Exercise 39(a)

(class SmallInteger SmallInteger
     ()
     (method num () self)
     (method den () 1)
  )
  
(check-expect (+ 5 1) 6)
(check-expect (* 5 2) 10)
(check-expect (/ 5 2) (/ 5 2))
(check-expect (/ 6 2) 3)