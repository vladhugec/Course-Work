;;******************************************************************************
;; Exercise T

;; 3 tests will test only class Natural

; summary -> Simple test for addition
(check-print
    (+ (new: Natural 5) (new: Natural 10)) 15)

; summary -> Test for add subtract mult and div and equals
(check-print
    (if (= (+ (sdiv: (new: Natural 20) 4) (* (new: Natural 5) (new: Natural 3)))
           (sdiv: (new: Natural 100) 5))
           {(- (new: Natural 1) (new: Natural 1))}
           {})
    0)
    
; summary -> test zeros and smod
(check-print
    (if (= (+ (new: Natural 0) (new: Natural 6)) (* (new: Natural 2) (new: Natural 3)))
        {(smod: (new: Natural 6) (new: Natural 2))}
        {1})
    0)
    
;; 3 tests will test the large-integer classes, 
;; which are built on top of class Natural.

;summary -> test sdiv
(check-print
    (sdiv: (new: LargeInteger 150) -10)
    -15)

;summary -> adding very large ints
(check-print
    (+ (asLargeInteger 10000000) (asLargeInteger 1000000))
    11000000)

;summary -> multiplying large ints
(check-print
    (* (asLargeInteger 10000000) 10)
    100000000)
    

;; 3 tests will test mixed arithmetic and comparison 
;; involving both small and large integers.

;; since i wasnt able to complete small ints in time
;; I havent written tests for them