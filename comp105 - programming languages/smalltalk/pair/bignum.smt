;;******************************************************************************
;; Exercise 42

(class Natural Magnitude
    []
    
    ;; highest base possible w/o overflow
    (class-method base () 32768)
    
    (class-method new: (i)
        (if (>= i 0)
            {(div_mod:: Natural (div: i (base Natural))
                                  (mod: i (base Natural)))}
            {(error: self 'negative)}))
    
    (class-method div_mod:: (m d)
        (if (and: (= m 0) {(= d 0)})
            {(new NatZero)}
            {(m_d:: NatNonzero m d)}))
            
    ;; Arithmetic definitions
    (method = (n) (compare_n_less_eq_great:::: self n {false} {true} {false}))
    (method < (n) (compare_n_less_eq_great:::: self n {true} {false} {false}))
    (method + (n) (plus_wCarry:: self n 0))
    (method - (n) (minus_borrow:: self n 0))
    (method * (n) (subclassResponsibility self))
    (method sdiv: (n) (sdivmodwith:: self n [block (a b) a]))
    (method smod: (n) (sdivmodwith:: self n [block (a b) b]))
    
    ;; Arithmetic subclassResponsibilities 
    (method sdivmodwith:: (n b) (subclassResponsibility Self))
    (method isZero () (subclassResponsibility Self))
    (method mod_Base () (subclassResponsibility self))
    (method div_Base () (subclassResponsibility self))
    (method mult_Base () (subclassResponsibility self))
    (method compare_n_less_eq_great:::: (n l e g) (subclassResponsibility self))
    (method plus_wCarry:: (n c) (subclassResponsibility self))
    (method minus_borrow:: (n c) (subclassResponsibility self))
            
    (method decimal () (digit_list: self (new List)))
    (method digit_list: (d)
        (if (isZero self)
            {(if (isEmpty d)
                {(addFirst: d 0)}
                {d})}
            {(digit_list: (sdiv: self 10) (addFirst: d (smod: self 10)))}))
            
    (method print ()
        (do: (decimal self) [block (digit) (print digit)]))
)

(class NatZero Natural
    []
    
    ;; Abstraction Function
    ;; A (self) = 0
    ;;    This class is for representing zero, so any instance is 0
    
    (class-method new () (new super))
    
    (method isZero () true)
    (method * (n) self)
    (method mod_base () 0)
    (method div_base () self)
    (method mult_base () self)
    (method sdivmodwith:: (n b) (value b (new NatZero) 0))
    
    (method compare_n_less_eq_great:::: (n l e g)
        (if (isZero n)
            {(value e)}
            {(value l)}))
            
    (method plus_wCarry:: (n c)
        (if (= c 0)
            {n}
            {(if (isZero n)
                {(new: Natural c)}
                {(init_m_d:: n 
                   (plus_wCarry:: self (div_base n) 
                                  (div: (+ (mod_base n) 1) (base Natural)))
                                  (mod: (+ (mod_base n) 1) (base Natural)))})}))
                                  
    (method minus_borrow:: (n c)
        (if (and: (isZero n) {(= c 0)})
            {self}
            {error: self 'negative}))
)

(class NatNonzero Natural
    [m d]
    
    ;; Abstraction
    ;; A (m d) = (m * base) + d
    ;;    Numbers constructed recursively with next digit constructed
    ;;    by seperate instance of Natural int, however, for single digit
    ;;    numbers, it is represented by NatZero
    
    (class-method m_d:: (m d)
        (if (= m 0)
            {(init_m_d:: (new self) (new NatZero) d)}
            {(init_m_d:: (new self) (new: Natural m) d)}))
            
    (method init_m_d:: (mz dz)
        (if (and: (isZero mz) {(= dz 0)})
            {(new NatZero)}
            {(set m mz) (set d dz) self}))
            
    ;; helpers
    (method m () m)
    (method d () d)
    
    (method mod_base () d)
    (method div_base () m)
    (method mult_base () (init_m_d:: (new: Natural 1) self 0))
    
    (method compare_n_less_eq_great:::: (n l e g)
        (if (isZero n)
            g
            {(compare_n_less_eq_great:::: m (div_base n) l
                {(if (= d (mod_base n))
                    e
                    {(if (< d (mod_base n))
                        l
                        g)})}
                 g)}))
            
    (method plus_wCarry:: (n c)
        (if (isZero n)
            {(plus_wCarry:: n self c)}
            {(init_m_d:: self (plus_wCarry:: m (div_base n)
                             (div: (+(+ d (mod_base n))c)(base Natural)))
                             (mod: (+(+ d (mod_base n))c)(base Natural)))
         }))
         
    (method borrow: (b)
        (if (= b 0)
            {self}
            {(if (= d 0)
                {(init_m_d:: self (borrow: m 1) 9)}
                {(init_m_d:: self m (- d 1))})}))
                
    (method minus_borrow:: (n b)
        (if (isZero n)
            {(borrow: self b)}
            {(init_m_d:: self (minus_borrow:: m (div_base n)
                                (if (<(-(- d (mod_base n))b) 0) {1} {0}))
                                (mod: (- (- d (mod_base n)) b) 
                                                    (base Natural)))}))
    (method * (n)
        (if (isZero n)
            {n}
            {(+ (new: Natural (* d (mod_base n)))
                (+ (mult_base (+ (* m (new: Natural (mod_base n)))
                                 (* (div_base n) (new: Natural d))))
                   (mult_base (mult_base (* m (div_base n))))))}))
                   
    (method sdivmodwith:: (x y)
        (if (< x 0)
            {(error: self 'badDivision)}
            {(value y
                (init_m_d:: (new: NatNonzero 1) (sdiv: m y)
                            (div: (+ (* (smod: m x) 
                                         (base Natural)) d) x))
                (mod: (+ (* (smod: m x) (base Natural)) d) x))}))
                
    (method isZero () false)
)

;;******************************************************************************
;; Exercise 43

;;starter code used from:
;; /comp/105/build-prove-compare/examples/usmalltalk/large-int.smt

(class LargeInteger Integer
  [magnitude]
  
  (class-method withMagnitude: (aNatural) 
      (magnitude: (new self) aNatural))
      
  (method magnitude () magnitude)
  
  (method magnitude: (aNatural) 
      (set magnitude aNatural)
      self)
      
  (class-method new: (anInteger)
     (if (negative anInteger) 
        {(magnitude: (new LargeNegativeInteger) 
                     (new: Natural (negated anInteger)))}
        {(magnitude: (new LargePositiveInteger) (new: Natural anInteger))}))
        
  (method asLargeInteger () self)
  
  (method isZero () (isZero magnitude))
  
  (method = (anInteger) (isZero   (- self anInteger)))
  (method < (anInteger) (negative (- self anInteger)))
  (method div: (n) (sdiv: self n)) ;taken from spec
  
)

(class LargePositiveInteger LargeInteger
    []
    (class-method new () (new super))
    (method print () (print magnitude))
    
    (method negative () false)
    (method nonnegative () true)
    (method strictlyPositive () (not (isZero self)))
    (method negated () (withMagnitude: LargeNegativeInteger magnitude))
    
    (method + (n) (addLargePositiveIntegerTo: n self))
    (method addLargePositiveIntegerTo: (n)
        (withMagnitude: LargePositiveInteger (+ magnitude (magnitude n))))
        
    (method addLargeNegativeIntegerTo: (n)
        (if (>= magnitude (magnitude n))
            {(withMagnitude: LargePositiveInteger (- magnitude (magnitude n)))}
            {(withMagnitude: LargePositiveInteger (-(magnitude n) magnitude))}))
            
    (method addSmallIntegerTo: (n) (+ self (asLargeInteger n)))

    (method * (n) (multiplyByLargePositiveInteger: n self))
    (method multiplyByLargePositiveInteger: (n)
        (withMagnitude: LargePositiveInteger (* magnitude (magnitude n))))
        
    (method multiplyByLargeNegativeInteger: (n)
        (negated (* n self)))
        
    (method multiplyBySmallInteger: (n)
        (* self (asLargeInteger n)))
    
    ;;function taken from spec
    (method sdiv: (anInteger)
         (ifTrue:ifFalse: (strictlyPositive anInteger)
            {(withMagnitude: LargePositiveInteger (sdiv: magnitude anInteger))}
            {(negated (sdiv: (- (- self (new: LargeInteger anInteger))
                      (new: LargeInteger 1))
                      (negated anInteger)))}))
                      
    (method smod: (n)
        (if (negative n)
            {(+ (smod: (magnitude self) (negated n)) n)}
            {(smod: (magnitude self) n)}))
)

(class LargeNegativeInteger LargeInteger
    []
    
  ;;NOTE: for large negative integer, I ran out of time and tokens,
  ;;      I was doing it solo and so it took me a while to figure out
  ;;      what was going on and after I did it was too late to finish 
  ;;      implementing SmallInteger and LargeNegativeInteger, even
  ;;     though they would be very similar to what I already have with
  ;;      LargePositiveInteger.
    
    ;; function taken from spec
    (method sdiv: (anInteger)
         (ifTrue:ifFalse: (strictlyPositive anInteger)
            {(negated (sdiv: (- (+ (negated self) (new: LargeInteger anInteger))
                                (new: LargeInteger 1))
                             anInteger))}
            {(sdiv: (negated self) (negated anInteger))}))
            
)