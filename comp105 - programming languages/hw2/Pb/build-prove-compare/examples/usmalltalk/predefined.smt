;;;usm.nw:5910
(class Boolean Object []
    (method ifTrue:ifFalse: (trueBlock falseBlock) (subclassResponsibility self))
  
    (method ifFalse:ifTrue: (falseBlock trueBlock) 
        (ifTrue:ifFalse: self trueBlock falseBlock))
    (method ifTrue:  (trueBlock)  (ifTrue:ifFalse: self trueBlock {}))
    (method ifFalse: (falseBlock) (ifTrue:ifFalse: self {} falseBlock))
    
    (method not ()          (ifTrue:ifFalse: self {false}          {true}))
    (method eqv: (aBoolean) (ifTrue:ifFalse: self {aBoolean}       {(not aBoolean)}))
    (method xor: (aBoolean) (ifTrue:ifFalse: self {(not aBoolean)} {aBoolean}))

    (method & (aBoolean) (ifTrue:ifFalse: self {aBoolean} {self}))
    (method | (aBoolean) (ifTrue:ifFalse: self {self} {aBoolean}))
  
    (method and: (alternativeBlock) (ifTrue:ifFalse: self alternativeBlock {self}))
    (method or:  (alternativeBlock) (ifTrue:ifFalse: self {self} alternativeBlock))

    (method if (trueBlock falseBlock) (ifTrue:ifFalse: self trueBlock falseBlock))
)
;;;usm.nw:5942
(class True Boolean []
  (method ifTrue:ifFalse: (trueBlock falseBlock) (value trueBlock))
)
(class False Boolean []
  (method ifTrue:ifFalse: (trueBlock falseBlock) (value falseBlock))
)
;;;usm.nw:6116
(class Block Object 
    [] ; internal representation
    (class-method new () {})
    (method value primitive value)
    (method whileTrue: (body)
        (ifTrue:ifFalse: (value self)
            {(value body)
             (whileTrue: self body)}
            {nil}))
    (method while (body) (whileTrue: self body))
    (method whileFalse: (body) 
         (ifTrue:ifFalse: (value self)
             {nil}
             {(value body) 
              (whileFalse: self body)}))
)
;;;usm.nw:9789
(class Symbol Object
    [] ; internal representation
    (class-method new () (error: self 'can't-send-new-to-Symbol))
    (class-method new:   primitive newSymbol)
    (method       print  primitive printSymbol)
)
;;;usm.nw:7443
(class Magnitude Object 
    [] ; abstract class
    (method =  (x) (subclassResponsibility self)) ; may not inherit from Object
    (method <  (x) (subclassResponsibility self))
    (method >  (y) (< y self))
    (method <= (x) (not (> self x)))
    (method >= (x) (not (< self x)))
    (method min: (aMagnitude) (if (< self aMagnitude) {self} {aMagnitude}))
    (method max: (aMagnitude) (if (> self aMagnitude) {self} {aMagnitude}))
)
;;;usm.nw:7459
(class Number Magnitude
    []  ; abstract class
    ;;;;;;; basic Number protocol
    (method +   (aNumber)     (subclassResponsibility self))
    (method *   (aNumber)     (subclassResponsibility self))
    (method negated    ()     (subclassResponsibility self))
    (method reciprocal ()     (subclassResponsibility self))
    
    (method asInteger  ()     (subclassResponsibility self))
    (method asFraction ()     (subclassResponsibility self))
    (method asFloat    ()     (subclassResponsibility self))
    (method coerce: (aNumber) (subclassResponsibility self))
    
;;;usm.nw:7480
(method -   (y) (+ self (negated  y)))
(method abs ()  (if (negative self) {(negated  self)} {self}))
(method /   (y) (* self (reciprocal y)))
;;;usm.nw:7489
(method negative         () (<  self (coerce: self 0)))
(method nonnegative      () (>= self (coerce: self 0)))
(method strictlyPositive () (>  self (coerce: self 0)))
;;;usm.nw:7498
(method squared () (* self self))
(method raisedToInteger: (anInteger)
    (if (= anInteger 0)
        {(coerce: self 1)}
        {(if (= anInteger 1) {self}
            {(* (squared (raisedToInteger: self (div: anInteger 2)))
                (raisedToInteger: self (mod: anInteger 2)))})}))
;;;usm.nw:7527
(method sqrt () (sqrtWithin: self (coerce: self (/ 1 10))))
(method sqrtWithin: (epsilon) [locals two x<i-1> x<i>]
    ; find square root of receiver within epsilon
    (set two    (coerce: self 2))
    (set x<i-1> (coerce: self 1))
    (set x<i>   (/ (+ x<i-1> (/ self x<i-1>)) two))
    (while {(> (abs (- x<i-1> x<i>)) epsilon)}
           {(set x<i-1> x<i>)
            (set x<i> (/ (+ x<i-1> (/ self x<i-1>)) two))})
    x<i>)
;;;usm.nw:7472
)
;;;usm.nw:7861
(class Integer Number
    [] ; abstract class
    (method div: (n) (subclassResponsibility self))
    (method mod: (n) (- self (* n (div: self n))))
    (method gcd: (n) (if (= n (coerce: self 0)) {self} {(gcd: n (mod: self n))}))
    (method lcm: (n) (* self (div: n (gcd: self n))))
    
;;;usm.nw:7886
(method asFraction () (num:den:  Fraction self 1))
(method asFloat    () (mant:exp: Float    self 0))
(method asInteger  () self)
;;;usm.nw:7895
(method coerce: (aNumber) (asInteger aNumber))
;;;usm.nw:7899
(method reciprocal () (num:den: Fraction 1 self)) 
(method / (aNumber) (/ (asFraction self) aNumber))
;;;usm.nw:7905
(method timesRepeat: (aBlock) [locals count]
    (ifTrue: (negative self) {(error: self 'negative-repeat-count)})
    (set count self)
    (while {(!= count 0)}
         {(value aBlock)
          (set count (- count 1))}))
;;;usm.nw:7868
)
;;;usm.nw:7933
(class SmallInteger Integer
    [] ; primitive representation
    (class-method new: primitive newSmallInteger:)
    (class-method new  () (new: self 0))
    (method negated    () (- 0 self))
    (method print  primitive printSmallInteger)
    (method printu primitive printu)
    (method +      primitive +)
    (method -      primitive -)
    (method *      primitive *)
    (method div:   primitive div)
    (method =      primitive eqObject)
    (method <      primitive <)
    (method >      primitive >)
)
;;;usm.nw:7974
(class Fraction Number
    [num den]
    (class-method num:den: (a b) (initNum:den: (new self) a b))
    (method initNum:den: (a b) ; private
        (setNum:den: self a b)
        (signReduce self)
        (divReduce self))
    (method setNum:den: (a b) (set num a) (set den b) self) ; private
    
;;;usm.nw:8014
(method signReduce () ; private
    (ifTrue: (negative den)
        {(set num (negated num)) (set den (negated den))})
    self)

(method divReduce () [locals temp] ; private
    (if (= num 0)
        {(set den 1)}
        {(set temp (gcd: (abs num) den))
         (set num  (div: num temp))
         (set den  (div: den temp))})
    self)
;;;usm.nw:8038
(method num () num)  ; private
(method den () den)  ; private
;;;usm.nw:8051
(method = (f)
    (and: (= num (num f)) {(= den (den f))}))
(method < (f)
    (< (* num (den f)) (* (num f) den)))
;;;usm.nw:8060
(method negated () (setNum:den: (new Fraction) (negated num) den))
;;;usm.nw:8073
(method * (f)
    (divReduce
        (setNum:den: (new Fraction)
                        (* num (num f))
                        (* den (den f)))))
;;;usm.nw:8092
(method + (f) [locals temp]
    (set temp (lcm: den (den f)))
    (divReduce
        (setNum:den: (new Fraction)
                     (+ (* num (div: temp den)) (* (num f) (div: temp (den f))))
                     temp)))
;;;usm.nw:8108
(method reciprocal ()
   (signReduce (setNum:den: (new Fraction) den num)))
;;;usm.nw:8113
(method print () (print num) (print '/) (print den) self)
;;;usm.nw:8123
(method asInteger  () (div: num den))
(method asFloat    () (/ (asFloat num) (asFloat den)))
(method asFraction () self)
(method coerce: (aNumber) (asFraction aNumber))
;;;usm.nw:8133
(method negative         () (negative num))
(method nonnegative      () (nonnegative num))
(method strictlyPositive () (strictlyPositive num))
;;;usm.nw:7983
)
;;;usm.nw:8189
(class Float Number
    [mant exp]
    (class-method mant:exp: (m e) (initMant:exp: (new self) m e))
    (method initMant:exp: (m e) ; private
        (set mant m) (set exp e) (normalize self))
    (method normalize ()    ; private
        (while {(> (abs mant) 32767)}
               {(set mant (div: mant 10))
                (set exp (+ exp 1))})
        self)
    
;;;usm.nw:8205
(method mant () mant)  ; private
(method exp  () exp)   ; private
;;;usm.nw:8213
(method < (x) (negative (- self x)))
(method = (x) (isZero   (- self x)))
(method isZero () (= mant 0))  ; private
;;;usm.nw:8220
(method negated () (mant:exp: Float (negated mant) exp))
;;;usm.nw:8243
(method + (prime) 
    (if (>= exp (exp prime))
        {(mant:exp: Float (+ (* mant (raisedToInteger: 10 (- exp (exp prime))))
                             (mant prime))
                          (exp prime))}
        {(+ prime self)}))
;;;usm.nw:8256
(method * (prime) 
    (mant:exp: Float (* mant (mant prime)) (+ exp (exp prime))))
;;;usm.nw:8265
(method reciprocal ()
    (mant:exp: Float (div: 1000000000 mant) (- -9 exp)))
;;;usm.nw:8271
(method coerce: (aNumber) (asFloat aNumber))
(method asFloat () self)
;;;usm.nw:8277
(method asInteger ()
    (if (negative exp)
        {(div: mant (raisedToInteger: 10 (negated exp)))}
        {(*    mant (raisedToInteger: 10 exp))}))
;;;usm.nw:8285
(method asFraction ()
    (if (< exp 0)
        {(num:den: Fraction mant (raisedToInteger: 10 (negated exp)))}
        {(num:den: Fraction (* mant (raisedToInteger: 10 exp)) 1)}))
;;;usm.nw:8311
(method negative         () (negative mant))
(method nonnegative      () (nonnegative mant))
(method strictlyPositive () (strictlyPositive mant))
;;;usm.nw:8329
(method print () 
    (print-normalize self) 
    (print mant) (print 'x10^) (print exp)
    (normalize self))

(method print-normalize ()
    (while {(and: (< exp 0) {(= (mod: mant 10) 0)})}
        {(set exp (+ exp 1)) (set mant (div: mant 10))}))
;;;usm.nw:8200
)
;;;usm.nw:6179
(class Char Object
   [code-point]
   (class-method new: (n) (init: (new self) n))
   (method init:      (n) (set code-point n) self) ;; private
   (method print      ()  (printu code-point))
   (method =          (c) (= code-point (code-point c)))
   (method code-point ()  code-point) ;; private
)
;;;usm.nw:6198
(val newline      (new: Char 10))   (val left-paren   (new: Char  40))
(val space        (new: Char 32))   (val right-paren  (new: Char  41))
(val semicolon    (new: Char 59))   (val left-curly   (new: Char 123))
(val quote        (new: Char 39))   (val right-curly  (new: Char 125))
                                    (val left-square  (new: Char  91))
                                    (val right-square (new: Char  93))
;;;usm.nw:6251
(class Collection Object
  [] ; abstract
  (method do:     (aBlock)       (subclassResponsibility self))
  (method add:    (newObject)    (subclassResponsibility self))
  (method remove:ifAbsent: (oldObject exnBlock)
                                 (subclassResponsibility self))
  (method similar: (aCollection) (subclassResponsibility self))
  (method species ()             (subclassResponsibility self))
  
;;;usm.nw:6264
(class-method with: (anObject) [locals temp]
    (add: (new self) anObject))
;;;usm.nw:6294
(method remove: (oldObject) 
    (remove:ifAbsent: self oldObject {(error: self 'tried-to-remove-absent-object)}))
(method addAll: (aCollection) 
    (do: aCollection [block (x) (add: self x)])
    self)
(method removeAll: (aCollection) 
    (do: aCollection [block (x) (remove: self x)])
    self)
;;;usm.nw:6320
(method isEmpty () (= (size self) 0))
(method size () [locals temp]
    (set temp 0)
    (do: self [block (_) (set temp (+ temp 1))])
    temp)
(method occurrencesOf: (anObject) [locals temp]
    (set temp 0)
    (do: self [block (x)
       (ifTrue: (= x anObject) {(set temp (+ temp 1))})])
    temp)
(method includes: (anObject) (< 0 (occurrencesOf: self anObject)))
(method detect: (aBlock)
    (detect:ifNone: self aBlock {(error: self 'no-object-detected)}))
(method detect:ifNone: (aBlock exnBlock) [locals answer searching]
    (set searching true)
    (do: self [block (x)
        (ifTrue: (and: searching {(value aBlock x)})
             {(set searching false)
              (set answer x)})])
    (ifTrue:ifFalse: searching exnBlock {answer}))
;;;usm.nw:6376
(method inject:into: (thisValue binaryBlock)
   (do: self [block (x) (set thisValue (value binaryBlock x thisValue))])
   thisValue)
;;;usm.nw:6387
(method select: (aBlock) [locals temp]
   (set temp (new (species self)))
   (do: self [block (x) (ifTrue: (value aBlock x) {(add: temp x)})])
   temp)
(method reject: (aBlock)
   (select: self [block (x) (not (value aBlock x))]))
(method collect: (aBlock) [locals temp]
   (set temp (new (species self)))
   (do: self [block (x) (add: temp (value aBlock x))])
   temp)
;;;usm.nw:6400
(method asSet () [locals temp]
     (set temp (new Set))
     (do: self [block (x) (add: temp x)])
     temp)
;;;usm.nw:6409
(method print ()
    (printName self)
    (print left-paren)
    (do: self [block (x) (print space) (print x)])
    (print space)
    (print right-paren)
    self)
(method printName () (print 'Collection))
;;;usm.nw:6260
)
;;;usm.nw:6468
(class Set Collection
    [members]  ; list of elements [invariant: no repeats]
    (class-method new () (initSet (new super)))
    (method initSet   () (set members (new List)) self) ; private
    (method do: (aBlock) (do: members aBlock))
    (method add: (item)
        (ifFalse: (includes: members item) {(add: members item)})
        self)
    (method remove:ifAbsent: (item exnBlock) 
        (remove:ifAbsent: members item exnBlock)
        self)
    (method similar:  (s) [locals looks-similar]
       (set looks-similar (= (size self) (size s)))
       (ifTrue: looks-similar
           {(do: self [block (x) (ifFalse: (includes: s x)
                                           {(set looks-similar false)})])})
       looks-similar)
    (method species   () Set)
    (method printName () (print 'Set))
    (method asSet     () self)
)
;;;usm.nw:6564
(class KeyedCollection Collection
    []  ; abstract class
    (method at:put: (key value)                (subclassResponsibility self))
    (method associationsDo: (aBlock)           (subclassResponsibility self))
    (method removeKey:ifAbsent: (key exnBlock) (subclassResponsibility self))
    
;;;usm.nw:6604
(method do: (aBlock) 
    (associationsDo: self [block (anAssoc) (value aBlock (value anAssoc))]))
;;;usm.nw:6611
(method at: (key)    
    (at:ifAbsent: self key {(error: self 'key-not-found)}))
(method at:ifAbsent: (key exnBlock) 
    (value (associationAt:ifAbsent: self key 
               {(key:value: Association nil (value exnBlock))})))
(method includesKey: (key) 
    (isKindOf: (associationAt:ifAbsent: self key {}) Association))
(method associationAt: (key) 
    (associationAt:ifAbsent: self key {(error: self 'key-not-found)}))
(method associationAt:ifAbsent: (key exnBlock) [locals finishBlock]
    (set finishBlock exnBlock)
    (associationsDo: self [block (x) 
        (ifTrue: (= (key x) key) {(set finishBlock {x})})])
    (value finishBlock))
;;;usm.nw:6631
(method keyAtValue: (value) 
    (keyAtValue:ifAbsent: self value {(error: self 'value-not-found)}))
(method keyAtValue:ifAbsent: (value aBlock) [locals finishBlock]
    (set finishBlock aBlock)
    (associationsDo: self [block (x) 
        (ifTrue: (= (value x) value) {(set finishBlock {(key x)})})])
    (value finishBlock))
;;;usm.nw:6646
(method removeKey: (key)    
    (removeKey:ifAbsent: self key {(error: self 'key-not-found)}))
;;;usm.nw:6654
(method similar: (collection) [locals looks-similar]
    (set looks-similar (= (size self) (size collection)))
    (ifTrue: looks-similar
        {(associationsDo: self
            [block (assn) (ifFalse: (and: (includesKey: collection (key assn))
                                          {(similar: (at: collection (key assn))
                                                     (value assn))})
                                    {(set looks-similar false)})])})
     looks-similar)
;;;usm.nw:6570
)
;;;usm.nw:6576
(class Association Object 
   [key value]
   (class-method key:value: (x y) (setKey:value: (new self) x y))
   (method setKey:value: (x y) (set key x) (set value y) self) ; private
   (method key    ()  key)
   (method value  ()  value)
   (method key:   (x) (set key   x))
   (method value: (y) (set value y))
   (method =      (a) (& (= key (key a)) (= value (value a))))
)
;;;usm.nw:6723
(class Dictionary KeyedCollection
    [table] ; list of Associations
    (class-method new ()      (initDictionary (new super)))
    (method initDictionary () (set table (new List)) self) ; private
    (method printName ()      (print 'Dictionary))
    (method species ()        Dictionary)
    
;;;usm.nw:6743
(method associationsDo: (aBlock) (do: table aBlock))
(method at:put: (key value) [locals tempassoc]
    (set tempassoc (associationAt:ifAbsent: self key {}))
    (if (isNil tempassoc)
         {(add: table (key:value: Association key value))}
         {(value: tempassoc value)})
    self)
;;;usm.nw:6755
(method removeKey:ifAbsent: (key exnBlock)
   [locals absent value-removed] ; flag to show absence, value found if not absent
   (set absent false)
   (set value-removed (at:ifAbsent: self key {(set absent true)}))
   (ifTrue:ifFalse: absent
      {exnBlock}
      {(set table (reject: table [block (assn) (= key (key assn))])) ; remove assoc
       value-removed}))
;;;usm.nw:6767
(method remove:ifAbsent: (value exnBlock)
   (error: self 'Dictionary-uses-remove:key:-not-remove:))
;;;usm.nw:6774
(method add: (_) (error: self 'can't-just-add:-to-a-Dictionary))
;;;usm.nw:6778
(method print () [locals print-comma]
    (set print-comma false)
    (printName self)
    (print left-paren)
    (associationsDo: self
        [block (x) (print space)
                   (ifTrue: print-comma {(print #,) (print space)})
                   (set print-comma true)
                   (print (key x))   (print space)
                   (print '|-->)     (print space)
                   (print (value x))])
    (print space)
    (print right-paren)
    self)
;;;usm.nw:6730
)
;;;usm.nw:6809
(class SequenceableCollection KeyedCollection
    [] ; abstract class
    (method firstKey () (subclassResponsibility self))
    (method lastKey  () (subclassResponsibility self))
    (method last     () (at: self (lastKey  self)))
    (method first    () (at: self (firstKey self)))
    (method at:ifAbsent: (index exnBlock) [locals current resultBlock]
        (set resultBlock exnBlock)
        (set current (firstKey self))
        (do: self [block (v)
            (ifTrue: (= current index) {(set resultBlock {v})})
            (set current (+ current 1))])
        (value resultBlock))
    (method associationsDo: (bodyBlock) [locals i last]
        (set i    (firstKey self))
        (set last (lastKey self))
        (whileTrue: {(<= i last)}
            {(value bodyBlock (key:value: Association i (at: self i)))
             (set i (+ i 1))}))
)
;;;usm.nw:7034
(class Cons Object
    [car cdr]
    (method car ()           car)
    (method cdr ()           cdr)
    (method car: (anObject)  (set car anObject) self)
    (method cdr: (anObject)  (set cdr anObject) self)
    (method pred: (aCons)    nil)
    
;;;usm.nw:7049
(method deleteAfter () [locals answer]
    (set answer (car cdr))
    (set cdr    (cdr cdr))
    (pred: cdr self)
    answer)
(method insertAfter: (anObject)
    (set cdr (car: (cdr: (new Cons) cdr) anObject))
    (pred: (cdr cdr) cdr)
    anObject)
;;;usm.nw:7075
(method do: (aBlock)
    (value aBlock car)
    (do: cdr aBlock))
;;;usm.nw:7089
(method rejectOne:ifAbsent:withPred: (aBlock exnBlock pred)
    (if (value aBlock self)
        {(deleteAfter pred)}
        {(rejectOne:ifAbsent:withPred: cdr aBlock exnBlock self)}))
;;;usm.nw:7042
)
;;;usm.nw:7102
(class ListSentinel Cons
    [pred]
    (method pred: (aCons)   (set pred aCons))
    (method pred  ()        pred)
    (class-method new ()    [locals tmp]
        (set tmp (new super))
        (pred: tmp tmp)
        (cdr:  tmp tmp)
        tmp)
    
;;;usm.nw:7079
(method do: (aBlock) nil)
;;;usm.nw:7094
(method rejectOne:ifAbsent:withPred: (aBlock exnBlock pred)
    (value exnBlock))
;;;usm.nw:7111
                                                   )
;;;usm.nw:6953
(class List SequenceableCollection
    [sentinel]
    (class-method new ()        (sentinel: (new super) (new ListSentinel)))
    (method sentinel: (s)       (set sentinel s) self) ; private
    (method isEmpty   ()        (= sentinel (cdr sentinel)))
    (method last      ()        (car (pred sentinel)))
    (method do:       (aBlock)  (do: (cdr sentinel) aBlock))
    (method species   ()        List)
    (method printName ()        (print 'List))
    
;;;usm.nw:6972
(method addLast:  (item)   (insertAfter: (pred sentinel) item) self)
(method addFirst: (item)   (insertAfter: sentinel item)        self)
(method add:      (item)   (addLast: self item))
;;;usm.nw:6979
(method removeFirst ()     (deleteAfter sentinel))
;;;usm.nw:6993
(method remove:ifAbsent: (oldObject exnBlock)
    (rejectOne:ifAbsent:withPred:
        (cdr sentinel)
        [block (x) (= oldObject (car x))]
        exnBlock
        sentinel))
;;;usm.nw:7003
(method removeKey:ifAbsent: (n exnBlock)
   (error: self 'removeKey:ifAbsent:-on-List-is-left-as-an-exercise))
;;;usm.nw:7010
(method firstKey () 0)
(method lastKey  () (- (size self) 1))
(method at:put: (n value) [locals tmp]
    (set tmp (cdr sentinel))
    (whileFalse: {(= n 0)}
       {(set n (- n 1))
        (set tmp (cdr tmp))})
    (car: tmp value)
    self)
;;;usm.nw:6963
)
;;;usm.nw:7201
(class Array SequenceableCollection
    [] ; representation is primitive
    (class-method new: primitive arrayNew:)
    (class-method new  () (error: self 'size-of-Array-must-be-specified))
    (method size       primitive arraySize)
    (method at:        primitive arrayAt:)
    (method at:update: primitive arrayAt:update:) ; private
    (method at:put:    (key value) (at:update: self key value) self)
    (method species    () Array)
    (method printName  () nil) ; names of arrays aren't printed
    
;;;usm.nw:7236
(method add:                (x)   (fixedSizeError self))
(method remove:ifAbsent:    (x b) (fixedSizeError self))
(method removeKey:ifAbsent: (x b) (fixedSizeError self))
(method fixedSizeError      ()    (error: self 'arrays-have-fixed-size))
;;;usm.nw:7244
(method select:  (_) (error: self 'select-on-arrays-not-implemented))
(method collect: (_) (error: self 'collect-on-arrays-not-implemented))
;;;usm.nw:7250
(method firstKey () 0)
(method lastKey  () (- (size self) 1))
(method do: (aBlock) [locals index]
    (set index (firstKey self))
    (timesRepeat: (size self)
       {(value aBlock (at: self index))
        (set index (+ index 1))}))
;;;usm.nw:7212
)
