;;;scheme.nw:7308
'(5 0 1 (Hello Dolly) 5 5 1 0)
(run 
  '((define mod (m n) (- m (* n (/ m n))))
    (define gcd (m n) (if (= n 0) m (gcd n (mod m n))))
    (mod 5 10)
    (mod 10 5)
    (mod 3 2)
    (cons 'Hello (cons 'Dolly '()))
    (println (gcd 5 10))
    (gcd 17 12)))
;;;scheme.nw:7322
'(5 0 1 #t 'blastoff 1 5 1 0)
(run
  '((define mod (m n) (- m (* n (/ m n))))
    (define not (x) (if x #f #t))
    (define != (x y) (not (= x y)))
    (define list6 (a b c d e f) (cons a (cons b (cons c (cons d (cons e (cons f '())))))))
    (define gcd (m n r)
      (begin
        (while (!= (set r (mod m n)) 0)
          (begin
            (set m n)
            (set n r)))
        n))
    (mod 5 10)
    (mod 10 5)
    (mod 3 2)
    (!= 2 3)
    (begin 5 4 3 2 1 'blastoff)
    (gcd 2 3 0)
    (gcd 5 10 0)
    (gcd 17 12 0)))
