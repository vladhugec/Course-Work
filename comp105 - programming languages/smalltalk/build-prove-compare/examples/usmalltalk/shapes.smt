;;;usm.nw:542
(class CoordPair Object
   [x y] ; two numbers         ;;;;;;;;;;;; Representation, as instance variables

   (class-method x:y: (anX aY) ;;;;;;;;;;;; Initialization
      (initializeX:andY: (new self) anX aY))
   (method initializeX:andY: (anX aY) ;; private
      (set x anX)
      (set y aY)
      self)

   (method print ()            ;;;;;;;;;;;; Printing
      (print left-paren) (print x) (print ',) (print y) (print right-paren))

   (method * (k)               ;;;;;;;;;;;; Scaling and translation
      (x:y: CoordPair (* x k) (* y k)))

   (method + (coords) (x:y: CoordPair (+ x (x coords)) (+ y (y coords))))
   (method - (coords) (x:y: CoordPair (- x (x coords)) (- y (y coords))))
   (method x () x) ;; private
   (method y () y) ;; private
)
;;;usm.nw:768
(class Tikzpicture Object
    [shapes] ; the representation: a list of shapes

    (method init () ; private      ;;;;;;;;;;;; Initialization
        (set shapes (new List))
        self)
    (class-method empty ()
        (init (new self)))

    (method add: (aShape)          ;;;;;;;;;;;; Add a shape, reply with the picture
        (add: shapes aShape)
        self)

    (method draw ()                ;;;;;;;;;;;; Encapsulated TikZ syntax
        (print '\begin) (print left-curly) (print 'tikzpicture) (print right-curly)
        (print left-square) (print 'x=4pt,y=4pt) (println right-square)
        (do: shapes [block (shape) (drawOn: shape self)])  ; <- draw each shape
        (print '\end) (print left-curly) (print 'tikzpicture) (println right-curly)
        self)

    (method drawPolygon: (coord-list)
        (print '\draw) (print space)
        (do: coord-list [block (pt) (print pt) (print '--)])
        (print 'cycle)
        (println semicolon))

    (method drawEllipseAt:width:height: (center w h)
        (print '\draw) (print space) (print center) (print 'ellipse)
        (print left-paren)
            (print (div: w 2)) (print space) (print 'and) (print space) (print (div: h 2))
        (print right-paren)
        (println semicolon))
)
;;;usm.nw:1084
(class Shape Object
   [center radius] ;; CoordPair and number
   (class-method new ()
       (center:radius: (new super) (x:y: CoordPair 0 0) 1))
   (method center:radius: (c r) ;; private
       (set center c)
       (set radius r)
       self)

   (method location: (point-name)
       (+ center (* (at: point-vectors point-name) radius)))
           
   (method locations: (point-names) [locals locs]
       (set locs (new List))
       (do: point-names [block (point) (add: locs (location: self point))])
       locs)
           
   (method adjustPoint:to: (point-name location)
       (set center (+ center (- location (location: self point-name))))
       self)

   (method scale: (k)
       (set radius (* radius k))
       self)

   (method drawOn: (picture)
      (subclassResponsibility self))
)
;;;usm.nw:1049
(class Circle Shape
   [] ;; no additional representation
   (method drawOn: (picture)
      (drawEllipseAt:width:height: picture center (* 2 radius) (* 2 radius)))
)
;;;usm.nw:1064
(class Square Shape
   [] ;; no additional representation
   (method drawOn: (picture)
      (drawPolygon: picture
                    (locations: self '(Northeast Northwest Southwest Southeast))))
)
;;;usm.nw:1154
(val point-vectors (new Dictionary))
(at:put: point-vectors 'Center    (x:y: CoordPair  0  0))
(at:put: point-vectors 'East      (x:y: CoordPair  1  0))
(at:put: point-vectors 'Northeast (x:y: CoordPair  1  1))
; ... six more definitions follow ...
;;;usm.nw:1160
(at:put: point-vectors 'West      (x:y: CoordPair -1  0))
(at:put: point-vectors 'North     (x:y: CoordPair  0  1))
(at:put: point-vectors 'South     (x:y: CoordPair  0 -1))
(at:put: point-vectors 'Northwest (x:y: CoordPair -1  1))
(at:put: point-vectors 'Southeast (x:y: CoordPair  1 -1))
(at:put: point-vectors 'Southwest (x:y: CoordPair -1 -1))
