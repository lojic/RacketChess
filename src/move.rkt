#lang racket

(require "./fixnum-fields.rkt")
(require racket/performance-hint)

(provide create-move
         east
         king-offsets
         knight-offsets
         move-captured-piece
         move-dst-idx
         move-is-castle-kingside?
         move-is-castle-queenside?
         move-is-ep-capture?
         move-promoted-piece
         move-src
         move-src-idx
         north
         north-east
         north-west
         south
         south-east
         south-west
         west)

(fixnum-fields move ([ src                 5        ]
                     [ src-idx             7        ]
                     [ dst-idx             7        ]
                     [ captured-piece      5        ]
                     [ promoted-piece      5        ]
                     [ is-castle-queenside 1 #:flag ]
                     [ is-castle-kingside  1 #:flag ]
                     [ is-ep-capture       1 #:flag ]))

;; Directions
(define north      -10)
(define north-east  -9)
(define east         1)
(define south-east  11)
(define south       10)
(define south-west   9)
(define west        -1)
(define north-west -11)

(define king-offsets (list north north-east east south-east south south-west west north-west))

(define knight-offsets
  (list (+ north north east)
        (+ east east north)
        (+ east east south)
        (+ south south east)
        (+ south south west)
        (+ west west south)
        (+ west west north)
        (+ north north west)))

(define-inline (create-move src
                            src-idx
                            dst-idx
                            #:captured-piece       [ captured-piece       #f ]
                            #:promoted-piece       [ promoted-piece       #f ]
                            #:is-castle-queenside? [ is-castle-queenside? #f ]
                            #:is-castle-kingside?  [ is-castle-kingside?  #f ]
                            #:is-ep-capture?       [ is-ep-capture?       #f ])
  (make-move src
             src-idx
             dst-idx
             (or captured-piece 0)
             (or promoted-piece 0)
             (if is-castle-queenside? 1 0)
             (if is-castle-kingside?  1 0)
             (if is-ep-capture?       1 0)))
