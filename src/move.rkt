#lang racket

(provide create-move)

(require "./fixnum-fields.rkt"
         racket/performance-hint)

(fixnum-fields move ([ src                 5        ]
                     [ src-idx             7        ]
                     [ dst-idx             7        ]
                     [ captured-piece      5        ]
                     [ promoted-piece      5        ]
                     [ is-castle-queenside 1 #:flag ]
                     [ is-castle-kingside  1 #:flag ]
                     [ is-ep-capture       1 #:flag ]))

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
