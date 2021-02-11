#lang racket

(require "./fixnum-fields.rkt")

(require racket/fixnum
         racket/performance-hint)

(provide initial-game-state
         set-state-b-kingside-ok?
         set-state-b-queenside-ok?
         set-state-w-kingside-ok?
         set-state-w-queenside-ok?
         set-state-whites-move?
         state-b-kingside-ok?
         state-b-queenside-ok?
         state-black-king-idx
         state-ep-idx
         state-w-kingside-ok?
         state-w-queenside-ok?
         state-white-king-idx
         state-whites-move?
         unset-state-b-kingside-ok?
         unset-state-b-queenside-ok?
         unset-state-w-kingside-ok?
         unset-state-w-queenside-ok?
         unset-state-whites-move?
         update-state-black-king-idx
         update-state-ep-idx
         update-state-white-king-idx)

(fixnum-fields state ([ ep-idx          7        ]
                      [ black-king-idx  7        ]
                      [ white-king-idx  7        ]
                      [ whites-move     1 #:flag ]
                      [ w-queenside-ok  1 #:flag ]
                      [ w-kingside-ok   1 #:flag ]
                      [ b-queenside-ok  1 #:flag ]
                      [ b-kingside-ok   1 #:flag ]))

(define (create-state)
  (make-state 0 0 0 0 0 0 0 0))

(define (initial-game-state pos->idx)
  (let ([ s (create-state) ])
    (set! s (update-state-ep-idx         s 0))
    (set! s (update-state-black-king-idx s (pos->idx "e8")))
    (set! s (update-state-white-king-idx s (pos->idx "e1")))
    (set! s (set-state-whites-move?      s))
    (set! s (set-state-w-queenside-ok?   s))
    (set! s (set-state-w-kingside-ok?    s))
    (set! s (set-state-b-queenside-ok?   s))
    (set! s (set-state-b-kingside-ok?    s))
    s))
