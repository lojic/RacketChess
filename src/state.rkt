#lang racket

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

(require "./fixnum-fields.rkt")

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

(define (initial-game-state)
  (let ([ s (create-state) ])
    (set! s (update-state-ep-idx         s 0))   ; No en passant square at start
    (set! s (update-state-black-king-idx s 25))  ; Index of E8
    (set! s (update-state-white-king-idx s 95))  ; Index of E1
    (set! s (set-state-whites-move?      s))     ; White's move at start
    (set! s (set-state-w-queenside-ok?   s))     ; White ok to castle queenside
    (set! s (set-state-w-kingside-ok?    s))     ; White ok to castle kingside
    (set! s (set-state-b-queenside-ok?   s))     ; Black ok to castle queenside
    (set! s (set-state-b-kingside-ok?    s))     ; Black ok to castle kingside
    s))
