#lang racket

(require "./state.rkt")
(require racket/fixnum
         racket/performance-hint)

(provide black-king-idx
         create-board
         file-rank->idx
         full-move
         get-ep-idx
         idx->pos
         init-moves!
         is-end-game?
         is-whites-move?
         may-castle?
         pop-game-state!
         pos->idx
         push-game-state!
         quiet-head
         quiet-moves
         reset-depth!
         revoke-castling!
         set-black-king-idx!
         set-ep-idx!
         set-full-move!
         set-quiet-head!
         set-tactical-head!
         set-white-king-idx!
         set-whites-move?!
         tactical-head
         tactical-moves
         white-king-idx
         (struct-out board))

;; 10x12 Board Representation
;; Index of upper left is 0. Index of lower right is 119
;;
;; FF FF FF FF FF FF FF FF FF FF | FF  FF  FF  FF  FF  FF  FF  FF  FF FF
;; FF FF FF FF FF FF FF FF FF FF | FF  FF  FF  FF  FF  FF  FF  FF  FF FF
;; FF 44 42 43 45 46 43 42 44 FF | FF 021 022 023 024 025 026 027 028 FF 8
;; FF 41 41 41 41 41 41 41 41 FF | FF 031 032 033 034 035 036 037 038 FF 7
;; FF 00 00 00 00 00 00 00 00 FF | FF 041 042 043 044 045 046 047 048 FF 6
;; FF 00 00 00 00 00 00 00 00 FF | FF 051 052 053 054 055 056 057 058 FF 5
;; FF 00 00 00 00 00 00 00 00 FF | FF 061 062 063 064 065 066 067 068 FF 4
;; FF 00 00 00 00 00 00 00 00 FF | FF 071 072 073 074 075 076 077 078 FF 3
;; FF 81 81 81 81 81 81 81 81 FF | FF 081 082 083 084 085 086 087 088 FF 2
;; FF 84 82 83 85 88 83 82 84 FF | FF 091 092 093 094 095 096 097 098 FF 1
;; FF FF FF FF FF FF FF FF FF FF | FF  FF  FF  FF  FF  FF  FF  FF  FF FF
;; FF FF FF FF FF FF FF FF FF FF | FF  FF  FF  FF  FF  FF  FF  FF  FF FF
;;                                      a   b   c   d   e   f   g   h

(struct board (depth
               squares
               move-i
               game-state
               game-stack
               quiet-moves    ; Indexed by depth
               quiet-head     ; Indexed by depth
               tactical-moves ; Indexed by depth
               tactical-head) ; Indexed by depth
        #:transparent #:mutable)

(define initial-squares
  (bytes->immutable-bytes
   (bytes
    #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF
    #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF
    #xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF
    #xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF
    #xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF
    #xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF
    #xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF
    #xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF
    #xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF
    #xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF
    #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF
    #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF)))

(define positions
  (vector-immutable
   "  " "  " "  " "  " "  " "  " "  " "  " "  " "  "   ;   0 -   9
   "  " "  " "  " "  " "  " "  " "  " "  " "  " "  "   ;  10 -  19
   "  " "a8" "b8" "c8" "d8" "e8" "f8" "g8" "h8" "  "   ;  20 -  29
   "  " "a7" "b7" "c7" "d7" "e7" "f7" "g7" "h7" "  "   ;  30 -  39
   "  " "a6" "b6" "c6" "d6" "e6" "f6" "g6" "h6" "  "   ;  40 -  49
   "  " "a5" "b5" "c5" "d5" "e5" "f5" "g5" "h5" "  "   ;  50 -  59
   "  " "a4" "b4" "c4" "d4" "e4" "f4" "g4" "h4" "  "   ;  60 -  69
   "  " "a3" "b3" "c3" "d3" "e3" "f3" "g3" "h3" "  "   ;  70 -  79
   "  " "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2" "  "   ;  80 -  89
   "  " "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1" "  "   ;  90 -  99
   "  " "  " "  " "  " "  " "  " "  " "  " "  " "  "   ; 100 - 109
   "  " "  " "  " "  " "  " "  " "  " "  " "  " "  ")) ; 120 - 119

(define max-depth 200)
(define max-moves 250)

(define (create-board)
  (let ([ b (board
             0                             ; depth
             (bytes-copy initial-squares)  ; squares
             0                             ; move-i
             (initial-game-state pos->idx) ; game-state
             (make-fxvector max-moves)     ; game-stack
             (make-vector max-depth)       ; quiet-moves
             (make-vector max-depth)       ; quiet-head
             (make-vector max-depth)       ; tactical-moves
             (make-vector max-depth)) ])   ; tactical-head
    (for ([ i (in-range max-depth) ])
      (vector-set! (board-quiet-head b) i -1)
      (vector-set! (board-quiet-moves b) i (make-vector max-moves))
      (vector-set! (board-tactical-head b) i -1)
      (vector-set! (board-tactical-moves b) i (make-vector max-moves)))

    b))

(define-inline (file-rank->idx file rank)
  (+ 21 file (* rank 10)))

(define-inline (full-move b)
  (add1 (arithmetic-shift (board-move-i b) -1)))

(define (idx->pos idx)
  (vector-ref positions idx))

(define-inline (init-moves! b)
  (let ([ d (board-depth b) ])
    (vector-set! (board-quiet-head b) d -1)
    (vector-set! (board-tactical-head b) d -1)))

(define-inline (is-end-game? b)
  ;; TODO make this better!
  (> (board-move-i b) 50))

;; Decrement move-i and pop the game state off the stack
(define (pop-game-state! b)
  (let* ([ move-i (sub1 (board-move-i b))                    ]
         [ state  (fxvector-ref (board-game-stack b) move-i) ])
    (set-board-game-state! b state)
    (set-board-move-i! b move-i)))

(define (pos->idx pos)
  (vector-member pos positions))

;; Push the game state onto the stack and increment move-i
(define (push-game-state! b)
  (let ([ state  (board-game-state b) ]
        [ move-i (board-move-i b)     ])
    (fxvector-set! (board-game-stack b) move-i state)
    (set-board-move-i! b (add1 move-i))))

(define-inline (quiet-head b [ d #f ])
  (vector-ref (board-quiet-head b)
              (if d d (board-depth b))))

(define-inline (quiet-moves b [ d #f ])
  (vector-ref (board-quiet-moves b)
              (if d d (board-depth b))))

(define-inline (reset-depth! b)
  (set-board-depth! b 0))

(define-inline (set-full-move! b full-move blacks-move?)
  (if blacks-move?
      (set-board-move-i! b (add1 (arithmetic-shift (sub1 full-move) 1)))
      (set-board-move-i! b (arithmetic-shift (sub1 full-move) 1))))

(define-inline (set-quiet-head! b v [ d #f ])
  (vector-set! (board-quiet-head b)
               (if d d (board-depth b))
               v))

(define-inline (set-tactical-head! b v [ d #f ])
  (vector-set! (board-tactical-head b)
               (if d d (board-depth b))
               v))

(define-inline (tactical-head b [ d #f ])
  (vector-ref (board-tactical-head b)
              (if d d (board-depth b))))

(define-inline (tactical-moves b [ d #f ])
  (vector-ref (board-tactical-moves b)
              (if d d (board-depth b))))

;; --------------------------------------------------------------------------------------------
;; Convenience functions for getting/setting state fields through the
;; board struct. When multiple fields need to be manipulated, it may
;; be more efficient to obtain the state fixnum, and then make
;; multiple calls.
;; --------------------------------------------------------------------------------------------

(define-inline (black-king-idx b)
  (state-black-king-idx (board-game-state b)))

(define-inline (get-ep-idx b)
  (state-ep-idx (board-game-state b)))

(define-inline (is-whites-move? b)
  (state-whites-move? (board-game-state b)))

(define-inline (may-castle? b white?)
  (let ([ s (board-game-state b) ])
    (if white?
        (or (state-w-kingside-ok? s) (state-w-queenside-ok? s))
        (or (state-b-kingside-ok? s) (state-b-queenside-ok? s)))))

(define-inline (revoke-castling! b white?)
  (if white?
      (begin
        (set-board-game-state! b
                               (unset-state-w-queenside-ok?
                                (unset-state-w-kingside-ok?
                                 (board-game-state b)))))
      (begin
        (set-board-game-state! b
                               (unset-state-b-queenside-ok?
                                (unset-state-b-kingside-ok?
                                 (board-game-state b)))))))

(define-inline (set-black-king-idx! b idx)
  (set-board-game-state!
   b
   (update-state-black-king-idx (board-game-state b) idx)))

(define-inline (set-ep-idx! b v)
  (set-board-game-state! b (update-state-ep-idx (board-game-state b) v)))

(define-inline (set-white-king-idx! b idx)
  (set-board-game-state!
   b
   (update-state-white-king-idx (board-game-state b) idx)))

(define-inline (set-whites-move?! b bool)
  (set-board-game-state!
   b
   (if bool
       (set-state-whites-move? (board-game-state b))
       (unset-state-whites-move? (board-game-state b)))))

(define-inline (white-king-idx b)
  (state-white-king-idx (board-game-state b)))

;; --------------------------------------------------------------------------------------------

(module+ test
  (require rackunit)

  ;; ------------------------------------------------------------------------------------------
  ;; create-board
  ;; ------------------------------------------------------------------------------------------

  (let ([ b (create-board) ])
    ;; depth
    (check-equal? (board-depth b) 0)

    ;; squares
    (check-equal? (bytes-length (board-squares b)) (* 10 12))

    ;; whites-move?
    (check-not-false (is-whites-move? b))

    ;; black-king-idx
    (check-equal? (black-king-idx b) 25)

    ;; white-king-idx
    (check-equal? (white-king-idx b) 95)

    ;; move-i
    (check-equal? (board-move-i b) 0)

    ;; ep-idx
    (check-equal? (get-ep-idx b) 0)

    ;; moves
    (check-equal? (vector-length (board-quiet-head b)) max-depth)
    (check-equal? (vector-length (board-quiet-moves b)) max-depth)
    (check-equal? (vector-length (board-tactical-head b)) max-depth)
    (check-equal? (vector-length (board-tactical-moves b)) max-depth)
    (for ([ i (in-range max-depth) ])
      (check-equal? (vector-ref (board-quiet-head b) i) -1)
      (check-equal? (vector-ref (board-tactical-head b) i) -1)
      (check-equal? (vector-length (vector-ref (board-quiet-moves b) i)) max-moves)
      (check-equal? (vector-length (vector-ref (board-tactical-moves b) i)) max-moves)))

  ;; ------------------------------------------------------------------------------------------
  ;; file-rank->idx
  ;; ------------------------------------------------------------------------------------------

  (check-equal? (file-rank->idx 0 0) 21)
  (check-equal? (file-rank->idx 7 0) 28)
  (check-equal? (file-rank->idx 0 7) 91)
  (check-equal? (file-rank->idx 7 7) 98)

  ;; ------------------------------------------------------------------------------------------
  ;; init-moves!
  ;; ------------------------------------------------------------------------------------------

  (let ([ b (create-board) ])
    (vector-set! (board-quiet-head b) 0 7)
    (vector-set! (board-tactical-head b) 0 8)
    (check-not-equal? (vector-ref (board-quiet-head b) 0) -1)
    (check-not-equal? (vector-ref (board-tactical-head b) 0) -1)
    (init-moves! b)
    (check-equal? (vector-ref (board-quiet-head b) 0) -1)
    (check-equal? (vector-ref (board-tactical-head b) 0) -1))

  ;; ------------------------------------------------------------------------------------------
  ;; pos->idx / idx->pos
  ;; ------------------------------------------------------------------------------------------

  (for ([ pair (in-list '(("a8" 21) ("h8" 28) ("a8" 21) ("a1" 91) ("h1" 98))) ])
    (check-equal? (pos->idx (first pair)) (second pair))
    (check-equal? (idx->pos (second pair)) (first pair)))

  ;; ------------------------------------------------------------------------------------------
  ;; reset-depth!
  ;; ------------------------------------------------------------------------------------------

  (let ([ b   (create-board)  ]
        [ idx (pos->idx "e3") ])
    (set-board-depth! b 7)
    (set-ep-idx! b idx)
    (reset-depth! b)
    (check-equal? (board-depth b) 0)
    (check-equal? (get-ep-idx b) idx))

  )
