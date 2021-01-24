#lang racket

(require racket/performance-hint)

(provide create-board
         file-rank->idx
         pos->idx
         init-moves!
         idx->pos
         (struct-out board))

;; 10x12 Board Representation
;; Index of upper left is 0. Index of lower right is 119
;;
;; FF FF FF FF FF FF FF FF FF FF | FF  FF  FF  FF  FF  FF  FF  FF  FF FF
;; FF FF FF FF FF FF FF FF FF FF | FF  FF  FF  FF  FF  FF  FF  FF  FF FF
;; FF 44 42 43 45 46 43 42 44 FF | FF 022 023 024 025 026 027 028 029 FF 8
;; FF 41 41 41 41 41 41 41 41 FF | FF 032 033 034 035 036 037 038 039 FF 7
;; FF 00 00 00 00 00 00 00 00 FF | FF 042 043 044 045 046 047 048 049 FF 6
;; FF 00 00 00 00 00 00 00 00 FF | FF 052 053 054 055 056 057 058 059 FF 5
;; FF 00 00 00 00 00 00 00 00 FF | FF 062 063 064 065 066 067 068 069 FF 4
;; FF 00 00 00 00 00 00 00 00 FF | FF 072 073 074 075 076 077 078 079 FF 3
;; FF 81 81 81 81 81 81 81 81 FF | FF 082 083 084 085 086 087 088 089 FF 2
;; FF 84 82 83 85 88 83 82 84 FF | FF 092 093 094 095 096 097 098 099 FF 1
;; FF FF FF FF FF FF FF FF FF FF | FF  FF  FF  FF  FF  FF  FF  FF  FF FF
;; FF FF FF FF FF FF FF FF FF FF | FF  FF  FF  FF  FF  FF  FF  FF  FF FF
;;                                      a   b   c   d   e   f   g   h

(struct board (depth
               squares
               whites-move?
               black-king-idx
               white-king-idx
               full-move
               ep-idx
               quiet-moves
               quiet-head
               tactical-moves
               tactical-head)
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

(define max-depth 50)
(define max-moves 250)

(define (create-board)
  (let ([ b (board
             0                             ; depth
             (bytes-copy initial-squares)  ; squares
             #t                            ; whites-move?
             25                            ; black-king-idx
             95                            ; white-king-idx
             1                             ; full-move
             0                             ; ep-idx
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

(define (init-moves! b)
  (let ([ d (board-depth b) ])
    (vector-set! (board-quiet-head b) d -1)
    (vector-set! (board-tactical-head b) d -1)))

(define-inline (file-rank->idx file rank)
  (+ 21 file (* rank 10)))

(define (pos->idx pos)
  (vector-member pos positions))

(define (idx->pos idx)
  (vector-ref positions idx))

(module+ test
  (require rackunit)

  ;; ------------------------------------------------------------------------------------------
  ;; pos->idx / idx->pos
  ;; ------------------------------------------------------------------------------------------
  (for ([ pair (in-list '(("a8" 21) ("h8" 28) ("a8" 21) ("a1" 91) ("h1" 98))) ])
    (check-equal? (pos->idx (first pair)) (second pair))
    (check-equal? (idx->pos (second pair)) (first pair)))

  )
