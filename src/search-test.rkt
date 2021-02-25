#lang racket

(require "./board.rkt"
         "./fen.rkt"
         "./move.rkt"
         "./movement.rkt"
         "./search.rkt")

(module+ test
  (require rackunit)

  ;; Create a board position where stalemate would occur if white
  ;; moves the "a" pawn instead of the "h" pawn.
  (match-let* ([ b (fen->board "k7/8/PK6/8/8/7P/8/8 w - - 0 100") ]
               [ (cons score move) (search b 9 2) ] ; Search to depth 9
               [ src-idx (move-src-idx move) ])
      (check-not-equal? src-idx (pos->idx "a6")) ; Don't move the a6 pawn
      (check-equal? src-idx (pos->idx "h3"))     ; Do move the h3 pawn
      (check-not-false (> score 900)))           ; White wins a queen

  )
