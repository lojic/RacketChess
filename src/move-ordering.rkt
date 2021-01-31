#lang racket

(require "./board.rkt"
         "./move.rkt"
         "./piece.rkt")

(require racket/performance-hint)

(provide order-moves!)

(define (order-moves! b)
  (vector-sort! (tactical-moves b)
                > ; Reverse the sort to get the highest values first
                0
                (add1 (tactical-head b))
                ;; The key will be the piece value of the victim
                ;; divided by the piece value of the attacker
                ;; i.e. most valuable victim - least valuable attacker
                ;; (MVV-LVA)
                #:key mvv-lva))

(define-inline (mvv-lva m)
  (let ([ victim (move-captured-piece m) ])
    (if victim
        (- (abs (piece-value victim))
           (abs (piece-value (move-src m))))
        0)))
