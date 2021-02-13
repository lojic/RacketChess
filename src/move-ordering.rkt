#lang racket

(require "./board.rkt"
         "./move.rkt"
         "./piece.rkt")

(require racket/fixnum
         racket/performance-hint)

(provide order-moves!)

;; TODO I don't think we should call (vector-sort!) here - maybe a selection sort instead
(define (order-moves! b)
  (define-inline (mvv-lva m)
    (let ([ victim (move-captured-piece m) ])
      (if (fx> victim 0)
          (- (abs (piece-value b victim (move-dst-idx m)))
             (abs (piece-value b (move-src m) (move-src-idx m))))
          0)))

  (vector-sort! (tactical-moves b)
                > ; Reverse the sort to get the highest values first
                0
                (add1 (tactical-head b))
                ;; The key will be the piece value of the victim
                ;; divided by the piece value of the attacker
                ;; i.e. most valuable victim - least valuable attacker
                ;; (MVV-LVA)
                #:key mvv-lva))
