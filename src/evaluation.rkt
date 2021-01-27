#lang racket

(require "./board.rkt"
         "./piece.rkt")

(provide evaluate)

;; For now, simply sum the piece values, and give slight preference
;; for pawns in the center.
(define (evaluate b)
  (for*/sum ([ rank (in-range 8) ]
             [ file (in-range 8) ])
    (let* ([ idx   (file-rank->idx file rank)        ]
           [ piece (bytes-ref (board-squares b) idx) ])
      (cond [ (is-pawn? piece)  (evaluate-pawn b piece idx) ]
            [ (is-piece? piece) (piece-value piece)         ]
            [ else              0.0                         ]))))

;; Not sure if this is a good idea, but give the 4 center positions
;; higher value to encourage controlling the center.
(define (evaluate-pawn b piece idx)
  (let ([ val (piece-value piece) ])
    (cond [ (= idx 64) (* val 1.02) ]
          [ (= idx 65) (* val 1.02) ]
          [ (= idx 54) (* val 1.02) ]
          [ (= idx 55) (* val 1.02) ]
          [ else       val          ])))
