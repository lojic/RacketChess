#lang racket

(module+ test
  (require "./board.rkt"
           "./board-funcs.rkt"
           "./evaluation.rkt"
           "./piece.rkt"
           "./fen.rkt")

  (require rackunit)

  ;; ------------------------------------------------------------------------------------------
  ;; create-board
  ;; ------------------------------------------------------------------------------------------

  (let* ([ b       (create-board)    ]
         [ squares (board-squares b) ])
    ;; All squares are empty
    (for* ([ file (in-range 8) ]
           [ rank (in-range 8) ])
      (check-equal? (bytes-ref squares (file-rank->idx file rank))
                    empty-square))

    )

  ;; ------------------------------------------------------------------------------------------
  ;; evaluate
  ;; ------------------------------------------------------------------------------------------
  (let ([ b (fen->board) ])
    (check-within (evaluate b) 0.0 0.00001)
    ;; Remove the White Queen
    (bytes-set! (board-squares b) 94 empty-square)
    (check-within (evaluate b) -9.0 0.00001))

  )
