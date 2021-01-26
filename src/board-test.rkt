#lang racket

(module+ test
  (require "./board.rkt")
  (require "./piece.rkt")

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
)
