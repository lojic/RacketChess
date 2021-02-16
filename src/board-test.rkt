#lang racket

(module+ test
  (require "./board.rkt"
           "./evaluation.rkt"
           "./global.rkt"
           "./make-move.rkt"
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
      (check-equal? (get-square squares (file-rank->idx file rank))
                    empty-square))

    )

  ;; ------------------------------------------------------------------------------------------
  ;; evaluate
  ;; ------------------------------------------------------------------------------------------
  (let ([ b (fen->board) ])
    (check-equal? (evaluate b) 0)
    ;; Remove the White Queen
    (set-square! (board-squares b) 94 empty-square)
    ;; Depends on pst value
    (check-equal? (evaluate b) -895))

  )
