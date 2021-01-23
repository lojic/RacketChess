#lang racket

(module+ test
  (require "./board-slow.rkt")
  (require "./piece.rkt")
  (require "./fen.rkt")

  (require rackunit)

  ;; ------------------------------------------------------------------------------------------
  ;; get-pieces-file-rank
  ;; ------------------------------------------------------------------------------------------
  (let* ([ b      (fen->board)             ]
         [ pieces (get-pieces-file-rank b) ])
    (for ([ tuple (in-list (list (list white-rook #\a #\1)
                                 (list white-knight #\b #\1)
                                 (list white-bishop #\c #\1)
                                 (list white-queen #\d #\1)
                                 (list white-king #\e #\1)
                                 (list white-bishop #\f #\1)
                                 (list white-knight #\g #\1)
                                 (list white-rook #\h #\1)
                                 (list white-pawn #\a #\2)
                                 (list white-pawn #\b #\2)
                                 (list white-pawn #\c #\2)
                                 (list white-pawn #\d #\2)
                                 (list white-pawn #\e #\2)
                                 (list white-pawn #\f #\2)
                                 (list white-pawn #\g #\2)
                                 (list white-pawn #\h #\2)
                                 (list black-pawn #\a #\7)
                                 (list black-pawn #\b #\7)
                                 (list black-pawn #\c #\7)
                                 (list black-pawn #\d #\7)
                                 (list black-pawn #\e #\7)
                                 (list black-pawn #\f #\7)
                                 (list black-pawn #\g #\7)
                                 (list black-pawn #\h #\7)
                                 (list black-rook #\a #\8)
                                 (list black-knight #\b #\8)
                                 (list black-bishop #\c #\8)
                                 (list black-queen #\d #\8)
                                 (list black-king #\e #\8)
                                 (list black-bishop #\f #\8)
                                 (list black-knight #\g #\8)
                                 (list black-rook #\h #\8))) ])
      (check-not-false (member tuple pieces))))

  )
