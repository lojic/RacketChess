#lang racket

(require "./board.rkt"
         "./global.rkt"
         "./piece.rkt"
         "./state.rkt")

(provide print-board)

(define (print-board b #:full? [ full? #f ])
  (define (file->letter file)
    (match file
      [ 0 "a" ]
      [ 1 "b" ]
      [ 2 "c" ]
      [ 3 "d" ]
      [ 4 "e" ]
      [ 5 "f" ]
      [ 6 "g" ]
      [ 7 "h" ]))

  (for ([ rank (in-range 8) ])
    (printf "---------------------------------\n|")
    (for ([ file (in-range 8) ])
      (let* ([ idx   (+ 21 file (* rank 10))           ]
             [ piece (get-square (board-squares b) idx) ]
             [ sym   (piece-symbol piece)              ])
        (printf " ~a |" sym)))
    (printf "  ~a\n" (- 8 rank)))
  (printf "---------------------------------\n|")
  (for ([ file (in-range 8) ])
    (printf " ~a |" (file->letter file)))
  (printf "\n")

  (if (is-whites-move? b)
      (printf "White's move\n")
      (printf "Black's move\n"))

  (when full?
    (printf "Depth: ~a. " (board-depth b))
    (printf "Move-i: ~a. " (board-move-i b))
    (when (> (get-ep-idx b) 0)
      (printf "EP Square: ~a. " (idx->pos (get-ep-idx b))))
    (printf "White king pos: ~a, Black king pos: ~a "
            (idx->pos (white-king-idx b))
            (idx->pos (black-king-idx b)))
    ;; Castling rights
    (printf "Castling: ~a\n" (castling-rights-string b)))
  (printf "\n"))

(define (castling-rights-string b)
  (define squares (board-squares b))
  (let* ([ s (board-game-state b) ]
         [ str
           (string-append
            (if (state-w-kingside-ok? s) "K" "")
            (if (state-w-queenside-ok? s) "Q" "")
            (if (state-b-kingside-ok? s) "k" "")
            (if (state-b-queenside-ok? s) "q" "")) ])
    (if (non-empty-string? str)
        str
        "-")))
