#lang racket

;; See https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation

(provide board->fen-placement
         fen-placement->board
         fen-placement->board!
         initial-fen-placement)

(require "./board.rkt"
         "./piece.rkt")

;; This FEN placement string represents an initial chess board setup
;; with white at the bottom and black at the top.
(define initial-fen-placement "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")

;; --------------------------------------------------------------------------------------------
;; Create a FEN placement string from a board
;; --------------------------------------------------------------------------------------------

;; Piece placement (from White's perspective). Each rank is described,
;; starting with rank 8 and ending with rank 1; within each rank, the
;; contents of each square are described from file "a" through file
;; "h". Following the Standard Algebraic Notation (SAN), each piece is
;; identified by a single letter taken from the standard English names
;; (pawn = "P", knight = "N", bishop = "B", rook = "R", queen = "Q"
;; and king = "K"). White pieces are designated using upper-case
;; letters ("PNBRQK") while black pieces use lowercase
;; ("pnbrqk"). Empty squares are noted using digits 1 through 8 (the
;; number of empty squares), and "/" separates ranks.
(define (board->fen-placement b)
  (string-join (for/list ([ rank (in-range 8) ])
                 (board->fen-placement-row b rank))
               "/"))

;; Placement for a single rank, e.g.: "rnbqkbnr"
(define (board->fen-placement-row b rank)
  ;; Add the number of blank squares, if any, to the result
  (define (add-blanks blanks result)
    (if (> blanks 0)
        (cons (number->string blanks) result)
        result))

  ;; Create a list of squares (in reverse order) e.g.
  ;; '("r" "2" "k" "3" "r") for "r3k2r"
  (define (placement-list)
    (let loop ([ result '() ][ file 0 ][ blanks 0 ])
      (if (> file 7)
          ;; Done with the row, add the final blanks if any
          (add-blanks blanks result)

          ;; Get the next square
          (let ([ piece (get-square b
                                   (file-rank->idx file rank)) ]
                [ file (add1 file) ])
            (if (is-piece? piece)
                ;; The square contains a piece, add it to result after
                ;; adding blanks, and reset blanks to 0
                (loop (cons (piece-symbol piece) (add-blanks blanks result)) file 0)
                ;; The square is empty, increment blanks
                (loop result file (add1 blanks)))))))

  (string-join (reverse (placement-list)) ""))

;; --------------------------------------------------------------------------------------------
;; Initialize a board from a FEN placement string
;; --------------------------------------------------------------------------------------------

;; Convenience function to create a blank board, initialize it with a
;; FEN placement, and return it
(define (fen-placement->board placement)
  (let ([ b (blank-board) ])
    (fen-placement->board! b placement)
    b))

;; Mutates a board to match the FEN placement string
(define (fen-placement->board! b placement)
  (for ([ placement-row (in-list (string-split placement "/")) ]
        [ rank          (in-range 8)                           ])
    (placement-row->board! b rank placement-row)))

(define (placement-row->board! b rank fen-row)
  (let loop ([ file 0 ][ chars (string->list fen-row) ])
    (when (and (< file 8) (not (null? chars)))
      (let ([ c (car chars) ])
        (if (char-numeric? c)
            (loop (+ file (- (char->integer c) (char->integer #\0))) (cdr chars))
            (let* ([ piece (symbol-piece (~a c)) ]
                   [ idx   (file-rank->idx file rank) ])
              (set-square! b idx piece)
              (loop (add1 file) (cdr chars))))))))

(module+ main
;  (print-board (fen-placement->board initial-fen-placement)))
  (print-board (fen-placement->board "2K3r1/8/5n2/n7/8/8/7b/8")))

;; --------------------------------------------------------------------------------------------
;; Tests
;; --------------------------------------------------------------------------------------------

(module+ test
  (require rackunit)

  ;; ------------------------------------------------------------------------------------------
  ;; Inverse relationship of fen-placement->board! and board->fen-placement
  ;; ------------------------------------------------------------------------------------------

  (check-equal? (board->fen-placement (fen-placement->board initial-fen-placement)) initial-fen-placement)

  ;; ------------------------------------------------------------------------------------------
  ;; board->fen-placement
  ;; ------------------------------------------------------------------------------------------

  (let ([ b (blank-board) ])
    (for ([ position (in-list '("a8" "b8" "c8" "d8" "e8" "f8" "g8" "h8"
                                "a7" "b7" "c7" "d7" "e7" "f7" "g7" "h7"
                                "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2"
                                "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1")) ]
          [ piece (in-list (list black-rook black-knight black-bishop black-queen
                                 black-king black-bishop black-knight black-rook
                                 black-pawn black-pawn black-pawn black-pawn
                                 black-pawn black-pawn black-pawn black-pawn
                                 white-pawn white-pawn white-pawn white-pawn
                                 white-pawn white-pawn white-pawn white-pawn
                                 white-rook white-knight white-bishop white-queen
                                 white-king white-bishop white-knight white-rook)) ])
      (set-square! b (pos->idx position) piece))

    (check-equal? (board->fen-placement b) initial-fen-placement))

  ;; ------------------------------------------------------------------------------------------
  ;;fen-placement->board!
  ;; ------------------------------------------------------------------------------------------

  (let ([ b (blank-board) ])
    (fen-placement->board! b initial-fen-placement)
    (for ([ position (in-list '("a8" "b8" "c8" "d8" "e8" "f8" "g8" "h8"
                                "a7" "b7" "c7" "d7" "e7" "f7" "g7" "h7"
                                "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2"
                                "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1")) ]
          [ piece (in-list (list black-rook black-knight black-bishop black-queen
                                 black-king black-bishop black-knight black-rook
                                 black-pawn black-pawn black-pawn black-pawn
                                 black-pawn black-pawn black-pawn black-pawn
                                 white-pawn white-pawn white-pawn white-pawn
                                 white-pawn white-pawn white-pawn white-pawn
                                 white-rook white-knight white-bishop white-queen
                                 white-king white-bishop white-knight white-rook)) ])
      (check-equal? (get-square b (pos->idx position)) piece))

    (fen-placement->board! b "p7/1p6/2p5/3p4/4P3/5P2/6P1/7P")
    (for ([ position (in-list '("a8" "b7" "c6" "d5" "e4" "f3" "g2" "h1")) ]
          [ piece (in-list (list black-pawn black-pawn black-pawn black-pawn
                                 white-pawn white-pawn white-pawn white-pawn)) ])
      (check-equal? (get-square b (pos->idx position)) piece)))

  )
