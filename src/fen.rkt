#lang racket

;; See https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation

(require "./board.rkt"
         "./board-slow.rkt"
         "./make-move.rkt"
         "./piece.rkt"
         "./state.rkt")

(require debug/repl)

(provide board->fen
         fen->board
         initial-fen)

(define initial-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

;; --------------------------------------------------------------------------------------------
;; Construct a board from a FEN record
;; --------------------------------------------------------------------------------------------

(define (fen->board [ fen initial-fen ])
  (match-let ([ (list placement
                      active-color
                      castling
                      ep-target
                      half-move
                      full-move) (string-split fen) ]
              [ b (create-board) ])
    (fen->board-placement!    b placement)
    (fen->board-active-color! b active-color)
    (fen->board-half-move!    b half-move)
    (fen->board-full-move!    b full-move active-color)
    (fen->board-castling!     b castling)
    (fen->board-ep-target!    b ep-target)
    b))

(define (fen->board-active-color! b active-color)
  (cond [ (string=? "w" active-color) (set-whites-move?! b #t) ]
        [ (string=? "b" active-color) (set-whites-move?! b #f) ]
        [ else (error "fen->board-active-color!: invalid active color") ]))

(define (fen->board-castling! b castling)
  (let ([ s (board-game-state b) ])
    ;; Unset all castling
    (set! s (unset-state-b-kingside-ok? s))
    (set! s (unset-state-b-queenside-ok? s))
    (set! s (unset-state-w-kingside-ok? s))
    (set! s (unset-state-w-queenside-ok? s))

    (if (regexp-match? #px"^([KQkq]+|-)$" castling)
        (begin
          (when (string-contains? castling "K")
            (set! s (set-state-w-kingside-ok? s)))
          (when (string-contains? castling "Q")
            (set! s (set-state-w-queenside-ok? s)))
          (when (string-contains? castling "k")
            (set! s (set-state-b-kingside-ok? s)))
          (when (string-contains? castling "q")
            (set! s (set-state-b-queenside-ok? s)))
          (set-board-game-state! b s))
        (error "fen->board-castling!: invalid castling string"))))

(define (fen->board-ep-target! b ep-target)
  (when (not (string=? ep-target "-"))
    (if (regexp-match? #px"^[a-h][0-9]$" ep-target)
        (set-ep-idx! b (pos->idx ep-target))
        (error "fen->board-ep-target!: invalid ep target"))))

(define (fen->board-half-move! b half-move)
  ;; We don't use the half move clock yet
  (void))

(define (fen->board-full-move! b full-move active-color)
  (if (regexp-match? #px"^[0-9]+$" full-move)
      (set-full-move! b (string->number full-move) (string=? "b" active-color))
      (error "fen->board-full-move!: full-move must be a number")))

(define (fen->board-placement! b placement)
  (for ([ fen-row (in-list (string-split placement "/")) ]
        [ rank    (in-range 8)                     ])
      (fen->board-placement-row! b rank fen-row)))

(define (fen->board-placement-row! b rank fen-row)
  (define squares (board-squares b))

  (let loop ([ file 0 ][ chars (string->list fen-row) ])
    (when (and (< file 8) (not (null? chars)))
      (let ([ c (car chars) ])
        (if (char-numeric? c)
            (loop (+ file (- (char->integer c) (char->integer #\0))) (cdr chars))
            (let* ([ piece (symbol-piece (~a c)) ]
                   [ idx   (file-rank->idx file rank) ])
              (bytes-set! squares idx piece)
              (when (is-king? piece)
                (fen->board-set-king-pos! b piece idx))
              (loop (add1 file) (cdr chars))))))))

(define (fen->board-set-king-pos! b piece idx)
  (when (not (is-king? piece)) (error "fen->board-set-king-pos!: not a king"))

  (if (is-white? piece)
      (set-white-king-idx! b idx)
      (set-black-king-idx! b idx)))

;; --------------------------------------------------------------------------------------------
;; Create a FEN record to represent a board
;; --------------------------------------------------------------------------------------------

;; Return a FEN record representing the board. For example, the FEN
;; record for the inital board is:
;; rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
(define (board->fen b)
  (string-join (list (board->fen-placement b)
                     (board->fen-active-color b)
                     (board->fen-castling b)
                     (board->fen-en-passant b)
                     (board->fen-half-move b)
                     (board->fen-full-move b))
               " "))

;; Return active color. "w" => white's move. "b" => black's move.
(define (board->fen-active-color b)
  (if (is-whites-move? b)
      "w"
      "b"))

;; Castling rights. Zero or more letters from "KQkq"
;; K => White may castle king side
;; Q => Qhite may castle queen side
;; k => Black may castle king side
;; q => Black may castle queen side
(define (board->fen-castling b)
  (let* ([ s (board-game-state b) ]
         [ castling
           (string-join
            (filter identity (list (if (state-w-kingside-ok? s)  "K" #f)
                                   (if (state-w-queenside-ok? s) "Q" #f)
                                   (if (state-b-kingside-ok? s)  "k" #f)
                                   (if (state-b-queenside-ok? s) "q" #f)))
            "") ])
    (if (non-empty-string? castling)
        castling
        "-")))

;; En passant target square in algebraic notation. If there's no en
;; passant target square, this is "-". If a pawn has just made a
;; two-square move, this is the position "behind" the pawn.
(define (board->fen-en-passant b)
  (let ([ idx (get-ep-idx b) ])
    (if (= idx 0)
        "-"
        (idx->pos idx))))

;; Fullmove number: The number of the full move. It starts at 1, and
;; is incremented after Black's move.
(define (board->fen-full-move b)
  (number->string (full-move b)))

;; Halfmove clock: This is the number of halfmoves since the last
;; capture or pawn advance. The reason for this field is that the
;; value is used in the fifty-move rule.
(define (board->fen-half-move b)
  ;; We don't use the half-move clock, yet
  "0")

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
          (let ([ piece (bytes-ref (board-squares b)
                                   (file-rank->idx file rank)) ]
                [ file (add1 file) ])
            (if (is-piece? piece)
                ;; The square contains a piece, add it to result after
                ;; adding blanks, and reset blanks to 0
                (loop (cons (piece-symbol piece) (add-blanks blanks result)) file 0)
                ;; The square is empty, increment blanks
                (loop result file (add1 blanks)))))))

  (string-join (reverse (placement-list)) ""))

(module+ main
  (let ([ b
          (fen->board "r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1") ])
    (void)))

(module+ test
  (require rackunit)

  ;; ------------------------------------------------------------------------------------------
  ;; fen->board!
  ;; ------------------------------------------------------------------------------------------

  (let* ([ fen "8/2pR1p2/1pP3kp/p7/5rp1/2P5/r3NKPP/5B1R w - - 0 34" ]
         [ b   (fen->board fen)                                     ]
         [ s   (board-game-state b)                                 ])

    (check-equal? (board->fen b) fen)
    (check-false (state-w-kingside-ok? s))
    (check-false (state-w-queenside-ok? s))
    (check-false (state-b-kingside-ok? s))
    (check-false (state-b-queenside-ok? s))

    (check-equal? (black-king-idx b) (pos->idx "g6"))
    (check-equal? (white-king-idx b) (pos->idx "f2")))

  ;;                                K  Q  k  q
  (for ([ tuple (in-list '(("-"    #f #f #f #f)
                           ("q"    #f #f #f #t)
                           ("k"    #f #f #t #f)
                           ("kq"   #f #f #t #t)
                           ("Q"    #f #t #f #f)
                           ("Qq"   #f #t #f #t)
                           ("Qk"   #f #t #t #f)
                           ("Qkq"  #f #t #t #t)
                           ("K"    #t #f #f #f)
                           ("Kq"   #t #f #f #t)
                           ("Kk"   #t #f #t #f)
                           ("Kkq"  #t #f #t #t)
                           ("KQ"   #t #t #f #f)
                           ("KQq"  #t #t #f #t)
                           ("KQk"  #t #t #t #f)
                           ("KQkq" #t #t #t #t))) ])
    (let* ([ b (fen->board (format "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w ~a - 0 1" (car tuple))) ]
           [ s(board-game-state b) ])
      (if (second tuple)
          (check-not-false (state-w-kingside-ok? s))
          (check-false (state-w-kingside-ok? s)))
      (if (third tuple)
          (check-not-false (state-w-queenside-ok? s))
          (check-false (state-w-queenside-ok? s)))
      (if (fourth tuple)
          (check-not-false (state-b-kingside-ok? s))
          (check-false (state-b-kingside-ok? s)))
      (if (fifth tuple)
          (check-not-false (state-b-queenside-ok? s))
          (check-false (state-b-queenside-ok? s)))))

  ;; ------------------------------------------------------------------------------------------
  ;; board->fen
  ;; ------------------------------------------------------------------------------------------

  (let* ([ b (fen->board initial-fen) ])
    (check-equal? (board->fen b) initial-fen))

  (let* ([ b       (create-board)    ]
         [ squares (board-squares b) ])
    (check-equal? (board->fen b) "8/8/8/8/8/8/8/8 w KQkq - 0 1")
    (bytes-set! squares (pos->idx "e1") white-king)
    (bytes-set! squares (pos->idx "a1") white-rook)
    (bytes-set! squares (pos->idx "h1") white-rook)
    (bytes-set! squares (pos->idx "e8") black-king)
    (bytes-set! squares (pos->idx "a8") black-rook)
    (bytes-set! squares (pos->idx "h8") black-rook)
    (check-equal? (board->fen b) "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1"))

  (let ([ b (fen->board initial-fen) ])
    (check-equal? (board->fen b) initial-fen)
    (check-equal? (board->fen-placement-row b 0) "rnbqkbnr")
    (check-equal? (board->fen-placement-row b 1) "pppppppp")
    (check-equal? (board->fen-placement-row b 2) "8")
    (check-equal? (board->fen-placement-row b 3) "8")
    (check-equal? (board->fen-placement-row b 4) "8")
    (check-equal? (board->fen-placement-row b 5) "8")
    (check-equal? (board->fen-placement-row b 6) "PPPPPPPP")
    (check-equal? (board->fen-placement-row b 7) "RNBQKBNR"))

  )
