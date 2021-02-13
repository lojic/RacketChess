#lang racket

(require "./board.rkt"
         "./piece.rkt"
         "./move.rkt"
         "./state.rkt")
(require racket/fixnum)

(provide make-move!
         print-board
         unmake-move!)

(define e2 (+ east east))
(define e3 (+ east east east))
(define n2 (+ north north))
(define s2 (+ south south))
(define w2 (+ west west))
(define w4 (+ west west west west))

(define (make-move! b m)
  ;; Save game state and increment move-i
  (push-game-state! b)

  ;; Reset EP square
  (set-ep-idx! b 0)

  (let* ([ squares    (board-squares b)           ]
         [ white?     (is-whites-move? b)         ]
         [ castle-ok? (may-castle? b white?)      ]
         [ piece      (move-src m)                ]
         [ src-idx    (move-src-idx m)            ]
         [ dst-idx    (move-dst-idx m)            ]
         [ dst        (bytes-ref squares dst-idx) ])
    (let ([ piece
            (cond [ (is-pawn? piece)
                    (make-pawn-move! b squares m white? piece src-idx dst-idx) ]
                  [ (is-king? piece)
                    (make-king-move! b squares m white? castle-ok? piece src-idx dst-idx)
                    piece ]
                  [ else piece ])])

      ;; Rook castling revocations
      (when (or castle-ok? (may-castle? b (not white?)))
        (revoke-rook-castling! b white? src-idx dst-idx))

      ;; Move the piece
      (bytes-set! squares dst-idx piece)
      (bytes-set! squares src-idx empty-square)

      ;; Player to move
      (set-whites-move?! b (not (is-whites-move? b)))

      ;; Increment depth
      (set-board-depth! b (add1 (board-depth b))))))

(define (revoke-rook-castling! b white? src-idx dst-idx)
  (let ([ s (board-game-state b) ])
    (if white?
        (begin
          ;; Moving white queenside rook
          (when (and (= 91 src-idx) (state-w-queenside-ok? s))
            (set! s (unset-state-w-queenside-ok? s)))

          ;; Moving white kingside rook
          (when (and (= 98 src-idx) (state-w-kingside-ok? s))
            (set! s (unset-state-w-kingside-ok? s)))

          ;; Capturing black queenside rook
          (when (and (= 21 dst-idx) (state-b-queenside-ok? s))
            (set! s (unset-state-b-queenside-ok? s)))

          ;; Capturing black kingside rook
          (when (and (= 28 dst-idx) (state-b-kingside-ok? s))
            (set! s (unset-state-b-kingside-ok? s))))

        (begin
          ;; Moving black queenside rook
          (when (and (= 21 src-idx) (state-b-queenside-ok? s))
            (set! s (unset-state-b-queenside-ok? s)))

          ;; Moving black kingside rook
          (when (and (= 28 src-idx) (state-b-kingside-ok? s))
            (set! s (unset-state-b-kingside-ok? s)))

          ;; Capturing white queenside rook
          (when (and (= 91 dst-idx) (state-w-queenside-ok? s))
            (set! s (unset-state-w-queenside-ok? s)))

          ;; Capturing white kingside rook
          (when (and (= 98 dst-idx) (state-w-kingside-ok? s))
            (set! s (unset-state-w-kingside-ok? s)))))

    (set-board-game-state! b s)))

;; Returns the source piece (possibly modified)
(define (make-king-move! b squares m white? castle-ok? piece src-idx dst-idx)
  ;; Update king idx
  (if white?
      (set-white-king-idx! b dst-idx)
      (set-black-king-idx! b dst-idx))

  (when castle-ok?
    (revoke-castling! b white?))

  ;; Handle castling
  (let ([ dist (- dst-idx src-idx) ])
    (cond [ (= dist e2)
            ;; Move king side rook
            (bytes-set! squares
                        (+ src-idx east)
                        (bytes-ref squares (+ src-idx e3)))
            (bytes-set! squares
                        (+ src-idx e3)
                        empty-square) ]

          [ (= dist w2)
            ;; Move queenside rook
            (bytes-set! squares
                        (+ src-idx west)
                        (bytes-ref squares (+ src-idx w4)))
            (bytes-set! squares
                        (+ src-idx w4)
                        empty-square) ])))

;; Returns the source piece (possibly modified)
(define (make-pawn-move! b squares m white? piece src-idx dst-idx)
  (let ([ dist (- dst-idx src-idx) ])

    ;; NOTE: the cond clauses are mutually exclusive, only one of them
    ;;       could occur:
    ;; * Pawn has moved 2 spaces
    ;; * Pawn has performed an en passant capture
    ;; * Pawn has reached the last rank and was promoted
    ;; * None of the above
    (cond
     ;; If a pawn has moved 2 spaces, record the en
     ;; passant capture idx
     [ (or (= dist n2) (= dist s2))
       ;; Set the EP target on the next level down
       (set-ep-idx! b (+ src-idx (arithmetic-shift dist -1)))
       piece ]

     ;; Handle en passant capture
     [ (move-is-ep-capture? m)
       ;; We've already stored the captured pawn in
       ;; captured_piece, so just remove the captured pawn
       ;; from the board
       (let ([ target (+ dst-idx (if white? south north)) ])
         (bytes-set! squares target empty-square))
       piece ]

     ;; Handle promotion - return the promoted piece instead of the
     ;; pawn.
     [ (fx> (move-promoted-piece m) 0)
       (move-promoted-piece m) ]

     ;; None of the above, just return the piece
     [ else piece ])))

(define (unmake-move! b m)
  ;; Decrement depth
  (set-board-depth! b (sub1 (board-depth b)))

  ;; Restore game state and decrement move-i
  (pop-game-state! b)

  (let* ([ squares        (board-squares b)       ]
         [ white?         (is-whites-move? b)     ]
         [ src-idx        (move-src-idx m)        ]
         [ dst-idx        (move-dst-idx m)        ]
         [ captured-piece (move-captured-piece m) ]
         [ piece          (move-src m)            ])

    ;; Move piece back to src position. Since we stored the piece in
    ;; the move, when we place it back at the src position, we restore
    ;; any modified bits also. This also undoes a promotion since the
    ;; src piece will be a pawn.
    (bytes-set! squares src-idx piece)

    (if (fx> captured-piece 0)
        ;; Unmake capture move
        (cond [ (move-is-ep-capture? m)
                ;; Unmake en passant capture
                (bytes-set! squares dst-idx empty-square)
                (let ([ target (+ dst-idx
                                  (if white? south north))])
                  (bytes-set! squares target captured-piece)) ]
              [ else
                ;; Unmake regular capture
                (bytes-set! squares dst-idx captured-piece) ])

        ;; Unmake quiet move
        (bytes-set! squares dst-idx empty-square))

    (when (is-king? piece)
      (unmake-king-move! b squares m white? piece src-idx))))

(define (unmake-king-move! b squares m white? piece src-idx)
  ;; Handle un-castling.
  (cond [ (move-is-castle-kingside? m)
          ;; Move rook back
          (bytes-set! squares
                      (+ src-idx e3)
                      (bytes-ref squares (+ src-idx east)))
          (bytes-set! squares (+ src-idx east) empty-square) ]
        [ (move-is-castle-queenside? m)
          ;; Move rook back
          (bytes-set! squares
                      (+ src-idx w4)
                      (bytes-ref squares (+ src-idx west)))
          (bytes-set! squares (+ src-idx west) empty-square) ]))

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
             [ piece (bytes-ref (board-squares b) idx) ]
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

(define (run)
  (let* ([ b (create-board) ]
         [ squares (board-squares b) ]
         [ src-idx (pos->idx "e2") ]
         [ dst-idx (pos->idx "e4") ]
         [ src (bytes-ref squares src-idx) ]
         [ m (create-move src src-idx dst-idx) ])
    (print-board b #:full? #t)
    (make-move! b m)
    (print-board b #:full? #t)
    (unmake-move! b m)
    (print-board b #:full? #t)
    ))

(module+ test
  (require rackunit)

  )
