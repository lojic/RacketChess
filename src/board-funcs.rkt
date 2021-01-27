#lang racket

(require "./board.rkt")
(require "./piece.rkt")
(require "./move.rkt")

(provide make-move!
         print-board
         unmake-move!)

(define e2 (+ east east))
(define n2 (+ north north))
(define s2 (+ south south))
(define w2 (+ west west))

(define (make-move! b m)
  (let* ([ squares (board-squares b)           ]
         [ white?  (board-whites-move? b)      ]
         [ piece   (move-src m)                ]
         [ src-idx (move-src-idx m)            ]
         [ dst-idx (move-dst-idx m)            ]
         [ dst     (bytes-ref squares dst-idx) ])
    (let ([ piece
            (cond [ (is-pawn? piece)
                    (make-pawn-move! b squares m white? piece src-idx dst-idx) ]
                  [ (is-king? piece)
                    (make-king-move! b squares m white? piece src-idx dst-idx) ]
                  [ else piece ])])

      ;; Move the piece
      (bytes-set! squares dst-idx (bitwise-ior piece piece-moved-bit))
      (bytes-set! squares src-idx empty-square)

      ;; Player to move
      (set-board-whites-move?! b (not (board-whites-move? b)))

      ;; Increment depth & move-i
      (set-board-move-i! b (add1 (board-move-i b)))
      (set-board-depth! b (add1 (board-depth b))))))

;; Returns the source piece (possibly modified)
(define (make-king-move! b squares m white? piece src-idx dst-idx)
  ;; Update king idx
  (if white?
      (set-board-white-king-idx! b dst-idx)
      (set-board-black-king-idx! b dst-idx))

  ;; Handle castling
  (let ([ dist (- dst-idx src-idx) ])
    (cond [ (= dist e2)

            ;; Move rook
            (bytes-set! squares
                        (+ src-idx east)
                        (bitwise-ior (bytes-ref squares (+ src-idx (* 3 east)))
                                     piece-moved-bit))
            (bytes-set! squares
                        (+ src-idx (* 3 east))
                        empty-square) ]

          [ (= dist w2)

            ;; Move rook
            (bytes-set! squares
                        (+ src-idx west)
                        (bitwise-ior (bytes-ref squares (+ src-idx (* 4 west)))
                                     piece-moved-bit))
            (bytes-set! squares
                        (+ src-idx (* 4 west))
                        empty-square) ])

    piece))

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
       (set-ep-idx! b (+ src-idx (arithmetic-shift dist -1)) 1)
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
     ;; pawn. Mark the piece as having moved.
     [ (move-promoted-piece m)
       (bitwise-ior (move-promoted-piece m) piece-moved-bit) ]

     ;; None of the above, just return the piece
     [ else piece ])))

(define (unmake-move! b m)
  ;; Decrement depth & move-i
  (set-board-depth! b (sub1 (board-depth b)))
  (set-board-move-i! b (sub1 (board-move-i b)))

  ;; Player to move
  (set-board-whites-move?! b (not (board-whites-move? b)))

  (let* ([ squares        (board-squares b)       ]
         [ white?         (board-whites-move? b)  ]
         [ src-idx        (move-src-idx m)        ]
         [ dst-idx        (move-dst-idx m)        ]
         [ captured-piece (move-captured-piece m) ]
         [ piece          (move-src m)            ])

    ;; Move piece back to src position. Since we stored the piece in
    ;; the move, when we place it back at the src position, we restore
    ;; any modified bits also. This also undoes a promotion since the
    ;; src piece will be a pawn.
    (bytes-set! squares src-idx piece)

    (if captured-piece
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
      (unmake-king-move! b squares m white? piece src-idx))

    ;; Reset EP square for one level down
    (set-ep-idx! b 0 1)))


(define (unmake-king-move! b squares m white? piece src-idx)
  ;; Revert king idx
  (if white?
      (set-board-white-king-idx! b src-idx)
      (set-board-black-king-idx! b src-idx))

  ;; Handle un-castling
  (cond [ (move-is-castle-kingside? m)
          ;; Move rook back
          (bytes-set! squares (+ src-idx (* 3 east)) (bytes-ref squares (+ src-idx east)))
          (bytes-set! squares (+ src-idx east) empty-square) ]
        [ (move-is-castle-queenside? m)
          ;; Move rook back
          (bytes-set! squares (+ src-idx (* 4 west)) (bytes-ref squares (+ src-idx west)))
          (bytes-set! squares (+ src-idx west) empty-square) ]))

(define (print-board b #:full? [ full? #f ])
  (for ([ rank (in-range 8) ])
    (for ([ file (in-range 8) ])
      (let* ([ idx   (+ 21 file (* rank 10))           ]
             [ piece (bytes-ref (board-squares b) idx) ]
             [ sym   (piece-symbol piece)              ])
        (printf "~a " sym)))
    (printf "\n"))

  (if (board-whites-move? b)
      (printf "White's move\n")
      (printf "Black's move\n"))

  (when full?
    (printf "Depth: ~a. " (board-depth b))
    (printf "Move-i: ~a. " (board-move-i b))
    (when (> (get-ep-idx b) 0)
      (printf "EP Square: ~a. " (idx->pos (get-ep-idx b))))
    (printf "White king pos: ~a, Black king pos: ~a\n"
            (idx->pos (board-white-king-idx b))
            (idx->pos (board-black-king-idx b))))

  (printf "\n"))

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
