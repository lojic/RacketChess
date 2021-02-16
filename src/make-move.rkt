#lang racket

(require "./board.rkt"
         "./board-utilities.rkt"
         "./global.rkt"
         "./piece.rkt"
         "./move.rkt"
         "./state.rkt"
         "./zobrist.rkt")
(require racket/performance-hint)

(provide get-hash-key
         make-move!
         set-hash-key!
         unmake-move!)

(define e2 (+ east east))
(define e3 (+ east east east))
(define n2 (+ north north))
(define s2 (+ south south))
(define w2 (+ west west))
(define w4 (+ west west west west))

;; Zobrist Key --------------------------------------------------------------------------------
(define hash-key 0)

(define (get-hash-key)
  hash-key)

(define (set-hash-key! val)
  (set! hash-key val))

(define-inline (xor-castling! s)
  (set! hash-key (xor-castling hash-key s)))

(define-inline (xor-ep-square! ep-idx)
  (set! hash-key (xor-ep-square hash-key ep-idx)))

(define-inline (xor-piece! piece idx)
  (set! hash-key (xor-piece hash-key piece idx)))

(define-inline (xor-whites-move!)
  (set! hash-key (xor-whites-move hash-key)))
;; --------------------------------------------------------------------------------------------

(define-inline (reset-ep-square! b)
  (let ([ ep-idx (get-ep-idx b) ])
    (when (fx> ep-idx 0)
      (xor-ep-square! ep-idx) ; Remove ep square from key
      (set-ep-idx! b 0))))

(define-inline (move-piece! squares from-piece from-idx to-piece to-idx dst-piece)
  ; Remove "from" piece
  (xor-piece! from-piece from-idx)

  ; Remove "dst" piece if it exists
  (when (not (fx= dst-piece empty-square))
    (xor-piece! dst-piece to-idx))

  ; Add "to" piece
  (xor-piece! to-piece to-idx)

  (set-square! squares to-idx to-piece)
  (set-square! squares from-idx empty-square))

(define (make-move! b m)
  ;; Save game state and increment move-i
  (push-game-state! b)

  ;; Reset EP square
  (reset-ep-square! b)

  (let* ([ squares    (board-squares b)           ]
         [ white?     (is-whites-move? b)         ]
         [ castle-ok? (may-castle? b white?)      ]
         [ orig-piece (move-src m)                ]
         [ piece      orig-piece                  ]
         [ src-idx    (move-src-idx m)            ]
         [ dst-idx    (move-dst-idx m)            ]
         [ dst        (get-square squares dst-idx) ])
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
      (move-piece! squares orig-piece src-idx piece dst-idx dst)

      ;; Player to move
      (xor-whites-move!)
      (set-whites-move?! b (not white?))

      ;; Increment depth
      (set-board-depth! b (fx+ 1 (board-depth b))))))

(define (revoke-rook-castling! b white? src-idx dst-idx)
  (let* ([ orig (board-game-state b) ]
         [ s orig ])
    (if white?
        (begin
          ;; Moving white queenside rook
          (when (and (fx= 91 src-idx) (state-w-queenside-ok? s))
            (set! s (unset-state-w-queenside-ok? s)))

          ;; Moving white kingside rook
          (when (and (fx= 98 src-idx) (state-w-kingside-ok? s))
            (set! s (unset-state-w-kingside-ok? s)))

          ;; Capturing black queenside rook
          (when (and (fx= 21 dst-idx) (state-b-queenside-ok? s))
            (set! s (unset-state-b-queenside-ok? s)))

          ;; Capturing black kingside rook
          (when (and (fx= 28 dst-idx) (state-b-kingside-ok? s))
            (set! s (unset-state-b-kingside-ok? s))))

        (begin
          ;; Moving black queenside rook
          (when (and (fx= 21 src-idx) (state-b-queenside-ok? s))
            (set! s (unset-state-b-queenside-ok? s)))

          ;; Moving black kingside rook
          (when (and (fx= 28 src-idx) (state-b-kingside-ok? s))
            (set! s (unset-state-b-kingside-ok? s)))

          ;; Capturing white queenside rook
          (when (and (fx= 91 dst-idx) (state-w-queenside-ok? s))
            (set! s (unset-state-w-queenside-ok? s)))

          ;; Capturing white kingside rook
          (when (and (fx= 98 dst-idx) (state-w-kingside-ok? s))
            (set! s (unset-state-w-kingside-ok? s)))))

    (when (not (fx= orig s))
      (xor-castling! orig)
      (xor-castling! s)
      (set-board-game-state! b s))))

;; Returns the source piece (possibly modified)
(define (make-king-move! b squares m white? castle-ok? piece src-idx dst-idx)
  ;; Update king idx
  (if white?
      (set-white-king-idx! b dst-idx)
      (set-black-king-idx! b dst-idx))

  (when castle-ok?
    (xor-castling! (board-game-state b))
    (revoke-castling! b white?)
    (xor-castling! (board-game-state b)))

  ;; Handle castling
  (let ([ dist (- dst-idx src-idx) ])
    (cond [ (fx= dist e2)
            ;; Move king side rook
            (let* ([ from (fx+ src-idx e3) ]
                   [ rook (get-square squares from) ]
                   [ to   (fx+ src-idx east) ])
              (xor-piece! rook from) ; Remove rook
              (xor-piece! rook to)   ; Add rook
              (set-square! squares to rook)
              (set-square! squares from empty-square)) ]

          [ (fx= dist w2)
            ;; Move queenside rook
            (let* ([ from (fx+ src-idx w4) ]
                   [ rook (get-square squares from) ]
                   [ to   (fx+ src-idx west) ])
              (xor-piece! rook from) ; Remove rook
              (xor-piece! rook to)   ; Add rook
              (set-square! squares to rook)
              (set-square! squares from empty-square)) ])))

;; Returns the source piece (possibly modified)
(define (make-pawn-move! b squares m white? piece src-idx dst-idx)
  (let ([ dist (fx- dst-idx src-idx) ])

    ;; NOTE: the cond clauses are mutually exclusive, only one of them
    ;;       could occur:
    ;; * Pawn has moved 2 spaces
    ;; * Pawn has performed an en passant capture
    ;; * Pawn has reached the last rank and was promoted
    ;; * None of the above
    (cond
     ;; If a pawn has moved 2 spaces, record the en
     ;; passant capture idx
     [ (or (fx= dist n2) (fx= dist s2))
       ;; Set the EP target
       (let ([ ep-idx (fx+ src-idx (fxrshift dist 1)) ])
         (xor-ep-square! ep-idx)
         (set-ep-idx! b ep-idx))
       piece ]

     ;; Handle en passant capture
     [ (move-is-ep-capture? m)
       ;; We've already stored the captured pawn in
       ;; captured_piece, so just remove the captured pawn
       ;; from the board
       (let ([ target (fx+ dst-idx (if white? south north)) ])
         (xor-piece! (move-captured-piece m) target) ; Removed captured ep pawn
         (set-square! squares target empty-square))
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

  (xor-whites-move!)

  ;; Restore game state and decrement move-i
  (let* ([ s1 (board-game-state b) ])
    (pop-game-state! b)
    (let* ([ s2 (board-game-state b) ]
           [ ep1 (state-ep-idx s1) ]
           [ ep2 (state-ep-idx s2) ])
      (when (not (fx= (castling-zobrist s1) (castling-zobrist s2)))
        (xor-castling! s1)
        (xor-castling! s2))
      (when (not (fx= ep1 ep2))
        (cond [ (fx= ep1 0) (xor-ep-square! ep2) ]
              [ (fx= ep2 0) (xor-ep-square! ep1) ]
              [ else
                (xor-ep-square! ep1)
                (xor-ep-square! ep2) ]))))

  (let* ([ squares        (board-squares b)       ]
         [ white?         (is-whites-move? b)     ]
         [ src-idx        (move-src-idx m)        ]
         [ dst-idx        (move-dst-idx m)        ]
         [ captured-piece (move-captured-piece m) ]
         [ promoted-piece (move-promoted-piece m) ]
         [ piece          (move-src m)            ])

    ;; Move piece back to src position. This also undoes a promotion
    ;; since the src piece will be a pawn.
    (xor-piece! piece src-idx) ; Add piece to src-idx
    (set-square! squares src-idx piece)

    (if (fx> captured-piece 0)
        ;; Unmake capture move
        (cond [ (move-is-ep-capture? m)
                ;; Unmake en passant capture
                (xor-piece! piece dst-idx) ; remove pawn
                (set-square! squares dst-idx empty-square)

                (let ([ target (fx+ dst-idx
                                    (if white? south north))])
                  (xor-piece! captured-piece target) ; Add captured piece back
                  (set-square! squares target captured-piece)) ]
              [ else
                ;; Unmake regular capture
                (if (fx> promoted-piece 0)
                    (xor-piece! promoted-piece dst-idx) ; Remove promoted piece
                    (xor-piece! piece dst-idx))         ; Remove piece
                (xor-piece! captured-piece dst-idx) ; Add captured piece back
                (set-square! squares dst-idx captured-piece) ])

        ;; Unmake quiet move
        (begin
          (if (fx> promoted-piece 0)
              (xor-piece! promoted-piece dst-idx) ; Remove promoted piece
              (xor-piece! piece dst-idx))         ; Remove piece
          (set-square! squares dst-idx empty-square)))

    (when (is-king? piece)
      (unmake-king-move! b squares m white? piece src-idx))))

(define (unmake-king-move! b squares m white? piece src-idx)
  ;; Handle un-castling.
  (cond [ (move-is-castle-kingside? m)
          ;; Move king side rook back
          (let* ([ from (fx+ src-idx east) ]
                 [ rook (get-square squares from) ]
                 [ to   (fx+ src-idx e3) ])
            (xor-piece! rook from) ; Remove rook
            (xor-piece! rook to)   ; Add rook
            (set-square! squares to rook)
            (set-square! squares from empty-square)) ]
        [ (move-is-castle-queenside? m)
          ;; Move queen side rook back
          (let* ([ from (fx+ src-idx west) ]
                 [ rook (get-square squares from) ]
                 [ to   (fx+ src-idx w4) ])
            (xor-piece! rook from) ; Remove rook
            (xor-piece! rook to)   ; Add rook
            (set-square! squares to rook)
            (set-square! squares from empty-square)) ]))

(define (run)
  (let* ([ b (create-board) ]
         [ squares (board-squares b) ]
         [ src-idx (pos->idx "e2") ]
         [ dst-idx (pos->idx "e4") ]
         [ src (get-square squares src-idx) ]
         [ m (create-move src src-idx dst-idx) ])
    (print-board b #:full? #t)
    (make-move! b m)
    (print-board b #:full? #t)
    (unmake-move! b m)
    (print-board b #:full? #t)
    ))
