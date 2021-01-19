#lang racket

(require "./board.rkt")
(require "./piece.rkt")
(require "./move.rkt")

(require racket/performance-hint)

(provide evaluate
         make-move!
         print-board
         quiet-head
         quiet-moves
         set-quiet-head!
         set-tactical-head!
         tactical-head
         tactical-moves
         unmake-move!)

(define (evaluate b)
  (for*/sum ([ rank (in-range 8) ]
             [ file (in-range 8) ])
    (let* ([ idx   (file-rank->idx file rank)        ]
           [ piece (bytes-ref (board-squares b) idx) ])
      (cond [ (is-pawn? piece)  (evaluate-pawn b piece idx) ]
            [ (is-piece? piece) (piece-value piece)         ]
            [ else              0.0                         ]))))

(define (fen b)
  (string-join (for/list ([ rank (in-range 8) ])
                 (fen-row b rank))
               "/"))

(define (fen-row b rank)
  (let loop ([ result "" ][ file 0 ][ blanks 0 ])
    (if (> file 7)
        (string-append result (if (> blanks 0)
                                  (number->string blanks)
                                  ""))
        (let* ([ idx   (file-rank->idx file rank)        ]
               [ piece (bytes-ref (board-squares b) idx) ])
          (if (is-piece? piece)
              (loop (string-append result (piece-symbol piece))
                    (add1 file)
                    blanks)
              (loop result
                    (add1 file)
                    (add1 blanks)))))))

(define-inline (file-rank->idx file rank)
  (+ 21 file (* rank 10)))

;; Return a list of all pieces on the board with their file & rank.
(define (get-pieces-file-rank b)
  (define squares (board-squares b))

  (for*/fold ([ result '() ])
             ([ rank (in-range 8) ]
              [ file (in-range 8) ])
    (let* ([ idx   (file-rank->idx file rank)        ]
           [ pos   (idx->pos idx)                    ]
           [ piece (bytes-ref (board-squares b) idx) ])
      (if (is-piece? piece)
          (cons (list piece (string-ref pos 0) (string-ref pos 1)) result)
          result))))

;; Not sure if this is a good idea, but give the 4 center positions
;; higher value to encourage controlling the center.
(define (evaluate-pawn b piece idx)
  (let ([ val (piece-value piece) ])
    (cond [ (= idx 64) (* val 1.02) ]
          [ (= idx 65) (* val 1.02) ]
          [ (= idx 54) (* val 1.02) ]
          [ (= idx 55) (* val 1.02) ]
          [ else       val          ])))

(define (make-move! b m)
  ;; Reset en passant capture idx (we may set it below if double push)
  (set-board-ep-idx! b 0)

  (let* ([ squares (board-squares b)           ]
         [ src-idx (move-src-idx m)            ]
         [ dst-idx (move-dst-idx m)            ]
         [ src     (bytes-ref squares src-idx) ]
         [ dst     (bytes-ref squares dst-idx) ])
    (cond [ (is-pawn? src)
            (let ([ dist (- dst-idx src-idx) ])
              (cond
               ;; If a pawn has moved 2 spaces, record the en
               ;; passant capture idx
               [ (or (= dist (* north 2))
                     (= dist (* south 2)))
                 (set-board-ep-idx! b (+ src-idx (arithmetic-shift dist -1))) ]

               ;; Handle en passant capture
               [ (move-is-ep-capture? m)
                 ;; We've already stored the captured pawn in
                 ;; captured_piece, so just remove the captured pawn
                 ;; from the board
                 (let ([ target (+ dst-idx (if (is-white? src) south north)) ])
                   (bytes-set! squares target empty-square)) ]

               ;; Handle promotion
               [ (move-promoted-piece m)
                 (set! src (move-promoted-piece m)) ])) ]

          [ (is-king? src)
            ;; Handle castling
            (let ([ dist (- dst-idx src-idx) ])
              (cond [ (= dist (* 2 east))
                      (set-move-is-castle-kingside?! m #t)
                      (set! src (bitwise-ior src king-castled-bit))
                      ;; Move rook
                      (bytes-set! squares
                                  (+ src-idx east)
                                  (bitwise-ior (bytes-ref squares (+ src-idx (* 3 east)))
                                               piece-moved-bit))
                      (bytes-set! squares
                                  (+ src-idx (* 3 east))
                                  empty-square) ]

                    [ (= dist (* 2 west))
                      (set-move-is-castle-queenside?! m #t)
                      (set! src (bitwise-ior src king-castled-bit))
                      ;; Move rook
                      (bytes-set! squares
                                  (+ src-idx west)
                                  (bitwise-ior (bytes-ref squares (+ src-idx (* 4 west)))
                                               piece-moved-bit))
                      (bytes-set! squares
                                  (+ src-idx (* 4 west))
                                  empty-square) ])) ])

    ;; Move
    (bytes-set! squares dst-idx (bitwise-ior src piece-moved-bit))
    (bytes-set! squares src-idx empty-square)
    (set-board-depth! b (add1 (board-depth b)))
    (set-board-whites-move?! b (not (board-whites-move? b)))))

(define (unmake-move! b m)
  ;; Reset en passant capture idx
  (set-board-ep-idx! b 0)

  (let* ([ squares (board-squares b)           ]
         [ src-idx (move-src-idx m)            ]
         [ dst-idx (move-dst-idx m)            ]
         [ src     (bytes-ref squares src-idx) ]
         [ dst     (bytes-ref squares dst-idx) ])
    ;; Move piece back to src position By storing the original piece in
    ;; the Move struct, when we restore it, we also restore the king
    ;; castled bit
    (bytes-set! squares src-idx (move-src m))

    ;; Replace captured piece if it exists; otherwise set dst to empty
    (cond [ (not (move-captured-piece m))
            (bytes-set! squares dst-idx empty-square) ]
          [ (move-is-ep-capture? m)
            ;; Unmake en passant capture
            (bytes-set! squares dst-idx empty-square)
            (let ([ target (+ dst-idx
                              (if (is-white? (move-src m)) south north))])
              (bytes-set! squares target (move-captured-piece m))) ]
          [ else
            (bytes-set! squares dst-idx (move-captured-piece m)) ])

    ;; Handle un-castling
    (cond [ (move-is-castle-kingside? m)
            ;; Move rook back
            (bytes-set! squares (+ src-idx (* 3 east)) (bytes-ref squares (+ src-idx east)))
            (bytes-set! squares (+ src-idx east) empty-square) ]
          [ (move-is-castle-queenside? m)
            ;; Move rook back
            (bytes-set! squares (+ src-idx (* 4 west)) (bytes-ref squares (+ src-idx west)))
            (bytes-set! squares (+ src-idx west) empty-square) ])

    ;; Reset player to move an depth
    (set-board-whites-move?! b (not (board-whites-move? b)))
    (set-board-depth! b (sub1 (board-depth b)))))

(define (print-board b #:full [ full #f ])
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

  (when full
    (printf "Depth: ~a. " (board-depth b))
    (when (> (board-ep-idx b) 0)
      (printf "EP Square: ~a. " (idx->pos (board-ep-idx b)))))

  (printf "\n"))

(define (quiet-head b [ d #f ])
  (vector-ref (board-quiet-head b)
              (if d d (board-depth b))))

(define (quiet-moves b [ d #f ])
  (vector-ref (board-quiet-moves b)
              (if d d (board-depth b))))

(define (set-quiet-head! b v [ d #f ])
  (vector-set! (board-quiet-head b)
               (if d d (board-depth b))
               v))

(define (set-tactical-head! b v [ d #f ])
  (vector-set! (board-tactical-head b)
               (if d d (board-depth b))
               v))

(define (tactical-head b [ d #f ])
  (vector-ref (board-tactical-head b)
              (if d d (board-depth b))))

(define (tactical-moves b [ d #f ])
  (vector-ref (board-tactical-moves b)
              (if d d (board-depth b))))

(define (run)
  (let* ([ b (create-board) ]
         [ squares (board-squares b) ]
         [ src-idx (pos->idx "e2") ]
         [ dst-idx (pos->idx "e4") ]
         [ src (bytes-ref squares src-idx) ]
         [ m (create-move src src-idx dst-idx) ])
    (print-board b #:full #t)
    (make-move! b m)
    (print-board b #:full #t)
    (unmake-move! b m)
    (print-board b #:full #t)
    ))

(module+ test
  (require rackunit)

  ;; ------------------------------------------------------------------------------------------
  ;; evaluate
  ;; ------------------------------------------------------------------------------------------
  (let ([ b (create-board) ])
    (check-within (evaluate b) 0.0 0.00001)
    ;; Remove the White Queen
    (bytes-set! (board-squares b) 94 empty-square)
    (check-within (evaluate b) -9.0 0.00001))

  ;; ------------------------------------------------------------------------------------------
  ;; fen
  ;; ------------------------------------------------------------------------------------------

  (let ([ b (create-board) ])
    (check-equal? (fen-row b 0) "rnbqkbnr")
    (check-equal? (fen-row b 1) "pppppppp")
    (check-equal? (fen-row b 2) "8")
    (check-equal? (fen-row b 3) "8")
    (check-equal? (fen-row b 4) "8")
    (check-equal? (fen-row b 5) "8")
    (check-equal? (fen-row b 6) "PPPPPPPP")
    (check-equal? (fen-row b 7) "RNBQKBNR")
    (check-equal? (fen b) "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"))

  ;; ------------------------------------------------------------------------------------------
  ;; get-pieces-file-rank
  ;; ------------------------------------------------------------------------------------------
  (let* ([ b      (create-board)           ]
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
