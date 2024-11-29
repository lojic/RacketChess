#lang racket

(provide generate-bishop-moves!
         generate-king-moves!
         generate-knight-moves!
         generate-queen-moves!
         generate-rook-moves!
         generate-moves!)

(require "./board.rkt"
         "./game.rkt"
         "./global.rkt"
         "./move.rkt"
         "./piece.rkt"
         "./state.rkt"
         racket/performance-hint)

;; --------------------------------------------------------------------------------------------
;; Public Interface
;; --------------------------------------------------------------------------------------------

(define (generate-moves! g #:quiet-moves? [ quiet-moves? #t ])
  (define is-white? (is-whites-move? g))

  ;; Initialize the move vectors at the current depth
  (init-moves! g)

  (for* ([ rank (in-range 8) ]
         [ file (in-range 8) ])
    (let* ([ idx   (file-rank->idx file rank)      ]
           [ piece (get-square (game-board g) idx) ])
      (when (is-right-color-piece? piece is-white?)
        (match (piece-type piece)
          [ (== pawn-piece)   (generate-pawn-moves!   g idx piece #:quiet-moves? quiet-moves?) ]
          [ (== knight-piece) (generate-knight-moves! g idx piece #:quiet-moves? quiet-moves?) ]
          [ (== bishop-piece) (generate-bishop-moves! g idx piece #:quiet-moves? quiet-moves?) ]
          [ (== rook-piece)   (generate-rook-moves!   g idx piece #:quiet-moves? quiet-moves?) ]
          [ (== queen-piece)  (generate-queen-moves!  g idx piece #:quiet-moves? quiet-moves?) ]
          [ (== king-piece)   (generate-king-moves!   g idx piece is-white?
                                                 #:quiet-moves? quiet-moves?) ])))))

(define (generate-bishop-moves! g idx piece #:quiet-moves? quiet-moves?)
  (generate-sliding-moves! g idx piece north-east quiet-moves?)
  (generate-sliding-moves! g idx piece south-east quiet-moves?)
  (generate-sliding-moves! g idx piece south-west quiet-moves?)
  (generate-sliding-moves! g idx piece north-west quiet-moves?))

(define (generate-king-moves! g idx piece is-white? #:quiet-moves? quiet-moves?)
  (generate-offset-moves! g idx piece king-offsets quiet-moves?)
  (when (and quiet-moves? (may-castle? g is-white?))
    (generate-castle-moves! g idx piece is-white?)))

(define (generate-knight-moves! g idx piece #:quiet-moves? quiet-moves?)
  (generate-offset-moves! g idx piece knight-offsets quiet-moves?))

(define (generate-queen-moves! g idx piece #:quiet-moves? quiet-moves?)
  (generate-bishop-moves! g idx piece #:quiet-moves? quiet-moves?)
  (generate-rook-moves! g idx piece #:quiet-moves? quiet-moves?))

(define (generate-rook-moves! g idx piece #:quiet-moves? quiet-moves?)
  (generate-sliding-moves! g idx piece north quiet-moves?)
  (generate-sliding-moves! g idx piece east quiet-moves?)
  (generate-sliding-moves! g idx piece south quiet-moves?)
  (generate-sliding-moves! g idx piece west quiet-moves?))

;; --------------------------------------------------------------------------------------------
;; Private Implementation
;; --------------------------------------------------------------------------------------------

(define (generate-castle-moves! g idx piece is-white?)
  (let ([ s       (game-state g) ]
        [ squares (game-board g) ])
    ;; King side
    (when (and (if is-white? (state-w-kingside-ok? s) (state-b-kingside-ok? s))
               (fx= empty-square (get-square squares (fx+ idx 1)))
               (fx= empty-square (get-square squares (fx+ idx 2))))
      (push-quiet-move! g
                       (create-move piece idx (fx+ idx 2) #:is-castle-kingside? #t)))
    ;; Queen side
    (when (and (if is-white? (state-w-queenside-ok? s) (state-b-queenside-ok? s))
               (fx= empty-square (get-square squares (fx- idx 1)))
               (fx= empty-square (get-square squares (fx- idx 2)))
               (fx= empty-square (get-square squares (fx- idx 3))))
      (push-quiet-move! g
                       (create-move piece idx (fx- idx 2) #:is-castle-queenside? #t)))))

(define (generate-offset-moves! b idx piece offsets quiet-moves?)
  (define board (game-board b))

  (let loop ([ lst offsets ])
    (when (not (null? lst))
      (let* ([ offset     (car lst)                       ]
             [ target-idx (fx+ idx offset)                ]
             [ target     (get-square board target-idx) ])
        (cond [ (fx= target empty-square)
                (when quiet-moves?
                  (push-quiet-move! b (create-move piece idx target-idx))) ]
              [ (is-other-piece? piece target)
                (push-tactical-move! b
                                    (create-move piece idx target-idx #:captured-piece target)) ]))
      (loop (cdr lst)))))

(define (generate-pawn-moves! g idx piece #:quiet-moves? quiet-moves?)
  (let-values ([ (white? n1-idx n2-idx nw-idx w-idx ne-idx e-idx min8th max8th min2nd max2nd)
                 (if (is-white? piece)
                     (values #t
                             (fx+ idx north)
                             (fx+ idx north north)
                             (fx+ idx north-west)
                             (fx+ idx west)
                             (fx+ idx north-east)
                             (fx+ idx east)
                             21 28 81 88)
                     (values #f
                             (fx+ idx south)
                             (fx+ idx south south)
                             (fx+ idx south-west)
                             (fx+ idx west)
                             (fx+ idx south-east)
                             (fx+ idx east)
                             91 98 31 38)) ])
    (let* ([ squares    (game-board g)              ]
           [ n1         (get-square squares n1-idx) ]
           [ n2         (get-square squares n2-idx) ]
           [ nw         (get-square squares nw-idx) ]
           [ ne         (get-square squares ne-idx) ]
           [ last-rank? (<= min8th n1-idx max8th)   ])

      (when (and (fx= n1 empty-square) quiet-moves?)
        ;; Single push
        (if last-rank?
            ;; Quiet promotions
            (generate-quiet-promotions! g white? piece idx n1-idx)

            ;; Regular single push
            (push-quiet-move! g (create-move piece idx n1-idx)))

        ;; Double push (only if single push was allowed)
        (when (and (fx<= min2nd idx max2nd)
                   (fx= n2 empty-square))
          (push-quiet-move! g (create-move piece idx n2-idx))))


      ;; Capture north west
      (cond [ (is-other-piece? piece nw)
              (if last-rank?
                  (generate-capture-promotions! g white? piece idx nw-idx nw)
                  (push-tactical-move! g (create-move piece idx nw-idx #:captured-piece nw))) ]
            [ (fx= (get-ep-idx g) nw-idx)
              ;; En passant capture
              (push-tactical-move! g
                                 (create-move piece idx nw-idx
                                              #:captured-piece (get-square squares w-idx) #:is-ep-capture? #t)) ])

      ;; Capture north east
      (cond [ (is-other-piece? piece ne)
              (if last-rank?
                  (generate-capture-promotions! g white? piece idx ne-idx ne)
                  (push-tactical-move! g (create-move piece idx ne-idx #:captured-piece ne))) ]
            [ (fx= (get-ep-idx g) ne-idx)
              ;; En passant capture
              (push-tactical-move! g
                                 (create-move piece idx ne-idx
                                              #:captured-piece (get-square squares e-idx) #:is-ep-capture? #t)) ]))))


;; Even though it's a quiet promotion, we add tactical moves since a
;; promotion is a great move.
(define (generate-quiet-promotions! b white? piece idx dst-idx)
  ;; Promotion to queen
  (push-tactical-move! b
                   (create-move piece idx dst-idx
                                #:promoted-piece (if white?
                                                     white-queen
                                                     black-queen)))
  ;; Promotion to rook
  (push-tactical-move! b
                   (create-move piece idx dst-idx
                                #:promoted-piece (if white?
                                                     white-rook
                                                     black-rook)))
  ;; Promotion to knight
  (push-tactical-move! b
                   (create-move piece idx dst-idx
                                #:promoted-piece (if white?
                                                     white-knight
                                                     black-knight)))
  ;; Promotion to bishop
  (push-tactical-move! b
                   (create-move piece idx dst-idx
                                #:promoted-piece (if white?
                                                     white-bishop
                                                     black-bishop))))

(define (generate-capture-promotions! b white? piece idx dst-idx captured)
  ;; Promotion to queen
  (push-tactical-move! b
                   (create-move piece idx dst-idx
                                #:captured-piece captured
                                #:promoted-piece (if white?
                                                     white-queen
                                                     black-queen)))
  ;; Promotion to rook
  (push-tactical-move! b
                   (create-move piece idx dst-idx
                                #:captured-piece captured
                                #:promoted-piece (if white?
                                                     white-rook
                                                     black-rook)))
  ;; Promotion to knight
  (push-tactical-move! b
                   (create-move piece idx dst-idx
                                #:captured-piece captured
                                #:promoted-piece (if white?
                                                     white-knight
                                                     black-knight)))
  ;; Promotion to bishop
  (push-tactical-move! b
                   (create-move piece idx dst-idx
                                #:captured-piece captured
                                #:promoted-piece (if white?
                                                     white-bishop
                                                     black-bishop))))

(define (generate-sliding-moves! g idx piece direction quiet-moves?)
  (define board (game-board g))

  (let loop ([ target-idx (fx+ idx direction) ])
    (let ([ target (get-square board target-idx) ])
      (cond [ (fx= target empty-square)
              (when quiet-moves?
                ;; Add quiet move and continue
                (push-quiet-move! g (create-move piece idx target-idx)))
              (loop (fx+ target-idx direction)) ]
            [ (is-other-piece? piece target)
              ;; Add tactical move for capture and exit
              (push-tactical-move! g
                                   (create-move piece idx target-idx #:captured-piece target)) ]))))

(define (print-moves g)
  ;; Tactical moves
  (printf "Tactical moves:\n")
  (for ([ i (in-range (tactical-length g)) ])
    (print-move (fxvector-ref (tactical-moves g) i)))

  ;; Quiet moves
  (printf "\nQuiet moves:\n")
  (for ([ i (in-range (quiet-length g)) ])
    (print-move (fxvector-ref (quiet-moves g) i)))

  (printf "\n"))

;; --------------------------------------------------------------------------------------------
;; Tests
;; --------------------------------------------------------------------------------------------

(module+ test
  (require "./fen.rkt" rackunit)

  (define fen1 "r3k2r/1p2b1p1/b1n1p3/2pp2qp/3PP1B1/1Q3pP1/PB1N4/R3K2R")

  ;; generate-moves!

  (let* ([ g (create-game)  ]
         [ b (game-board g) ])
    (fen-placement->board! b fen1)
    (print-board b)
    (generate-moves! g)
    (print-moves g))

  )
