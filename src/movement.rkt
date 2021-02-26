#lang racket

(require "./board.rkt"
         "./global.rkt"
         "./move-ordering.rkt"
         "./move.rkt"
         "./piece.rkt"
         "./state.rkt")
(require racket/performance-hint)

(provide generate-bishop-moves!
         generate-king-moves!
         generate-knight-moves!
         generate-queen-moves!
         generate-rook-moves!
         generate-moves!
         move-iterator!
         print-move)

;; --------------------------------------------------------------------------------------------
;; Public Interface
;; --------------------------------------------------------------------------------------------

(define (generate-moves! b #:quiet-moves? [ quiet-moves? #t ])
  (define is-white? (is-whites-move? b))

  (init-moves! b)

  (for* ([ rank (in-range 8) ]
         [ file (in-range 8) ])
    (let* ([ idx   (fx+ 21 file (fx* rank 10))        ]
           [ piece (get-square (board-squares b) idx) ])
      (when (is-right-color-piece? piece is-white?)
        (match (piece-type piece)
          [ #b001 (generate-pawn-moves!   b idx piece #:quiet-moves? quiet-moves?) ]
          [ #b010 (generate-knight-moves! b idx piece #:quiet-moves? quiet-moves?) ]
          [ #b011 (generate-bishop-moves! b idx piece #:quiet-moves? quiet-moves?) ]
          [ #b100 (generate-rook-moves!   b idx piece #:quiet-moves? quiet-moves?) ]
          [ #b101 (generate-queen-moves!  b idx piece #:quiet-moves? quiet-moves?) ]
          [ #b110 (generate-king-moves!   b idx piece is-white? #:quiet-moves? quiet-moves?) ])))))

(define (generate-bishop-moves! b idx piece #:quiet-moves? [ quiet-moves? #t ])
  (generate-sliding-moves! b idx piece north-east quiet-moves?)
  (generate-sliding-moves! b idx piece south-east quiet-moves?)
  (generate-sliding-moves! b idx piece south-west quiet-moves?)
  (generate-sliding-moves! b idx piece north-west quiet-moves?))

(define (generate-king-moves! b idx piece is-white? #:quiet-moves? [ quiet-moves? #t ])
  (generate-offset-moves! b idx piece king-offsets quiet-moves?)
  (when (and quiet-moves? (may-castle? b is-white?))
    (generate-castle-moves! b idx piece is-white?)))

(define (generate-knight-moves! b idx piece #:quiet-moves? [ quiet-moves? #t ])
  (generate-offset-moves! b idx piece knight-offsets quiet-moves?))

(define (generate-queen-moves! b idx piece #:quiet-moves? [ quiet-moves? #t ])
  (generate-bishop-moves! b idx piece #:quiet-moves? quiet-moves?)
  (generate-rook-moves! b idx piece #:quiet-moves? quiet-moves?))

(define (generate-rook-moves! b idx piece #:quiet-moves? [ quiet-moves? #t ])
  (generate-sliding-moves! b idx piece north quiet-moves?)
  (generate-sliding-moves! b idx piece east quiet-moves?)
  (generate-sliding-moves! b idx piece south quiet-moves?)
  (generate-sliding-moves! b idx piece west quiet-moves?))

(define (print-move m)
  (printf "~a~a~a~a"
          (piece-symbol (move-src m))
          (idx->pos (move-src-idx m))
          (if (fx> (move-captured-piece m) 0)
              "x"
              "-")
          (idx->pos (move-dst-idx m)))
  (let ([ promoted (move-promoted-piece m) ])
    (when (fx> promoted 0)
      (printf "=~a" (piece-symbol promoted))))
  (printf "\n"))

(define (move-iterator! b
                        #:order-moves? [ order-moves? #t ]
                        #:tt-move      [ tt-move      #f ]
                        #:quiet-moves? [ quiet-moves? #t ])
  (define do-gen #t)
  (define qhead  #f)
  (define qi     #f)
  (define qmoves #f)
  (define thead  #f)
  (define ti     #f)
  (define tmoves #f)

  (define (gen-moves)
    (generate-moves! b #:quiet-moves? quiet-moves?)
    (set! qhead  (quiet-head b))
    (set! qi     0)
    (set! qmoves (quiet-moves b))
    (set! thead  (tactical-head b))
    (set! ti     0)
    (set! tmoves (tactical-moves b)))

  (define (get-tt-move)
    (set! func get-tactical-move)
    (if tt-move
        tt-move
        (begin
          (set! tt-move 0) ; To allow comparing with fx= below
          (get-tactical-move))))

  (define (get-tactical-move)
    (when do-gen
      (set! do-gen #f)
      (gen-moves)
      (when order-moves? (order-moves! b)))

    (cond [ (fx<= ti thead)
            (let ([ m (vecref tmoves ti) ])
              (set! ti (fx+ 1 ti))
              (if (fx= m tt-move)
                  (get-tactical-move)
                  m)) ]
          [ else
            (set! func get-quiet-move)
            (get-quiet-move) ]))

  (define (get-quiet-move)
    (cond [ (fx<= qi qhead)
            (let ([m (vecref qmoves qi) ])
              (set! qi (fx+ 1 qi))
              (if (fx= m tt-move)
                  (get-quiet-move)
                  m)) ]
          [ else #f]))

  (define func get-tt-move)

  (λ ()
    (func)))

;; --------------------------------------------------------------------------------------------
;; Private Implementation
;; --------------------------------------------------------------------------------------------

(define-inline (add-quiet-move! b m)
  (let ([ head (fx+ 1 (quiet-head b)) ])
    (set-quiet-head! b head)
    (vecset! (quiet-moves b) head m)))

(define-inline (add-tactical-move! b m)
  (let ([ head (fx+ 1 (tactical-head b)) ])
    (set-tactical-head! b head)
    (vecset! (tactical-moves b) head m)))

(define (generate-castle-moves! b idx piece is-white?)
  (let ([ s       (board-game-state b) ]
        [ squares (board-squares b)    ])
    ;; King side
    (when (and (if is-white? (state-w-kingside-ok? s) (state-b-kingside-ok? s))
               (fx= empty-square (get-square squares (fx+ idx 1)))
               (fx= empty-square (get-square squares (fx+ idx 2))))
      (add-quiet-move! b
                       (create-move piece idx (fx+ idx 2) #:is-castle-kingside? #t)))
    ;; Queen side
    (when (and (if is-white? (state-w-queenside-ok? s) (state-b-queenside-ok? s))
               (fx= empty-square (get-square squares (fx- idx 1)))
               (fx= empty-square (get-square squares (fx- idx 2)))
               (fx= empty-square (get-square squares (fx- idx 3))))
      (add-quiet-move! b
                       (create-move piece idx (fx- idx 2) #:is-castle-queenside? #t)))))

(define (generate-offset-moves! b idx piece offsets quiet-moves?)
  (let ([ squares (board-squares b) ])
    (let loop ([ lst offsets ])
      (when (not (null? lst))
        (let* ([ offset     (car lst)                       ]
               [ target-idx (fx+ idx offset)                ]
               [ target     (get-square squares target-idx) ])
          (cond [ (fx= target empty-square)
                  (when quiet-moves?
                    (add-quiet-move! b (create-move piece idx target-idx))) ]
                [ (is-other-piece? piece target)
                  (add-tactical-move! b
                                      (create-move piece idx target-idx #:captured-piece target)) ]))
        (loop (cdr lst))))))

(define (generate-pawn-moves! b idx piece #:quiet-moves? [ quiet-moves? #t ])
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
    (let* ([ squares    (board-squares b)           ]
           [ n1         (get-square squares n1-idx) ]
           [ n2         (get-square squares n2-idx) ]
           [ nw         (get-square squares nw-idx) ]
           [ ne         (get-square squares ne-idx) ]
           [ last-rank? (<= min8th n1-idx max8th)   ])

      (when (and (fx= n1 empty-square) quiet-moves?)
        ;; Single push
        (if last-rank?
            ;; Quiet promotions
            (generate-quiet-promotions! b white? piece idx n1-idx)

            ;; Regular single push
            (add-quiet-move! b (create-move piece idx n1-idx)))

        ;; Double push (only if single push was allowed)
        (when (and (fx<= min2nd idx max2nd)
                   (fx= n2 empty-square))
          (add-quiet-move! b (create-move piece idx n2-idx))))


      ;; Capture north west
      (cond [ (is-other-piece? piece nw)
              (if last-rank?
                  (generate-capture-promotions! b white? piece idx nw-idx nw)
                  (add-tactical-move! b (create-move piece idx nw-idx #:captured-piece nw))) ]
            [ (fx= (get-ep-idx b) nw-idx)
              ;; En passant capture
              (add-tactical-move! b
                                 (create-move piece idx nw-idx
                                              #:captured-piece (get-square squares w-idx) #:is-ep-capture? #t)) ])

      ;; Capture north east
      (cond [ (is-other-piece? piece ne)
              (if last-rank?
                  (generate-capture-promotions! b white? piece idx ne-idx ne)
                  (add-tactical-move! b (create-move piece idx ne-idx #:captured-piece ne))) ]
            [ (fx= (get-ep-idx b) ne-idx)
              ;; En passant capture
              (add-tactical-move! b
                                 (create-move piece idx ne-idx
                                              #:captured-piece (get-square squares e-idx) #:is-ep-capture? #t)) ]))))

;; Even though it's a quiet promotion, we add tactical moves since a
;; promotion is a great move.
(define (generate-quiet-promotions! b white? piece idx dst-idx)
  ;; Promotion to queen
  (add-tactical-move! b
                   (create-move piece idx dst-idx
                                #:promoted-piece (if white?
                                                     white-queen
                                                     black-queen)))
  ;; Promotion to rook
  (add-tactical-move! b
                   (create-move piece idx dst-idx
                                #:promoted-piece (if white?
                                                     white-rook
                                                     black-rook)))
  ;; Promotion to knight
  (add-tactical-move! b
                   (create-move piece idx dst-idx
                                #:promoted-piece (if white?
                                                     white-knight
                                                     black-knight)))
  ;; Promotion to bishop
  (add-tactical-move! b
                   (create-move piece idx dst-idx
                                #:promoted-piece (if white?
                                                     white-bishop
                                                     black-bishop))))

(define (generate-capture-promotions! b white? piece idx dst-idx captured)
  ;; Promotion to queen
  (add-tactical-move! b
                   (create-move piece idx dst-idx
                                #:captured-piece captured
                                #:promoted-piece (if white?
                                                     white-queen
                                                     black-queen)))
  ;; Promotion to rook
  (add-tactical-move! b
                   (create-move piece idx dst-idx
                                #:captured-piece captured
                                #:promoted-piece (if white?
                                                     white-rook
                                                     black-rook)))
  ;; Promotion to knight
  (add-tactical-move! b
                   (create-move piece idx dst-idx
                                #:captured-piece captured
                                #:promoted-piece (if white?
                                                     white-knight
                                                     black-knight)))
  ;; Promotion to bishop
  (add-tactical-move! b
                   (create-move piece idx dst-idx
                                #:captured-piece captured
                                #:promoted-piece (if white?
                                                     white-bishop
                                                     black-bishop))))

(define (generate-sliding-moves! b idx piece direction quiet-moves?)
  (let loop ([ target-idx (fx+ idx direction) ])
    (let ([ target (get-square (board-squares b) target-idx) ])
      (cond [ (fx= target empty-square)
              (when quiet-moves?
                ;; Add quiet move and continue
                (add-quiet-move! b (create-move piece idx target-idx)))
              (loop (fx+ target-idx direction)) ]
            [ (is-other-piece? piece target)
              ;; Add tactical move for capture and exit
              (add-tactical-move! b
                                 (create-move piece idx target-idx #:captured-piece target)) ]))))

(define (print-moves b)
  ;; Tactical moves
  (printf "Tactical moves:\n")
  (for ([ i (in-range (fx+ 1 (tactical-head b))) ])
    (print-move (fxvector-ref (tactical-moves b) i)))

  ;; Quiet moves
  (printf "Quiet moves:\n")
  (for ([ i (in-range (fx+ 1 (quiet-head b))) ])
    (print-move (fxvector-ref (quiet-moves b) i)))

  (printf "\n"))

(module+ test
  (require rackunit)

  (let ([ b (create-board) ])
    (generate-moves! b))

  )
