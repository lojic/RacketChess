#lang racket

(require "./board.rkt")
(require "./board-funcs.rkt")
(require "./move.rkt")
(require "./piece.rkt")

(provide generate-bishop-moves!
         generate-king-moves!
         generate-knight-moves!
         generate-queen-moves!
         generate-rook-moves!
         generate-moves!
         print-move)

(define (add-quiet-move! b m)
  (let ([ head (add1 (quiet-head b)) ])
    (set-quiet-head! b head)
    (vector-set! (quiet-moves b) head m)))

(define (add-tactical-move! b m)
  (let ([ head (add1 (tactical-head b)) ])
    (set-tactical-head! b head)
    (vector-set! (tactical-moves b) head m)))

(define (generate-bishop-moves! b idx piece)
  (generate-sliding-moves! b idx piece north-east)
  (generate-sliding-moves! b idx piece south-east)
  (generate-sliding-moves! b idx piece south-west)
  (generate-sliding-moves! b idx piece north-west))

(define (generate-king-moves! b idx piece)
  (generate-offset-moves! b idx piece king-offsets)
  (generate-castle-moves! b idx piece))

(define (generate-castle-moves! b idx piece)
  (when (not (has-moved? piece))
    (let ([ squares (board-squares b) ])
      (let ([ kr (bytes-ref squares (+ idx 3)) ]
            [ qr (bytes-ref squares (- idx 4)) ])
        ;; King side
        (when (and (not (has-moved? kr))
                   (= empty-square (bytes-ref squares (+ idx 1)))
                   (= empty-square (bytes-ref squares (+ idx 2))))
          (add-quiet-move! b
                           (create-move piece idx (+ idx 2) #:is-castle-kingside? #t)))
        ;; Queen side
        (when (and (not (has-moved? qr))
                   (= empty-square (bytes-ref squares (- idx 1)))
                   (= empty-square (bytes-ref squares (- idx 2))))
          (add-quiet-move! b
                           (create-move piece idx (- idx 2) #:is-castle-queenside? #t)))))))

(define (generate-knight-moves! b idx piece)
  (generate-offset-moves! b idx piece knight-offsets))

(define (generate-moves! b)
  (define is-white? (board-whites-move? b))

  (init-moves! b)

  (for* ([ rank (in-range 8) ]
         [ file (in-range 8) ])
    (let* ([ idx   (+ 21 file (* rank 10))           ]
           [ piece (bytes-ref (board-squares b) idx) ])
      (when (is-right-color-piece? piece is-white?)
        (match (piece-type piece)
          [ #b001 (generate-pawn-moves! b idx piece)   ]
          [ #b010 (generate-knight-moves! b idx piece) ]
          [ #b011 (generate-bishop-moves! b idx piece) ]
          [ #b100 (generate-rook-moves! b idx piece)   ]
          [ #b101 (generate-queen-moves! b idx piece)  ]
          [ #b110 (generate-king-moves! b idx piece)   ])))))

(define (generate-offset-moves! b idx piece offsets)
  (let ([ squares (board-squares b) ])
    (for ([ offset (in-list offsets) ])
      (let* ([ target-idx (+ idx offset)                 ]
             [ target     (bytes-ref squares target-idx) ])
        (cond [ (= target empty-square)
                (add-quiet-move! b (create-move piece idx target-idx)) ]
              [ (is-other-piece? piece target)
                (add-tactical-move! b
                                   (create-move piece idx target-idx #:captured-piece target)) ])))))

(define (generate-pawn-moves! b idx piece)
  (let-values ([ (is-white? n1-idx n2-idx nw-idx w-idx ne-idx e-idx min8th max8th)
                 (if (is-white? piece)
                     (values #t
                             (+ idx north)
                             (+ idx north north)
                             (+ idx north-west)
                             (+ idx west)
                             (+ idx north-east)
                             (+ idx east)
                             21 28)
                     (values #f
                             (+ idx south)
                             (+ idx south south)
                             (+ idx south-west)
                             (+ idx west)
                             (+ idx south-east)
                             (+ idx east)
                             91 98)) ])
    (let* ([ squares (board-squares b)          ]
           [ n1      (bytes-ref squares n1-idx) ]
           [ n2      (bytes-ref squares n2-idx) ]
           [ nw      (bytes-ref squares nw-idx) ]
           [ ne      (bytes-ref squares ne-idx) ])

      ;; Single push
      (when (= n1 empty-square)
        (if (and (>= n1-idx min8th) (<= n1-idx max8th))
            ;; Promotion
            (add-quiet-move! b
                            (create-move piece idx n1-idx
                                         #:promoted-piece (if is-white?
                                                               white-queen
                                                               black-queen)))
            ;; Regular single push
            (add-quiet-move! b (create-move piece idx n1-idx)))

        ;; Double push (only if single push was allowed)
        (when (and (not (has-moved? piece))
                   (= n2 empty-square))
          (add-quiet-move! b (create-move piece idx n2-idx))))

      ;; Capture north west
      (cond [ (is-other-piece? piece nw)
              (add-tactical-move! b (create-move piece idx nw-idx #:captured-piece nw)) ]
            [ (= (get-ep-idx b) nw-idx)
              (add-tactical-move! b
                                 (create-move piece idx nw-idx
                                              #:captured-piece (bytes-ref squares w-idx) #:is-ep-capture? #t)) ])

      ;; Capture north east
      (cond [ (is-other-piece? piece ne)
              (add-tactical-move! b (create-move piece idx ne-idx #:captured-piece ne)) ]
            [ (= (get-ep-idx b) ne-idx)
              (add-tactical-move! b
                                 (create-move piece idx ne-idx
                                              #:captured-piece (bytes-ref squares e-idx) #:is-ep-capture? #t)) ]))))

(define (generate-sliding-moves! b idx piece direction)
  (let loop ([ target-idx (+ idx direction) ])
    (let ([ target (bytes-ref (board-squares b) target-idx) ])
      (cond [ (= target empty-square)
              ;; Add quiet move and continue
              (add-quiet-move! b (create-move piece idx target-idx))
              (loop (+ target-idx direction)) ]
            [ (is-other-piece? piece target)
              ;; Add tactical move for capture and exit
              (add-tactical-move! b
                                 (create-move piece idx target-idx #:captured-piece target)) ]))))

(define (generate-queen-moves! b idx piece)
  (generate-bishop-moves! b idx piece)
  (generate-rook-moves! b idx piece))

(define (generate-rook-moves! b idx piece)
  (generate-sliding-moves! b idx piece north)
  (generate-sliding-moves! b idx piece east)
  (generate-sliding-moves! b idx piece south)
  (generate-sliding-moves! b idx piece west))

(define (print-move m)
  (printf "~a~a~a~a\n"
          (piece-symbol (move-src m))
          (idx->pos (move-src-idx m))
          (if (move-captured-piece m)
              "x"
              "-")
          (idx->pos (move-dst-idx m))
          ))

(define (print-moves b)
  ;; Tactical moves
  (printf "Tactical moves:\n")
  (for ([ i (in-range (add1 (tactical-head b))) ])
    (print-move (vector-ref (tactical-moves b) i)))

  ;; Quiet moves
  (printf "Quiet moves:\n")
  (for ([ i (in-range (add1 (quiet-head b))) ])
    (print-move (vector-ref (quiet-moves b) i)))

  (printf "\n"))

(module+ test
  (require rackunit)

  (let ([ b (create-board) ])
    (generate-moves! b))

  )
