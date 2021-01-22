#lang racket

(require "./board.rkt")
(require "./board-funcs.rkt")
(require "./fen.rkt")
(require "./piece.rkt")
(require "./move.rkt")
(require "./movement.rkt")

(provide pgn-load-file pgn-move)

;; Groups for pat-pawn:
;; 2 - From file when capturing (optional) e.g. "cx"
;; 3 - To file e.g. "e"
;; 4 - To rank e.g. "4"
;; 5 - Promotion (optional) e.g. "=N"
;; 6 - Check indication (optional) e.g. "+" or "#"
(define pat-pawn #px"^([a-h]x)?([a-h])([1-8])(=[QBNR])?([+]|#)?$")

;; Groups for pat-piece
;; 2 - Piece letter e.g. "Q"
;; 3 - From file (optional) e.g. "c"
;; 4 - From rank (optional) e.g. "4"
;; 5 - To file & rank e.g. "d5"
;; 6 - Check indication (optional) e.g. "+" or "#"
(define pat-piece #px"^([KQBNR])([a-h])?([1-8])?([a-h][1-8])([+]|#)?$")

;; Groups for pat-castle
;; 2 - Castle move e.g. "O-O" or "O-O-O"
;; 3 - Check indication
(define pat-castle #px"^(O-O|O-O-O)([+]|#)?$")

(define (pgn-load-file b path)
  (printf "Loading file: ~a\n" path)
  (for ([ line (in-list (file->lines path)) ])
    (printf "~a\n" line)
    (let* ([ groups (regexp-match #px"^([0-9]+)[.] ([-a-zQBNRO0-9]+) ([-a-zQBNRO0-9]+)$" line) ]
           [ n  (second groups) ]
           [ wstr (third groups) ]
           [ bstr (fourth groups) ])
      (let ([ m (pgn-move b wstr) ])
        (printf "~a's move: " (if (board-whites-move? b) "White" "Black"))
        (print-move m)
        (make-move! b m))
      (let ([ m (pgn-move b bstr) ])
        (printf "~a's move: " (if (board-whites-move? b) "White" "Black"))
        (print-move m)
        (make-move! b m))
      (printf "\n"))))

(define (make-pgn-move! b str)
  (make-move! b (pgn-move b str)))

(define (pgn-move b str)
  (let loop ([ lst pat-func ])
    (if (null? lst)
        (error "pgn-move: no pattern found for move")
        (match-let* ([ (list pat func) (car lst)              ]
                     [ groups          (regexp-match pat str) ])
          (if groups
              (func b (board-whites-move? b) (cdr groups))
              (loop (cdr lst)))))))

;; groups is of the form:
;; '(<from file>  (optional) indication of source file when capturing e.g. "cx"
;;   <to file>    e.g. "d"
;;   <to rank>    e.g. "4"
;;   <promotion>  (optional) e.g. "=N"
;;   <check>)     (optional) indication of check e.g. "+" or "#" for mate
(define (pgn-pawn-move b is-white? groups)
  (match-let* ([ (list from-file to-file to-rank promotion check) groups ]
               [ from-file (if (non-empty-string? from-file)
                               (substring from-file 0 1)
                               #f) ]
               [ to-rank (string->number to-rank) ]
               [ promoted (if (non-empty-string? promotion)
                              (substring promotion 1 2)
                              #f) ])
    (if from-file
        (pgn-pawn-capture b is-white? from-file to-file to-rank promoted)
        (pgn-pawn-push b is-white? to-file to-rank promoted))))

(define (pgn-pawn-capture b is-white? from-file to-file to-rank promoted)
  (define squares (board-squares b))
  
  (let* ([ dst-idx  (pos->idx (format "~a~a" to-file to-rank))    ]
         [ dst      (bytes-ref squares dst-idx)                   ]
         [ src-rank (if is-white?
                        (sub1 to-rank)
                        (add1 to-rank))                           ]
         [ src-idx  (pos->idx (format "~a~a" from-file src-rank)) ]
         [ src      (bytes-ref squares src-idx)                   ])
    (if (= dst empty-square)
        ;; En passant capture
        (let* ([ cap-idx   (pos->idx (format "~a~a" to-file src-rank)) ]
               [ cap-piece (bytes-ref squares cap-idx)                 ])
          (create-move src src-idx dst-idx #:captured-piece cap-piece #:is-ep-capture #t))
        ;; Regular capture
        (create-move src src-idx dst-idx #:captured-piece dst))))

(define (pgn-pawn-push b is-white? to-file to-rank promoted)
  (void))

(define (pgn-piece-move b groups)
  (printf "piece move\n")
  groups)

(define (pgn-castle-move b groups)
  (printf "castle move\n")
  groups)

(define pat-func (list (list pat-pawn   pgn-pawn-move)
                       (list pat-piece  pgn-piece-move)
                       (list pat-castle pgn-castle-move)))

(pgn-move (create-board) "d4")

(define (orig-pgn-move b str)
  (define is-white? (board-whites-move? b))
  (cond [ (regexp-match? #px"^[a-h][1-8][QBNR]?$" str)
          (pgn-pawn-push b is-white? str) ]
        [ (regexp-match? #px"^[a-h]x[a-h][1-8]$" str)
          (pgn-pawn-capture b is-white? str) ]
        [ (regexp-match? #px"^[a-h][1-8][-x][a-h][1-8]$" str)
          (pgn-piece-move b is-white? str) ]
        [ (string=? str "O-O")
          (pgn-castle-kingside b is-white?) ]
        [ (string=? str "O-O-O")
          (pgn-castle-queenside b is-white?) ]))

(define (pgn-castle-kingside b is-white?)
  (let-values ([ (src-idx dst-idx)
                 (if is-white?
                     (values (pos->idx "e1") (pos->idx "g1"))
                     (values (pos->idx "e8") (pos->idx "g8"))) ])
    (create-move (bytes-ref (board-squares b) src-idx) src-idx dst-idx #:is-castle-kingside? #t)))

(define (pgn-castle-queenside b is-white?)
  (let-values ([ (src-idx dst-idx)
                 (if is-white?
                     (values (pos->idx "e1") (pos->idx "c1"))
                     (values (pos->idx "e8") (pos->idx "c8"))) ])
    (create-move (bytes-ref (board-squares b) src-idx) src-idx dst-idx #:is-castle-queenside? #t)))

(define (orig-pgn-pawn-capture b is-white? str)
  (let* ([ groups (regexp-match #px"^([a-h])x([a-h])([1-8])$" str) ]
         [ source-file (second groups) ]
         [ target-file (third groups) ]
         [ target-rank (string->number (fourth groups)) ]
         [ source-rank (if is-white?
                           (sub1 target-rank)
                           (add1 target-rank)) ]
         [ src-idx (pos->idx (format "~a~a" source-file source-rank)) ]
         [ dst-idx (pos->idx (format "~a~a" target-file target-rank)) ]
         [ piece (bytes-ref (board-squares b) src-idx) ]
         [ target (bytes-ref (board-squares b) dst-idx) ])
    (if (= target empty-square)
        ;; En passant capture
        (let* ([ cap-idx (pos->idx (format "~a~a" target-file source-rank)) ]
               [ cap-piece (bytes-ref (board-squares b) cap-idx) ])
          (create-move piece src-idx dst-idx #:captured-piece cap-piece #:is-ep-capture? #t))
        ;; Else, regular capture
        (create-move piece src-idx dst-idx #:captured-piece target))))

(define (orig-pgn-pawn-push b is-white? str)
  (let* ([ groups   (regexp-match #px"^([a-h])([1-8])([QBNR])?$" str) ]
         [ file     (second groups)                                   ]
         [ rank     (string->number (third groups))                   ]
         [ promoted (fourth groups)                                   ]
         [ pos      (format "~a~a" file rank)                         ]
         [ idx      (pos->idx pos)                                ]
         [ i1       (+ idx (if is-white? south north))                ]
         [ i2       (+ i1  (if is-white? south north))                ])
    (if (= (bytes-ref (board-squares b) i1) empty-square)
        ;; One square before is empty, so must be double push
        (let ([ piece (bytes-ref (board-squares b) i2) ])
          (create-move piece i2 idx))
        ;; Otherwise, single push
        (let ([ piece (bytes-ref (board-squares b) i1) ])
          (if promoted
              ;; Pawn promotion
              (if is-white?
                  (pgn-white-pawn-promotion piece i1 idx rank promoted)
                  (pgn-black-pawn-promotion piece i1 idx rank promoted))
              ;; Pawn push
              (create-move piece i1 idx))))))

(define (pgn-white-pawn-promotion piece src-idx dst-idx rank letter)
  (if (= rank 8)
      (create-move piece src-idx dst-idx
                   #:promoted-piece (match letter
                                      [ "Q" white-queen  ]
                                      [ "B" white-bishop ]
                                      [ "N" white-knight ]
                                      [ "R" white-rook   ]))
      (error "invalid white pawn promotion")))

(define (pgn-black-pawn-promotion piece src-idx dst-idx rank letter)
  (if (= rank 1)
      (create-move piece src-idx dst-idx
                   #:promoted-piece (match letter
                                      [ "Q" black-queen  ]
                                      [ "B" black-bishop ]
                                      [ "N" black-knight ]
                                      [ "R" black-rook   ]))
      (error "invalid black pawn promotion")))

(define (orig-pgn-piece-move b is-white? str)
  (let* ([ groups (regexp-match #px"^([a-h][1-8])([-x])([a-h][1-8])$" str) ]
         [ src-pos     (second groups)                       ]
         [ src-idx     (pos->idx src-pos)                ]
         [ src         (bytes-ref (board-squares b) src-idx) ]
         [ cap-or-move (third groups)                        ]
         [ dst-pos     (fourth groups)                       ]
         [ dst-idx     (pos->idx dst-pos)                ]
         [ dst         (bytes-ref (board-squares b) dst-idx) ])
    (if (or (and (string=? cap-or-move "x")
                 (is-piece? dst))
            (and (string=? cap-or-move "-")
                 (= dst empty-square)))
        (create-move src src-idx dst-idx
                     #:captured-piece (if (= dst empty-square)
                                          #f
                                          dst))
        (begin
          (error (format "invalid move spec: ~a" str))))))

(module+ test
  (require rackunit)

  ;; ------------------------------------------------------------------------------------------
  ;; pgn-move
  ;; ------------------------------------------------------------------------------------------

  ;; ------------------------------------------------------------------------------------------
  ;; regex patterns
  ;; ------------------------------------------------------------------------------------------

  ;; pat-pawn
  (for ([ pair (in-list '(("d4" (#f "d" "4" #f #f))
                          ("d4+" (#f "d" "4" #f "+"))
                          ("d4#" (#f "d" "4" #f "#"))
                          ("d8=N" (#f "d" "8" "=N" #f))
                          ("d8=Q+" (#f "d" "8" "=Q" "+"))
                          ("d8=Q#" (#f "d" "8" "=Q" "#"))
                          ("cxd4" ("cx" "d" "4" #f #f))
                          ("cxd4+" ("cx" "d" "4" #f "+"))
                          ("cxd4#" ("cx" "d" "4" #f "#"))
                          ("cxd8=B" ("cx" "d" "8" "=B" #f))
                          ("cxd8=R+" ("cx" "d" "8" "=R" "+"))
                          ("cxd8=R#" ("cx" "d" "8" "=R" "#")))) ])
    (match-let ([ (list str groups) pair ])
      (check-equal? (regexp-match pat-pawn str) (cons str groups))))

  ;; pat-piece
  (for ([ pair (in-list '(("Qd4" ("Q" #f #f "d4" #f))
                          ("Qd4+" ("Q" #f #f "d4" "+"))
                          ("Qd4#" ("Q" #f #f "d4" "#"))
                          ("Q3d4" ("Q" #f "3" "d4" #f))
                          ("Q3d4+" ("Q" #f "3" "d4" "+"))
                          ("Q3d4#" ("Q" #f "3" "d4" "#"))
                          ("Qcd4" ("Q" "c" #f "d4" #f))
                          ("Qcd4+" ("Q" "c" #f "d4" "+"))
                          ("Qcd4#" ("Q" "c" #f "d4" "#"))
                          ("Qc3d4" ("Q" "c" "3" "d4" #f))
                          ("Qc3d4+" ("Q" "c" "3" "d4" "+"))
                          ("Qc3d4#" ("Q" "c" "3" "d4" "#")))) ])
    (match-let ([ (list str groups) pair ])
      (check-equal? (regexp-match pat-piece str) (cons str groups))))

  ;; pat-castle
  (for ([ pair (in-list '(
                          ("O-O" ("O-O" #f))
                          ("O-O-O" ("O-O-O" #f))
                          ("O-O+" ("O-O" "+"))
                          ("O-O-O+" ("O-O-O" "+"))
                          ("O-O#" ("O-O" "#"))
                          ("O-O-O#" ("O-O-O" "#"))
                          )) ])
    (match-let ([ (list str groups) pair ])
      (check-equal? (regexp-match pat-castle str) (cons str groups))))

  ;; (let ([ b (create-board) ])
  ;;   (make-pgn-move! b "d5")
  ;;   ;; (make-pgn-move! b "d5")
  ;;   ;; (make-pgn-move! b "c4")
  ;;   ;; (make-pgn-move! b "e6")
  ;;   (print-board b))

  )
