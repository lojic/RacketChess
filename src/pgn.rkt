#lang racket

(require "./board.rkt")
(require "./board-funcs.rkt")
(require "./board-slow.rkt")
(require "./fen.rkt")
(require "./piece.rkt")
(require "./move.rkt")
(require "./movement.rkt")
(require threading)
(require debug/repl)

(provide make-pgn-move!
         pgn-load-file!
         pgn-move)

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
;; 5 - Capture e.g. "x"
;; 6 - To file & rank e.g. "d5"
;; 7 - Check indication (optional) e.g. "+" or "#"
(define pat-piece #px"^([KQBNR])([a-h])?([1-8])?(x)?([a-h][1-8])([+]|#)?$")

;; Groups for pat-castle
;; 2 - Castle move e.g. "O-O" or "O-O-O"
;; 3 - Check indication
(define pat-castle #px"^(O-O|O-O-O)([+]|#)?$")

(define (pgn-load-file! b path)
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
  (make-move! b (pgn-move b str))
  (set-board-depth! b 0))

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
(define (pgn-pawn-move b white? groups)
  (match-let* ([ (list from-file to-file to-rank promotion check) groups ]
               [ from-file (if (non-empty-string? from-file)
                               (substring from-file 0 1)
                               #f) ]
               [ to-rank (string->number to-rank) ]
               [ promoted (if (non-empty-string? promotion)
                              (substring promotion 1 2)
                              #f) ])
    (if from-file
        (pgn-pawn-capture b white? from-file to-file to-rank promoted)
        (pgn-pawn-push b white? to-file to-rank promoted))))

(define (pgn-pawn-capture b white? from-file to-file to-rank promoted)
  (define squares (board-squares b))

  (let* ([ dst-idx  (pos->idx (format "~a~a" to-file to-rank))    ]
         [ dst      (bytes-ref squares dst-idx)                   ]
         [ src-rank (if white?
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

(define (pgn-pawn-push b white? to-file to-rank promoted)
  (define squares (board-squares b))

  (define (get-promoted-piece)
    (if white?
        (match promoted
          [ "Q" white-queen  ]
          [ "B" white-bishop ]
          [ "N" white-knight ]
          [ "R" white-rook   ])
        (match promoted
          [ "Q" black-queen  ]
          [ "B" black-bishop ]
          [ "N" black-knight ]
          [ "R" black-rook   ])))

  (let* ([ dst-idx (pos->idx (format "~a~a" to-file to-rank))               ]
         [ dst     (bytes-ref squares dst-idx)                              ]
         [ s1-idx  (+ dst-idx (if white? south north))                   ]
         [ s1      (bytes-ref squares s1-idx)                               ]
         [ s1-ok   (and (is-pawn? s1) (is-right-color-piece? s1 white?)) ]
         [ s2-idx  (+ s1-idx (if white? south north))                    ]
         [ s2      (bytes-ref squares s2-idx)                               ]
         [ s2-ok   (and (is-pawn? s2) (is-right-color-piece? s2 white?)) ])
    (cond [ (and (not s1-ok) s2-ok)
            ;; Double push
            (create-move s2 s2-idx dst-idx) ]
          [ (and promoted s1-ok (= to-rank (if white? 8 1)))
            ;; Promotion push
            (create-move s1 s1-idx dst-idx #:promoted-piece (get-promoted-piece)) ]
          [ s1-ok
            ;; Regular single push
            (create-move s1 s1-idx dst-idx) ]
          [ else (error "pgn-pawn-push: invalid move") ])))

(define (pgn-piece-move b white? groups)
  (match-let* ([ (list letter from-file from-rank capture to-file-rank check)
                 groups ]
               [ (list src src-idx)
                 (pgn-get-src-idx b letter white? from-file from-rank to-file-rank) ]
               [ dst-idx (pos->idx to-file-rank) ])
    (if capture
        (let ([ piece (bytes-ref (board-squares b) dst-idx) ])
          (create-move src src-idx dst-idx #:captured-piece piece))
        (create-move src src-idx dst-idx))))

;; Return a list of the form:
;; (piece src-idx)
(define (pgn-get-src-idx b letter white? from-file from-rank pos)
  (define (get-predicate)
    (let ([ color-pred? (if white? is-white? is-black?) ]
          [ piece-pred? (match letter
                          [ "K" is-king?   ]
                          [ "Q" is-queen?  ]
                          [ "B" is-bishop? ]
                          [ "N" is-knight? ]
                          [ "R" is-rook?   ]) ])
      (λ (p)
        (and (color-pred? p) (piece-pred? p)))))

  (let* ([ lst (get-pieces-file-rank b (get-predicate)) ]
         [ len (length lst)                             ])
    (cond [ (= len 1)
            (match-let ([ (list piece file rank) (car lst) ])
              (list piece (pos->idx (format "~a~a" file rank)))) ]
          [ (> len 1)
            (disambiguate lst b from-file from-rank pos) ]
          [ else (error "pgn-get-src-idx: invalid move") ])))

(define (disambiguate lst b from-file from-rank pos)
  (define (file-ok? file)
    (if from-file
        (char=? file (string-ref from-file 0))
        #t))

  (define (rank-ok? rank)
    (if from-rank
        (let ([ from (match from-rank
                       [ "8" 0 ]
                       [ "7" 1 ]
                       [ "6" 2 ]
                       [ "5" 3 ]
                       [ "4" 4 ]
                       [ "3" 5 ]
                       [ "2" 6 ]
                       [ "1" 7 ]) ])
          (= rank from))
        #t))

  (define (pred? tuple)
    (match-let ([ (list piece file rank) tuple ])
      (and (file-ok? file)
           (rank-ok? rank)
           (member pos (targets b piece file rank)))))

  (let ([ lst (filter pred? lst) ])
    (if (= 1 (length lst))
        (match-let ([ (list piece file rank) (car lst) ])
          (list piece (pos->idx (format "~a~a" file rank))))
        (error "disambiguate: invalid move"))))

;; Not sure how I feel about this function. Due to the way
;; movement.rkt was coded for maximum efficiency, there isn't a "nice"
;; way to obtain the target squares for a particular piece.
;;
;; The method employed here is to generate the moves into the two
;; vectors on the board, and then extract the information from them.
(define (targets b piece file rank)
  (let ([ idx (pos->idx (format "~a~a" file rank)) ])
    (init-moves! b)
    (cond [ (is-king? piece)   (generate-king-moves! b idx piece)   ]
          [ (is-queen? piece)  (generate-queen-moves! b idx piece)  ]
          [ (is-rook? piece)   (generate-rook-moves! b idx piece)   ]
          [ (is-bishop? piece) (generate-bishop-moves! b idx piece) ]
          [ (is-knight? piece) (generate-knight-moves! b idx piece) ]
          [ else               (error "targets: invalid piece")    ])
    (let* ([ qhead   (quiet-head b)     ]
           [ qmoves  (quiet-moves b)    ]
           [ thead   (tactical-head b)  ]
           [ tmoves  (tactical-moves b) ]
           [ targets (append (for/list ([ i (in-range (add1 qhead)) ])
                               (idx->pos (move-dst-idx (vector-ref qmoves i))))
                             (for/list ([ i (in-range (add1 thead)) ])
                               (idx->pos (move-dst-idx (vector-ref tmoves i))))) ])
      (init-moves! b)
      targets)))

;; Groups:
;; 1 - Castle move e.g. "O-O" or "O-O-O"
;; 2 - Check indication
(define (pgn-castle-move b white? groups)
  (define squares (board-squares b))

  (let* ([ castle    (car groups) ]
         [ kingside? (cond [ (string=? castle "O-O")   #t ]
                           [ (string=? castle "O-O-O") #f ]
                           [ else (error "pgn-castle-move: invalid castle spec") ]) ]
         [ king-idx (if white? (pos->idx "e1") (pos->idx "e8")) ]
         [ king     (bytes-ref squares king-idx)                ]
         [ dst-pos  (if white?
                        (if kingside? "g1" "c1")
                        (if kingside? "g8" "c8")) ]
         [ dst-idx  (pos->idx dst-pos) ])
    (create-move king king-idx dst-idx
                 #:is-castle-queenside? (not kingside?)
                 #:is-castle-kingside? kingside?)))

(define pat-func (list (list pat-pawn   pgn-pawn-move)
                       (list pat-piece  pgn-piece-move)
                       (list pat-castle pgn-castle-move)))

(define (pgn-castle-kingside b white?)
  (let-values ([ (src-idx dst-idx)
                 (if white?
                     (values (pos->idx "e1") (pos->idx "g1"))
                     (values (pos->idx "e8") (pos->idx "g8"))) ])
    (create-move (bytes-ref (board-squares b) src-idx) src-idx dst-idx #:is-castle-kingside? #t)))

(define (pgn-castle-queenside b white?)
  (let-values ([ (src-idx dst-idx)
                 (if white?
                     (values (pos->idx "e1") (pos->idx "c1"))
                     (values (pos->idx "e8") (pos->idx "c8"))) ])
    (create-move (bytes-ref (board-squares b) src-idx) src-idx dst-idx #:is-castle-queenside? #t)))

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
  ;; regex patterns
  ;; ------------------------------------------------------------------------------------------

  ;; pat-pawn ---------------------------------------------------------------------------------
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

  ;; pat-piece --------------------------------------------------------------------------------

  ;; pat-piece move
  (for ([ pair (in-list '(("Qd4" ("Q" #f #f #f "d4" #f))
                          ("Qd4+" ("Q" #f #f #f "d4" "+"))
                          ("Qd4#" ("Q" #f #f #f "d4" "#"))
                          ("Q3d4" ("Q" #f "3" #f "d4" #f))
                          ("Q3d4+" ("Q" #f "3" #f "d4" "+"))
                          ("Q3d4#" ("Q" #f "3" #f "d4" "#"))
                          ("Qcd4" ("Q" "c" #f #f "d4" #f))
                          ("Qcd4+" ("Q" "c" #f #f "d4" "+"))
                          ("Qcd4#" ("Q" "c" #f #f "d4" "#"))
                          ("Qc3d4" ("Q" "c" "3" #f "d4" #f))
                          ("Qc3d4+" ("Q" "c" "3" #f "d4" "+"))
                          ("Qc3d4#" ("Q" "c" "3" #f "d4" "#")))) ])
    (match-let ([ (list str groups) pair ])
      (check-equal? (regexp-match pat-piece str) (cons str groups))))

  ;; pat-piece capture
  (for ([ pair (in-list '(("Qxd4" ("Q" #f #f "x" "d4" #f))
                          ("Qxd4+" ("Q" #f #f "x" "d4" "+"))
                          ("Qxd4#" ("Q" #f #f "x" "d4" "#"))
                          ("Q3xd4" ("Q" #f "3" "x" "d4" #f))
                          ("Q3xd4+" ("Q" #f "3" "x" "d4" "+"))
                          ("Q3xd4#" ("Q" #f "3" "x" "d4" "#"))
                          ("Qcxd4" ("Q" "c" #f "x" "d4" #f))
                          ("Qcxd4+" ("Q" "c" #f "x" "d4" "+"))
                          ("Qcxd4#" ("Q" "c" #f "x" "d4" "#"))
                          ("Qc3xd4" ("Q" "c" "3" "x" "d4" #f))
                          ("Qc3xd4+" ("Q" "c" "3" "x" "d4" "+"))
                          ("Qc3xd4#" ("Q" "c" "3" "x" "d4" "#")))) ])
    (match-let ([ (list str groups) pair ])
      (check-equal? (regexp-match pat-piece str) (cons str groups))))

  ;; pat-castle -------------------------------------------------------------------------------
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

  )
