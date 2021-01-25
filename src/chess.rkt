#lang racket

(require "./board-funcs.rkt")
(require "./board.rkt")
(require "./legality.rkt")
(require "./move.rkt")
(require "./movement.rkt")
(require "./pgn.rkt")
(require "./fen.rkt")
(require "./piece.rkt")
(require debug/repl)

(define (search b max-level)
  (set-board-depth! b 0)
  (alpha-beta! b max-level -inf.0 +inf.0))

(define (move-iterator! b)
  (generate-moves! b)
  (let ([ tmoves (tactical-moves b) ]
        [ thead  (tactical-head b)  ]
        [ ti     0                  ]
        [ qmoves (quiet-moves b)    ]
        [ qhead  (quiet-head b)     ]
        [ qi     0                  ])
    (λ ()
      (cond [ (<= ti thead)
              (set! ti (add1 ti))
              (vector-ref tmoves (sub1 ti)) ]
            [ (<= qi qhead)
              (set! qi (add1 qi))
              (vector-ref qmoves (sub1 qi)) ]
            [ else #f ]))))

(define (alpha-beta! b max-level alpha beta)
  (define depth (board-depth b))
  (if (= depth max-level)
      (evaluate b)
      (let ([ maximizing (board-whites-move? b) ]
            [ get-move (move-iterator! b) ])
        (let loop ([ alpha alpha ]
                   [ beta  beta  ]
                   [ move  #f    ])
          (let ([ m (get-move) ])
            (if (or (not m) (>= alpha beta))
                ;; No more moves, or alpha >= beta
                (if maximizing
                    ;; If we're at the top level, return move & score;
                    ;; otherwise, just score
                    (if (= depth 0) (cons alpha move) alpha)
                    (if (= depth 0) (cons beta move)  beta))
                (begin
                  (make-move! b m)
                  (if (is-legal? b m)
                      ;; Legal move, continue
                      (let ([ score (alpha-beta! b max-level alpha beta) ])
                        (unmake-move! b m)
                        (if maximizing
                            (if (> score alpha)
                                (loop score beta m)
                                (loop alpha beta move))
                            (if (< score beta)
                                (loop alpha score m)
                                (loop alpha beta move))))
                      ;; Illegal move, ignore move
                      (begin
                        (unmake-move! b m)
                        (loop alpha beta move))))))))))

(define (game depth computer-plays-black? [ fen #f ])
  (define b (if fen
                (fen->board fen)
                (fen->board)))

  (when computer-plays-black?
    (make-human-move! b))

  (let loop ()
    (let* ([ pair (time (search b depth)) ]
           [ score (car pair) ]
           [ m (cdr pair) ])
      (if m
          (begin
            (displayln "")
            (print-move m)
            (displayln "")
            (make-move! b m)
            (print-board b #:full #t)
            (displayln "")
            (make-human-move! b)
            (loop))
          (printf "No move!\n")))))

(define (make-human-move! b)
  (display "Enter move: ")
  (let ([ result (with-handlers ([ exn:fail? (λ (e) #f) ])
                   (make-pgn-move! b (read-line))) ])
    (if result
        (print-board b #:full #t)
        (make-human-move! b))))


;; TODO why move 8 ?!?!  Did the potential ep capture confuse it?
;; The board printed after the move doesn't show the protecting pawn !!!!
;; https://lichess.org/OfP4YCgQKjjC

(game 7 #f "rnbqkb1r/p4p1p/2p1p1p1/1p1pP3/3Pn2P/2P2QP1/PP3P2/RNB1KBNR w KQkq d6 0 1")

;(game 8 #t "./game.01")
;(game 7 #f "3rr1k1/pqp2pbp/3np1p1/8/3n4/Q2B1P2/PP4PP/R1B2KNR b - - 0 0")
;(game 7 #f "8/2pR1p2/1pP3kp/p7/5rp1/2P5/r3NKPP/5B1R w - - 0 34")
;(game 7 #t "5k1r/5Bp1/p2bQ2p/1p6/3B1q2/2P3nP/PP3PP1/4RK2 w - - 0 1")
;(game 7 #f)
;(game 11 #f "2k5/p1p5/8/6K1/2P5/8/PP1N2r1/R1BR4 w - - 0 1")
;(game 8)
