#lang racket

(require "./board-funcs.rkt")
(require "./board.rkt")
(require "./evaluation.rkt")
(require "./legality.rkt")
(require "./move.rkt")
(require "./movement.rkt")
(require "./pgn.rkt")
(require "./fen.rkt")
(require "./piece.rkt")
(require debug/repl)

(define MIN-SCORE -1000.0)
(define MAX-SCORE 1000.0)

(define (search b max-level)
  (set-board-depth! b 0)
  (alpha-beta! b max-level MIN-SCORE MAX-SCORE))

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

                ;; If we're at the top level, return move & score;
                ;; otherwise, just score. If no move was found, mate
                ;; is implied. Adjust the score by depth to favor
                ;; shorter mates to prevent the program from just
                ;; gobbling up pieces instead of going for the mate!
                (if maximizing
                    (if (= depth 0)
                        (cons alpha move)
                        (if move
                            alpha
                            (+ MIN-SCORE depth)))
                    (if (= depth 0)
                        (cons beta move)
                        (if move
                            beta
                            (- MAX-SCORE depth))))
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
            (make-move! b m)
            (print-board b #:full? #t)
            (print-move m)
            (displayln "")
            (make-human-move! b)
            (loop))
          (printf "No move!\n")))))

(define (get-move b)
  (with-handlers ([ exn:fail?
                    (λ (e)
                      (displayln (exn-message e))
                      #f) ])
    (display "Enter move: ")
    (pgn-move b (read-line))))

(define (make-move b m)
  (with-handlers ([ exn:fail?
                    (λ (e)
                      (displayln (exn-message e))
                      #f) ])
    (make-move! b m)
    (if (is-legal? b m)
        #t
        (begin
          (displayln "Illegal move")
          (unmake-move! b m)
          #f))))

(define (make-human-move! b)
  (let ([ m (get-move b) ])
    (if (and m (make-move b m))
        (print-board b #:full? #t)
        (make-human-move! b))))

(module+ main
  ;;(game 7 #f "3Q4/1p2N1pk/5p1p/7P/1Pn5/5pP1/5r2/5RK1 w - - 0 1")
  (game 7 #f)
  )
