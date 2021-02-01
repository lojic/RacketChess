#lang racket

(require "./board-funcs.rkt")
(require "./board.rkt")
(require "./evaluation.rkt")
(require "./legality.rkt")
(require "./move.rkt")
(require "./movement.rkt")
(require "./move-ordering.rkt")
(require "./pgn.rkt")
(require "./fen.rkt")
(require "./piece.rkt")
(require debug/repl)

(define MIN-SCORE -100000)
(define MAX-SCORE 100000)
(define QUIESCE-LEVELS 3)

(define (search b max-level)
  (set-board-depth! b 0)
  (alpha-beta! b max-level MIN-SCORE MAX-SCORE))

(define (quiesce! b max-level alpha beta)
  (define stand-pat (evaluate b))
  (define depth     (board-depth b))
  (define get-move  (move-iterator! b #:quiet-moves? #f))

  (if (= depth max-level)
      stand-pat
      (if (>= stand-pat beta)
          beta
          (let loop ([ alpha (max alpha stand-pat) ])
            (let ([ m (get-move) ])
              (if (or (not m) (>= alpha beta))
                  ;; No more moves, or alpha >= beta
                  alpha
                  (begin
                    (make-move! b m)
                    (if (is-legal? b m)
                        ;; Legal move, continue
                        (let ([ score (- (quiesce! b max-level (- beta) (- alpha))) ])
                          (unmake-move! b m)
                          (cond [ (>= score beta) beta         ]
                                [ (> score alpha) (loop score) ]
                                [ else            (loop alpha) ]))
                        ;; Illegal move, ignore move
                        (begin
                          (unmake-move! b m)
                          (loop alpha))))))))))

(define (alpha-beta! b max-level alpha beta)
  (define depth (board-depth b))

  (if (= depth max-level)
      (quiesce! b (+ max-level QUIESCE-LEVELS) alpha beta)
      (let ([ get-move (move-iterator! b) ])
        (let loop ([ alpha alpha ]
                   [ move  #f    ])
          (let ([ m (get-move) ])
            (if (or (not m) (>= alpha beta))
                ;; No more moves, or alpha >= beta

                ;; If we're at the top level, return move & score;
                ;; otherwise, just score. If no move was found, mate
                ;; is implied. Adjust the score by depth to favor
                ;; shorter mates to prevent the program from just
                ;; gobbling up pieces instead of going for the mate!
                (if (= depth 0)
                    (cons alpha move)
                    (if move
                        alpha
                        (+ MIN-SCORE depth)))
                (begin
                  (make-move! b m)
                  (if (is-legal? b m)
                      ;; Legal move, continue
                      (let ([ score (- (alpha-beta! b max-level (- beta) (- alpha))) ])
                        (unmake-move! b m)
                        (cond [ (>= score beta) beta              ]
                              [ (> score alpha) (loop score m)    ]
                              [ else            (loop alpha move) ]))
                      ;; Illegal move, ignore move
                      (begin
                        (unmake-move! b m)
                        (loop alpha move))))))))))

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
            (printf "Score: ~a\n" score)
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
  (let ([ computer-plays-black?
          (let loop ()
            (displayln "Computer plays white or black? (w or b): ")
            (let ([ player (read-line) ])
              (cond [ (string=? player "w") #f ]
                    [ (string=? player "b") #t ]
                    [ else (loop) ]))) ])
    (game 6 computer-plays-black?)))
