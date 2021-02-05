#lang racket

(require "./board-funcs.rkt"
         "./board.rkt"
         "./evaluation.rkt"
         "./legality.rkt"
         "./movement.rkt")

(require racket/performance-hint)

(provide search)

(define MIN-SCORE -100000)
(define MAX-SCORE 100000)

(define think-seconds #f)
(define start-seconds #f)
(define num-nodes 0)

(define (init-timer! seconds)
  (set! think-seconds seconds)
  (set! start-seconds (current-seconds))
  (set! num-nodes 0))

(define-inline (is-timeout?)
  (set! num-nodes (add1 num-nodes))

  (if (= 0 (bitwise-and num-nodes #b11111111111))
      (let ([ elapsed-seconds (- (current-seconds) start-seconds) ])
        (if (> elapsed-seconds think-seconds)
            (begin
              (printf "~a nodes per second\n" (exact->inexact (/ num-nodes elapsed-seconds)))
              #t)
            #f))
      #f))

(define (search b max-level seconds)
  (init-timer! seconds)

  (let loop ([ level 1 ][ best (cons #f #f) ])
    (if (or (> level max-level)
            (is-timeout?))
        best
        (begin
          (set-board-depth! b 0)
          (match-let ([ (cons score m) (alpha-beta! b level MIN-SCORE MAX-SCORE is-timeout?) ])
            (if m
                (begin
                  (printf "Best move (~a): " level)
                  (print-move m)
                  (loop (add1 level) (cons score m)))
                best))))))

(define (alpha-beta! b max-level alpha beta is-timeout?)
  (define depth (board-depth b))

  (cond [ (is-timeout?) #f ]
        [ (= depth max-level)
          (quiesce! b alpha beta is-timeout?) ]
        [ else
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
                          (let* ([ result (alpha-beta! b max-level (- beta) (- alpha) is-timeout?) ]
                                 [ score  (and result (- result)) ])
                            (unmake-move! b m)
                            (cond [ (not score)     #f                ]
                                  [ (>= score beta) beta              ]
                                  [ (> score alpha) (loop score m)    ]
                                  [ else            (loop alpha move) ]))
                          ;; Illegal move, ignore move
                          (begin
                            (unmake-move! b m)
                            (loop alpha move)))))))) ]))

(define (quiesce! b alpha beta is-timeout?)
  (define stand-pat (evaluate b))
  (define depth     (board-depth b))
  (define get-move  (move-iterator! b #:quiet-moves? #f))

  (cond [ (is-timeout?) #f ]
        [ else
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
                            (let* ([ result (quiesce! b (- beta) (- alpha) is-timeout?) ]
                                   [ score  (and result (- result)) ])
                              (unmake-move! b m)
                              (cond [ (not score)     #f           ]
                                    [ (>= score beta) beta         ]
                                    [ (> score alpha) (loop score) ]
                                    [ else            (loop alpha) ]))
                            ;; Illegal move, ignore move
                            (begin
                              (unmake-move! b m)
                              (loop alpha)))))))) ]))
