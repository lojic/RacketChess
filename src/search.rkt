#lang racket

(require "./board.rkt"
         "./evaluation.rkt"
         "./global.rkt"
         "./legality.rkt"
         "./make-move.rkt"
         "./movement.rkt"
         "./stats.rkt"
         "./tt.rkt")

(require racket/performance-hint)

(provide search)

(define MIN-SCORE -100000)
(define MAX-SCORE 100000)

(define (search b max-level seconds)
  (define stats-obj (create-stats))
  (init-timer! stats-obj seconds)

  (let loop ([ level 1 ][ best #f ])
    (if (or (fx> level max-level)
            (timeout? stats-obj))
        best
        (begin
          (set-board-depth! b 0)
          (let ([ result (alpha-beta! b level stats-obj MIN-SCORE MAX-SCORE) ])
            (if result
                (let ([ score (car result) ]
                      [ m     (cdr result) ])
                  (if m
                      (begin
                        (printf "Best move (~a): " level)
                        (print-move m)
                        (loop (fx+ 1 level) (cons score m)))
                      best))
                best))))))

(define (alpha-beta! b max-level stats-obj alpha beta)
  (increment-nodes! stats-obj)
  (define depth (board-depth b))
  (define-values (tt-score tt-move) (read-tt-entry (get-hash-key) (fx- max-level depth) alpha beta))

  (when tt-move (increment-tt-moves! stats-obj))

  (if (and tt-score tt-move)
      ;; Found a TT entry
      (begin
        (increment-tt-hits! stats-obj)
        (if (fx= depth 0)
            (cons tt-score tt-move)
            tt-score))
      ;; No TT entry found
      (cond [ (timeout? stats-obj) #f ]
            [ (fx= depth max-level)
              (quiesce! b stats-obj alpha beta) ]
            [ else
              (let ([ get-move (move-iterator! b #:tt-move tt-move) ])
                (let loop ([ legal-move? #f         ]
                           [ alpha       alpha      ]
                           [ move        #f         ]
                           [ type        NODE-ALPHA ])
                  (let ([ m (get-move) ])
                    (if (or (not m) (fx>= alpha beta))
                        ;; No more moves, or alpha >= beta
                        ;; If we're at the top level, return move &
                        ;; score; otherwise, just score.
                        (if (fx= depth 0)
                            (cons alpha move)
                            (cond [ move
                                    (write-tt-entry! (get-hash-key) (fx- max-level depth) alpha move type)
                                    alpha ]
                                  [ legal-move?
                                    alpha ]
                                  [ else
                                    ;; No legal moves, so either stalemate or checkmate
                                    (if (is-king-in-check? b)
                                        (fx+ MIN-SCORE depth)
                                        0) ]))
                        (begin
                          (make-move! b m)
                          (if (is-legal? b m)
                              ;; Legal move, continue
                              (let* ([ result (alpha-beta! b max-level stats-obj (fx- beta) (fx- alpha)) ]
                                     [ score  (and result (fx- result)) ])
                                (unmake-move! b m)
                                (cond [ (not score)
                                        #f ]
                                      [ (fx>= score beta)
                                        (write-tt-entry! (get-hash-key) (fx- max-level depth) beta move NODE-BETA)
                                        beta ]
                                      [ (fx> score alpha)
                                        (loop #t score m NODE-EXACT) ]
                                      [ else
                                        (loop #t alpha move type) ]))
                              ;; Illegal move, ignore move
                              (begin
                                (unmake-move! b m)
                                (loop legal-move? alpha move type)))))))) ])))

(define (quiesce! b stats-obj alpha beta)
  (define stand-pat (evaluate b))
  (define depth     (board-depth b))
  (define get-move  (move-iterator! b #:quiet-moves? #f))

  (cond [ (timeout? stats-obj) #f ]
        [ else
          (if (fx>= stand-pat beta)
              beta
              (let loop ([ alpha (max alpha stand-pat) ])
                (let ([ m (get-move) ])
                  (if (or (not m) (fx>= alpha beta))
                      ;; No more moves, or alpha >= beta
                      alpha
                      (begin
                        (make-move! b m)
                        (if (is-legal? b m)
                            ;; Legal move, continue
                            (let* ([ result (quiesce! b stats-obj (fx- beta) (fx- alpha)) ]
                                   [ score  (and result (fx- result)) ])
                              (unmake-move! b m)
                              (cond [ (not score)     #f           ]
                                    [ (fx>= score beta) beta         ]
                                    [ (fx> score alpha) (loop score) ]
                                    [ else            (loop alpha) ]))
                            ;; Illegal move, ignore move
                            (begin
                              (unmake-move! b m)
                              (loop alpha)))))))) ]))
