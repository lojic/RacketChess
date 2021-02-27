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

;; Iterative deepening
(define (search b max-level seconds)
  (define stats-obj (create-stats))
  (init-timer! stats-obj seconds)

  (let loop ([ level 1 ][ best #f ])
    (if (or (fx> level max-level)
            (timeout? stats-obj))
        (begin
          (print-stats stats-obj)
          best)
        (begin
          (set-board-depth! b 0)
          (let* ([ best-move (if best (cdr best) #f) ]
                 [ result (alpha-beta-root! b level stats-obj MIN-SCORE MAX-SCORE best-move) ])
            (if result
                (let ([ score (car result) ]
                      [ m     (cdr result) ])
                  (if m
                      (begin
                        (printf "Best move (~a) (~a) [~a] "
                                level
                                (~r (exact->inexact
                                     (- (current-inexact-milliseconds)
                                        (stats-start-milliseconds stats-obj)))
                                    #:precision 1)
                                (~r (/ score 100) #:precision 2))
                        (print-move m)
                        (loop (fx+ 1 level) (cons score m)))
                      (begin
                        (print-stats stats-obj)
                        best)))
                (begin
                  (print-stats stats-obj)
                  best)))))))

(define (search-move get-move main-loop stats-obj depth max-level b m alpha beta best-move type legal-move?)
  (make-move! b m)
  (cond [ (is-legal? b m)
          ;; Legal move, continue
          (let* ([ result (alpha-beta! b max-level stats-obj (fx- beta) (fx- alpha)) ]
                 [ score  (and result (fx- result)) ])
            (unmake-move! b m)
            (cond [ (not score)
                    #f ]
                  [ (fx>= score beta)
                    (increment-beta-cuts! stats-obj)
                    (write-tt-entry! (get-hash-key) max-level depth beta m NODE-BETA)
                    beta ]
                  [ (fx> score alpha)
                    (main-loop get-move depth score m NODE-EXACT #t) ]
                  [ else
                    (main-loop get-move depth alpha best-move type #t) ])) ]
        [ else
          ;; Illegal move, ignore move
          (unmake-move! b m)
          (main-loop get-move depth alpha best-move type legal-move?) ]))

;; Returns (cons score move)
(define (alpha-beta-root! b max-level stats-obj alpha beta prev-best-move)
  ;; Helper functions -------------------------------------------------------------------------
  (define (return-score-move alpha move type legal-move?)
    (cond [ move
            (write-tt-entry! (get-hash-key) max-level 0 alpha move type)
            (cons alpha move) ]
          [ legal-move?
            (write-tt-entry! (get-hash-key) max-level 0 alpha #f type)
            (cons alpha #f) ]
          [ else
            ;; No legal moves, so either stalemate or checkmate
            (cond [ (is-king-in-check? b)
                    ;; Checkmate
                    (write-tt-entry! (get-hash-key) max-level 0 (fx- MATE-SCORE) #f NODE-EXACT)
                    (cons MIN-SCORE #f) ]
                  [ else
                    ;; Stalemate
                    (write-tt-entry! (get-hash-key) max-level 0 0 #f NODE-EXACT)
                    (cons 0 #f) ]) ]))

  (define (main-loop get-move _ alpha best-move type legal-move?)
    (let ([ m (get-move) ])
      (if m
          (search-move get-move main-loop stats-obj 0 max-level b m alpha beta best-move type legal-move?)
          (return-score-move alpha best-move type legal-move?))))
  ;; ------------------------------------------------------------------------------------------

  (increment-nodes! stats-obj)
  (define get-move (move-iterator! b #:tt-move prev-best-move))
  (main-loop get-move 0 alpha #f NODE-ALPHA #f))

(define (alpha-beta! b max-level stats-obj alpha beta)
  ;; Helper functions -------------------------------------------------------------------------
  (define (return-score depth alpha move type legal-move?)
    (cond [ move
            (write-tt-entry! (get-hash-key) (fx- max-level depth) depth alpha move type)
            alpha ]
          [ legal-move?
            (write-tt-entry! (get-hash-key) (fx- max-level depth) depth alpha #f type)
            alpha ]
          [ else
            ;; No legal moves, so either stalemate or checkmate
            (cond [ (is-king-in-check? b)
                    (let ([ score (fx+ (fx- MATE-SCORE) depth) ])
                      (write-tt-entry! (get-hash-key) (fx- max-level depth) depth
                                       score #f NODE-EXACT)
                      score) ]
                  [ else
                    (write-tt-entry! (get-hash-key) (fx- max-level depth) depth
                                     0 #f NODE-EXACT)
                    0 ]) ]))

  (define (main-loop get-move depth alpha best-move type legal-move?)
    (let ([ m (get-move) ])
      (if m
          (search-move get-move main-loop stats-obj depth max-level b m alpha beta best-move type legal-move?)
          (return-score depth alpha best-move type legal-move?))))
  ;; ------------------------------------------------------------------------------------------

  (increment-nodes! stats-obj)
  (define depth (board-depth b))
  (define-values (tt-score tt-move)
    (read-tt-entry (get-hash-key) (fx- max-level depth) depth alpha beta))
  (when tt-move (increment-tt-moves! stats-obj))

  (cond [ tt-score
          ;; Found a TT entry
          (increment-tt-hits! stats-obj)
          tt-score ]
        [ else
          ;; No TT entry found
          (increment-tt-misses! stats-obj)
          (cond [ (timeout? stats-obj)
                  ;; Timeout - indicate no result
                  #f ]
                [ (fx= depth max-level)
                  ;; Maximum depth - quiesce
                  (let ([ val (quiesce! b stats-obj alpha beta) ])
                    (write-tt-entry! (get-hash-key) 0 depth val #f NODE-EXACT)
                    val) ]
                [ else
                  ;; Otherwise, run main loop
                  (let ([ get-move (move-iterator! b #:tt-move tt-move) ])
                    (main-loop get-move depth alpha tt-move NODE-ALPHA #f)) ]) ]))

(define (quiesce! b stats-obj alpha beta)
  (increment-quiesce-nodes! stats-obj)
  (define stand-pat (evaluate b))
  (define depth     (board-depth b))
  (set-stats-seldepth! stats-obj depth)
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
                              (cond [ (not score)       #f           ]
                                    [ (fx>= score beta) beta         ]
                                    [ (fx> score alpha) (loop score) ]
                                    [ else              (loop alpha) ]))
                            ;; Illegal move, ignore move
                            (begin
                              (unmake-move! b m)
                              (loop alpha)))))))) ]))
