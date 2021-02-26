#lang racket

(require "./board.rkt"
         "./global.rkt"
         "./piece.rkt")

(provide evaluate)

;; Compute a list of all square indices
(define square-indices (for*/list ([ rank (in-range 8) ]
                                   [ file (in-range 8) ])
                         (file-rank->idx file rank)))

;; Switched to negamax, so return the score relative to the player
(define (evaluate b)
  (define squares (board-squares b))

  (let ([ score (let loop ([ lst square-indices ][ sum 0 ])
                  (if (null? lst)
                      sum
                      (let* ([ idx   (car lst)                ]
                             [ piece (get-square squares idx) ])
                        (loop (cdr lst)
                              (if (fx= piece empty-square)
                                  sum
                                  (fx+ sum (piece-value b piece idx))))))) ])
    (if (is-whites-move? b)
        score
        (fx- score))))

(module+ main
  (require "./fen.rkt")
  ;; Queen takes pawn, bishop takes pawn:  r4rk1/ppp2ppp/2n5/3Bp3/8/2P4q/P1PPKP2/R1BQ4 b - - 0 1
  ;; Queen back to e4:  r4rk1/ppp2ppp/2n5/3pp3/4q3/1BP4P/P1PP1P2/R1BQ1K2 b - - 0 1
  (let ([ b (fen->board "r4rk1/ppp2ppp/2n5/3Bp3/8/2P4q/P1PPKP2/R1BQ4 b - - 0 1") ])
    (printf "Evaluation = ~a\n" (evaluate b))))

(module+ test
  (require "./fen.rkt")
  (require rackunit)

  ;; NOTE: all of these tests will need to change when I code a "real" evaluation function

  ;; Starting position has score of zero
  (let ([ b (fen->board) ])
    (check-equal? (evaluate b) 0))

  ;; White has castled behind 3 pawns
  (let ([ b (fen->board "8/8/8/8/8/8/5PPP/5RK1 w - - 0 1") ])
    (check-equal? (evaluate b)
                  (+ 110 110 105 ; 3 pawns
                     506         ; Rook
                     30)))       ; King

  ;; Black has castled behind 3 pawns
  (let ([ b (fen->board "5rk1/5ppp/8/8/8/8/8/8 b - - 0 1") ])
    (check-equal? (evaluate b)
                  (+ 110 110 105 ; 3 pawns
                     506           ; Rook
                     30)))         ; King

  ;; Random sample from a lichess game
  (let ([ b (fen->board "r3k2r/p1qpn1p1/1pnppp2/7p/3PN3/2P1PNP1/PP3PKP/R2Q3R w kq - 0 1") ])
    (check-equal? (evaluate b) 80))

  ;; Random sample from a lichess game
  (let ([ b (fen->board "1r3k2/p2q1p2/1r1p2p1/2pPp3/2b1PbnP/P1N2Q2/1B3RB1/1R5K w - - 0 1") ])
    (check-equal? (evaluate b) -160))

  ;; Random sample from a lichess game
  (let ([ b (fen->board "r2q1rk1/p3ppbp/1p3np1/2ppN3/3P4/1P1PP2P/PB3PP1/R2Q1RK1 w - - 0 1") ])
    (check-equal? (evaluate b) 45))

  )
