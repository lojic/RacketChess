#lang racket

(require "./board.rkt"
         "./piece.rkt")

(require racket/performance-hint)

(provide evaluate)

;; Switched to negamax, so return the score relative to the player
(define (evaluate b)
  (let ([ score
          (for*/sum ([ rank (in-range 8) ]
                     [ file (in-range 8) ])
            (let* ([ idx    (file-rank->idx file rank)        ]
                   [ piece  (bytes-ref (board-squares b) idx) ])
              (if (= piece empty-square)
                  0
                  (piece-value b piece idx)))) ])
    (if (board-whites-move? b)
        score
        (- score))))

(module+ main
  (require "./fen.rkt")
  ;; Queen takes pawn, bishop takes pawn:  r4rk1/ppp2ppp/2n5/3Bp3/8/2P4q/P1PPKP2/R1BQ4 b - - 0 1
  ;; Queen back to e4:  r4rk1/ppp2ppp/2n5/3pp3/4q3/1BP4P/P1PP1P2/R1BQ1K2 b - - 0 1
  (let ([ b (fen->board "r4rk1/ppp2ppp/2n5/3Bp3/8/2P4q/P1PPKP2/R1BQ4 b - - 0 1") ])
    (printf "Evaluation = ~a\n" (evaluate b))))
