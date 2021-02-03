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
                  (let ([ white? (is-white? piece)   ]
                        [ val    (piece-value piece) ])
                    (cond [ (is-pawn? piece)   (evaluate-pawn white? val idx)     ]
                          [ (is-king? piece)   (evaluate-king white? val idx)     ]
                          [ (is-queen? piece)  val                                ]
                          [ (is-rook? piece)   val                                ]
                          [ (is-bishop? piece) (evaluate-bishop piece white? val) ]
                          [ (is-knight? piece) (evaluate-knight piece white? val) ]))))) ])
    (if (board-whites-move? b)
        score
        (- score))))

(define-inline (evaluate-bishop piece white? val)
  ;; Slightly higher value for deployed bishop
  (if (has-moved? piece)
      ((if white? + -) val 1)
      val))

(define-inline (evaluate-king white? val idx)
  ;; Slightly higher value for king on castled square
  (if white?
      (if (or (= idx 93) (= idx 97))
          (+ val 5)
          val)
      (if (or (= idx 23) (= idx 27))
          (- val 5)
          val)))

(define-inline (evaluate-knight piece white? val)
  ;; Slightly higher value for deployed knight
  (if (has-moved? piece)
      ((if white? + -) val 1)
      val))

(define-inline (evaluate-pawn white? val idx)
  ;; Slightly higher value for pawns on a center square
  (if white?
      (if (or (= idx 64) (= idx 65))
          (+ val 5)
          val)
      (if (or (= idx 54) (= idx 55))
          (- val 5)
          val)))

(module+ main
  (require "./fen.rkt")
  ;; Queen takes pawn, bishop takes pawn:  r4rk1/ppp2ppp/2n5/3Bp3/8/2P4q/P1PPKP2/R1BQ4 b - - 0 1
  ;; Queen back to e4:  r4rk1/ppp2ppp/2n5/3pp3/4q3/1BP4P/P1PP1P2/R1BQ1K2 b - - 0 1
  (let ([ b (fen->board "r4rk1/ppp2ppp/2n5/3Bp3/8/2P4q/P1PPKP2/R1BQ4 b - - 0 1") ])
    (printf "Evaluation = ~a\n" (evaluate b))))
