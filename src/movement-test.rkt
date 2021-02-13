#lang racket

(module+ test
  (require "./board.rkt"
           "./fen.rkt"
           "./make-move.rkt"
           "./pgn.rkt")

  (require rackunit)

  ;; ------------------------------------------------------------------------------------------
  ;; pgn-move
  ;; ------------------------------------------------------------------------------------------

  ;; Moves from first game: https://lichess.org/V4lzqXvZI0OX
  (let ([ b (fen->board) ])
    (for ([ m (in-list '("d4" "d5" "c4" "e6" "Nc3" "Nf6" "Bg5" "h6" "Bxf6" "Qxf6" "cxd5" "Bb4"
                         "Qa4" "Nc6" "dxc6" "Bxc3" "bxc3" "O-O" "Qc4" "b6" "e4" "a5" "Qb3" "e5"
                         "dxe5" "Qxe5" "f3" "Be6" "Qc2" "Rad8" "Rb1" "Rd6" "Rb5" "Qf6" "e5" "Bf5"
                         "Qe2" "Bd3" "exf6" "Bxe2" "Nxe2" "Rxf6" "Nd4" "Re8" "Kf2" "Rd6" "Rf5"
                         "g6" "Rf4" "g5" "Rf5" "Kg7" "Rb5" "Kg6" "Rb3" "Rd5" "Ne2" "Rd2" "Rb5"
                         "Rxa2" "f4" "g4" "Rd5" "Re4" "Rd7" "Rxf4")) ])
      (make-pgn-move! b m))
    (check-equal? (board->fen b) "8/2pR1p2/1pP3kp/p7/5rp1/2P5/r3NKPP/5B1R w - - 0 34"))


  ;; Debugging
  #;(let ([ b (fen->board) ]
        [ pgn-moves '("d4" "d5" "c4" "e6" "Nc3" "Nf6" "Bg5" "h6" "Bxf6" "Qxf6" "cxd5" "Bb4"
                         "Qa4" "Nc6" "dxc6" "Bxc3" "bxc3" "O-O" "Qc4" "b6" "e4" "a5" "Qb3" "e5"
                         "dxe5" "Qxe5" "f3" "Be6" "Qc2" "Rad8" "Rb1" "Rd6" "Rb5" "Qf6" "e5" "Bf5"
                         "Qe2" "Bd3" "exf6" "Bxe2" "Nxe2" "Rxf6" "Nd4" "Re8" "Kf2" "Rd6" "Rf5"
                         "g6" "Rf4" "g5" "Rf5" "Kg7" "Rb5" "Kg6" "Rb3" "Rd5" "Ne2" "Rd2" "Rb5"
                         "Rxa2" "f4" "g4" "Rd5" "Re4" "Rd7" "Rxf4") ])
    (check-equal? (board->fen b) initial-fen)
    (print-board b #:full? #t)
    (let ([ moves (let loop ([ moves pgn-moves ][ result '() ])
                    (if (null? moves)
                        result
                        (let ([ m (pgn-move b (car moves)) ])
                          (make-move! b m)
                          (loop (cdr moves) (cons m result))))) ])
      (print-board b #:full? #t)
      (for ([ m (in-list moves) ])
        (unmake-move! b m))
      (print-board b #:full? #t)))

  )
