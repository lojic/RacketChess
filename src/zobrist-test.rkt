#lang racket

(module+ test
  (require rackunit)
  (require "./board.rkt"
           "./fen.rkt"
           "./make-move.rkt"
           "./piece.rkt"
           "./state.rkt"
           "./zobrist.rkt")

  (require math/base
           racket/fixnum
           racket/performance-hint)

  ;; Sanity check - value will change if random seed or generate-zobrist-key changes
  (let ([ val 76728950293162329 ]
        [ key (generate-zobrist-key (fen->board)) ])
    (check-not-false (fixnum? key))
    (check-equal? key val))

  ;; Compare incremental update to full zobrist key gen
  (let* ([ b1   (fen->board) ]
         [ key1 (generate-zobrist-key b1) ]
         ;; Board after 1. e4
         [ b2   (fen->board "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1") ]
         ;; Board after 1. ... Nc6
         [ b3   (fen->board "r1bqkbnr/pppppppp/2n5/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1") ]
         [ e2 (pos->idx "e2") ]
         [ e3 (pos->idx "e3") ]
         [ e4 (pos->idx "e4") ]
         [ b8 (pos->idx "b8") ]
         [ c6 (pos->idx "c6") ])

    (set-hash-key! key1)

    (check-equal? (bytes-ref (board-squares b1) e2) white-pawn)
    (check-equal? (bytes-ref (board-squares b1) e3) empty-square)
    (check-equal? (bytes-ref (board-squares b1) e4) empty-square)
    (check-equal? (bytes-ref (board-squares b1) b8) black-knight)
    (check-equal? (get-ep-idx b1) empty-square)

    (check-equal? (bytes-ref (board-squares b2) e2) empty-square)
    (check-equal? (bytes-ref (board-squares b2) e3) empty-square)
    (check-equal? (bytes-ref (board-squares b2) e4) white-pawn)
    (check-equal? (bytes-ref (board-squares b2) b8) black-knight)
    (check-equal? (get-ep-idx b2) e3)

    (check-equal? (bytes-ref (board-squares b3) e2) empty-square)
    (check-equal? (bytes-ref (board-squares b3) e3) empty-square)
    (check-equal? (bytes-ref (board-squares b3) e4) white-pawn)
    (check-equal? (bytes-ref (board-squares b3) b8) empty-square)
    (check-equal? (bytes-ref (board-squares b3) c6) black-knight)
    (check-equal? (get-ep-idx b3) 0)

    (set-hash-key! (xor-piece (get-hash-key) white-pawn e2)) ;; Remove pawn from e2
    (set-hash-key! (xor-piece (get-hash-key) white-pawn e4)) ;; Add pawn to e4
    (set-hash-key! (xor-ep-square (get-hash-key) e3))        ;; Add EP square
    (set-hash-key! (xor-whites-move (get-hash-key)))         ;; Remove white's move

    (check-equal? (generate-zobrist-key b2) (get-hash-key))

    (set-hash-key! (xor-piece (get-hash-key) black-knight b8)) ;; Remove knight from b8
    (set-hash-key! (xor-piece (get-hash-key) black-knight c6)) ;; Add knight to c6
    (set-hash-key! (xor-ep-square (get-hash-key) e3))          ;; Remove EP square
    (set-hash-key! (xor-whites-move (get-hash-key)))           ;; Add white's move

    (check-equal? (generate-zobrist-key b3) (get-hash-key)))

  )
