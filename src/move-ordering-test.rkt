#lang racket

(module+ test
  (require "./board.rkt"
           "./fen.rkt"
           "./move.rkt"
           "./move-ordering.rkt"
           "./movement.rkt")
  (require rackunit)

  (let ([ b (fen->board "4k3/8/3p4/2rq1n2/2RPPB2/8/8/4K3 w - - 0 1") ])
    (define squares (board-squares b))
    (generate-moves! b)
    (order-moves! b)
    (let* ([ vec (vector-copy (tactical-moves b) 0 (add1 (tactical-head b))) ]
           [ c5 (pos->idx "c5") ]
           [ br (bytes-ref squares c5) ]
           [ d5 (pos->idx "d5") ]
           [ bq (bytes-ref squares d5) ]
           [ d6 (pos->idx "d6") ]
           [ bp (bytes-ref squares d6) ]
           [ f5 (pos->idx "f5") ]
           [ bn (bytes-ref squares f5) ]
           [ c4 (pos->idx "c4") ]
           [ wr (bytes-ref squares c4) ]
           [ d4 (pos->idx "d4") ]
           [ leftp (bytes-ref squares d4) ]
           [ e4 (pos->idx "e4") ]
           [ rightp (bytes-ref squares e4) ]
           [ f4 (pos->idx "f4") ]
           [ wb (bytes-ref squares f4) ])
      (check-equal? (vector-length vec) 5)
      (check-equal? (vector-ref vec 0)
                    (create-move rightp e4 d5 #:captured-piece bq))
      (check-equal? (vector-ref vec 1)
                    (create-move leftp d4 c5 #:captured-piece br))
      (check-equal? (vector-ref vec 2)
                    (create-move rightp e4 f5 #:captured-piece bn))
      (check-equal? (vector-ref vec 3)
                    (create-move wr c4 c5 #:captured-piece br))
      (check-equal? (vector-ref vec 4)
                    (create-move wb f4 d6 #:captured-piece bp))
      ))

  )
