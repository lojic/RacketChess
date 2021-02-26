#lang racket

(require "./global.rkt")
(require racket/performance-hint)

(provide read-tt-entry
         write-tt-entry!
         MATE-SCORE
         MAX-SCORE
         MIN-SCORE
         NODE-ALPHA
         NODE-BETA
         NODE-EXACT)

;; Hash table size needs to be a power of 2 so that hash-key can
;; simply do an 'and' to obtain the index
(define NUM-HASH-ENTRIES (expt 2 28))
(define HASH-MASK (sub1 NUM-HASH-ENTRIES))

(define MIN-SCORE -100000)
(define MAX-SCORE  100000)
(define MATE-SCORE  90000)
(define MATE-VALUE  80000)

(define NODE-EXACT 1)
(define NODE-BETA  2)
(define NODE-ALPHA 3)

(struct tt (zobrist-key
            draft
            best-move
            type ; Exact, Beta or Alpha
            score)
        #:transparent)

(define tt-table (make-vector NUM-HASH-ENTRIES #f))

(define-inline (hash-index zobrist-key)
  (fxand zobrist-key HASH-MASK))

;; Returns (values score move). If no matching entry found, both score and move are #f
(define (read-tt-entry key draft ply alpha beta)
  (define-inline (adjust-score score ply)
    (cond [ (fx< score (fx- MATE-VALUE)) (fx+ score ply) ]
          [ (fx> score MATE-VALUE)       (fx- score ply) ]
          [ else                         score           ]))

  (let ([ entry (vecref tt-table (hash-index key)) ])
    (if (and entry (fx= key (tt-zobrist-key entry)))
        ;; Entry found, and it matches the key
        (let ([ best-move (tt-best-move entry) ])
          (if (fx>= (tt-draft entry) draft)
              ;; We have sufficient draft
              (let ([ score (adjust-score (tt-score entry) ply) ]
                    [ type  (tt-type entry)  ])
                (cond [ (fx= NODE-EXACT type)                          (values score best-move) ]
                      [ (and (fx= NODE-ALPHA type) (fx<= score alpha)) (values alpha best-move) ]
                      [ (and (fx= NODE-BETA type) (fx>= score beta))   (values beta best-move)  ]
                      [ else                                           (values #f best-move)    ]))
              ;; Insufficient draft, we can't use the score, but return
              ;; best move
              (values #f best-move)))
        ;; Nothing found
        (values #f #f))))

(define (write-tt-entry! key draft ply score best-move type)
  (let ([ score (cond [ (fx< score (fx- MATE-SCORE)) (fx- score ply) ]
                      [ (fx> score MATE-SCORE)       (fx+ score ply) ]
                      [ else                         score           ]) ])
    (vecset! tt-table
             (hash-index key)
             (tt key
                 draft
                 best-move
                 type
                 score))))

(module+ test
  (require math/base
           rackunit)

  (let ([ key (hash-index 495045541071373927) ])
    (check-not-false (fixnum? key))
    (check-not-false (< key NUM-HASH-ENTRIES)))

  )
