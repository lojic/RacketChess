 #lang racket

(require "./global.rkt")
(require racket/performance-hint)

(provide read-tt-entry
         write-tt-entry!
         NODE-ALPHA
         NODE-BETA
         NODE-EXACT)

;; Hash table size needs to be a power of 2 so that hash-key can
;; simply do an 'and' to obtain the index
(define NUM-HASH-ENTRIES (expt 2 28))
(define HASH-MASK (sub1 NUM-HASH-ENTRIES))

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

;; Returns (cons score move) or #f if no matching entry found
(define (read-tt-entry key draft alpha beta)
  (let ([ entry (vecref tt-table (hash-index key)) ])
    (if (and entry (fx= key (tt-zobrist-key entry)))
        ;; Entry found, and it matches the key
        (let ([ best-move (tt-best-move entry) ])
          (if (fx>= (tt-draft entry) draft)
              ;; We have sufficient draft
              (let ([ score (tt-score entry) ]
                    [ type  (tt-type entry)  ])
                (cond [ (fx= NODE-EXACT type)                          (cons score best-move) ]
                      [ (and (fx= NODE-ALPHA type) (fx<= score alpha)) (cons alpha best-move) ]
                      [ (and (fx= NODE-BETA type) (fx>= score beta))   (cons beta best-move)  ]
                      [ else                                           (cons #f best-move)    ]))
              ;; Insufficient draft, we can't use the score, but return
              ;; best move
              (cons #f best-move)))
        ;; Nothing found
        #f)))

(define (write-tt-entry! key draft score best-move type)
  (vecset! tt-table
           (hash-index key)
           (tt key
               draft
               best-move
               type
               score)))

(module+ test
  (require math/base
           rackunit)

  (let ([ key (hash-index 495045541071373927) ])
    (check-not-false (fixnum? key))
    (check-not-false (< key NUM-HASH-ENTRIES)))

  )
