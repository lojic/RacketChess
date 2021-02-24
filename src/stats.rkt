#lang racket

(require "./global.rkt")
(require racket/performance-hint)

(provide create-stats
         increment-nodes!
         increment-tt-hits!
         increment-tt-moves!
         init-timer!
         timeout?
         (struct-out stats))

(struct stats (beta-cuts
               nodes
               quiesce-nodes
               start-seconds
               think-seconds
               tt-hits
               tt-moves
               tt-reads)
        #:transparent
        #:mutable)

(define (create-stats)
  (stats 0 0 0 0 0 0 0 0))

(define-inline (increment-nodes! obj)
  (set-stats-nodes! obj (fx+ 1 (stats-nodes obj))))

(define-inline (increment-tt-hits! obj)
  (set-stats-tt-hits! obj (fx+ 1 (stats-tt-hits obj))))

(define-inline (increment-tt-moves! obj)
  (set-stats-tt-moves! obj (fx+ 1 (stats-tt-moves obj))))

(define (init-timer! obj seconds)
  (set-stats-think-seconds! obj seconds)
  (set-stats-start-seconds! obj (current-seconds)))

(define-inline (timeout? obj)
  (if (fx= 0 (fxand (stats-nodes obj) #b1111111111111))
      (let ([ elapsed-seconds (fx- (current-seconds) (stats-start-seconds obj)) ])
        (fx> elapsed-seconds (stats-think-seconds obj)))
      #f))
