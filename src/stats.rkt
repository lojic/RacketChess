#lang racket

(require "./global.rkt"
         "./utility.rkt")
(require racket/performance-hint)

(provide create-stats
         increment-beta-cuts!
         increment-nodes!
         increment-quiesce-nodes!
         increment-tt-hits!
         increment-tt-misses!
         increment-tt-moves!
         init-timer!
         print-stats
         timeout?
         (struct-out stats))

(struct stats (beta-cuts
               nodes
               quiesce-nodes
               start-seconds
               think-seconds
               tt-hits
               tt-misses
               tt-moves)
        #:transparent
        #:mutable)

(define (create-stats)
  (stats 0 0 0 0 0 0 0 0))

(define-inline (increment-beta-cuts! obj)
  (set-stats-beta-cuts! obj (fx+ 1 (stats-beta-cuts obj))))

(define-inline (increment-nodes! obj)
  (set-stats-nodes! obj (fx+ 1 (stats-nodes obj))))

(define-inline (increment-quiesce-nodes! obj)
  (set-stats-quiesce-nodes! obj (fx+ 1 (stats-quiesce-nodes obj))))

(define-inline (increment-tt-hits! obj)
  (set-stats-tt-hits! obj (fx+ 1 (stats-tt-hits obj))))

(define-inline (increment-tt-misses! obj)
  (set-stats-tt-misses! obj (fx+ 1 (stats-tt-misses obj))))

(define-inline (increment-tt-moves! obj)
  (set-stats-tt-moves! obj (fx+ 1 (stats-tt-moves obj))))

(define (init-timer! obj seconds)
  (set-stats-think-seconds! obj seconds)
  (set-stats-start-seconds! obj (current-seconds)))

(define (print-stats obj)
  (define (format-integer n)
    (number->delimited n #:include-fraction? #f))

  (let* ([ elapsed-seconds (exact->inexact
                            (fx- (current-seconds)
                                 (stats-start-seconds obj))) ]
         [ nodes           (stats-nodes obj)                ]
         [ qnodes          (stats-quiesce-nodes obj)        ]
         [ beta-cuts       (stats-beta-cuts obj)            ]
         [ total-nodes     (+ nodes qnodes)                 ]
         [ tt-hits         (stats-tt-hits obj)              ]
         [ tt-misses       (stats-tt-misses obj)            ]
         [ tt-reads        (+ tt-hits tt-misses)            ]
         [ tt-moves        (stats-tt-moves obj)             ])
    (printf "\n")
    (printf "Time        = ~a\n" elapsed-seconds)
    (printf "Nodes       = ~a\n" (format-integer nodes))
    (printf "Q Nodes     = ~a\n" (format-integer qnodes))
    (printf "Total Nodes = ~a\n" (format-integer total-nodes))
    (printf "TT Hits     = ~a (~a %)\n" (format-integer tt-hits) (~r (* 100.0 (/ tt-hits tt-reads)) #:precision 2))
    (printf "TT Misses   = ~a\n" (format-integer tt-misses))
    (printf "TT Moves    = ~a\n" (format-integer tt-moves))
    (printf "Beta cuts   = ~a\n" (format-integer beta-cuts))
    (printf "Nodes/sec   = ~a\n" (number->delimited (/ total-nodes elapsed-seconds)))))

(define-inline (timeout? obj)
  (if (fx= 0 (fxand (stats-nodes obj) #b1111111111111))
      (let ([ elapsed-seconds (fx- (current-seconds) (stats-start-seconds obj)) ])
        (fx>= elapsed-seconds (stats-think-seconds obj)))
      #f))
