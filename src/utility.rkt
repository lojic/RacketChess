#lang racket

(provide number->delimited)

(define (number->delimited n #:include-fraction? [ include-fraction? #t ])
  (let* ([ sign     (if (negative? n) "-" "")   ]
         [ n        (+ (abs n) 0.005)           ]
         [ whole    (if include-fraction?
                        (exact-truncate n)
                        (exact-round n)) ]
         [ fraction (exact-truncate (* (- n whole) 100)) ]
         [ chunks   (let loop ([m whole][acc '()])
                      (if (< m 1000)
                          (cons (number->string m) acc)
                          (let-values ([(quot rem) (quotient/remainder m 1000)])
                            (loop quot
                                  (cons (~r rem #:min-width 3 #:pad-string "0")
                                        acc))))) ])

    (string-append sign
                   (string-join chunks ",")
                   (if include-fraction?
                       (string-append
                        "."
                        (~r fraction #:min-width 2 #:pad-string "0"))
                       ""))))

;; --------------------------------------------------------------------------------------------
;; Tests
;; --------------------------------------------------------------------------------------------

(module+ test
  (require rackunit)

  ;; ------------------------------------------------------------------------------------------
  ;; number->delimited
  ;; ------------------------------------------------------------------------------------------

  (for ([ pair (in-list '((0       "0.00")
                          (1       "1.00")
                          (3.14159 "3.14")
                          (4.499   "4.50")
                          (1000    "1,000.00")
                          (9999999 "9,999,999.00"))) ])
    (check-equal? (number->delimited (first pair)) (second pair)))

  (for ([ pair (in-list '((0       "0")
                          (1       "1")
                          (3.14159 "3")
                          (4.599   "5")
                          (1000    "1,000")
                          (9999999 "9,999,999"))) ])
    (check-equal? (number->delimited (first pair) #:include-fraction? #f) (second pair)))

  )
