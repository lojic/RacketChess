#lang racket

;; fixnum-fields combines struct-like convenience with the
;; efficiciency of storing all of the fields in a single fixnum
;;
;; Original code contributed by: Jens Axel Søgaard
;;
;; TODO: ensure total width specified fits in a fixnum

(require "./global.rkt"
         (for-syntax racket/syntax syntax/parse "./global.rkt")
         racket/performance-hint)

(provide fixnum-fields)

(begin-for-syntax
  (define-syntax-class width-clause
    (pattern [name:id width:exact-positive-integer]         #:attr flag #'#f)
    (pattern [name:id width:exact-positive-integer #:flag]  #:attr flag #'#t))

  (define (make-mask from to)
    ; > (number->string (make-mask 3 7) 2)
    ; "1111000"
    (define delta (- to from))
    (define delta-ones (- (expt 2 delta) 1))
    (fxlshift delta-ones from))

  (define (make-update-mask total-width from to)
    ; same as make-mask, but with bits flipped
    ; > (number->string (make-mask 3 7) 2)
    ; "1111000"
    (define total-width-ones (- (expt 2 total-width) 1))
    (fxxor total-width-ones (make-mask from to))))

(define-syntax (fixnum-fields stx)
  (syntax-parse stx
    [(_fixnum-fields prefix (wc:width-clause ...))
     ;; Extract clause attributes
     (define widths (syntax->datum #'(wc.width ...)))
     (define names  (syntax->list  #'(wc.name ...)))
     (define flags  (syntax->datum  #'(wc.flag ...)))
     ;; Calculate indices and bit masks
     (define cumulated-widths (for/fold
                                  ([ws '(0)] #:result (reverse ws))
                                  ([w widths])
                                (cons (+ (car ws) w) ws)))
     (define total-width  (apply + widths))
     (define froms        (reverse (cdr (reverse cumulated-widths))))
     (define tos          (cdr cumulated-widths))
     (define masks        (map make-mask froms tos))
     (define update-masks (for/list ([from froms] [to tos])
                            (make-update-mask total-width from to)))
     ; Now bitfield i start at froms[i] and ends at tos[i],
     ; where we are using inclusive/exclusive indices as usual.
     ;; Calculate indices for flags
     (define flag-indices     (for/list ([from froms] [flag flags] #:when flag)
                                from))
     (define flag-masks       (for/list ([index flag-indices])
                                (expt 2 index)))
     (define flag-input-masks (for/list ([flag flags] [width widths])
                                (- (expt 2 width) 1)))

     ;; Compute the names
     (define prefix-names         (for/list ([name names])
                                    (format-id name "~a-~a" #'prefix name #:source name)))
     (define update-prefix-names  (for/list ([name names])
                                    (format-id name "update-~a-~a" #'prefix name #:source name)))
     (define prefix-name?s        (for/list ([name names] [flag flags] #:when flag)
                                    (format-id name "~a-~a?" #'prefix name #:source name)))
     (define set-prefix-name?s (for/list ([name names])
                                 (format-id name "set-~a-~a?" #'prefix name #:source name)))
     (define unset-prefix-name?s (for/list ([name names])
                                   (format-id name "unset-~a-~a?" #'prefix name #:source name)))
     (define make-prefix          (format-id #'prefix "make-~a" #'prefix #:source #'prefix))

     ;; Bind pattern variables to be used in the template.
     (with-syntax ([(prefix-name ...)        prefix-names]
                   [(update-prefix-name ...) update-prefix-names]
                   [(prefix-name? ...)       prefix-name?s]
                   [(set-prefix-name? ...)   set-prefix-name?s]
                   [(unset-prefix-name? ...) unset-prefix-name?s]
                   [make-prefix              make-prefix]
                   [(from ...)               froms]
                   [(mask ...)               masks]
                   [(update-mask ...)        update-masks]
                   [(flag-mask ...)          flag-masks]
                   [(flag-input-mask ...)    flag-input-masks]
                   [(name ...)               names])
       ;; Generate the code from the template:
       (syntax/loc stx
         (begin
           (begin-encourage-inline
             (define (make-prefix name ...)
               (fxior (fxlshift (fxand name flag-input-mask) from)
                      ...))
             (define (prefix-name fx)
               (fxrshift (fxand fx mask) from))
             ...
             (define (update-prefix-name fx val)
               (fxior (fxlshift (fxand val flag-input-mask) from)
                      (fxand    fx update-mask)))
             ...
             (define (prefix-name? fx)
               (not (fx= (fxand fx flag-mask) 0)))
             ...
             (define (set-prefix-name? fx)
               (fxior (fxlshift 1 from)
                      (fxand fx update-mask)))
             ...
             (define (unset-prefix-name? fx)
               (fxior 0 (fxand fx update-mask)))
             ...))))]))

(module+ test
  (require rackunit)

  (fixnum-fields move ([ src                 5 ]
                       [ src-idx             7 ]
                       [ dst-idx             7 ]
                       [ captured-piece      5 ]
                       [ promoted-piece      5 ]
                       [ is-castle-queenside 1 #:flag ]
                       [ is-castle-kingside  1 #:flag ]
                       [ is-ep-capture       1 #:flag ]))

  ;; ------------------------------------------------------------------------------------------
  ;; Ensure we don't store more bits than the field width
  ;; ------------------------------------------------------------------------------------------

  (let* ([ m (make-move #b1111111111111111
                        #b1111111111111111
                        #b1111111111111111
                        #b1111111111111111
                        #b1111111111111111
                        #b1111111111111111
                        #b1111111111111111
                        #b1111111111111111) ]
         [ src            (move-src m)            ]
         [ src-idx        (move-src-idx m)        ]
         [ dst-idx        (move-dst-idx m)        ]
         [ captured-piece (move-captured-piece m) ]
         [ promoted-piece (move-promoted-piece m) ]
         [ is-castle-queenside? (move-is-castle-queenside? m) ]
         [ is-castle-kingside?  (move-is-castle-kingside? m)  ]
         [ is-ep-capture?       (move-is-ep-capture? m)       ])
    (check-not-false (fixnum? m))
    (check-not-false (fixnum? src))
    (check-not-false (fixnum? src-idx))
    (check-not-false (fixnum? dst-idx))
    (check-not-false (fixnum? captured-piece))
    (check-not-false (fixnum? promoted-piece))
    (check-not-false (boolean? is-castle-queenside?))
    (check-not-false (boolean? is-castle-kingside?))
    (check-not-false (boolean? is-ep-capture?))

    (check-equal? (move-src m) #b11111)
    (check-equal? (move-src-idx m) #b1111111)
    (check-equal? (move-dst-idx m) #b1111111)
    (check-equal? (move-captured-piece m) #b11111)
    (check-equal? (move-promoted-piece m) #b11111)

    (check-not-false (move-is-castle-queenside? m))
    (check-not-false (move-is-castle-kingside? m))
    (check-not-false (move-is-ep-capture? m)))

  ;; ------------------------------------------------------------------------------------------
  ;; Getters
  ;; ------------------------------------------------------------------------------------------

  (let* ([ m (make-move #b101 27 35 #b110 #b011 1 0 0) ]
         [ src            (move-src m)            ]
         [ src-idx        (move-src-idx m)        ]
         [ dst-idx        (move-dst-idx m)        ]
         [ captured-piece (move-captured-piece m) ]
         [ promoted-piece (move-promoted-piece m) ]
         [ is-castle-queenside? (move-is-castle-queenside? m) ]
         [ is-castle-kingside?  (move-is-castle-kingside? m)  ]
         [ is-ep-capture?       (move-is-ep-capture? m)       ])

    (check-not-false (fixnum? m))
    (check-not-false (fixnum? src))
    (check-not-false (fixnum? src-idx))
    (check-not-false (fixnum? dst-idx))
    (check-not-false (fixnum? captured-piece))
    (check-not-false (fixnum? promoted-piece))
    (check-not-false (boolean? is-castle-queenside?))
    (check-not-false (boolean? is-castle-kingside?))
    (check-not-false (boolean? is-ep-capture?))

    (check-equal? (move-src m) #b101)
    (check-equal? (move-src-idx m) 27)
    (check-equal? (move-dst-idx m) 35)
    (check-equal? (move-captured-piece m) #b110)
    (check-equal? (move-promoted-piece m) #b011)

    (check-not-false (move-is-castle-queenside? m))
    (check-false     (move-is-castle-kingside? m))
    (check-false     (move-is-ep-capture? m)))

  ;; ------------------------------------------------------------------------------------------
  ;; Setters
  ;; ------------------------------------------------------------------------------------------

  (let ([ m (make-move 0 0 0 0 0 0 0 0) ])
    (check-equal? (move-src m)            0)
    (check-equal? (move-src-idx m)        0)
    (check-equal? (move-dst-idx m)        0)
    (check-equal? (move-captured-piece m) 0)
    (check-equal? (move-promoted-piece m) 0)
    (check-false  (move-is-castle-queenside? m))
    (check-false  (move-is-castle-kingside?  m))
    (check-false  (move-is-ep-capture?       m))

    (check-equal?    (move-src                  (update-move-src m #b101)) #b101)
    (check-equal?    (move-src-idx              (update-move-src-idx m 127)) 127)
    (check-equal?    (move-dst-idx              (update-move-dst-idx m 119)) 119)
    (check-equal?    (move-captured-piece       (update-move-captured-piece m #b010)) #b010)
    (check-equal?    (move-promoted-piece       (update-move-promoted-piece m #b110)) #b110)

    (check-not-false (move-is-castle-queenside? (set-move-is-castle-queenside? m)))
    (check-not-false (move-is-castle-kingside?  (set-move-is-castle-kingside? m)))
    (check-not-false (move-is-ep-capture?       (set-move-is-ep-capture? m)))

    (check-false (move-is-castle-queenside?
                  (unset-move-is-castle-queenside?
                   (set-move-is-castle-queenside? m))))
    (check-false (move-is-castle-kingside?
                  (unset-move-is-castle-kingside?
                   (set-move-is-castle-kingside? m))))
    (check-false (move-is-ep-capture?
                  (unset-move-is-ep-capture?
                   (set-move-is-ep-capture? m)))))

  )
