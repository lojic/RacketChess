#lang racket

(begin
  (require racket/fixnum)
  (define-syntax-rule (get-byte bs ...)   (bytes-ref bs ...))
  (define-syntax-rule (set-byte! bs ...)  (bytes-set! bs ...))
  (define-syntax-rule (vecref vec i)      (vector-ref vec i))
  (define-syntax-rule (vecset! vec i ...) (vector-set! vec i ...))
  )

#;(begin
  (require (only-in racket/fixnum make-fxvector)
           racket/unsafe/ops)
  (define-syntax-rule (fx* a ...)             (unsafe-fx* a ...))
  (define-syntax-rule (fx+ a ...)             (unsafe-fx+ a ...))
  (define-syntax-rule (fx- a ...)             (unsafe-fx- a ...))
  (define-syntax-rule (fx< a ...)             (unsafe-fx< a ...))
  (define-syntax-rule (fx<= a ...)            (unsafe-fx<= a ...))
  (define-syntax-rule (fx= a ...)             (unsafe-fx= a ...))
  (define-syntax-rule (fx> a ...)             (unsafe-fx> a ...))
  (define-syntax-rule (fx>= a ...)            (unsafe-fx>= a ...))
  (define-syntax-rule (fxand a ...)           (unsafe-fxand a ...))
  (define-syntax-rule (fxior a ...)           (unsafe-fxior a ...))
  (define-syntax-rule (fxlshift a ...)        (unsafe-fxlshift a ...))
  (define-syntax-rule (fxrshift a ...)        (unsafe-fxrshift a ...))
  (define-syntax-rule (fxvector-length v)     (unsafe-fxvector-length v))
  (define-syntax-rule (fxvector-ref v ...)    (unsafe-fxvector-ref v ...))
  (define-syntax-rule (fxvector-set! vec ...) (unsafe-fxvector-set! vec ...))
  (define-syntax-rule (fxxor a ...)           (unsafe-fxxor a ...))
  (define-syntax-rule (get-byte bs ...)       (unsafe-bytes-ref bs ...))
  (define-syntax-rule (set-byte! bs ...)      (unsafe-bytes-set! bs ...))
  (define-syntax-rule (vecref vec i)          (unsafe-vector-ref vec i))
  (define-syntax-rule (vecset! vec i ...)     (unsafe-vector-set! vec i ...))
  )

(provide fx*
         fx+
         fx-
         fx<
         fx<=
         fx=
         fx>
         fx>=
         fxand
         fxior
         fxlshift
         fxrshift
         fxvector-length
         fxvector-ref
         fxvector-set!
         fxxor
         get-byte
         make-fxvector
         set-byte!
         vecref
         vecset!)
