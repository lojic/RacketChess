#lang racket


#;(begin
  (require racket/fixnum)

  (provide (all-from-out racket/fixnum)
           get-square
           set-square!
           vecref
           vecset!)

  (define-syntax-rule (get-square squares idx) (bytes-ref squares idx))

  (define-syntax-rule (set-square! squares idx piece) (bytes-set! squares idx piece))

  (define-syntax-rule (vecref vec i)    (vector-ref vec i))
  (define-syntax-rule (vecset! vec i v) (vector-set! vec i v))
  )

(begin
  (require (only-in racket/fixnum make-fxvector)
           racket/unsafe/ops)

  (provide fx*
           fx+
           fx-
           fx<=
           fx=
           fx>
           fx>=
           fxvector-length
           fxvector-ref
           fxvector-set!
           fxand
           fxior
           fxlshift
           fxrshift
           fxxor
           get-square
           make-fxvector
           set-square!
           vecref
           vecset!)


  (define-syntax-rule (fx* a ...)  (unsafe-fx* a ...))
  (define-syntax-rule (fx+ a ...)  (unsafe-fx+ a ...))
  (define-syntax-rule (fx- a ...)  (unsafe-fx- a ...))
  (define-syntax-rule (fx<= a ...) (unsafe-fx<= a ...))
  (define-syntax-rule (fx= a ...)  (unsafe-fx= a ...))
  (define-syntax-rule (fx> a ...)  (unsafe-fx> a ...))
  (define-syntax-rule (fx>= a ...) (unsafe-fx>= a ...))

  (define-syntax-rule (fxvector-length v)       (unsafe-fxvector-length v))
  (define-syntax-rule (fxvector-ref v i)        (unsafe-fxvector-ref v i))
  (define-syntax-rule (fxvector-set! vec i val) (unsafe-fxvector-set! vec i val))

  (define-syntax-rule (fxand a ...)  (unsafe-fxand a ...))
  (define-syntax-rule (fxior a ...)  (unsafe-fxior a ...))
  (define-syntax-rule (fxlshift a b) (unsafe-fxlshift a b))
  (define-syntax-rule (fxrshift a b) (unsafe-fxrshift a b))
  (define-syntax-rule (fxxor a ...)  (unsafe-fxxor a ...))

  (define-syntax-rule (get-square squares idx) (unsafe-bytes-ref squares idx))

  (define-syntax-rule (set-square! squares idx piece) (unsafe-bytes-set! squares idx piece))

  (define-syntax-rule (vecref vec i)    (unsafe-vector-ref vec i))
  (define-syntax-rule (vecset! vec i v) (unsafe-vector-set! vec i v))
  )
