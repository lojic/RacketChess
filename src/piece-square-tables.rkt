#lang racket

(require "./board.rkt"
         "./global.rkt")

(provide bishop-pst-black
         bishop-pst-white
         king-end-pst-black
         king-end-pst-white
         king-middle-pst-black
         king-middle-pst-white
         knight-pst-black
         knight-pst-white
         pawn-pst-black
         pawn-pst-white
         queen-pst-black
         queen-pst-white
         rook-pst-black
         rook-pst-white)

(define mailbox-size 120) ; 10 x 12 board

(define pawn-raw
  #(  0   0   0   0   0   0   0   0
     50  50  50  50  50  50  50  50
     10  10  20  30  30  20  10  10
      5   5  10  25  25  10   5   5
      0   0   0  20  20   0   0   0
      5  -5 -10   0   0 -10  -5   5
      5  10  10 -20 -20  10  10   5
      0   0   0   0   0   0   0   0))

(define knight-raw
  #(-50 -40 -30 -30 -30 -30 -40 -50
    -40 -20   0   0   0   0 -20 -40
    -30   0  10  15  15  10   0 -30
    -30   5  15  20  20  15   5 -30
    -30   0  15  20  20  15   0 -30
    -30   5  10  15  15  10   5 -30
    -40 -20   0   5   5   0 -20 -40
    -50 -40 -30 -30 -30 -30 -40 -50))

(define bishop-raw
  #(-20 -10 -10 -10 -10 -10 -10 -20
    -10   0   0   0   0   0   0 -10
    -10   0   5  10  10   5   0 -10
    -10   5   5  10  10   5   5 -10
    -10   0  10  10  10  10   0 -10
    -10  10  10  10  10  10  10 -10
    -10   5   0   0   0   0   5 -10
    -20 -10 -10 -10 -10 -10 -10 -20))

;; What about slightly encouraging the rook to the castled rook square?
(define rook-raw
  #(0   0   0   0   0   0   0   0
    5  10  10  10  10  10  10   5
   -5   0   0   0   0   0   0  -5
   -5   0   0   0   0   0   0  -5
   -5   0   0   0   0   0   0  -5
   -5   0   0   0   0   0   0  -5
   -5   0   0   0   0   0   0  -5
    0   0   0   6   7   6   0   0))

(define queen-raw
  #(-20 -10 -10  -5  -5 -10 -10 -20
    -10   0   0   0   0   0   0 -10
    -10   0   5   5   5   5   0 -10
     -5   0   5   5   5   5   0  -5
      0   0   5   5   5   5   0  -5
    -10   5   5   5   5   5   0 -10
    -10   0   5   0   0   0   0 -10
    -20 -10 -10  -5  -5 -10 -10 -20))

(define king-raw-middle
  #(-30 -40 -40 -50 -50 -40 -40 -30
    -30 -40 -40 -50 -50 -40 -40 -30
    -30 -40 -40 -50 -50 -40 -40 -30
    -30 -40 -40 -50 -50 -40 -40 -30
    -20 -30 -30 -40 -40 -30 -30 -20
    -10 -20 -20 -20 -20 -20 -20 -10
     20  20   0   0   0   0  20  20
     20  30  30   0   0  10  30  20))

(define king-raw-end
  #(-50 -40 -30 -20 -20 -30 -40 -50
    -30 -20 -10   0   0 -10 -20 -30
    -30 -10  20  30  30  20 -10 -30
    -30 -10  30  40  40  30 -10 -30
    -30 -10  30  40  40  30 -10 -30
    -30 -10  20  30  30  20 -10 -30
    -30 -30   0   0   0   0 -30 -30
    -50 -30 -30 -30 -30 -30 -30 -50))

(define (init-pst raw)
  (let ([ vec (make-fxvector mailbox-size 0) ])
    (for* ([ rank (in-range 8) ]
           [ file (in-range 8) ])
      (let ([val (vector-ref raw (fx+ (fx* rank 8) file)) ])
        (fxvector-set! vec (file-rank->idx file rank) val)))
    vec))

(define (flip src)
  (let ([ dst (make-vector (vector-length src) 0) ])
    (for* ([ rank (in-range 8) ]
           [ file (in-range 8) ])
      (let ([ val (vector-ref src (fx+ (fx* rank 8) file)) ])
        (vecset! dst (fx+ (fx* (fx- 7 rank) 8) file) val)))
    dst))

(define pawn-pst-white (init-pst pawn-raw))
(define pawn-pst-black (init-pst (flip pawn-raw)))

(define knight-pst-white (init-pst knight-raw))
(define knight-pst-black (init-pst (flip knight-raw)))

(define bishop-pst-white (init-pst bishop-raw))
(define bishop-pst-black (init-pst (flip bishop-raw)))

(define rook-pst-white (init-pst rook-raw))
(define rook-pst-black (init-pst (flip rook-raw)))

(define queen-pst-white (init-pst queen-raw))
(define queen-pst-black (init-pst (flip queen-raw)))

(define king-middle-pst-white (init-pst king-raw-middle))
(define king-middle-pst-black (init-pst (flip king-raw-middle)))

(define king-end-pst-white (init-pst king-raw-end))
(define king-end-pst-black (init-pst (flip king-raw-end)))

(module+ test
  (require rackunit)

  ;; White Pawn
  (check-equal? (fxvector-ref pawn-pst-white (pos->idx "e4")) 20)
  (check-equal? (fxvector-ref pawn-pst-white (pos->idx "e5")) 25)
  (check-equal? (fxvector-ref pawn-pst-white (pos->idx "b2")) 10)

  ;; Black Pawn
  (check-equal? (fxvector-ref pawn-pst-black (pos->idx "d5")) 20)
  (check-equal? (fxvector-ref pawn-pst-black (pos->idx "d4")) 25)
  (check-equal? (fxvector-ref pawn-pst-black (pos->idx "g7")) 10)

  )
