#lang racket

(provide bishop-piece
         black-bishop
         black-king
         black-knight
         black-pawn
         black-queen
         black-rook
         empty-square
         is-bishop?
         is-black-king?
         is-black?
         is-king?
         is-knight?
         is-other-piece?
         is-own-piece?
         is-pawn?
         is-piece?
         is-queen?
         is-right-color-piece?
         is-rook?
         is-white-king?
         is-white?
         king-piece
         knight-piece
         pawn-piece
         piece-symbol
         piece-type
         queen-piece
         rook-piece
         symbol-piece
         white-bishop
         white-king
         white-knight
         white-pawn
         white-queen
         white-rook)

(require "./global.rkt"
         racket/performance-hint)

;; --------------------------------------------------------------------------------------------
;; Chess Piece
;;
;; Bits 0-2 Type
;;   000 Empty square
;;   001 Pawn
;;   010 Knight
;;   011 Bishop
;;   100 Rook
;;   101 Queen
;;   110 King
;;   111 Not used
;; Bit 3 Black
;; Bit 4 White
;; Bit 5 Not used
;; Bit 6 Not used
;; Bit 7 Not used

(define piece-type-bits       #b00000111)
(define black-bit             #b00001000)
(define white-bit             #b00010000)
(define color-bits            #b00011000)
(define piece-type-color-bits (fxior piece-type-bits color-bits))

;; --------------------------------------------------------------------------------------------

;; Piece types
(define pawn-piece   #b001)
(define knight-piece #b010)
(define bishop-piece #b011)
(define rook-piece   #b100)
(define queen-piece  #b101)
(define king-piece   #b110)

;; White pieces
(define white-pawn   #b00010001)
(define white-knight #b00010010)
(define white-bishop #b00010011)
(define white-rook   #b00010100)
(define white-queen  #b00010101)
(define white-king   #b00010110)

;; Black pieces
(define black-pawn   #b00001001)
(define black-knight #b00001010)
(define black-bishop #b00001011)
(define black-rook   #b00001100)
(define black-queen  #b00001101)
(define black-king   #b00001110)

(define empty-square #b00000000)

(define piece-symbol-alist (list (cons white-pawn    "P")
                                 (cons white-knight  "N")
                                 (cons white-bishop  "B")
                                 (cons white-rook    "R")
                                 (cons white-queen   "Q")
                                 (cons white-king    "K")
                                 (cons black-pawn    "p")
                                 (cons black-knight  "n")
                                 (cons black-bishop  "b")
                                 (cons black-rook    "r")
                                 (cons black-queen   "q")
                                 (cons black-king    "k")))

;; hash with key = piece & value = symbol
(define piece-symbols
  (for/hash ([ pair (in-list (cons (cons empty-square " ")
                                   piece-symbol-alist)) ])
    (values (car pair) (cdr pair))))

;; hash with key = symbol & value = piece
(define symbol-pieces
  (for/hash ([ pair (in-list piece-symbol-alist) ])
    (values (cdr pair) (car pair))))

;; --------------------------------------------------------------------------------------------
;; Predicates
;; --------------------------------------------------------------------------------------------

(define-inline (is-bishop? piece)
  (fx= (fxand piece piece-type-bits) bishop-piece))

(define-inline (is-black? piece)
  (fx> (fxand piece black-bit) #b0))

(define-inline (is-black-king? piece)
  (and (is-black? piece)
       (is-king? piece)))

(define-inline (is-king? piece)
  (fx= (fxand piece piece-type-bits) king-piece))

(define-inline (is-knight? piece)
  (fx= (fxand piece piece-type-bits) knight-piece))

;; A major piece is any piece other than a pawn
(define-inline (is-major? piece)
  (fx> (fxand piece piece-type-bits)
       pawn-piece))

;; Is other a different color than mine?
(define-inline (is-other-piece? mine other)
  (fx= (fxand (fxxor mine other)
              color-bits)
       color-bits))

;; Is other the same color as mine?
(define-inline (is-own-piece? mine other)
  (fx= (fxand (fxxor mine other)
              color-bits)
       #b0))

(define-inline (is-pawn? piece)
  (fx= (fxand piece piece-type-bits)
       pawn-piece))

;; Is this a piece vs. an empty square?
(define-inline (is-piece? piece)
  (fx> (fxand piece piece-type-bits) #b0))

(define-inline (is-queen? piece)
  (fx= (fxand piece piece-type-bits) queen-piece))

;; Indicate whether the piece is either:
;; white if is-white? is true, or
;; black if is-white? is false
(define-inline (is-right-color-piece? piece is-white?)
  (fx> (fxand piece (if is-white? white-bit black-bit)) #b0))

(define-inline (is-rook? piece)
  (fx= (fxand piece piece-type-bits) rook-piece))

(define-inline (is-white? piece)
  (fx> (fxand piece white-bit) #b0))

(define-inline (is-white-king? piece)
  (and (is-white? piece)
       (is-king? piece)))

;; --------------------------------------------------------------------------------------------
;; Other functions
;; --------------------------------------------------------------------------------------------

(define (piece-symbol piece)
  (hash-ref piece-symbols
            (fxand piece piece-type-color-bits)))

(define-inline (piece-type piece)
  (fxand piece piece-type-bits))

(define (symbol-piece sym)
  (hash-ref symbol-pieces sym))

;; --------------------------------------------------------------------------------------------
;; Tests
;; --------------------------------------------------------------------------------------------

(module+ test
  (require rackunit)

  ;; ------------------------------------------------------------------------------------------
  ;; is-<color>? predicates
  ;; ------------------------------------------------------------------------------------------

  (check-not-false (is-black? black-bishop))
  (check-not-false (is-black? black-king))
  (check-not-false (is-black? black-knight))
  (check-not-false (is-black? black-pawn))
  (check-not-false (is-black? black-queen))
  (check-not-false (is-black? black-rook))

  (check-false (is-black? white-bishop))
  (check-false (is-black? white-king))
  (check-false (is-black? white-knight))
  (check-false (is-black? white-pawn))
  (check-false (is-black? white-queen))
  (check-false (is-black? white-rook))

  (check-not-false (is-white? white-bishop))
  (check-not-false (is-white? white-king))
  (check-not-false (is-white? white-knight))
  (check-not-false (is-white? white-pawn))
  (check-not-false (is-white? white-queen))
  (check-not-false (is-white? white-rook))

  (check-false (is-white? black-bishop))
  (check-false (is-white? black-king))
  (check-false (is-white? black-knight))
  (check-false (is-white? black-pawn))
  (check-false (is-white? black-queen))
  (check-false (is-white? black-rook))

  ;; is-major? --------------------------------------------------------------------------------

  (check-not-false (is-major? black-bishop))
  (check-not-false (is-major? black-king))
  (check-not-false (is-major? black-knight))
  (check-not-false (is-major? black-queen))
  (check-not-false (is-major? black-rook))

  (check-not-false (is-major? white-bishop))
  (check-not-false (is-major? white-king))
  (check-not-false (is-major? white-knight))
  (check-not-false (is-major? white-queen))
  (check-not-false (is-major? white-rook))

  (check-false (is-major? black-pawn))
  (check-false (is-major? white-pawn))
  (check-false (is-major? empty-square))

  ;; ------------------------------------------------------------------------------------------
  ;; is-<piece>? predicates
  ;; ------------------------------------------------------------------------------------------

  (check-not-false (is-bishop? black-bishop))
  (check-not-false (is-bishop? white-bishop))
  (check-false     (is-bishop? black-knight))

  (check-not-false (is-king? black-king))
  (check-not-false (is-king? white-king))
  (check-false     (is-king? white-queen))

  (check-not-false (is-knight? black-knight))
  (check-not-false (is-knight? white-knight))
  (check-false     (is-knight? black-rook))

  (check-not-false (is-pawn? black-pawn))
  (check-not-false (is-pawn? white-pawn))
  (check-false     (is-pawn? white-bishop))

  (check-not-false (is-queen? black-queen))
  (check-not-false (is-queen? white-queen))
  (check-false     (is-queen? black-pawn))

  (check-not-false (is-rook? black-rook))
  (check-not-false (is-rook? white-rook))
  (check-false     (is-rook? white-king))

  ;; ------------------------------------------------------------------------------------------
  ;; is-<color>-<piece>? predicates
  ;; ------------------------------------------------------------------------------------------

  (check-not-false (is-black-king? black-king))
  (check-not-false (is-white-king? white-king))

  (check-false (is-black-king? black-bishop))
  (check-false (is-white-king? white-queen))

  ;; is-other-piece?

  (check-false     (is-other-piece? black-rook black-king))
  (check-not-false (is-other-piece? black-rook white-king))
  (check-false     (is-other-piece? white-pawn white-knight))
  (check-not-false (is-other-piece? white-pawn black-rook))

  ;; is-own-piece?

  (check-not-false (is-own-piece? black-rook black-king))
  (check-false     (is-own-piece? black-rook white-king))
  (check-not-false (is-own-piece? white-pawn white-knight))
  (check-false     (is-own-piece? white-pawn black-rook))

  ;; ------------------------------------------------------------------------------------------
  ;; is-piece?
  ;; ------------------------------------------------------------------------------------------

  (check-not-false (is-piece? black-bishop))
  (check-not-false (is-piece? black-king))
  (check-not-false (is-piece? black-knight))
  (check-not-false (is-piece? black-pawn))
  (check-not-false (is-piece? black-queen))
  (check-not-false (is-piece? black-rook))

  (check-not-false (is-piece? white-bishop))
  (check-not-false (is-piece? white-king))
  (check-not-false (is-piece? white-knight))
  (check-not-false (is-piece? white-pawn))
  (check-not-false (is-piece? white-queen))
  (check-not-false (is-piece? white-rook))

  (check-false (is-piece? empty-square))

  ;; is-right-color-piece?

  (check-not-false (is-right-color-piece? black-bishop #f))
  (check-not-false (is-right-color-piece? white-bishop #t))

  ;; ------------------------------------------------------------------------------------------
  ;; piece-symbol
  ;; ------------------------------------------------------------------------------------------

  (check-equal? (piece-symbol black-bishop) "b")
  (check-equal? (piece-symbol black-king)   "k")
  (check-equal? (piece-symbol black-knight) "n")
  (check-equal? (piece-symbol black-pawn)   "p")
  (check-equal? (piece-symbol black-queen)  "q")
  (check-equal? (piece-symbol black-rook)   "r")

  (check-equal? (piece-symbol white-bishop) "B")
  (check-equal? (piece-symbol white-king)   "K")
  (check-equal? (piece-symbol white-knight) "N")
  (check-equal? (piece-symbol white-pawn)   "P")
  (check-equal? (piece-symbol white-queen)  "Q")
  (check-equal? (piece-symbol white-rook)   "R")

  ;; ------------------------------------------------------------------------------------------
  ;; symbol-piece
  ;; ------------------------------------------------------------------------------------------

  (check-equal? (symbol-piece "b") black-bishop)
  (check-equal? (symbol-piece "k") black-king)
  (check-equal? (symbol-piece "n") black-knight)
  (check-equal? (symbol-piece "p") black-pawn)
  (check-equal? (symbol-piece "q") black-queen)
  (check-equal? (symbol-piece "r") black-rook)

  (check-equal? (symbol-piece "B") white-bishop)
  (check-equal? (symbol-piece "K") white-king)
  (check-equal? (symbol-piece "N") white-knight)
  (check-equal? (symbol-piece "P") white-pawn)
  (check-equal? (symbol-piece "Q") white-queen)
  (check-equal? (symbol-piece "R") white-rook)

  )
