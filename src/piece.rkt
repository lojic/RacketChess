#lang racket

(require racket/performance-hint)

(provide black-bishop
         black-king
         black-knight
         black-pawn
         black-queen
         black-rook
         empty-square
         has-moved?
         is-bishop?
         is-black?
         is-black-king?
         is-king?
         is-knight?
         is-other-color?
         is-other-piece?
         is-own-piece?
         is-pawn?
         is-piece?
         is-queen?
         is-right-color-piece?
         is-rook?
         is-white?
         is-white-king?
         piece-moved-bit
         piece-moved-mask
         piece-symbol
         piece-type
         piece-value
         symbol-piece
         white-bishop
         white-king
         white-knight
         white-pawn
         white-queen
         white-rook)

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
;; Bit 3 Piece has moved
;; Bit 4 Not used
;; Bit 5 Not used
;; Bit 6 Black
;; Bit 7 White

(define black-bit             #b01000000)
(define color-bits            #b11000000)
(define guard-square          #b11111111) ; Note: both black and white bits are set purposely :)
(define piece-moved-bit       #b00001000)
(define piece-moved-mask      #b11110111)
(define piece-type-bits       #b00000111)
(define piece-type-color-bits #b11000111)
(define white-bit             #b10000000)

;; Piece types
(define pawn-bits   #b001)
(define knight-bits #b010)
(define bishop-bits #b011)
(define rook-bits   #b100)
(define queen-bits  #b101)
(define king-bits   #b110)

;;                     WB__M___
(define white-pawn   #b10000001) ; 0x81
(define white-knight #b10000010) ; 0x82
(define white-bishop #b10000011) ; 0x83
(define white-rook   #b10000100) ; 0x84
(define white-queen  #b10000101) ; 0x85
(define white-king   #b10000110) ; 0x86

(define black-pawn   #b01000001) ; 0x41
(define black-knight #b01000010) ; 0x42
(define black-bishop #b01000011) ; 0x43
(define black-rook   #b01000100) ; 0x44
(define black-queen  #b01000101) ; 0x45
(define black-king   #b01000110) ; 0x46

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

(define piece-symbols (make-immutable-hash (cons (cons empty-square " ") piece-symbol-alist)))

(define symbol-pieces
  (for/hash ([ pair (in-list piece-symbol-alist) ])
    (values (cdr pair) (car pair))))



(define-inline (has-moved? piece)
  (> (bitwise-and piece piece-moved-bit) #b0))

(define-inline (is-bishop? piece)
  (= (bitwise-and piece piece-type-bits) bishop-bits))

(define-inline (is-black? piece)
  (> (bitwise-and piece black-bit) #b0))

(define-inline (is-black-king? piece)
  (and (is-black? piece)
       (is-king? piece)))

(define-inline (is-king? piece)
  (= (bitwise-and piece piece-type-bits) king-bits))

(define-inline (is-knight? piece)
  (= (bitwise-and piece piece-type-bits) knight-bits))

(define-inline (is-other-color? p1 p2)
  (= (bitwise-ior (bitwise-and p1 color-bits)
                  (bitwise-and p2 color-bits))
     color-bits))

(define-inline (is-other-piece? mine other)
  (= (bitwise-and (bitwise-xor mine other)
                  color-bits)
     color-bits))

(define-inline (is-own-piece? mine other)
  (= (bitwise-and (bitwise-xor mine other)
                  color-bits)
     #b0))

(define-inline (is-pawn? piece)
  (= (bitwise-and piece piece-type-bits)
     pawn-bits))

(define-inline (is-piece? piece)
  (> (bitwise-and piece piece-type-bits) #b0))

(define-inline (is-queen? piece)
  (= (bitwise-and piece piece-type-bits) queen-bits))

(define-inline (is-right-color-piece? piece is-white?)
  (> (bitwise-and piece (if is-white? white-bit black-bit)) #b0))

(define-inline (is-rook? piece)
  (= (bitwise-and piece piece-type-bits) rook-bits))

(define-inline (is-white? piece)
  (> (bitwise-and piece white-bit) #b0))

(define-inline (is-white-king? piece)
  (and (is-white? piece)
       (is-king? piece)))

(define (piece-symbol piece)
  (hash-ref piece-symbols
            (bitwise-and piece piece-type-color-bits)))

(define-inline (piece-type piece)
  (bitwise-and piece piece-type-bits))

(define-inline (piece-value piece)
  (let ([ type (bitwise-and piece piece-type-bits) ])
    (if (is-white? piece)
        (cond [ (= type #b001)   1.0 ]
              [ (= type #b010)   3.0 ]
              [ (= type #b011)   3.0 ]
              [ (= type #b100)   5.0 ]
              [ (= type #b101)   9.0 ]
              [ (= type #b110) 100.0 ])
        (cond [ (= type #b001)   -1.0 ]
              [ (= type #b010)   -3.0 ]
              [ (= type #b011)   -3.0 ]
              [ (= type #b100)   -5.0 ]
              [ (= type #b101)   -9.0 ]
              [ (= type #b110) -100.0 ]))))

(define (symbol-piece sym)
  (hash-ref symbol-pieces sym))

(module+ test
  (require rackunit)

  ;; has-moved? -------------------------------------------------------------------------------

  (check-false (has-moved? black-king))
  (check-not-false (has-moved? (bitwise-ior black-king piece-moved-bit)))

  (check-false (has-moved? white-king))
  (check-not-false (has-moved? (bitwise-ior white-king piece-moved-bit)))

  ;; ------------------------------------------------------------------------------------------
  ;; is-<color>? predicates
  ;; ------------------------------------------------------------------------------------------

  (check-not-false (is-black? black-bishop))
  (check-not-false (is-black? black-king))
  (check-not-false (is-black? black-knight))
  (check-not-false (is-black? black-pawn))
  (check-not-false (is-black? black-queen))
  (check-not-false (is-black? black-rook))

  (check-not-false (is-white? white-bishop))
  (check-not-false (is-white? white-king))
  (check-not-false (is-white? white-knight))
  (check-not-false (is-white? white-pawn))
  (check-not-false (is-white? white-queen))
  (check-not-false (is-white? white-rook))

  ;; ------------------------------------------------------------------------------------------
  ;; is-<piece>? predicates
  ;; ------------------------------------------------------------------------------------------

  (check-not-false (is-bishop? black-bishop))
  (check-not-false (is-king?   black-king))
  (check-not-false (is-knight? black-knight))
  (check-not-false (is-pawn?   black-pawn))
  (check-not-false (is-queen?  black-queen))
  (check-not-false (is-rook?   black-rook))

  (check-not-false (is-bishop? white-bishop))
  (check-not-false (is-king?   white-king))
  (check-not-false (is-knight? white-knight))
  (check-not-false (is-pawn?   white-pawn))
  (check-not-false (is-queen?  white-queen))
  (check-not-false (is-rook?   white-rook))

  ;; ------------------------------------------------------------------------------------------
  ;; is-<color>-<piece>? predicates
  ;; ------------------------------------------------------------------------------------------

  (check-not-false (is-black-king? black-king))
  (check-not-false (is-white-king? white-king))

  ;; is-other-color?

  (check-not-false (is-other-color? black-pawn white-rook))
  (check-not-false (is-other-color? white-pawn black-rook))

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
  ;; piece-value
  ;; ------------------------------------------------------------------------------------------

  (check-equal? (piece-value black-pawn)   -1.0)
  (check-equal? (piece-value black-knight) -3.0)
  (check-equal? (piece-value black-bishop) -3.0)
  (check-equal? (piece-value black-rook)   -5.0)
  (check-equal? (piece-value black-queen)  -9.0)
  (check-not-false (<= (piece-value black-king) -100.0))

  (check-equal? (piece-value white-pawn)   1.0)
  (check-equal? (piece-value white-knight) 3.0)
  (check-equal? (piece-value white-bishop) 3.0)
  (check-equal? (piece-value white-rook)   5.0)
  (check-equal? (piece-value white-queen)  9.0)
  (check-not-false (>= (piece-value white-king) 100.0))

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
