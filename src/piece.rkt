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
         is-black?
         is-black-king?
         is-king?
         is-other-color?
         is-other-piece?
         is-own-piece?
         is-pawn?
         is-piece?
         is-right-color-piece?
         is-rook?
         is-white?
         is-white-king?
         king-castled-bit
         piece-moved-bit
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
;; Bit 4 King has castled
;; Bit 5 Not used
;; Bit 6 Black
;; Bit 7 White

(define black-bit             #b01000000)
(define color-bits            #b11000000)
(define guard-square          #b11111111) ; Note: both black and white bits are set purposely :)
(define king-castled-bit      #b00010000)
(define piece-moved-bit       #b00001000)
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

;;                     WB_CM___
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

(define-inline (is-black? piece)
  (> (bitwise-and piece black-bit) #b0))

(define-inline (is-black-king? piece)
  (and (is-black? piece)
       (is-king? piece)))

(define-inline (is-king? piece)
  (= (bitwise-and piece piece-type-bits) king-bits))

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

(define (piece-value piece)
  (let ([ val (match (bitwise-and piece piece-type-bits)
                [ #b001 1.0 ]
                [ #b010 3.0 ]
                [ #b011 3.0 ]
                [ #b100 5.0 ]
                [ #b101 9.0 ]
                [ #b110 100.0 ]) ])
    (if (is-white? piece)
        val
        (- val))))

(define (symbol-piece sym)
  (hash-ref symbol-pieces sym))

(module+ test
  (require rackunit)

  ;; ------------------------------------------------------------------------------------------
  ;; piece-value
  ;; ------------------------------------------------------------------------------------------
  (check-equal? (piece-value white-pawn) 1.0)
  (check-equal? (piece-value black-pawn) -1.0)
  (check-equal? (piece-value white-queen) 9.0)
  (check-equal? (piece-value black-queen) -9.0)

  ;; ------------------------------------------------------------------------------------------
  ;; piece-symbol
  ;; ------------------------------------------------------------------------------------------
  (check-equal? (piece-symbol white-king) "K")
  (check-equal? (piece-symbol black-rook) "r")
  (check-equal? (piece-symbol white-knight) "N")

  ;; ------------------------------------------------------------------------------------------
  ;; Bitwise operations
  ;; ------------------------------------------------------------------------------------------
  (check-false (has-moved? white-pawn))
  (check-not-false (is-king? white-king))
  (check-not-false (is-king? black-king))
  (check-not-false (is-other-color? black-pawn white-rook))
  (check-not-false (is-other-color? white-pawn black-rook))

  (check-not-false (is-own-piece? white-pawn white-knight))
  (check-false     (is-own-piece? white-pawn black-rook))
  (check-not-false (is-own-piece? black-rook black-king))
  (check-false     (is-own-piece? black-rook white-king))

  (check-false     (is-other-piece? white-pawn white-knight))
  (check-not-false (is-other-piece? white-pawn black-rook))
  (check-false     (is-other-piece? black-rook black-king))
  (check-not-false (is-other-piece? black-rook white-king))

  (check-not-false (is-pawn? white-pawn))
  (check-not-false (is-pawn? black-pawn))

  (check-not-false (is-piece? white-king))

  (check-not-false (is-right-color-piece? white-bishop #t))
  (check-not-false (is-right-color-piece? black-bishop #f))

  (check-not-false (is-white? white-knight))

  )
