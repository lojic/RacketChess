#lang racket

(require "./board.rkt"
         "./piece-square-tables.rkt")
(require racket/fixnum
         racket/performance-hint)

(provide black-bishop black-king black-knight black-pawn black-queen
         black-rook empty-square is-bishop? is-black? is-black-king? is-king?
         is-knight? is-other-color? is-other-piece? is-own-piece? is-pawn?
         is-piece? is-queen? is-right-color-piece? is-rook? is-white?
         is-white-king? piece-symbol piece-type piece-value piece-zobrist
         symbol-piece white-bishop white-king white-knight white-pawn
         white-queen white-rook)

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
;; Bit 3 Not used
;; Bit 4 Not used
;; Bit 5 Not used
;; Bit 6 Black
;; Bit 7 White

(define black-bit             #b01000000)
(define color-bits            #b11000000)
(define guard-square          #b11111111) ; Note: both black and white bits are set purposely :)
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

(define-inline (is-major? piece)
  (> (bitwise-and piece piece-type-bits)
     1))

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

;; https://www.chessprogramming.org/Simplified_Evaluation_Function
;; 1. Avoid exchanging one minor piece for three pawns.
;; 2. Encourage the engine to have the bishop pair.
;; 3. Avoid exchanging of two minor pieces for a rook and a pawn.
;; 4. Stick to human chess experience.
;;
;;    B > N > 3P
;;    B + N = R + 1.5P
;;    Q + P = 2R
(define (piece-value b piece idx)
  (let ([ type (bitwise-and piece piece-type-bits) ])
    (if (is-white? piece)
        (cond [ (= type #b001)
                (+ 100 (vector-ref pawn-pst-white idx)) ]
              [ (= type #b010)
                (+ 320 (vector-ref knight-pst-white idx)) ]
              [ (= type #b011)
                (+ 330 (vector-ref bishop-pst-white idx)) ]
              [ (= type #b100)
                (+ 500 (vector-ref rook-pst-white idx)) ]
              [ (= type #b101)
                (+ 900 (vector-ref queen-pst-white idx)) ]
              [ else
                (if (is-end-game? b)
                    (vector-ref king-end-pst-white idx)
                    (vector-ref king-middle-pst-white idx)) ])
        (- (cond [ (= type #b001)
                   (+ 100 (vector-ref pawn-pst-black idx)) ]
                 [ (= type #b010)
                   (+ 320 (vector-ref knight-pst-black idx)) ]
                 [ (= type #b011)
                   (+ 330 (vector-ref bishop-pst-black idx)) ]
                 [ (= type #b100)
                   (+ 500 (vector-ref rook-pst-black idx)) ]
                 [ (= type #b101)
                   (+ 900 (vector-ref queen-pst-black idx)) ]
                 [ else
                   (if (is-end-game? b)
                       (vector-ref king-end-pst-black idx)
                       (vector-ref king-middle-pst-black idx)) ])))))

;; Convert piece to a number suitable for use in Zobrist hashing.
;; 0001 Black Pawn
;; 0010 Black Knight
;; 0011 Black Bishop
;; 0100 Black Rook
;; 0101 Black Queen
;; 0110 Black King
;; 1001 White Pawn
;; 1010 White Knight
;; 1011 White Bishop
;; 1100 White Rook
;; 1101 White Queen
;; 1110 White King
;; Maximum value is 13 since we'll sub1 the result to get a zero based index
(define-inline (piece-zobrist piece)
  (fx- (fxior (fxrshift (fxand piece
                               white-bit)
                        4)
              (fxand piece
                     piece-type-bits))
       1))

(define (symbol-piece sym)
  (hash-ref symbol-pieces sym))

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

  (check-not-false (is-white? white-bishop))
  (check-not-false (is-white? white-king))
  (check-not-false (is-white? white-knight))
  (check-not-false (is-white? white-pawn))
  (check-not-false (is-white? white-queen))
  (check-not-false (is-white? white-rook))

  ;; is-major? --------------------------------------------------------------------------------

  (check-not-false (is-major? black-bishop))
  (check-not-false (is-major?   black-king))
  (check-not-false (is-major? black-knight))
  (check-not-false (is-major?  black-queen))
  (check-not-false (is-major?   black-rook))

  (check-not-false (is-major? white-bishop))
  (check-not-false (is-major?   white-king))
  (check-not-false (is-major? white-knight))
  (check-not-false (is-major?  white-queen))
  (check-not-false (is-major?   white-rook))

  (check-false (is-major? black-pawn))
  (check-false (is-major? white-pawn))
  (check-false (is-major? empty-square))

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
  ;; piece-value - dependent on the current values in piece-square-tables.rkt
  ;; ------------------------------------------------------------------------------------------

  (let ([ b (create-board) ])
    (check-equal? (piece-value b black-pawn   (pos->idx "e7"))  -80)
    (check-equal? (piece-value b black-knight (pos->idx "g8")) -280)
    (check-equal? (piece-value b black-bishop (pos->idx "f8")) -320)
    (check-equal? (piece-value b black-rook   (pos->idx "h8")) -500)
    (check-equal? (piece-value b black-queen  (pos->idx "d8")) -895)
    (check-equal? (piece-value b black-king   (pos->idx "e8"))    0)

    (check-equal? (piece-value b white-pawn   (pos->idx "e2"))  80)
    (check-equal? (piece-value b white-knight (pos->idx "g1")) 280)
    (check-equal? (piece-value b white-bishop (pos->idx "f1")) 320)
    (check-equal? (piece-value b white-rook   (pos->idx "h1")) 500)
    (check-equal? (piece-value b white-queen  (pos->idx "d1")) 895)
    (check-equal? (piece-value b white-king   (pos->idx "e1"))   0))

  ;; ------------------------------------------------------------------------------------------
  ;; piece-zobrist
  ;; ------------------------------------------------------------------------------------------

  (check-equal? (piece-zobrist black-pawn)   (sub1 #b0001))
  (check-equal? (piece-zobrist black-knight) (sub1 #b0010))
  (check-equal? (piece-zobrist black-bishop) (sub1 #b0011))
  (check-equal? (piece-zobrist black-rook)   (sub1 #b0100))
  (check-equal? (piece-zobrist black-queen)  (sub1 #b0101))
  (check-equal? (piece-zobrist black-king)   (sub1 #b0110))
  (check-equal? (piece-zobrist white-pawn)   (sub1 #b1001))
  (check-equal? (piece-zobrist white-knight) (sub1 #b1010))
  (check-equal? (piece-zobrist white-bishop) (sub1 #b1011))
  (check-equal? (piece-zobrist white-rook)   (sub1 #b1100))
  (check-equal? (piece-zobrist white-queen)  (sub1 #b1101))
  (check-equal? (piece-zobrist white-king)   (sub1 #b1110))

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
