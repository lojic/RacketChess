#lang racket

(provide blank-board
         east
         file-rank->idx
         get-square
         idx->pos
         king-offsets
         knight-offsets
         north
         north-east
         north-west
         pos->idx
         print-board
         set-square!
         south
         south-east
         south-west
         west)

(require "./global.rkt"
         "./piece.rkt"
         racket/performance-hint)

;; 10x12 Mailbox Board Representation
;; Index of upper left is 0. Index of lower right is 119
;;
;; FF  FF  FF  FF  FF  FF  FF  FF  FF FF
;; FF  FF  FF  FF  FF  FF  FF  FF  FF FF
;; FF  21  22  23  24  25  26  27  28 FF 8
;; FF  31  32  33  34  35  36  37  38 FF 7
;; FF  41  42  43  44  45  46  47  48 FF 6
;; FF  51  52  53  54  55  56  57  58 FF 5
;; FF  61  62  63  64  65  66  67  68 FF 4
;; FF  71  72  73  74  75  76  77  78 FF 3
;; FF  81  82  83  84  85  86  87  88 FF 2
;; FF  91  92  93  94  95  96  97  98 FF 1
;; FF  FF  FF  FF  FF  FF  FF  FF  FF FF
;; FF  FF  FF  FF  FF  FF  FF  FF  FF FF
;;      a   b   c   d   e   f   g   h

(define empty-board
  (bytes->immutable-bytes
   (bytes
    #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF
    #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF
    #xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF
    #xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF
    #xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF
    #xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF
    #xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF
    #xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF
    #xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF
    #xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF
    #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF
    #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF)))

(define positions
  (vector-immutable
   "  " "  " "  " "  " "  " "  " "  " "  " "  " "  "   ;   0 -   9
   "  " "  " "  " "  " "  " "  " "  " "  " "  " "  "   ;  10 -  19
   "  " "a8" "b8" "c8" "d8" "e8" "f8" "g8" "h8" "  "   ;  20 -  29
   "  " "a7" "b7" "c7" "d7" "e7" "f7" "g7" "h7" "  "   ;  30 -  39
   "  " "a6" "b6" "c6" "d6" "e6" "f6" "g6" "h6" "  "   ;  40 -  49
   "  " "a5" "b5" "c5" "d5" "e5" "f5" "g5" "h5" "  "   ;  50 -  59
   "  " "a4" "b4" "c4" "d4" "e4" "f4" "g4" "h4" "  "   ;  60 -  69
   "  " "a3" "b3" "c3" "d3" "e3" "f3" "g3" "h3" "  "   ;  70 -  79
   "  " "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2" "  "   ;  80 -  89
   "  " "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1" "  "   ;  90 -  99
   "  " "  " "  " "  " "  " "  " "  " "  " "  " "  "   ; 100 - 109
   "  " "  " "  " "  " "  " "  " "  " "  " "  " "  ")) ; 110 - 119

;; --------------------------------------------------------------------------------------------
;; Directions & offsets
;; --------------------------------------------------------------------------------------------

(define north      -10)
(define north-east  -9)
(define east         1)
(define south-east  11)
(define south       10)
(define south-west   9)
(define west        -1)
(define north-west -11)

(define king-offsets (list north north-east east south-east south south-west west north-west))

(define knight-offsets
  (list (+ north north east)
        (+ east east north)
        (+ east east south)
        (+ south south east)
        (+ south south west)
        (+ west west south)
        (+ west west north)
        (+ north north west)))

;; --------------------------------------------------------------------------------------------

(define (blank-board)
  (bytes-copy empty-board))

;; NOTE: in this case rank 0 is the top row of the board!
(define-inline (file-rank->idx file rank)
  (+ 21 file (* rank 10)))

(define-inline (get-square b idx)
  (get-byte b idx))

(define (idx->pos idx)
  (vector-ref positions idx))

(define (pos->idx pos)
  (vector-member pos positions))

(define (print-board b [ out (current-output-port) ])
  (define (file->letter file)
    (match file
      [ 0 "a" ]
      [ 1 "b" ]
      [ 2 "c" ]
      [ 3 "d" ]
      [ 4 "e" ]
      [ 5 "f" ]
      [ 6 "g" ]
      [ 7 "h" ]))

  (for ([ rank (in-range 8) ])
    (fprintf out "---------------------------------\n|")
    (for ([ file (in-range 8) ])
      (let* ([ idx   (file-rank->idx  file rank) ]
             [ piece (get-square b idx)      ]
             [ sym   (piece-symbol piece)    ])
        (fprintf out " ~a |" sym)))
    (fprintf out "  ~a\n" (- 8 rank)))
  (fprintf out "---------------------------------\n|")
  (for ([ file (in-range 8) ])
    (fprintf out " ~a |" (file->letter file)))
  (fprintf out "\n"))

(define-inline (set-square! b idx piece)
  (set-byte! b idx piece))

;; --------------------------------------------------------------------------------------------
;; Tests
;; --------------------------------------------------------------------------------------------

(module+ test
  (require rackunit)

  ;; ------------------------------------------------------------------------------------------
  ;; blank-board
  ;; ------------------------------------------------------------------------------------------

  (let ([ b (blank-board) ])
    (check-equal? (bytes-length b) (* 10 12))) ; Length of 10 x 12 mailbox representation

  ;; ------------------------------------------------------------------------------------------
  ;; directions & offsets
  ;; ------------------------------------------------------------------------------------------

  (let ([ initial (pos->idx "d4") ])
    (for ([ offset (in-list king-offsets)                                   ]
          [ pos    (in-list (list "d5" "e5" "e4" "e3" "d3" "c3" "c4" "c5")) ])
      (check-equal? (idx->pos (+ initial offset)) pos))

    (for ([ offset (in-list knight-offsets)                                 ]
          [ pos    (in-list (list "e6" "f5" "f3" "e2" "c2" "b3" "b5" "c6")) ])
      (check-equal? (idx->pos (+ initial offset)) pos)))

  ;; ------------------------------------------------------------------------------------------
  ;; file-rank->idx
  ;; ------------------------------------------------------------------------------------------

  (check-equal? (file-rank->idx 0 0) 21)
  (check-equal? (file-rank->idx 7 0) 28)
  (check-equal? (file-rank->idx 0 7) 91)
  (check-equal? (file-rank->idx 7 7) 98)

  ;; ------------------------------------------------------------------------------------------
  ;; get-square
  ;; ------------------------------------------------------------------------------------------

  (let ([ b (blank-board) ])
    (check-equal? (get-square b 0) #xFF)   ; First
    (check-equal? (get-square b 119) #xFF) ; Last
    (check-equal? (get-square b (file-rank->idx 0 7)) #x00)
    (check-equal? (get-square b (file-rank->idx 7 0)) #x00)

    (let ([ idx (file-rank->idx 0 7) ])
      (set-square! b idx black-bishop)
      (check-equal? (get-square b idx) black-bishop))

    )

  ;; ------------------------------------------------------------------------------------------
  ;; pos->idx / idx->pos
  ;; ------------------------------------------------------------------------------------------

  (for ([ pair (in-list '(("a8" 21)      ; Top left
                          ("b7" 32)      ; ... down + right
                          ("h8" 28)      ; Top right
                          ("g7" 37)      ; ... down + left
                          ("a1" 91)      ; Bottom left
                          ("b2" 82)      ; ... up + right
                          ("h1" 98)      ; Bottom right
                          ("g2" 87)      ; ... up + left
                          ("d4" 64))) ]) ; Middle
    (check-equal? (pos->idx (first pair)) (second pair))
    (check-equal? (idx->pos (second pair)) (first pair)))

  ;; ------------------------------------------------------------------------------------------
  ;; print-board
  ;; ------------------------------------------------------------------------------------------

  (let ([ b   (blank-board)        ]
        [ out (open-output-string) ])
    (set-square! b (pos->idx "a8") black-rook)
    (set-square! b (pos->idx "b8") black-knight)
    (set-square! b (pos->idx "d8") black-queen)
    (set-square! b (pos->idx "f8") black-bishop)
    (set-square! b (pos->idx "e1") white-king)
    (set-square! b (pos->idx "d1") white-queen)
    (set-square! b (pos->idx "c5") white-rook)
    (set-square! b (pos->idx "h2") black-pawn)
    (print-board b out)
    (check-equal? (get-output-string out) #<<EOS
---------------------------------
| r | n |   | q |   | b |   |   |  8
---------------------------------
|   |   |   |   |   |   |   |   |  7
---------------------------------
|   |   |   |   |   |   |   |   |  6
---------------------------------
|   |   | R |   |   |   |   |   |  5
---------------------------------
|   |   |   |   |   |   |   |   |  4
---------------------------------
|   |   |   |   |   |   |   |   |  3
---------------------------------
|   |   |   |   |   |   |   | p |  2
---------------------------------
|   |   |   | Q | K |   |   |   |  1
---------------------------------
| a | b | c | d | e | f | g | h |

EOS
                  ))

  )
