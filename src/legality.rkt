#lang racket

(require "./board.rkt"
         "./global.rkt"
         "./move.rkt"
         "./piece.rkt")

(require racket/performance-hint)

(provide is-king-in-check?
         is-legal?)

(define east2 (+ east east))
(define west2 (+ west west))

(define (is-king-in-check? b)
  (define squares (board-squares b))
  (let-values ([ (white? king-idx other-king-idx own? enemy?)
                 (if (is-whites-move? b)
                     (values #t
                             (white-king-idx b)
                             (black-king-idx b)
                             is-white?
                             is-black?)
                     (values #f
                             (black-king-idx b)
                             (white-king-idx b)
                             is-black?
                             is-white?)
                     ) ])
    (is-square-attacked? squares king-idx other-king-idx own? enemy? white?)))

(define (is-legal? b m)
  (define squares (board-squares b))
  ;; Note, the move has already been made, so the board will show it's
  ;; the opposite color's move, than the color we're checking :)
  (let-values ([ (white? king-idx other-king-idx own? enemy?)
                 (if (is-whites-move? b)
                     (values #f
                             (black-king-idx b)
                             (white-king-idx b)
                             is-black?
                             is-white?)
                     (values #t
                             (white-king-idx b)
                             (black-king-idx b)
                             is-white?
                             is-black?)) ])
    (not
     (or (is-square-attacked? squares king-idx other-king-idx own? enemy? white?)
         (is-castle-square-attacked? m squares king-idx other-king-idx own? enemy? white?)))))

(define (is-square-attacked? squares king-idx other-king-idx own? enemy? white?)
  (or (knight-attacking? squares king-idx enemy?)
      (adjacent-attacking? squares king-idx other-king-idx enemy? white?)
      (ray-attacking? squares king-idx own? enemy? north      queen-rook?)
      (ray-attacking? squares king-idx own? enemy? north-east queen-bishop?)
      (ray-attacking? squares king-idx own? enemy? east       queen-rook?)
      (ray-attacking? squares king-idx own? enemy? south-east queen-bishop?)
      (ray-attacking? squares king-idx own? enemy? south      queen-rook?)
      (ray-attacking? squares king-idx own? enemy? south-west queen-bishop?)
      (ray-attacking? squares king-idx own? enemy? west       queen-rook?)
      (ray-attacking? squares king-idx own? enemy? north-west queen-bishop?)))

(define (is-castle-square-attacked? m squares king-idx other-king-idx own? enemy? white?)
  (cond [ (move-is-castle-kingside? m)
          (or
           (is-square-attacked? squares (+ king-idx west) other-king-idx own? enemy? white?)
           (is-square-attacked? squares (+ king-idx west2) other-king-idx own? enemy? white?)) ]
        [ (move-is-castle-queenside? m)
          (or
           (is-square-attacked? squares (+ king-idx east) other-king-idx own? enemy? white?)
           (is-square-attacked? squares (+ king-idx east2) other-king-idx own? enemy? white?)) ]
        [ else #f ]))

(define-inline (king-queen-bishop? piece)
  (or (is-king? piece)
      (is-queen? piece)
      (is-bishop? piece)))

(define-inline (king-queen-rook? piece)
  (or (is-king? piece)
      (is-queen? piece)
      (is-rook? piece)))

(define-inline (queen-bishop? piece)
  (or (is-queen? piece)
      (is-bishop? piece)))

(define-inline (queen-rook? piece)
  (or (is-queen? piece)
      (is-rook? piece)))

;; Check for attacks by king or pawn
(define (adjacent-attacking? squares king-idx other-king-idx enemy? white?)
  (or
   ;; King attacks
   (or (fx= king-idx (fx+ other-king-idx north))
       (fx= king-idx (fx+ other-king-idx north-east))
       (fx= king-idx (fx+ other-king-idx east))
       (fx= king-idx (fx+ other-king-idx south-east))
       (fx= king-idx (fx+ other-king-idx south))
       (fx= king-idx (fx+ other-king-idx south-west))
       (fx= king-idx (fx+ other-king-idx west))
       (fx= king-idx (fx+ other-king-idx north-west)))

   ;; Pawn attacks from the east
   (let ([ src (get-square squares
                          (fx+ king-idx
                             (if white? north-east south-east))) ])
     (and (is-pawn? src)
          (enemy? src)))

   ;; Pawn attacks from the west
   (let ([ src (get-square squares
                          (fx+ king-idx
                             (if white? north-west south-west))) ])
     (and (is-pawn? src)
          (enemy? src)))))

(define (knight-attacking? squares king-idx enemy?)
  (let loop ([ offsets knight-offsets ])
    (if (null? offsets)
        #f
        (let ([ src (get-square squares (fx+ king-idx (car offsets))) ])
          (if (and (is-knight? src) (enemy? src))
              #t
              (loop (cdr offsets)))))))

;; Guard squares off the board are both black & white, so they will be
;; #t for own?
(define (ray-attacking? squares king-idx own? enemy? dir is-attacker?)
  (let loop ([ idx (fx+ king-idx dir) ])
    (let ([ src (get-square squares idx) ])
      (cond [ (own? src)             #f                     ]
            [ (fx= src empty-square) (loop (fx+ idx dir)) ]
            [ else                   (is-attacker? src)     ]))))
