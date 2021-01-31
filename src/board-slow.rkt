#lang racket

;; Functions that are not speed sensitive

(require "./board.rkt")
(require "./piece.rkt")

(provide get-pieces-file-rank
         set-piece-has-moved!)

;; Return a list of all pieces on the board in the following form:
;; ( (piece file rank)
;;   (piece file rank)
;;   ... )
;;
;; Where file & rank are characters e.g. #\c & #\4
(define (get-pieces-file-rank b [ pred? is-piece? ])
  (define squares (board-squares b))

  (for*/fold ([ result '() ])
             ([ rank (in-range 8) ]
              [ file (in-range 8) ])
    (let* ([ idx   (file-rank->idx file rank)        ]
           [ pos   (idx->pos idx)                    ]
           [ piece (bytes-ref (board-squares b) idx) ])
      (if (pred? piece)
          (cons (list piece (string-ref pos 0) (string-ref pos 1)) result)
          result))))

(define (set-piece-has-moved! b idx)
  (let ([ squares (board-squares b) ])
    (bytes-set! squares idx (bitwise-ior (bytes-ref squares idx)
                                         piece-moved-bit))))
