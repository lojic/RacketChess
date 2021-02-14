#lang racket

(require "./board.rkt"
         "./piece.rkt"
         "./state.rkt")

(require math/base
         racket/performance-hint)

(require racket/require
         (only-in racket/fixnum make-fxvector)
         ; racket/fixnum
         (filtered-in
          (λ (name)
            (and (regexp-match #rx"^unsafe-fx" name)
                 (regexp-replace #rx"unsafe-" name "")))
          racket/unsafe/ops))

(provide generate-zobrist-key
         square-keys
         xor-castling
         xor-ep-square
         xor-piece
         xor-whites-move)

(define square-keys (make-fxvector (* 10 12 16) 0)) ; 10 columns x 12 rows x 16 piece numbers
(define side-key    #f)
(define castle-keys (make-fxvector 16 0))
(define ep-keys     (make-fxvector (* 10 12) 0))

;; TODO change back to 60 ???
(define-inline (get-random-number)
  (random-bits 60))

(define-inline (xor-castling key s)
  (fxxor key (fxvector-ref castle-keys
                           (castling-zobrist s))))

(define-inline (xor-ep-square key ep-idx)
  (fxxor key (fxvector-ref ep-keys
                           ep-idx)))

(define-inline (xor-piece key piece idx)
  (fxxor key (fxvector-ref square-keys
                           (fx+ (fx* 120 (piece-zobrist piece))
                                idx))))

(define-inline (xor-whites-move key)
  (fxxor key side-key))

(define (initialize-zobrist!)
  (random-seed #b110010110011010110101010111110)

  ;; Square keys:
  ;; Even though there are only 64 squares on the chess board, we'll use
  ;; the entire 10x12 vector for speed. It will allow us to use the
  ;; actual index of the square.
  (for ([ i (in-range (fxvector-length square-keys)) ])
    (fxvector-set! square-keys i (get-random-number)))

  ;; Represent whether it's white's move
  (set! side-key (get-random-number))

  ;; Represent castling rights
  ;; 16 combinations of KQkq castling rights
  (for ([ i (in-range (fxvector-length castle-keys)) ])
    (fxvector-set! castle-keys i (get-random-number)))

  ;; Represent the ep square
  ;; 10 columns x 12 rows
  (for ([ i (in-range (fxvector-length ep-keys)) ])
    (fxvector-set! ep-keys i (get-random-number))))

(define (generate-zobrist-key b)
  (define key 0)

  ;; Board squares
  (for* ([ rank (in-range 8) ]
         [ file (in-range 8) ])
    (let* ([ idx   (file-rank->idx file rank)        ]
           [ piece (bytes-ref (board-squares b) idx) ])
      (when (not (fx= piece empty-square))
        (set! key (xor-piece key piece idx)))))

  ;; White's move?
  (when (is-whites-move? b)
    (set! key (xor-whites-move key)))

  ;; Castling
  (set! key (xor-castling key (board-game-state b)))

  ;; EP square
  (let ([ ep-idx (get-ep-idx b) ])
    (when (fx> ep-idx 0)
      (set! key (xor-ep-square key ep-idx))))

  key)

(initialize-zobrist!)
