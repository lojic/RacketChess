#lang racket

(provide black-king-idx
         create-game
         get-ep-idx
         init-moves!
         is-whites-move?
         may-castle?
         push-quiet-move!
         push-tactical-move!
         quiet-length
         quiet-moves
         reset-game-depth!
         set-black-king-idx!
         set-ep-idx!
         set-white-king-idx!
         set-whites-move?!
         tactical-length
         tactical-moves
         white-king-idx
         (struct-out game))

(require "./global.rkt"
         "./board.rkt"
         "./state.rkt"
         racket/performance-hint)

;; --------------------------------------------------------------------------------------------
;; State information for a chess game:
;; --------------------------------------------------------------------------------------------
;; board           : The 10 x 12 mailbox representation of a chess board
;; depth           : The current depth of the search tree
;; state           : A fixnum-fields representing game state
;; quiet-length    : An fxvector (indexed by depth) containing lengths of quiet-moves vectors
;; quiet-moves     : A vector (indexed by depth) of fxvectors containing quiet moves
;; tactical-length : An fxvector (indexed by depth) containing the lengths of tactical-moves vectors
;; tactical-moves  : A vector (indexed by depth) of fxvectors containing tactical moves

(struct game (board
              depth
              state
              quiet-length    ; Indexed by depth
              quiet-moves     ; Indexed by depth
              tactical-length ; Indexed by depth
              tactical-moves) ; Indexed by depth
  #:transparent #:mutable)

(define max-game-depth 200)
(define max-moves      250)

(define (create-game)
  (let ([ g (game (blank-board)                    ; board
                  0                                ; depth
                  (initial-game-state)             ; state
                  (make-fxvector max-game-depth)   ; quiet-length
                  (make-vector max-game-depth)     ; quiet-moves
                  (make-fxvector max-game-depth)   ; tactical-length
                  (make-vector max-game-depth)) ]) ; tactical-moves
    (for ([ i (in-range max-game-depth) ])
      ;; Initialize quiet-length values
      (fxvector-set! (game-quiet-length g) i 0)
      ;; Initialize quiet-moves vectors
      (vecset! (game-quiet-moves g) i (make-fxvector max-moves))
      ;; Initialize tactical-length values
      (fxvector-set! (game-tactical-length g) i 0)
      ;; Initialize tactical-moves vectors
      (vecset! (game-tactical-moves g) i (make-fxvector max-moves)))

    g))

;; --------------------------------------------------------------------------------------------
;; Public Interface
;; --------------------------------------------------------------------------------------------

;; Initialize the quiet and tactical moves vectors for the current depth.
(define-inline (init-moves! g)
  (let ([ d (game-depth g) ])
    (fxvector-set! (game-quiet-length g)    d 0)
    (fxvector-set! (game-tactical-length g) d 0)))

(define-inline (push-quiet-move! g m)
  (let* ([ depth (game-depth g)           ]
         [ vec   (game-quiet-length g)    ]
         [ len   (fxvector-ref vec depth) ])
    ;; Add a quiet move to the stack
    (fxvector-set! (quiet-moves g) len m)
    ;; Increment quiet move length
    (fxvector-set! vec depth (add1 len))))

(define-inline (push-tactical-move! g m)
  (let* ([ depth (game-depth g)           ]
         [ vec   (game-tactical-length g)    ]
         [ len   (fxvector-ref vec depth) ])
    ;; Add a tactical move to the stack
    (fxvector-set! (tactical-moves g) len m)
    ;; Increment tactical move length
    (fxvector-set! vec depth (add1 len))))

;; Return the length of the quiet moves vector for the current depth.
(define-inline (quiet-length g)
  (fxvector-ref (game-quiet-length g) (game-depth g)))

;; Return the quiet moves vector for the current depth.
(define-inline (quiet-moves g)
  (vecref (game-quiet-moves g) (game-depth g)))

;; Reset game depth to zero
(define-inline (reset-game-depth! g)
  (set-game-depth! g 0))

;; Return the length of the tactical moves vector for the current depth.
(define-inline (tactical-length g)
  (fxvector-ref (game-tactical-length g) (game-depth g)))

;; Return the tactical moves vector for the current depth
(define-inline (tactical-moves g)
  (vecref (game-tactical-moves g) (game-depth g)))

;; --------------------------------------------------------------------------------------------
;; Convenience functions for getting/setting state fields via the game
;; module When multiple fields need to be manipulated, it may be more
;; efficient to obtain the state fixnum, and then make multiple calls.
;; --------------------------------------------------------------------------------------------

(define-inline (black-king-idx g)
  (state-black-king-idx (game-state g)))

(define-inline (get-ep-idx g)
  (state-ep-idx (game-state g)))

(define-inline (is-whites-move? g)
  (state-whites-move? (game-state g)))

(define-inline (may-castle? g white?)
  (let ([ s (game-state g) ])
    (if white?
        (or (state-w-kingside-ok? s) (state-w-queenside-ok? s))
        (or (state-b-kingside-ok? s) (state-b-queenside-ok? s)))))

(define-inline (set-black-king-idx! g idx)
  (set-game-state!
   g
   (update-state-black-king-idx (game-state g) idx)))

(define-inline (set-ep-idx! g v)
  (set-game-state! g (update-state-ep-idx (game-state g) v)))

(define-inline (set-white-king-idx! g idx)
  (set-game-state!
   g
   (update-state-white-king-idx (game-state g) idx)))

(define-inline (set-whites-move?! g bool)
  (set-game-state!
   g
   (if bool
       (set-state-whites-move? (game-state g))
       (unset-state-whites-move? (game-state g)))))

(define-inline (white-king-idx g)
  (state-white-king-idx (game-state g)))

;; --------------------------------------------------------------------------------------------
;; Tests
;; --------------------------------------------------------------------------------------------

(module+ test
  (require rackunit)

  ;; ------------------------------------------------------------------------------------------
  ;; create-game
  ;; ------------------------------------------------------------------------------------------

  (let ([ g (create-game) ])
    ;; depth
    (check-equal? (game-depth g) 0)

    ;; board
    (check-equal? (bytes-length (game-board g)) (* 10 12))

    ;; moves
    (check-equal? (fxvector-length (game-quiet-length g)) max-game-depth)
    (check-equal? (vector-length (game-quiet-moves g)) max-game-depth)
    (check-equal? (fxvector-length (game-tactical-length g)) max-game-depth)
    (check-equal? (vector-length (game-tactical-moves g)) max-game-depth)

    (for ([ i (in-range max-game-depth) ])
      (check-equal? (fxvector-ref (game-quiet-length g) i) 0)
      (check-equal? (fxvector-ref (game-tactical-length g) i) 0)

      (check-equal? (fxvector-length (vector-ref (game-quiet-moves g) i)) max-moves)
      (check-equal? (fxvector-length (vector-ref (game-tactical-moves g) i)) max-moves)))

  ;; ------------------------------------------------------------------------------------------
  ;; init-moves!
  ;; ------------------------------------------------------------------------------------------

  (let ([ g (create-game) ])
    (fxvector-set! (game-quiet-length g) 0 7)
    (fxvector-set! (game-tactical-length g) 0 8)

    (check-not-equal? (fxvector-ref (game-quiet-length g) 0) 0)
    (check-not-equal? (fxvector-ref (game-tactical-length g) 0) 0)

    (init-moves! g)

    (check-equal? (fxvector-ref (game-quiet-length g) 0) 0)
    (check-equal? (fxvector-ref (game-tactical-length g) 0) 0))

  ;; ------------------------------------------------------------------------------------------
  ;; reset-game-depth!
  ;; ------------------------------------------------------------------------------------------

  (let ([ g (create-game) ])
    (set-game-depth! g 7)
    (reset-game-depth! g)
    (check-equal? (game-depth g) 0))

  )
