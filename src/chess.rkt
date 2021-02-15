#lang racket

(require "./board.rkt"
         "./board-utilities.rkt"
         "./fen.rkt"
         "./legality.rkt"
         "./make-move.rkt"
         "./movement.rkt"
         "./pgn.rkt"
         "./search.rkt"
         "./zobrist.rkt")

(define (game depth computer-plays-black? seconds [ fen #f ])
  (define (normalize-score score)
    (if computer-plays-black?
        (- score)
        score))

  (define b (if fen
                (fen->board fen)
                (fen->board)))

  ;; Initialize Zobrist key
  (set-hash-key! (generate-zobrist-key b))

  (when (and computer-plays-black?
             (is-whites-move? b))
    (make-human-move! b))

  (let loop ([ seconds seconds ])
    (match-let ([ (cons score m) (search b depth seconds) ])
      (if m
          (let ([ score (normalize-score score) ])
            (displayln "")
            (make-move! b m)
            (print-board b #:full? #t)
            (print-move m)
            (printf "Score: ~a\n" (/ score 100.0))
            (displayln "")
            (let ([ new-seconds (make-human-move! b) ])
              (if new-seconds
                  (begin
                    (printf "New think time = ~a\n" new-seconds)
                    (loop new-seconds))
                  (loop seconds))))
          (printf "No move!\n")))))

(define (get-human-move b)
  (with-handlers ([ exn:fail?
                    (λ (e)
                      (displayln (exn-message e))
                      (cons #f #f)) ])
    (display "Enter move: ")
    (let ([ lst (string-split (read-line)) ])
      (if (null? lst)
          ;; Apparently just hit enter key
          (cons #f #f)
          ;; Something entered
          (cons
           (pgn-move b (car lst))
           (if (null? (cdr lst))
               #f
               (string->number (cadr lst))))))))

(define (make-move b m)
  (with-handlers ([ exn:fail?
                    (λ (e)
                      (displayln (exn-message e))
                      #f) ])
    (make-move! b m)
    (if (is-legal? b m)
        #t
        (begin
          (displayln "Illegal move")
          (unmake-move! b m)
          #f))))

(define (make-human-move! b)
  (match-let ([ (cons m secs) (get-human-move b) ])
    (if (and m (make-move b m))
        (print-board b #:full? #t)
        (make-human-move! b))
    secs))

(module+ main
  (let ([ computer-plays-black?
          (let loop ()
            (displayln "Computer plays white or black? (w or b): ")
            (let ([ player (read-line) ])
              (cond [ (string=? player "w") #f ]
                    [ (string=? player "b") #t ]
                    [ else (loop) ]))) ])
    (game 20 computer-plays-black? 5)))
