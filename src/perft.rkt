#lang racket

;; NOTE: to get perft results on stockfish
;; 1) start the program
;; 2) position fen r3k2r/1b4bq/8/8/8/8/7B/R3K2R w KQkq - 0 1
;; 3) go perft 3

(require "./board.rkt"
         "./evaluation.rkt"
         "./legality.rkt"
         "./make-move.rkt"
         "./move.rkt"
         "./movement.rkt")

(require racket/fixnum
         racket/performance-hint)

(define LONG-TEST #f)  ;; 1/31/2021 took 1 min 40 sec
(define REALLY-LONG-TEST #f)

(struct counts (nodes
                captures
                ep-captures
                castles
                promotions)
        #:transparent #:mutable)

(define (show-divide lst)
  (for ([ str (in-list lst) ])
    (printf "~a\n" str)))

(define (create-counts nodes captures ep-captures castles promotions)
  (counts nodes captures ep-captures castles promotions))

(define (create-zero-counts)
  (counts 0 0 0 0 0))

(define (reset-counts! obj)
  (set-counts-nodes!       obj 0)
  (set-counts-captures!    obj 0)
  (set-counts-ep-captures! obj 0)
  (set-counts-castles!     obj 0)
  (set-counts-promotions!  obj 0))

(define-inline (update-counts! obj m)
  ;; Total nodes
  (set-counts-nodes! obj (add1 (counts-nodes obj)))

  (when (fx> (move-captured-piece m) 0)
    ;; Captures
    (set-counts-captures! obj (add1 (counts-captures obj)))
    (when (move-is-ep-capture? m)
      ;; EP Captures
      (set-counts-ep-captures! obj (add1 (counts-ep-captures obj)))))

  (when (or (move-is-castle-queenside? m)
            (move-is-castle-kingside? m))
    ;; Castles
    (set-counts-castles! obj (add1 (counts-castles obj))))

  (when (fx> (move-promoted-piece m) 0)
    ;; Promotions
    (set-counts-promotions! obj (add1 (counts-promotions obj)))))

(define (divide b max-level)
  (define obj (create-zero-counts))
  (generate-moves! b)
  (let ([ get-move (move-iterator! b) ])
    (let loop ([ result '() ][ m (get-move) ])
      (if (not m)
          (reverse result)
          (begin
            (make-move! b m)
            (if (is-legal? b m)
                (begin
                  (reset-counts! obj)
                  (perft! b max-level obj m)
                  (unmake-move! b m)
                  (let ([ key (format "~a~a"
                                      (idx->pos (move-src-idx m))
                                      (idx->pos (move-dst-idx m))) ])
                    (loop (cons (format "~a: ~a" key (counts-nodes obj))
                                result)
                          (get-move))))
                (begin
                  (unmake-move! b m)
                  (loop result (get-move)))))))))

(define (perft! b max-level counts #:evaluate? [ evaluate? #f ] [ m #f ])
  (if (= (board-depth b) max-level)
      (begin
        (update-counts! counts m)
        (when evaluate?
          (evaluate b)))
      (begin
        (generate-moves! b)
        (let ([ tmoves (tactical-moves b) ]
              [ thead  (tactical-head b)  ]
              [ qmoves (quiet-moves b)    ]
              [ qhead  (quiet-head b)     ])
          (let loop ([ ti 0 ][ qi 0 ])
            (cond [ (<= ti thead)
                    (let ([ m (vector-ref tmoves ti)])
                      (make-move! b m)
                      (if (is-legal? b m)
                          (begin
                            (perft! b max-level counts
                                    #:evaluate? evaluate?
                                    m)
                            (unmake-move! b m))
                          (unmake-move! b m))
                      (loop (add1 ti) qi)) ]
                  [ (<= qi qhead)
                    (let ([ m (vector-ref qmoves qi) ])
                      (make-move! b m)
                      (if (is-legal? b m)
                          (begin
                            (perft! b max-level counts
                                    #:evaluate? evaluate?
                                    m)
                            (unmake-move! b m))
                          (unmake-move! b m))
                      (loop ti (add1 qi))) ]))))))

(module+ main
  (require "./fen.rkt")

  ;; FEN position after castling and some pawn moves:
  ;; "2kr3r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/2KR3R w - - 0 1"
  (let* ([ max-level 5 ]
         [ counts (create-zero-counts) ])
    (time (perft! (fen->board "2kr3r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/2KR3R w - - 0 1") max-level #:evaluate? #f counts))
    (printf "perft(~a) results:\n" max-level)
    (printf "  Totals:     ~a\n" (counts-nodes counts))
    (printf "  Captures:   ~a\n" (counts-captures counts))
    (printf "  EP Cap:     ~a\n" (counts-ep-captures counts))
    (printf "  Castles:    ~a\n" (counts-castles counts))
    (printf "  Promotions: ~a\n" (counts-promotions counts)))
  )

(module+ test
  (require rackunit)
  (require "./fen.rkt"
           "./state.rkt"
           "./zobrist.rkt")

  ;; ------------------------------------------------------------------------------------------
  ;; Initial position CPW
  ;; ------------------------------------------------------------------------------------------
  (let* ([ obj (create-zero-counts) ]
         [ b   (fen->board "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") ]
         [ key (generate-zobrist-key b) ])

    (set-hash-key! key)

    ;; Depth 1
    (reset-counts! obj)
    (perft! b 1 obj)
    (check-equal? (get-hash-key) key)

    (check-equal? (counts-nodes obj) 20)
    (check-equal? (counts-captures obj) 0)
    (check-equal? (counts-ep-captures obj) 0)
    (check-equal? (counts-castles obj) 0)
    (check-equal? (counts-promotions obj) 0)

    ;; Depth 2
    (reset-counts! obj)
    (perft! b 2 obj)
    (check-equal? (get-hash-key) key)

    (check-equal? (counts-nodes obj) 400)
    (check-equal? (counts-captures obj) 0)
    (check-equal? (counts-ep-captures obj) 0)
    (check-equal? (counts-castles obj) 0)
    (check-equal? (counts-promotions obj) 0)

    ;; Depth 3
    (reset-counts! obj)
    (perft! b 3 obj)
    (check-equal? (get-hash-key) key)

    (check-equal? (counts-nodes obj) 8902)
    (check-equal? (counts-captures obj) 34)
    (check-equal? (counts-ep-captures obj) 0)
    (check-equal? (counts-castles obj) 0)
    (check-equal? (counts-promotions obj) 0)

    ;; Depth 4
    (reset-counts! obj)
    (perft! b 4 obj)
    (check-equal? (get-hash-key) key)

    (check-equal? (counts-nodes obj) 197281)
    (check-equal? (counts-captures obj) 1576)
    (check-equal? (counts-ep-captures obj) 0)
    (check-equal? (counts-castles obj) 0)
    (check-equal? (counts-promotions obj) 0)

    ;; Depth 5
    (reset-counts! obj)
    (perft! b 5 obj)
    (check-equal? (get-hash-key) key)

    (check-equal? (counts-nodes obj) 4865609)
    (check-equal? (counts-captures obj) 82719)
    (check-equal? (counts-ep-captures obj) 258)
    (check-equal? (counts-castles obj) 0)
    (check-equal? (counts-promotions obj) 0)

    (when LONG-TEST
      ;; Depth 6 ~ 40 seconds
      (reset-counts! obj)
      (perft! b 6 obj)
      (check-equal? (get-hash-key) key)

      (check-equal? (counts-nodes obj) 119060324)
      (check-equal? (counts-captures obj) 2812008)
      (check-equal? (counts-ep-captures obj) 5248)
      (check-equal? (counts-castles obj) 0)
      (check-equal? (counts-promotions obj) 0))

    (when REALLY-LONG-TEST
      ;; Depth 7 ~ 18 minutes
      (reset-counts! obj)
      (perft! b 7 obj)
      (check-equal? (get-hash-key) key)

      (check-equal? (counts-nodes obj) 3195901860)
      (check-equal? (counts-captures obj) 108329926)
      (check-equal? (counts-ep-captures obj) 319617)
      (check-equal? (counts-castles obj) 883453)
      (check-equal? (counts-promotions obj) 0)))

  ;; ------------------------------------------------------------------------------------------
  ;; Position 2 - CPW also known as Kiwipete by Peter McKenzie
  ;; ------------------------------------------------------------------------------------------
  (let* ([ obj (create-zero-counts) ]
         [ b   (fen->board "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1") ]
         [ key (generate-zobrist-key b) ])

    (set-hash-key! key)

    ;; Depth 1
    (reset-counts! obj)
    (perft! b 1 obj)
    (check-equal? (get-hash-key) key)

    (check-equal? (counts-nodes obj) 48)
    (check-equal? (counts-captures obj) 8)
    (check-equal? (counts-ep-captures obj) 0)
    (check-equal? (counts-castles obj) 2)
    (check-equal? (counts-promotions obj) 0)

    ;; Depth 2
    (reset-counts! obj)
    (perft! b 2 obj)
    (check-equal? (get-hash-key) key)

    (check-equal? (counts-nodes obj) 2039)
    (check-equal? (counts-captures obj) 351)
    (check-equal? (counts-ep-captures obj) 1)
    (check-equal? (counts-castles obj) 91)
    (check-equal? (counts-promotions obj) 0)

    ;; Depth 3
    (reset-counts! obj)
    (perft! b 3 obj)
    (check-equal? (get-hash-key) key)

    (check-equal? (counts-nodes obj) 97862)
    (check-equal? (counts-captures obj) 17102)
    (check-equal? (counts-ep-captures obj) 45)
    (check-equal? (counts-castles obj) 3162)
    (check-equal? (counts-promotions obj) 0)

    ;; Depth 4
    (reset-counts! obj)
    (perft! b 4 obj)
    (check-equal? (get-hash-key) key)

    (check-equal? (counts-nodes obj) 4085603)
    (check-equal? (counts-captures obj) 757163)
    (check-equal? (counts-ep-captures obj) 1929)
    (check-equal? (counts-castles obj) 128013)
    (check-equal? (counts-promotions obj) 15172)

    (when LONG-TEST
      ;; Depth 5
      (reset-counts! obj)
      (perft! b 5 obj)
      (check-equal? (get-hash-key) key)

      (check-equal? (counts-nodes obj) 193690690)
      (check-equal? (counts-captures obj) 35043416)
      (check-equal? (counts-ep-captures obj) 73365)
      (check-equal? (counts-castles obj) 4993637)
      (check-equal? (counts-promotions obj) 8392))

    )

  ;; ------------------------------------------------------------------------------------------
  ;; Position 3 CPW
  ;; ------------------------------------------------------------------------------------------
  (let* ([ obj (create-zero-counts) ]
         [ b   (fen->board "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1") ]
         [ key (generate-zobrist-key b) ])

    (set-hash-key! key)

    ;; Depth 1
    (reset-counts! obj)
    (perft! b 1 obj)
    (check-equal? (get-hash-key) key)

    (check-equal? (counts-nodes obj) 14)
    (check-equal? (counts-captures obj) 1)
    (check-equal? (counts-ep-captures obj) 0)
    (check-equal? (counts-castles obj) 0)
    (check-equal? (counts-promotions obj) 0)

    ;; Depth 2
    (reset-counts! obj)
    (perft! b 2 obj)
    (check-equal? (get-hash-key) key)

    (check-equal? (counts-nodes obj) 191)
    (check-equal? (counts-captures obj) 14)
    (check-equal? (counts-ep-captures obj) 0)
    (check-equal? (counts-castles obj) 0)
    (check-equal? (counts-promotions obj) 0)

    ;; Depth 3
    (reset-counts! obj)
    (perft! b 3 obj)
    (check-equal? (get-hash-key) key)

    (check-equal? (counts-nodes obj) 2812)
    (check-equal? (counts-captures obj) 209)
    (check-equal? (counts-ep-captures obj) 2)
    (check-equal? (counts-castles obj) 0)
    (check-equal? (counts-promotions obj) 0)

    ;; Depth 4
    (reset-counts! obj)
    (perft! b 4 obj)
    (check-equal? (get-hash-key) key)

    (check-equal? (counts-nodes obj) 43238)
    (check-equal? (counts-captures obj) 3348)
    (check-equal? (counts-ep-captures obj) 123)
    (check-equal? (counts-castles obj) 0)
    (check-equal? (counts-promotions obj) 0)

    ;; Depth 5
    (reset-counts! obj)
    (perft! b 5 obj)
    (check-equal? (get-hash-key) key)

    (check-equal? (counts-nodes obj) 674624)
    (check-equal? (counts-captures obj) 52051)
    (check-equal? (counts-ep-captures obj) 1165)
    (check-equal? (counts-castles obj) 0)
    (check-equal? (counts-promotions obj) 0)

    (when LONG-TEST
      ;; Depth 6
      (reset-counts! obj)
      (perft! b 6 obj)
      (check-equal? (get-hash-key) key)

      (check-equal? (counts-nodes obj) 11030083)
      (check-equal? (counts-captures obj) 940350)
      (check-equal? (counts-ep-captures obj) 33325)
      (check-equal? (counts-castles obj) 0)
      (check-equal? (counts-promotions obj) 7552))

    )

  ;; ------------------------------------------------------------------------------------------
  ;; Position 4 CPW
  ;; ------------------------------------------------------------------------------------------
  (let ([ obj (create-zero-counts) ])
    (for ([ b (in-list (list
                        (fen->board "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")
                        (fen->board "r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1"))) ])
      (let ([ key (generate-zobrist-key b) ])

        (set-hash-key! key)

        ;; Depth 1
        (reset-counts! obj)
        (perft! b 1 obj)
        (check-equal? (get-hash-key) key)

        (check-equal? (counts-nodes obj) 6)
        (check-equal? (counts-captures obj) 0)
        (check-equal? (counts-ep-captures obj) 0)
        (check-equal? (counts-castles obj) 0)
        (check-equal? (counts-promotions obj) 0)

        ;; Depth 2
        (reset-counts! obj)
        (perft! b 2 obj)
        (check-equal? (get-hash-key) key)

        (check-equal? (counts-nodes obj) 264)
        (check-equal? (counts-captures obj) 87)
        (check-equal? (counts-ep-captures obj) 0)
        (check-equal? (counts-castles obj) 6)
        (check-equal? (counts-promotions obj) 48)

        ;; Depth 3
        (reset-counts! obj)
        (perft! b 3 obj)
        (check-equal? (get-hash-key) key)

        (check-equal? (counts-nodes obj) 9467)
        (check-equal? (counts-captures obj) 1021)
        (check-equal? (counts-ep-captures obj) 4)
        (check-equal? (counts-castles obj) 0)
        (check-equal? (counts-promotions obj) 120)

        ;; Depth 4
        (reset-counts! obj)
        (perft! b 4 obj)
        (check-equal? (get-hash-key) key)

        (check-equal? (counts-nodes obj) 422333)
        (check-equal? (counts-captures obj) 131393)
        (check-equal? (counts-ep-captures obj) 0)
        (check-equal? (counts-castles obj) 7795)
        (check-equal? (counts-promotions obj) 60032)

        (when LONG-TEST
          ;; Depth 5
          (reset-counts! obj)
          (perft! b 5 obj)
          (check-equal? (get-hash-key) key)

          (check-equal? (counts-nodes obj) 15833292)
          (check-equal? (counts-captures obj) 2046173)
          (check-equal? (counts-ep-captures obj) 6512)
          (check-equal? (counts-castles obj) 0)
          (check-equal? (counts-promotions obj) 329464))

        )))

  ;; ------------------------------------------------------------------------------------------
  ;; Position 5 CPW
  ;; This position was discussed on Talkchess and caught bugs in
  ;; engines several years old at depth 3 and was also reported wrong
  ;; here, hopefully now corrected with the results given by Steven
  ;; Edwards, July 18, 2015 https://www.chessprogramming.org/Perft_Results
  ;; ------------------------------------------------------------------------------------------
  (let* ([ obj (create-zero-counts) ]
         [ b   (fen->board "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8") ]
         [ s   (board-game-state b) ]
         [ key (generate-zobrist-key b) ])

    (set-hash-key! key)

    (check-not-false (state-w-kingside-ok? s))
    (check-not-false (state-w-queenside-ok? s))
    (check-false (state-b-kingside-ok? s))
    (check-false (state-b-queenside-ok? s))

    ;; Depth 1
    (reset-counts! obj)
    (perft! b 1 obj)
    (check-equal? (get-hash-key) key)

    (check-equal? (counts-nodes obj) 44)

    ;; Depth 2
    (reset-counts! obj)
    (perft! b 2 obj)
    (check-equal? (get-hash-key) key)
    (check-equal? (counts-nodes obj) 1486)

    ;; Depth 3
    (reset-counts! obj)
    (perft! b 3 obj)
    (check-equal? (get-hash-key) key)
    (check-equal? (counts-nodes obj) 62379)

    ;; Depth 4
    (reset-counts! obj)
    (perft! b 4 obj)
    (check-equal? (get-hash-key) key)
    (check-equal? (counts-nodes obj) 2103487)

    (when LONG-TEST
      ;; Depth 5
      (reset-counts! obj)
      (perft! b 5 obj)
      (check-equal? (get-hash-key) key)
      (check-equal? (counts-nodes obj) 89941194))

    )

  ;; ------------------------------------------------------------------------------------------
  ;; Position 6 CPW
  ;; ------------------------------------------------------------------------------------------
  (let* ([ obj (create-zero-counts) ]
         [ b   (fen->board "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10") ]
         [ key (generate-zobrist-key b) ])

    (set-hash-key! key)

    ;; Depth 1
    (reset-counts! obj)
    (perft! b 1 obj)
    (check-equal? (get-hash-key) key)
    (check-equal? (counts-nodes obj) 46)

    ;; Depth 2
    (reset-counts! obj)
    (perft! b 2 obj)
    (check-equal? (get-hash-key) key)
    (check-equal? (counts-nodes obj) 2079)

    ;; Depth 3
    (reset-counts! obj)
    (perft! b 3 obj)
    (check-equal? (get-hash-key) key)
    (check-equal? (counts-nodes obj) 89890)

    ;; Depth 4
    (reset-counts! obj)
    (perft! b 4 obj)
    (check-equal? (get-hash-key) key)
    (check-equal? (counts-nodes obj) 3894594)

    (when LONG-TEST
      ;; Depth 5
      (reset-counts! obj)
      (perft! b 5 obj)
      (check-equal? (get-hash-key) key)
      (check-equal? (counts-nodes obj) 164075551))

    )

  ;; From: http://talkchess.com/forum3/viewtopic.php?f=7&t=55787#p616236
  ;; Also from: http://www.rocechess.ch/perft.html
  (let* ([ obj (create-zero-counts) ]
         [ b   (fen->board "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1") ]
         [ s   (board-game-state b) ]
         [ key (generate-zobrist-key b) ])

    (set-hash-key! key)

    (check-false (state-w-kingside-ok? s))
    (check-false (state-w-queenside-ok? s))
    (check-false (state-b-kingside-ok? s))
    (check-false (state-b-queenside-ok? s))

    ;; Depth 1
    (reset-counts! obj)
    (perft! b 1 obj)
    (check-equal? (get-hash-key) key)
    (check-equal? (counts-nodes obj) 24)

    ;; Depth 2
    (reset-counts! obj)
    (perft! b 2 obj)
    (check-equal? (get-hash-key) key)
    (check-equal? (counts-nodes obj) 496)

    ;; Depth 3
    (reset-counts! obj)
    (perft! b 3 obj)
    (check-equal? (get-hash-key) key)
    (check-equal? (counts-nodes obj) 9483)

    ;; Depth 4
    (reset-counts! obj)
    (perft! b 4 obj)
    (check-equal? (get-hash-key) key)
    (check-equal? (counts-nodes obj) 182838)

    ;; Depth 5
    (reset-counts! obj)
    (perft! b 5 obj)
    (check-equal? (get-hash-key) key)
    (check-equal? (counts-nodes obj) 3605103)

    (when LONG-TEST
      ;; Depth 6
      (reset-counts! obj)
      (perft! b 6 obj)
      (check-equal? (get-hash-key) key)
      (check-equal? (counts-nodes obj) 71179139))

    )

  ;; From: http://talkchess.com/forum3/viewtopic.php?f=7&t=76466&p=881175#p881149
  (let* ([ obj (create-zero-counts) ]
         [ b   (fen->board "8/8/8/8/3kpP1R/8/6K1/8 b - f3 0 1") ]
         [ s   (board-game-state b) ]
         [ key (generate-zobrist-key b) ])

    (set-hash-key! key)

    (check-false (state-w-kingside-ok? s))
    (check-false (state-w-queenside-ok? s))
    (check-false (state-b-kingside-ok? s))
    (check-false (state-b-queenside-ok? s))

    (reset-counts! obj)
    (perft! b 1 obj)
    (check-equal? (get-hash-key) key)
    (check-equal? (counts-nodes obj) 7))

  ;; Run file of tests
  (let ([ obj (create-zero-counts) ])
    (for ([ line (in-list (file->lines "../test-data/perftsuite.epd")) ])
      (let* ([ lst (string-split line ";") ]
             [ fen (string-trim (car lst)) ])
        (let loop ([ lst (cdr lst) ])
          (when (not (null? lst))
            (let* ([ groups (regexp-match #px"^D(\\d) (\\d+)$" (string-trim (car lst))) ]
                   [ depth  (string->number (second groups)) ]
                   [ nodes  (string->number (third groups))  ])
              (when (< depth (cond
                              [ REALLY-LONG-TEST 7 ]
                              [ LONG-TEST        6 ]
                              [ else             5 ]))
                (let* ([ b (fen->board fen) ]
                       [ key (generate-zobrist-key b) ])
                  (set-hash-key! key)
                  (reset-counts! obj)
                  (perft! b depth obj)
                  (check-equal? (get-hash-key) key)
                  (check-equal? (counts-nodes obj) nodes (format "Depth: ~a, FEN: ~a" depth fen)))))
              (loop (cdr lst)))))))

  )
