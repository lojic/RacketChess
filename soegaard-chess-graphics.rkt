#lang at-exp racket
(require metapict metapict/crop)

;;;
;;; Rough Bounding Box
;;; 
(define (curve->pts c)
  (append* (for/list ([b (curve:-bezs c)])
             (defm (bez p q r s) b)
             (list r s))))

(define (curves->pts cs)
  (append* (map curve->pts cs)))


; bounding box given by start and end points of the bezier curve
(define (rough-bbox cs)
  (def pts (curves->pts cs))
  (def xmin (apply min (map pt-x pts)))
  (def xmax (apply max (map pt-x pts)))
  (def ymin (apply min (map pt-y pts)))
  (def ymax (apply max (map pt-y pts)))
  (window xmin xmax ymin ymax))


;;;
;;; Chess Characters
;;;

(def white-king   '♔)
(def white-queen  '♕)
(def white-rook   '♖)
(def white-bishop '♗)
(def white-knight '♘)
(def white-pawn   '♙)

(def black-king   '♚)
(def black-queen  '♛)
(def black-rook   '♜)
(def black-bishop '♝)
(def black-knight '♞)
(def black-pawn   '♟)

(def white-symbols
  (list white-king white-queen white-rook white-bishop white-knight white-pawn))
(def black-symbols
  (list black-king black-queen black-rook black-bishop black-knight black-pawn))
(def all-chess-symbols (append white-symbols black-symbols))

(define (white-symbol? c) (member c white-symbols))
(define (black-symbol? c) (member c black-symbols))

(define (symbol->char s) (string-ref (~a s) 0))
(define (char->symbol s) (string->symbol (~a s)))

(define (white-character? c) (white-symbol? (char->symbol c)))
(define (black-character? c) (black-symbol? (char->symbol c)))

(define (to-black-char c)
  (def s (char->symbol c))
  (symbol->char
    (cond
      [(eqv? s white-king)   black-king]
      [(eqv? s white-queen)  black-queen]
      [(eqv? s white-rook)   black-rook]
      [(eqv? s white-bishop) black-bishop]
      [(eqv? s white-knight) black-knight]
      [(eqv? s white-pawn)   black-pawn]
      [else s])))

(define (to-white-char c)
  (def s (char->symbol c))
  (symbol->char
    (cond
      [(eqv? s black-king)   white-king]
      [(eqv? s black-queen)  white-queen]
      [(eqv? s black-rook)   white-rook]
      [(eqv? s black-bishop) white-bishop]
      [(eqv? s black-knight) white-knight]
      [(eqv? s black-pawn)   white-pawn]
      [else s])))

;;;
;;; Forsyth–Edwards Notation
;;;

(define (fen->game fen)
  ; find the 6 fields, pass the board placements to fen->board
  (match (string-split fen)
    [(list placements active-color castling en-passant-target-square
           halfmove-clock fullmove-number)
     (fen->board placements)]
    [(list* placements more)
     (fen->board placements)]
    [_ (error 'fen->game
              "expected string with 6 space separated fields in FEN-notation")]))

(define (fen-char->unicode-symbol c)
  (case c
    [(#\k)     black-king]
    [(#\q)     black-queen]
    [(#\r)     black-rook]
    [(#\b)     black-bishop]
    [(#\n)     black-knight]
    [(#\p)     black-pawn]
    
    [(#\K)     white-king]
    [(#\Q)     white-queen]
    [(#\R)     white-rook]
    [(#\B)     white-bishop]
    [(#\N)     white-knight]
    [(#\P)     white-pawn]
    [(#\space) '| |]
    [else      (string->symbol " ")]))

(define (fen->board placements)
  ; split the placements into rows, expand digits into blanks
  ; return list of row (8th row is first) represented as strings
  (define (digit->blanks d)
    (make-string (- (char->integer d) (char->integer #\0)) #\space))
  (define rows (string-split placements "/"))
  (for/list ([row rows])
    (apply string-append
           (for/list ([c row])
             (case c 
               [(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8)
                (digit->blanks c)]
               [else
                (symbol->string (fen-char->unicode-symbol c))])))))

; (fen->board "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")

(define dark-color  (make-color* #xAE #x89 #x68))
(define light-color (make-color* #xEC #xD9 #xB9))

(define (board->pict rows
                     #:light-color   [light-color light-color]
                     #:dark-color    [dark-color  dark-color]
                     #:white-outline [white-outline "black"]
                     #:white-fill    [white-fill    "white"]
                     #:black-outline [black-outline "white"]
                     #:black-fill    [black-fill    "black"])
  (defm (list width height) (get-field-device-size))
  (for/draw ([row rows] [j (in-naturals)])
    (for/draw ([c row] [i (in-naturals)])
      (def field-color   (if (even? (+ i j)) light-color dark-color))
      (def outline-color (if (white-character? c) white-outline black-outline))
      (def fill-color    (if (white-character? c) white-fill    black-fill))      
      (draw (brushcolor field-color (fill (shifted i j unitsquare)))
            (and (not (equal? c #\space))
                 (label-cnt (char->pict (to-white-char c)
                                        outline-color fill-color width height)
                            (pt (+ i 0.5) (+ j 0.5))))))))



(define (chessboard m n)
  (for*/draw ([i m] [j n])
    (and (even? (+ i j))
         (brushcolor "red" (fill (shifted i j unitsquare))))))

(define (center-pict p w h)
  (def pw (pict-width  p))
  (def ph (pict-height p))
  (def padx (* 0.5 (- w pw)))
  (def pady (* 0.5 (- h ph)))
  (beside
   (blank padx 0)
   (above (blank 0 pady) p (blank 0 pady))
   (blank padx 0)))

(define (crop-text p)
  ; remove space above and below actual text
  (def w (pict-width   p))
  (def h (pict-height  p))
  (def a (pict-ascent  p))
  (def d (pict-descent p))
  (parameterize ([curve-pict-width  w]
                 [curve-pict-height h])
    (with-window (window 0 w 0 h)
      (crop p w (- a d) 0 d))))

(define (post-scale p h)
  (scale (/ h (pict-height p)) p))

(define (piece c size text-color)
  (font-size size (text (~a c) #:text-color text-color)))

(define (transfer from-p to-p)
  ; use ascent and descent fom from-p on the to-p pict
  (defm (pict draw width height ascent descent children panbox last) to-p)
  (pict draw width height (pict-ascent from-p) (pict-descent from-p) children panbox last))

(define (piecemask c p size iw ih ix iy)
  (defm (list dx dy) (pict-dims p))
  ; (displayln (list 'piecemask dx dy))
  (parameterize ([curve-pict-width  dx]
                 [curve-pict-height dy])
    (with-window (window 0 dx 0 dy)
      (defv (outline bbox) (text-outline (make-similar-font the-font #:size size)
                                         (~a c) #:return-bounding-box? #t))
      (def bb (rough-bbox outline))
      (defm (window xmin xmax ymin ymax) bb)
      ; (displayln bbox) (displayln bb) ; shows that the ymin ymax values are identical
      (def α 0.98)
      (def β (/ (- 1 α) 2))
      (def shift ((shifted 0 (+ (- (- dy (+ iy ih)) (* -1 β ih))))
                  (scaled (* α (/ ih (- ymax ymin))))
                  (shifted 0  (- ymin))))
      (def  out (map shift outline))
      (def pm
        (draw         
         #;(color "green" (draw (shifted 0 (- dy (+ iy ih)) 
                                         (curve (pt ix 0) -rectangle (pt (+ ix iw) ih)))))
         (map fill out)
         #;(color "yellow"   (draw  (curve (pt ix 0) -- (pt ix dy))))))
      (transfer p pm))))

(def example
  @~a{R N B Q K B N R
        P P P P P P P P
        - - - - - - - -
        - - - - - - - -  
        - - - - - - - -  
        - - - - - - - -
        p p p p p p p p
        r n b q k b n r})

(set-curve-pict-size 600 600)

(define (get-field-device-size)
  (defm (vec dx dy) (pt- ((current-curve-transformation) (pt 1 1))
                         ((current-curve-transformation) (pt 0 0))))
  (list (abs dx) (abs dy)))

;; (with-window (window -1 9 -1 9)a
;;   (def field-size (get-field-device-size))
;;   (def h (second field-size))
  
;;   (def r1 (label-cnt (piece black-rook h) (pt 0.5 0.5)))
;;   (def k1 (label-cnt (piece black-king h) (pt 1.5 0.5)))
;;   (def k2 (label-cnt (piece white-king h) (pt 2.5 0.5)))
;;   (def p1 (label-cnt (piece black-pawn h) (pt 3.5 0.5)))
  
;;   (draw (chessboard 8 8)
;;         k1 k2 p1
;;         #;(label-bbox l))  
;;   )

(def the-font (make-similar-font (new-font) #:face "Arial Unicode MS" #:size 50))
(current-font the-font)

(define (deep t x)
  (define (d x) (deep t x))
  (if (list? x)
      (map d x)
      (t x)))

;; (with-window (window -1 9 -1 9)
;;   (def field-size (get-field-device-size))
;;   (def h (second field-size))
  
;;   (def r1 (label-cnt (piece black-rook h) (pt 0.5 0.5)))
;;   (def k1 (label-cnt (piece black-king h) (pt 1.5 0.5)))
;;   (def k2 (label-cnt (piece white-king h) (pt 2.5 0.5)))
;;   (def p1 (label-cnt (piece black-pawn h) (pt 3.5 0.5)))
  
;;   (draw (chessboard 8 8)
;;         k1 k2 p1        
;;         #;(label-bbox l))  
;;   )

(define (pict-info p)
  (list (list 'width   (pict-width   p))
        (list 'height  (pict-height  p))        
        (list 'ascent  (pict-ascent  p))
        (list 'descent (pict-descent p))))

(define (pict-dims p)
  (list (pict-width p) (pict-height p)))


(require metapict/path-operations)

(define (debug-pict p)
  (displayln (pict-info p))
  (def w (pict-width   p))
  (def h (pict-height  p))
  (def a (pict-ascent  p))
  (def d (pict-descent p))
  (displayln (list 'w w 'h h 'a a 'd d))
  
  (parameterize ([curve-pict-width  w]
                 [curve-pict-height h])
    (with-window (window 0 w 0 h)
      (draw (curve (pt 0 0) -rectangle (pt w h))
            (color "blue" (draw (curve (pt 0 d) -- (pt w d))))
            (color "red"  (draw (curve (pt 0 a) -- (pt w a))))
            p))))


; (pict-info (text "♖" the-font))
; (scale 16 (debug-pict (text "♖" the-font)))

(require "chess-glyph-info.rkt")
(define desc (create_font_desc "Arial Unicode MS" 12 #f #f 0))
; (chess-metrics desc "♖")



;; (define (annotate-pict str)
;;   ;; 1. Find font with chess symbols
;;   (define face "Arial Unicode MS")  
;;   (define the-size 50)
;;   (define my-font (make-similar-font the-font #:face face #:size the-size))
;;   ;; 2. Make a `text` pict with the symbol (will end as the top layer of the image)
;;   (define p (text str my-font))
;;   ;; 3. Extract information on the text pict
;;   (displayln (pict-info p))
;;   (def w (pict-width   p))
;;   (def h (pict-height  p))
;;   (def a (pict-ascent  p))
;;   (def d (pict-descent p))
;;   (displayln (list 'w w 'h h 'a a 'd d))
;;   ;; 4. Get information on the glyph - especially the inked aread
;;   (define m (chess-metrics (create_font_desc face the-size  #f #f 0) str))
;;   (define-values (bl iw ih ix iy lw lh lx ly)
;;     (match m
;;       [(list (list 'baseline bl)
;;              (list 'ink     (list (list 'w iw) (list 'h ih) (list 'x ix) (list 'y iy)
;;                                   (list 'y+h iy+h)))
;;              (list 'logical (list (list 'w lw) (list 'h lh) (list 'x lx) (list 'y ly)
;;                                   (list 'y+h ly+h))))
;;        (values bl iw ih ix iy lw lh lx ly)]))
;;   ; Note: the baseline and y-values here are oriented the opposite way of MetaPict coordinates,
;;   ;       so use (- h bl) (- h iy)  (- h ly) etc.
;;   (displayln (list 'ix ix))
;;   (parameterize ([curve-pict-width  w]
;;                  [curve-pict-height h])
;;     ; (defv (outline bbox) (text-outline my-font str #:return-bounding-box? #t))
;;     ; (def bb (rough-bbox outline))
;;     ; (displayln bbox)
;;     ; (displayln bb)
;;     ; (defm (window xmin xmax ymin ymax) bb)
;;     ; (def shift (shifted 0 (- h (+ iy ih)) (scaled (/ ih (- ymax ymin)) (shifted 0 (- ymin)))))
;;     ; (displayln outline)
;;     ;; 5. Transfer ascent and descent from p to the mask
;;     (def pm  (transfer p (color "red" (piecemask str p h iw ih ix iy))))
;;     (with-window (window 0 w 0 h)
;;       (draw (curve (pt 0 0) -rectangle (pt w h))
;;             ; (color "blue"   (draw (curve (pt 0 d) -- (pt w d))))
;;             ; (color "red"    (draw (curve (pt 0 a) -- (pt w a))))
;;             ; (color "green"  (draw (curve (pt 0 (- h bl)) -- (pt w (- h bl)))))
;;             (color "blue"   (draw (curve    (pt 0 (- h iy))
;;                                             -- (pt 0 (- h (+ iy ih)))
;;                                             -- (pt w (- h (+ iy ih)))
;;                                             -- (pt w (- h iy))
;;                                             -- cycle)))
;;             ; (color "yellow" (draw (map shift outline)))
;;             pm
;;             p))))

; (scale 4 (annotate-pict "♖"))
; (annotate-pict "♖" p)

(define (piece/mask c size outline-col fill-col)
  (def p    (piece c size outline-col))
  (def ph   (pict-height p))
  (define m (chess-metrics (create_font_desc "Arial Unicode MS" size #f #f 0) (~a c)))
  (match m
    [(list (list 'baseline bl)
           (list 'ink     (list (list 'w iw) (list 'h ih) (list 'x ix) (list 'y iy)
                                (list 'y+h iy+h)))
           (list 'logical (list (list 'w lw) (list 'h lh) (list 'x lx) (list 'y ly)
                                (list 'y+h ly+h))))
     (def pm  (transfer p (color fill-col (piecemask c p (pict-height p) iw ih ix iy))))
     (draw pm p)]))


(define (char->pict c outline-color fill-color
                    field-width field-height
                    [font-description-string "Arial Unicode MS"])
  (set! c (if (string? c) (string-ref c 0) c))
  (def w         field-width)
  (def h         field-height)
  (def p         (piece c field-height outline-color))
  (def font-desc (create_font_desc font-description-string field-height  #f #f 0))
  (define m      (chess-metrics font-desc (string c)))
  (match m
    [(list (list 'baseline bl)
           (list 'ink     (list (list 'w iw) (list 'h ih) (list 'x ix) (list 'y iy)
                                (list 'y+h iy+h)))
           (list 'logical (list (list 'w lw) (list 'h lh) (list 'x lx) (list 'y ly)
                                (list 'y+h ly+h))))
     (def pc  (center-pict (crop/inked p) w h))
     (def pm  (transfer p  (color fill-color (piecemask c p (pict-height p) iw ih ix iy))))
     (def pmc (center-pict (crop/inked pm) w h))
     
     (draw pmc pc)]))

;; (with-window (window -1 9 -1 9)
;;   (def field-size (get-field-device-size))
;;   (defm (list dx dy) field-size)
;;   (def c white-rook)
;;   (def p   (piece c dy "black"))
;;   (define m (chess-metrics (create_font_desc "Arial Unicode MS" dy  #f #f 0) "♖"))
;;   (match m
;;     [(list (list 'baseline bl)
;;            (list 'ink     (list (list 'w iw) (list 'h ih) (list 'x ix) (list 'y iy)
;;                                 (list 'y+h iy+h)))
;;            (list 'logical (list (list 'w lw) (list 'h lh) (list 'x lx) (list 'y ly)
;;                                 (list 'y+h ly+h))))
;;      (displayln (list 'baseline bl))
;;      (def pc  (center-pict (crop/inked p)  dx dy))
;;      (def pm  (transfer p (color "red" (piecemask c p (pict-height p) iw ih ix iy))))
;;      (def pmc (center-pict (crop/inked pm)  dx dy))
;;      (displayln (pict-info p))
;;      (displayln (pict-info pm))
;;      (displayln (pict-info (center-pict p  dx dy)))
;;      (displayln (pict-info (center-pict pm dx dy)))
     
;;      (scale 4 (draw pmc pc))]))


 (for/list ([c all-chess-symbols])
   (char->pict (~a c) "white" "blue" 100 100))

;; (char->pict "♖" "black" "blue" 100 100)


(set-curve-pict-size 600 600)
(with-window (window -1 9 -1 9)
  (def field-size (get-field-device-size))  
  (board->pict
   (fen->board "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")))
