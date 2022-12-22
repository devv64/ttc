#lang racket
;Dev Patel
;I pledge my honor that I have abided by the Stevens Honor System.

(require graphics/graphics
         racket/draw)
(open-graphics)

;takes input, size is courtyard length & width
(display "Choose your n value: ")
(define input (string->number (read-line (current-input-port) 'any)))
(define start-time (current-inexact-milliseconds))
(define size (expt 2 input))

;important functions for scaling/ makes math easier
(define ScaleWindowFactor (/ 800 size))
(define (Windowscale x)
  (* x ScaleWindowFactor))

;creates courtyard's viewport
(define Window
  (open-viewport "#lang eopl" (Windowscale size) (Windowscale size)))

;(define Window2
 ; (open-pixmap "courtyard" (Windowscale size) (Windowscale size)))

;(define (disp)
 ; ;(sleep 1)
  ;(copy-viewport Window2 Window))

;(for ([i (in-range 0 (Windowscale size) ScaleWindowFactor)])
 ; (for ([j (in-range 0 (Windowscale size) ScaleWindowFactor)])
  ;  ((draw-rectangle Window) (make-posn i j) ScaleWindowFactor ScaleWindowFactor "black")))

;set - colors a singular tile, get - checks if singular tile is colored (boolean)
(define (courtyard-set x y color)
  ((draw-solid-rectangle Window) (make-posn (Windowscale x) (Windowscale y)) ScaleWindowFactor ScaleWindowFactor color))

(define (courtyard-get x y)
  (cond
    [(= 0 ((get-pixel Window) (make-posn (floor(+ (Windowscale x) (/ ScaleWindowFactor 2))) (floor(+ (Windowscale y) (/ ScaleWindowFactor 2)))))) #f]
    [else #t]))

;creates random color *with opacity between .5-1
(define (randcolor)
  (make-color (random 256) (random 256) (random 256) ;(+ .5 (/ (random 50) 100))
              ))

;initializes color
(define color "black")

;places trominos by placing *1 rectangle and 1 square, randomizes color after placing everything
;4 functions to place troms in 4 different orientations
(define (trom-tr x y)
  ;((draw-solid-polygon Window) (list (make-posn 0 0) (make-posn (Windowscale -2) 0) (make-posn (Windowscale -2) (Windowscale 1)) (make-posn (Windowscale -1) (Windowscale 1)) (make-posn (Windowscale -1) (Windowscale 2)) (make-posn 0 (Windowscale 2))) (make-posn (Windowscale (+ x 1)) (Windowscale y)) color)
  ((draw-solid-rectangle Window) (make-posn (Windowscale (- x 1)) (Windowscale y)) ( * 2 ScaleWindowFactor) ScaleWindowFactor color)
  ;(courtyard-set x y color)
  ;(courtyard-set (- x 1) y color)
  (courtyard-set x (+ y 1) color)
  (set! color (randcolor)))

(define (trom-tl x y)
  ;((draw-solid-polygon Window) (list (make-posn 0 0) (make-posn (Windowscale 2) 0) (make-posn (Windowscale 2) (Windowscale 1)) (make-posn (Windowscale 1) (Windowscale 1)) (make-posn (Windowscale 1) (Windowscale 2)) (make-posn 0 (Windowscale 2))) (make-posn (Windowscale x) (Windowscale y)) color)
  ((draw-solid-rectangle Window) (make-posn (Windowscale x) (Windowscale y)) ( * 2 ScaleWindowFactor) ScaleWindowFactor color)
  ;(courtyard-set x y color)
  ;(courtyard-set (+ x 1) y color)
  (courtyard-set x (+ y 1) color)
  (set! color (randcolor)))

(define (trom-br x y)
  ;((draw-solid-polygon Window) (list (make-posn 0 0) (make-posn(Windowscale 1) 0) (make-posn (Windowscale 1) (Windowscale 2)) (make-posn (Windowscale -1) (Windowscale 2)) (make-posn (Windowscale -1) (Windowscale 1)) (make-posn (Windowscale 0) (Windowscale 1))) (make-posn (Windowscale x) (Windowscale (- y 1))) color)
  ((draw-solid-rectangle Window) (make-posn (Windowscale (- x 1)) (Windowscale y)) ( * 2 ScaleWindowFactor) ScaleWindowFactor color)
  ;(courtyard-set x y color)
  ;(courtyard-set (- x 1) y color)
  (courtyard-set x (- y 1) color)
  (set! color (randcolor)))

(define (trom-bl x y)
  ;((draw-solid-polygon Window) (list (make-posn 0 (Windowscale -1)) (make-posn (Windowscale 1) (Windowscale -1)) (make-posn (Windowscale 1) (Windowscale 0)) (make-posn (Windowscale 2) (Windowscale 0)) (make-posn (Windowscale 2) (Windowscale 1)) (make-posn 0 (Windowscale 1))) (make-posn (Windowscale x) (Windowscale y)) color)
  ((draw-solid-rectangle Window) (make-posn (Windowscale x) (Windowscale y)) ( * 2 ScaleWindowFactor) ScaleWindowFactor color)
  ;(courtyard-set x y color)
  ;(courtyard-set (+ x 1) y color)
  (courtyard-set x (- y 1) color)
  (set! color (randcolor)))

;creates hole in courtyard (black) then randomizes color
(courtyard-set (random size) (random size) color)
(set! color (randcolor))

;places tiles when down to a 2x2, one tile will always already be filled
(define (change-tiles x y)
  (for ([i 2])
    (for ([j 2])
      (when (not(courtyard-get (+ x i) (+ y j)))
      (courtyard-set (+ x i) (+ y j) color))))
  (set! color (randcolor)))

;finds already tiled/ missing tile then places tromino with regards to that
(define (place-trom size x y)
  (define coloredx 0)
  (define coloredy 0)

  ;finds missing tile
  (for ([i (in-range x (+ x size))])
    (for ([j (in-range y (+ y size))])
      (when (courtyard-get i j)
        (set! coloredx i)
        (set! coloredy j))))
  
;places trom considering missing tile
(cond
    [(and (< coloredx (+ x (/ size 2))) (< coloredy (+ y (/ size 2)))) (trom-br (+ x (/ size 2)) (+ y (/ size 2)))]
    [(and (>= coloredx (+ x (/ size 2))) (< coloredy (+ y (/ size 2)))) (trom-bl (- (+ x (/ size 2)) 1) (+ y (/ size 2)))]
    [(and (< coloredx (+ x (/ size 2))) (>= coloredy (+ y (/ size 2)))) (trom-tr (+ x (/ size 2)) (- (+ y (/ size 2)) 1))]
    [(and (>= coloredx (+ x (/ size 2))) (>= coloredy (+ y (/ size 2)))) (trom-tl (- (+ x (/ size 2)) 1) (- (+ y (/ size 2)) 1))]))

;(define num 0)

;recursive algorithm
(define (fill-courtyard recsize x y)
  (cond
    [(= input 0) (void)]
    [(eq? recsize 2) (change-tiles x y)]
    [else (begin
            (place-trom recsize x y)
            (fill-courtyard (/ recsize 2) x y)
            (fill-courtyard (/ recsize 2) (+ x (/ recsize 2)) y)
            (fill-courtyard (/ recsize 2) x (+ y (/ recsize 2)))
            (fill-courtyard (/ recsize 2) (+ x (/ recsize 2)) (+ y (/ recsize 2)))
            ;(cond
            ;[(< input 6) (disp)]
            ;[(> input 5) (println (list num (/ (log size) (log 2)) (remainder num (/ (log size) (log 2)))))]
            ;[(equal? (remainder num (* 2 size)) 0) (disp)])
            ;(set! num (+ num 1))
            )]))

(fill-courtyard size 0 0)
(println "Your courtyard has been tiled.")
(define end-time (current-inexact-milliseconds))
(println (~a "This took " ( / (- end-time start-time) 1000) " seconds"))