(define (cube-root x)
  (cube-root-iter 1.0 2.0 x))

(define (cube-root-iter guess old-guess x)
  (if (good-enough? guess old-guess x)
      guess
      (cube-root-iter (improve guess x) guess x)))

(define (improve guess x)
  (/ (+ (/ x
           (* guess guess))
        (* 2 guess))
     3))

(define (good-enough? guess old-guess x)
  (< (abs (- guess
             old-guess))
     (* guess 0.001)))

(define (abs x)
  (if (< x 0)
      (* -1 x)
      x))

(cube-root 27)
