(define (abs x)
  (if (< x 0)
      (* -1 x)
      x))

(define (square x)
  (* x x))

(define (sqrt-iter guess old-guess x)
  (if (good-enough? guess old-guess x)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess old-guess x)
  (< (abs (- guess old-guess)) (* guess 0.001)))

(define (sqrt x)
  (sqrt-iter 1.0 2.0 x))

(define (new-if predicate then-clause else-clause)
  (if (predicate)
      then-clause
      else-clause))

(sqrt 0.0001)



