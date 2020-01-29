(define (double a)
  (+ a a))

(define (fast-* a n)
  (if (even? n)
      (fast-*-iter 0 (/ n 2) (double a))
      (fast-*-iter 0 n a)))

(define (fast-*-iter product count a)
  (if (= count 0)
      product
      (fast-*-iter (+ product a) (- count 1) a)))

(fast-* 3 3)
