(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(define (fast-expt b n)
  (if (even? n)
      (fast-expt-iter 1
                      (/ n 2)
                      (square b))
      (fast-expt-iter 1 n b)))

(define (fast-expt-iter product count b)
  (if (= count 0)
      product
      (fast-expt-iter (* b product) (- count 1) b)))

(fast-expt 2 3)
