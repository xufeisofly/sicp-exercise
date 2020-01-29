(define (smallest-divisor n)
  (find-divisor-iter n 2))

(define (find-divisor-iter n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor-iter n (+ test-divisor 1)))))

(define (divides? n divisor)
  (= (remainder n divisor) 0))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)
