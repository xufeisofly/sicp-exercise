(define (expmod base exp mod)
  (remainder (expt base exp)
             mod))

(define (expt base exp)
  (cond ((= exp 0) 1)
        (else (* base
                 (expt base (- exp 1))))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1(random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 561 561)
