(define (expmod base exp mod)
  (remainder (expt base exp)
             mod))

(define (expt base exp)
  (cond ((= exp 0) 1)
        (else (* base
                 (expt base (- exp 1))))))

(define (try-it a n)
  (= (expmod a n n) a))

(define (fermat-test a n)
  (cond ((= a 1) true)
        ((try-it a n) (fermat-test (- a 1) n))
        (else false)))

(define (prime? n)
  (fermat-test (- n 1) n))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (even? n)
  (= (remainder n 2) 0))

(define (next n)
  (cond ((even? n) (+ n 1))
        (else (+ n 2))))

(define (search-for-primes min max)
  (timed-prime-test min)
  (cond ((< min max) (search-for-primes (+ 1 min) max))
        (else true)))


(search-for-primes 1000 1100)
