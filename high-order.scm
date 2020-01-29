(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (even? n)
  (= 0 (remainder n 2)))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (inc m) (+ m 1))
  (define (yk k) (f (+ a (* k h))))
  (define (simpson-term k)
    (* (yk k)
       (cond ((or (= k n) (= 0 k)) 1)
             ((even? k) 2)
             (else 4))))
  (* (/ h 3)
     (sum simpson-term 0 inc n)))


(define (product term a next b conn null-value filter)
  (if (> a b)
      null-value
      (if (filter a)
          (conn (term a)
                (product term (next a) next b conn null-value filter))
          (conn null-value
                (product term (next a) next b conn null-value filter)))))

(define (multiply a b)
  (* a b))

(define (test-formula a b)
  (define (filter? a)
    true)
  (define (incr n)
    (+ n 1))
  (define (term k)
    (define num
      (* (floor (/ (+ k 2) 2))
         2))
    (define den
      (cond ((even? k) (- num 1))
            (else (+ num 1))))
    (/ num den))

  (product term a incr b multiply 1.0 filter?))

(* 4 (test-formula 1 100))
