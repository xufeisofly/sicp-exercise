(define (even? x)
  (= 0 (remainder x 2)))

(define (odd? x)
  (not (even? x)))

(define (same-parity x . items)
  (define (select f l)
    (define (iter ll result)
      (if (null? ll)
          result
          (if (f (car ll))
              (iter (cdr ll) (cons result (car ll)))
              (iter (cdr ll) result))
          ))
    (iter l '()))

  (if (odd? x)
      (cons x (select odd? items))
      (cons x (select even? items))))
