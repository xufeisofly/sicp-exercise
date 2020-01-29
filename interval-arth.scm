(define (make-interval a b) (cons a b))

(define upper-bound cdr)

(define lower-bound car)

(define (sub-interval a b)
  (make-interval (- (lower-bound a) (upper-bound b))
                 (- (upper-bound a) (lower-bound b))))
