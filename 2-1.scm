(define (make-rat n d)
  (cond ((< d 0) (cons (* -1 n) (* -1 d)))
        (else (cons n d))))

