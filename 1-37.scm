(define (cont-frac n d k)
  (define (try m)
    (if (= m k)
        (/ (n m)
           (d m))
        (/ (n m)
           (+ (d m)
              (try (+ 1 m))))))
  (try 1.0))


(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           2)
