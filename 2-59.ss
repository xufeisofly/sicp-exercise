(define (union-set set1 set2)
  (if (element-of-set? (car set1) set2)
      (union-set (cdr set1) set2)
      (cons (car set1) (union-set (cdr set1) set2))))
