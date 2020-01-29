(define (reverse l)
  (if (= (length l) 1)
      (list (car l))
      (append (reverse (cdr l)) (list (car l)))))

(reverse (list 1 2 3))

(define (fringe tree)
  (define nil '())

  (if (null? tree)
      nil
      (let ((first (car tree)))
        (if (not (pair? first))
            (cons first (fringe (cdr tree)))
            (append (fringe first) (fringe (cdr tree)))))))

(fringe (list (list 1 2) (list 3 4)))
