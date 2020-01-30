(define (square-tree tree)
  "directly"
  (define nil '())

  (if (null? tree)
      nil
      (let ((first (car tree)))
        (if (not (pair? first))
            (cons (square first) (square-tree (cdr tree)))
            (cons (square-tree first) (square-tree (cdr tree)))))))

(square-tree (list (list 1 2) (list 3 4)))

(define (square-tree tree)
  "use map"
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(square-tree (list (list 1 2) (list 3 4)))
