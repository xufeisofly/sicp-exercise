(define (square x)
  (* x x))

(define (square-list items)
  (define (iter things answer)
    (newline)
    (display answer)
    (if (null? things)
        answer
        (iter (cdr things) (cons (square (car things))
                                 answer))))
  (iter items '()))


;; (square-list '(1 2 3 4 5))

(define (for-each proc items)
  (if (null? items)
      true
      (begin
        (proc (car items))
        (for-each proc (cdr items)))))

(for-each (lambda (x)
            (newline)
            (display x))
          (list 1 2 3 4))
