(define table '())

(define (put type op f)
  (set! table (cons (list type op f) table)))

(define (get type op)
  (define (iter a)
    (if (not (pair? a))
        'failed
        (let ((row (car a)))
          (cond ((and (eq? (car row) type) (eq? (cadr row) op))
                 (car (cddr row)))
                (else
                 (iter (cdr a)))))))
  (iter table)


(define (install-A)
  (define (name obj) 'A)
  (put 'a 'name name))

(define (install-B)
  (define (name obj) 'B)
  (put 'b 'name name))

(define (make-obj type data)
  (cons type data))

(define (get-type obj)
  (car obj))

(install-A)
(install-B)

(get 'a 'name)

(define (apply-generic op obj)
  (let ((type (get-type obj)))
    (let ((f (get type op)))
      (apply f (list obj)))))

(apply-generic 'name (make-obj 'b 3))
