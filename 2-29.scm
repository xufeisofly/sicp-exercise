(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (if (null? mobile)
      0
      (let ((first (car mobile)))
        (if (not (pair? first))
            (+ first (total-weight (cdr mobile)))
            (+ (total-weight first) (total-weight (cdr mobile)))))))
