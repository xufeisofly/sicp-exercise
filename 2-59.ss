;; exercise 2.59

(define (union-set set1 set2)
  (if (element-of-set? (car set1) set2)
      (union-set (cdr set1) set2)
      (cons (car set1) (union-set (cdr set1) set2))))

;; exercise 2.60

;; element-of-set will be the same with non-duplicated version
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))


(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) (remove-set-element (car set1) set2))))
        (else (intersection-set (cdr set1) set2))))

(define (remove-set-element x set)
  (cond ((null? set) '())
        ((equal? x (car set)) (cdr set))
        (else (remove-set-element x (cdr set)))))
