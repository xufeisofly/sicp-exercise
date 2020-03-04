(define (variable? x) (symbol? x))

(define (same-variable? x1 x2)
  (and (variable? x1) (variable? x2) (eq? x1 x2)))

(define (make-sum x1 x2) (list x1 'x x2))

(define (make-product x1 x2) (list x1 '* x2))

(define (sum? exp)
  (and (pair? exp) (eq? (car exp) '+)))

(define (product? exp)
  (and (pair? exp) (eq? (car exp) '*)))

(define (addend exp) (cadr exp))

(define (augent exp)
  (let ((a (cddr exp)))
    (if (= (length a) 1)
        (car a)
        (make-sum-list a))))

(define (multiplier exp) (cadr exp))

(define (multiplicand exp) (caddr exp))

(define (exponent? exp)
  (and (pair? exp) (eq? (car exp) '^)))

(define (make-exponent base exponentant) (list '^ base exponentant))

(define (base exp) (cadr exp))

(define (exponentant exp) (caddr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum
                         (make-product (multiplier exp)
                                       (deriv (multiplicand exp) var))
                         (make-product (multiplicand exp)
                                       (deriv (multiplier exp) var))))
        ((exponent? exp) (make-product (make-product (exponentant exp)
                                                     (make-exponent (base exp) (- (exponentant exp) 1)))
                                       (deriv (base exp) var)))
        (else
         (error "unknow expression type: DERIV" exp))))


(memq 'x '(+ x 2 (4 x)))
