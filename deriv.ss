(define (variable? x) (symbol? x))

(define (same-variable? x1 x2)
  (and (variable? x1) (variable? x2) (eq? x1 x2)))

(define (make-sum x1 x2) (list '+ x1 x2))

(define (make-product x1 x2) (list '* x1 x2))

(define (sum? exp)
  (and (pair? exp) (eq? (car exp) '+)))

(define (product? exp)
  (and (pair? exp) (eq? (car exp) '*)))

(define (addend exp) (cadr exp))

(define (augend exp) (caddr exp))

(define (multiplier exp) (cadr exp))

(define (multiplicand exp) (caddr exp))

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
        (else
         (error "unknow expression type: DERIV" exp))))


(deriv '(* x (+ y x)) 'x)
