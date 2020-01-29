(define (derive f)
  (define dx 0.00001)
  (lambda (x) (/ (- (f (+ x dx))
                    (f x))
                 dx)))

(define (cube x)
  (* x x x))



(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

(define (compose f g)
  (lambda (x) (f (g x))))


(define (square x) (* x x))

(define (inc x) (+ x 1))

((compose square inc) 6)

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

((repeated square 2) 5)


(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

((repeated smooth 10) f)
