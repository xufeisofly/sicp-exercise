(define (abs a)
  (if (< a 0)
      (* -1 a)
      a))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)

  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))

  (define (try guess)
    (let ((y (f guess)))
      (display y)
      (newline)
      (if (close-enough? guess y)
          y
          (try y))))
  (try first-guess))

(define (average a b)
  (\ (+ a b) 2.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

;; (fixed-point (lambda (x)
;;                (/ (log 1000)
;;                   (log x)))
;;              2.0)

;; (fixed-point (lambda (x)
;;                (/ (+ x
;;                      (/ (log 1000)
;;                         (log x)))
;;                   2))
;;              2.0)

;; (fixed-point (average-damp (lambda (x) (/ (log 1000)
;;                                           (log x))))
;;              2.0)


(define (iterative-improve good-enough? f)
  (lambda (x)
    (define (iter n)
      (if (good-enough? n)
          n
          (iter (f n))))
    (iter x)))
