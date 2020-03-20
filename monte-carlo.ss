;; produce random number between low and high
(define (random-in-range low high) 'random)

(define (estimate-integral)
  (define (P-experiment)
    (let ((x (cons (random-in-range x-low x-high))) (y (cons (random-in-range y-low y-high))))
      (P x y)))
  (* (integral x y) (monte-carlo 100 (P-experiment))))


(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (begin
             (set! trials-passed (+ trials-passed 1))
             (set! trials-remaining (- trials-remaining 1))
             (iter trials-remaining trials-passed)))
          (else
           (begin
             (set! trials-remaining (- trials-remaining 1))
             (iter trials-remaining trials-passed)))))
  (iter trials 0))

