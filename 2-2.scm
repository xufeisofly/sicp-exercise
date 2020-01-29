(define (make-segment start-seg end-seg)
  (cons start-seg end-seg))

(define (start-segment line-seg)
  (car line-seg))

(define (end-segment line-seg)
  (cdr line-seg))

(define (make-point x-poi y-poi)
  (cons x-poi y-poi))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment line-seg)
  (let ((start-seg (start-segment line-seg))
        (end-seg (end-segment line-seg)))
    (let ((start-x-poi (x-point start-seg))
          (start-y-poi (y-point start-seg))
          (end-x-poi (x-point end-seg))
          (end-y-poi (y-point end-seg)))
      (make-point (/ (+ start-x-poi end-x-poi) 2)
                  (/ (+ start-x-poi start-y-poi) 2)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
