(define no-more? null?)

(define (except-first-denomination items)
  (cdr items))

(define (first-denomination items)
  (car items))
