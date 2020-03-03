(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define nil '())

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil
              sequence))

(define (square x)
  (* x x))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(map square (list 1 2 3 4))

(append (list 1 2 3) (list 4 5 6))
