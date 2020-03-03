(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval n)
  (if (= n 0)
      nil
      (append (enumerate-interval (- n 1)) (list n))))

(map (lambda (i)
       (map (lambda (j) (list i j))
            (enumerate-interval (- i 1))))
     (enumerate-interval 8))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


(define (unique-pairs n)
  (if (= n 1)
      nil
      (flatmap (lambda (x)
                 (map (lambda (y) (list x y))
                      (enumerate-interval (- x 1))))
               (enumerate-interval n))))

(unique-pairs 5)

(define (all-ordered-triples n)
  (if (= n 1)
      nil
      (flatmap (lambda (x)
                 (map (lambda (y) (cons x y))
                      (unique-pairs (- x 1))))
               (enumerate-interval n))))


(all-ordered-triples 5)
