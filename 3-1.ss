(define (make-accumulator init)
  (let ((sum init))
    (lambda (num)
      (begin
        (set! sum (+ sum num))
        sum))))

(define A1 (make-accumulator 30))

(A1 20)
(A1 31)

(define A2 (make-accumulator 9))

(A2 1)
(A2 3)

;; 3-2
(define (make-monitored fn)
  (let ((count 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?)
             count)
            ((eq? arg 'reset-count)
             (begin
               (set! count 0)
               count))
            (else
             (begin
               (set! count (+ count 1))
               (fn arg)))))))

(define s (make-monitored sqrt))
(s 1000)
(s 100)

(s 'how-many-calls?)
(s 'reset-count)
(s 100)
(s 'how-many-calls?)

;; 3-3
(define (make-account balance pwd)
  (define count 0)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (make-joint new-pwd)
    (make-account balance new-pwd))

  (define (dispatch p m)
    (if (eq? p pwd)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'make-joint) make-joint)
              (else (error "Unknow request")))
        (begin
          (if (= count 3)
              (begin
                (set! count 0)
                'call-the-cops)
              (begin
                (set! count (+ count 1))
                (error "Incorrect Password"))))))
  dispatch)

(define acc (make-account 100 'password))
((acc 'password 'withdraw) 10)
((acc 'password 'withdraw) 20)
