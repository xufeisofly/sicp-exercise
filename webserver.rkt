#lang racket

(require web-server/servlet
         web-server/servlet-env)

(define (start req)
  (response/xexpr
   `(html (head (title "Hello World!"))
          (body (p "Hey out there!")))))

(serve/servlet start
               #:port 8080)
