(define (double f)
  (lambda (x)
    (f (f x))))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 0)
    (lambda (x) x)    ;identity function
    (compose f (repeated f (- n 1)))))
