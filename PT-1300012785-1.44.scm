(define dx 0.00001)

(define (smooth f)
  (lambda (x) 
    (/ (+ 
         (f x) 
         (f (+ x dx)) 
         (f (- x dx))) 
       3)))

(define (smooth-n-times f n)
  ((repeated smooth n) f))

;functions from 1.42 and 1.43
(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 0)
    (lambda (x) x)    ;identity function
    (compose f (repeated f (- n 1)))))

;test
(printf "~A~N~A~N~A~N"
        (abs 0)
        ((smooth abs) 0)
        ((smooth-n-times abs 5) 0))

