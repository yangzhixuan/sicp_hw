(define tolerance 0.000001)

(define (fixed-point f guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
     (if (close-enough? guess next)
       next
       (try next))))
  (try guess))

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

(define dx 0.000001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (newton-sqrt a)
  (newton-method (lambda (x) (- a (* x x))) 1.0))

(define (cubic a b c)
  (lambda (x) 
    (+ (* x x x)
       (* a x x)
       (* b x)
       (* c))))

;test
(printf "~A~N~A~N"
        (newton-sqrt 2)
        (newton-method (cubic 1 2 3) 1.0))
