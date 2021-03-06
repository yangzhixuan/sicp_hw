(define (cube-root x)
  (define (good-enough? y)
    (< (abs (- x (* y y y))) 0.000001))
  (define (improve y)
    (/ (+ (/ x (* y y)) (* 2 y)) 3))
  (define (iter y)
    (if (good-enough? y)
      y
      (iter (improve y))))
  (iter 1.0))
