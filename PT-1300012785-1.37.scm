(define (cont-frac n d k)
  (define (iter result i)
    (if (< i 1)
      result
      (iter (/ (n k) (+ (d k) result)) (- i 1))))
  (iter 0 k))

; k is at least 11 for 4-digit precision
(cont-frac (lambda (x) 1.0) (lambda (x) 1.0) 11)

(define (cont-frac-recursive n d k)
  (define (recur i)
    (if (> i k)
      0
      (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1)) 

