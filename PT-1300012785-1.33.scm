(define (filtered-accumulate combiner null-value filter term a next b)
  (define (iter result a)
    (let ((value (term a)))
     (cond ((> a b) result)
           ((filter value) (iter (combiner result value) (next a)))
           (else (iter result (next a))))))
  (iter null-value a))

; sum from 1 to 100
(filtered-accumulate + 0 (lambda (x) #t) (lambda (x) x) 1 (lambda (x) (+ x 1)) 100)

(define (sum-primes-between a b)
  (filtered-accumulate 
    +
    0
    (lambda (x) (miller-rabin x 20))
    (lambda (x) x)
    a
    (lambda (x) (+ x 1))
    b))

(define (product-relative-prime n)
  (filtered-accumulate 
    *
    1
    (lambda (x) (= (gcd x n) 1)) (lambda (x) x)
    1
    (lambda (x) (+ x 1))
    (- n 1)))

;greatest-common-divisor function
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

;prime test function from ex1.29
(define (miller-rabin n times)
  (define (mr-exp-mod base expt)
    (if (= expt 0) 
      1
      (let* ((half (mr-exp-mod base (quotient expt 2)))
             (squared-half (remainder (* half half) n)))
        (cond ((and (= squared-half 1)
                    (not (= half 1)) 
                    (not (= half (- n 1))))
               0)
              ((even? expt) squared-half)
              (else (remainder (* base squared-half) n))))))

  (define (test)
    (define (try-it a)
      (= (mr-exp-mod a (- n 1)) 1))
    (try-it (+ 1 (random (- n 1)))))

  (cond ((< n 2) #f)
        ((= times 0) #t)
        (else (and (test) (miller-rabin n (- times 1))))))
