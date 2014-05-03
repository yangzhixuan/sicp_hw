;(use extras)
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

(define (count-prime n)
  (if (< n 0)
    0
    (+ (count-prime (- n 1)) (if (miller-rabin n 10) 1 0))))

(define (find-primes-after n k)
  (if (= k 0)
    '()
    (if (miller-rabin n 20)
      (cons n (find-primes-after (+ 1 n) (- k 1)))
      (find-primes-after (+ 1 n) k))))
