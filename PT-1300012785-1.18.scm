(define (slow-mul b n)
  (define (iter current base expt)
    (if (= expt 0)
      current
      (iter (+ current 
               (if (= (remainder expt 2) 1)
                 base 
                 0)) 
            (+ base base) 
            (quotient expt 2))))
  (iter 0 b n))
