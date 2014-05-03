(define (fast-expt b n)
  (define (iter current base expt)
    (if (= expt 0)
      current
      (iter (* current 
               (if (= (remainder expt 2) 1)
                 base 
                 1)) 
            (* base base) 
            (quotient expt 2))))
  (iter 1 b n))
