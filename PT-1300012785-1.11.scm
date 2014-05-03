(define (f-recursive n)
  (if (< n 3) n (+ (f-recursive (- n 1))
                 (* 2 (f-recursive (- n 2)))
                 (* 3 (f-recursive (- n 3))))))

(define (f-iterative n)
  (define (iter count f-n f-n-1 f-n-2)
    (if (= count n)
      f-n
      (iter (+ count 1)
            (+ f-n (* 2 f-n-1) (* 3 f-n-2))
            f-n
            f-n-1)))
  (if (< n 3)
    n
    (iter 2 2 1 0)))
