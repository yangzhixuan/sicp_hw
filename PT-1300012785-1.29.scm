(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))

  (define (coefficient i)
    (cond ((or (= i 0) (= i n)) 1)
          ((even? i) 2)
          (else 4)))

  (define (iter result i)
    (if (> i n)
      result
      (iter (+ result (* (coefficient i) (f (+ a (* i h)))))
            (+ 1 i))))

  (* (/ h 3.0) (iter 0.0 0)))

(printf "~A\n~A\n" 
        (simpsons-rule (lambda (x) (* x x x)) 0 1 100)
        (simpsons-rule (lambda (x) (* x x x)) 0 1 1000))
