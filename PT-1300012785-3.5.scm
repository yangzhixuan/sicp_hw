(define (random-in-range low high)
  (define (random-float n)
    (* n (/ (random 32768) 32768.0)))
  (+ low (random-float (- high low))))

(define (monte-carlo trials experiments)
  (let loop ((n trials)
             (passed 0))
    (if (= n 0) 
      (/ passed trials)
      (loop (- n 1)
            (+ passed (if (experiments) 1 0))))))

(define (estimate-pi-by-gcd trials)
  (define (cesaro-test)
    (= (gcd (random 32768) (random 32768)) 1))
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (* (* (- x2 x1) (- y2 y1))
     (monte-carlo trials
                  (lambda () (p (random-in-range x1 x2)
                                (random-in-range y1 y2))))))

(define (estimate-pi-by-integral trials)
  (estimate-integral (lambda (x y) 
                       (<= (+ (* y y) (* x x)) 1))
                     -1 1 -1 1 trials))

