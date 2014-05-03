(define (enumerate-integer s e)
  (if (> s e)
    '()
    (cons s (enumerate-integer (+ s 1) e))))

(define (flatmap op seq)
  (foldr append '() (map op seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list j i)) 
                  (enumerate-integer 1 (- i 1))))
           (enumerate-integer 1 n)))

