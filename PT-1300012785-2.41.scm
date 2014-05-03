;(use srfi-1)

(define (enumerate-integer s e)
  (if (> s e)
    '()
    (cons s (enumerate-integer (+ s 1) e))))

(define (flatmap op seq)
  (foldr append '() (map op seq)))

(define (triples-sum-to-s s n)
  (flatmap (lambda (i)
             (filter (lambda (triple)
                       (let ((k (car triple))
                             (j (cadr triple)))
                        (and (>= k 1)
                             (< k j)))) 
                     (map (lambda (j) 
                            (let ((k (- s i j)))
                             (list k j i))) 
                          (enumerate-integer 1 (- i 1)))))
           (enumerate-integer 1 n)))

(printf "~A~N" (length (triples-sum-to-s 2000 2000)))
