(define (accumulate combiner init l)
  (if (null? l)
    init
    (combiner (car l)
              (accumulate combiner init (cdr l)))))

(define (map p l)
  (accumulate 
    (lambda (x y) (cons (p x) y))
    '()
    l))

(define (append l1 l2)
  (accumulate cons l2 l1))

(define (length l)
  (accumulate (lambda (x y) (+ 1 y)) 0 l))
