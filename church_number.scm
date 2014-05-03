(define zero
  (lambda (f)
    (lambda (x) x)))

(define one
  (lambda (f)
    (lambda (x) (f x))))

(define two
  (lambda (f)
    (lambda (x) (f (f x)))))

(define (add-1 n)
  (lambda (f)
    (lambda (x) (f ((n f) x)))))

(define (add-church n m)
  (lambda (f)
    (lambda (x) ( (n f) ((m f) x)))))

(define (church->int n)
  (define (inc x) (+ x 1))
  ((n inc) 0))

