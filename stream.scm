(define-syntax mydelay
  (syntax-rules ()
    [(_ expr1 expr2 ...)
     (lambda () (begin expr1 expr2 ...))]))

(define (myforce promise)
  (promise))

(define-syntax stream-cons
  (syntax-rules ()
    [(_ a b)
     (cons a (mydelay b))]))

(define (stream-car p)
  (car p))

(define (stream-cdr p)
  (myforce (cdr p)))

(define (stream-null) '())

(define (stream-null? stream)
  (null? stream))

(define (stream-map op . ls)
  (let [(cars (map stream-car ls))]
   (if (memv stream-null cars)
     stream-null
     (stream-cons
       (apply op cars)
       (apply stream-map (cons op (map stream-cdr ls)))))))

(define ns
  (stream-cons 
    1
    (stream-map (lambda (x) (+ x 1)) ns)))

(define (print-stream s)
  (let loop ([i 0] [s s])
   (cond [(or (> i 20) (null? s)) #f]
         [else (printf "~A~N" (stream-car s)) 
          (loop (+ i 1) (stream-cdr s))])))
