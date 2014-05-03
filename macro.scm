(define-syntax mylet
  (syntax-rules ()
    ((_ ((x e) ...) b1 b2 ...)
     ((lambda (x ...) b1 b2 ...) e ...))))

(define-syntax mylet*
  (syntax-rules ()
    [(_ () b1 b2 ...)
     (let () b1 b2 ...)]
    [(_ ((x1 e1) (x2 e2) ...) b1 b2 ...)
     (let ((x1 e1))
       (mylet* ((x2 e2) ...) b1 b2 ...))]))

(define-syntax myand
  (syntax-rules ()
    [(_) #t]            ; match zero argument
    [(_ e1)             ; match exactly one argument
     (if e1 e1 #f)]
    [(_ e1 e2 e3 ...)   ; general case
     (if e1 (myand e2 e3 ...) #f)]))

(define-syntax myor ; incorrect!
  (syntax-rules ()
    [(_) #f]
    [(_ e1 e2 ...)
     (let ([t e1])
       (if t t (myor e2 ...)))]))

(define-syntax mywhen
  (syntax-rules ()
    [(_ test expr ...)
     (if test
       (begin expr ...)
       #f)]))

(define-syntax myunless
  (syntax-rules ()
    [(_ test expr ...)
     (when (not test) expr ...)]))


(let ([x (call/cc (lambda (k) k))])
 (x (lambda (ignore) "time travel")))

(((call/cc (lambda (k) k))
  (lambda (x) x)) 
 "HEY!")

; print infinite integers without recursion.
(define (printing)
  (let ([k (call/cc (lambda (x) (cons x 0)))])
   (printf "~A~N" (cdr k))
   ((car k) (cons (car k) (+ (cdr k) 1)))))


(define (product l)
  (let loop ([result 1]
             [l l])
    (cond [(null? l) result]
          [(zero? (car l)) 0]
          [else (loop (* result (car l)) (cdr l))])))
