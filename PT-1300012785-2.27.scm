(define (deep-reverse l)
  (define (iter result rest)
    (cond ((null? rest) 
            result)
          ((pair? (car rest)) 
            (iter (cons (deep-reverse (car rest)) result) (cdr rest) ))
          (else 
            (iter (cons (car rest) result) (cdr rest)))))
    (iter '() l))
