(use srfi-1)

(define (enumerate-integer s e)
  (if (> s e)
    '()
    (cons s (enumerate-integer (+ s 1) e))))

(define (flatmap op seq)
  (foldl append '() (map op seq)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list '())
      (filter
        (lambda (position) (safe? position))
        (flatmap 
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (cons new-row rest-of-queens))
                 (enumerate-integer 1 board-size)))
          (queen-cols (- k 1))))))
  
  (define (safe? position)
    (define (check rest-queens x-axis-dist)
      (cond ((null? rest-queens) #t)
            ((equal? (car rest-queens) (car position)) #f)
            ((equal? x-axis-dist 
                     (abs (- (car rest-queens) (car position))))
             #f) ; two queens in the same diagonal
            (else (check (cdr rest-queens) (+ x-axis-dist 1)))))
    (check (cdr position) 1))

  (queen-cols board-size))

