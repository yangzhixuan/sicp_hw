;;;  node
(define (make-node item)
  (cons item (cons '() '())))

(define (set-next-ptr! node v)
  (set-car! (cdr node) v))

(define (set-prev-ptr! node v)
  (set-cdr! (cdr node) v))

(define (node-item node)
  (car node))

(define (node-next-ptr node)
  (cadr node))

(define (node-prev-ptr node)
  (cddr node))
;;;  


;;; deque
(define (make-deque)
  (cons '() '()))

(define front-ptr car)

(define rear-ptr cdr)

(define set-front-ptr! set-car!)

(define set-rear-ptr! set-cdr!)


(define (empty-deque? q)
  (null? (front-ptr q)))

(define (front-insert-deque! q item)
  (let [(new-node (make-node item))]
   (cond [(empty-deque? q)
          (set-front-ptr! q new-node)
          (set-rear-ptr! q new-node)]
         [else
          (set-next-ptr! new-node (front-ptr q))
          (set-prev-ptr! (front-ptr q) new-node)
          (set-front-ptr! q new-node)])))

(define (rear-insert-deque! q item)
  (let [(new-node (make-node item))]
   (cond [(empty-deque? q)
          (set-front-ptr! q new-node)
          (set-rear-ptr! q new-node)]
         [else
          (set-prev-ptr! new-node (rear-ptr q))
          (set-next-ptr! (rear-ptr q) new-node)
          (set-rear-ptr! q new-node)])))

(define (print-deque q)
  (let loop [(node (front-ptr q))]
   (cond [(null? node) (printf "~N")]
         [else 
          (printf "~A " (node-item node))
          (loop (node-prev-ptr node))])))

(define (front-deque q)
  (node-item (front-ptr q)))

(define (rear-deque q)
  (node-item (rear-ptr q)))

(define (front-delete-deque! q)
  (cond [(empty-deque? q) (error "delete-front an empty deque")]
        [(eq? (front-ptr q) (rear-ptr q))
         (set-front-ptr! q '())
         (set-rear-ptr! q '())]
        [else
         (set-front-ptr! q (node-next-ptr (front-ptr q)))
         (set-prev-ptr! (front-ptr q) '())]))

(define (rear-delete-deque! q)
  (cond [(empty-deque? q) (error "rear-delete an empty deque")]
        [(eq? (front-ptr q) (rear-ptr q))
         (set-front-ptr! q '())
         (set-rear-ptr! q '())]
        [else
         (set-rear-ptr! q (node-prev-ptr (rear-ptr q)))
         (set-next-ptr! (rear-ptr q) '())]))

(define q (make-deque))
(front-insert-deque! q 1)
(front-insert-deque! q 5)
(rear-insert-deque! q 6)
(rear-insert-deque! q 8)
(print-deque q) ; 5 1 6 8
(printf "front: ~A~N" (front-deque q))
(printf "rear: ~A~N" (rear-deque q))

(front-delete-deque! q) ; 1 6 8 
(print-deque q)

(front-delete-deque! q) ; 6 8
(print-deque q)

(rear-insert-deque! q 9) ; 6 8 9
(print-deque q)

(rear-delete-deque! q) ; 6 8 
(print-deque q)

(rear-delete-deque! q) ; 6

(rear-delete-deque! q) ; empty deque

(print-deque q)
