;;; Generic Arithmetic

;;; Code for creating the table, you don't need to worry about this.

(define (make-table)
  (let ((local-table (list '*table*)))

    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))


(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))




;;; The bottom level typing system

(define attach-tag cons)

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
         ((number? datum) 'scheme-number)
         (else (error "Bad typed datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
         ((number? datum) datum)
         (else (error "Bad typed datum -- CONTENTS" datum))))


;;; The apply-generic mechanism.  
;;;  Note that we don't deal with coercion here.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for the given types -- APPLY-GENERIC"
                 (list op type-tags))))))


;;; Some generic arithmetic procedures

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (negate x) (apply-generic 'negate x))


;;; The rational number package

(define (install-rational-package)
  ;; internal procedures
  (define numer car)
  (define denom cdr)
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (negate x)
    (make-rat (- (numer x)) (denom x)))
  
  (define (=zero? x)
    (= (numer x) 0))

  ;; interfaces
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put '=zero? '(rational) =zero?)

  (put 'negate '(rational) negate)

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-scheme-number n)
  ((get 'make 'scheme-number n)))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Your code goes here ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-scheme-number-package)
  (put 'add '(scheme-number scheme-number) +)
  (put 'sub '(scheme-number scheme-number) -)
  (put 'mul '(scheme-number scheme-number) *)
  (put 'div '(scheme-number scheme-number) /)
  (put '=zero? '(scheme-number) 
       (lambda (x) (equal? x 0)))
  (put 'negate '(scheme-number) 
       (lambda (x) (- x)))
  (put 'make 'scheme-number
       (lambda (x) x))
  'done)


;;; underlying package for complex number
(define (install-rectangular-package)
  (define (square x) (* x x))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (tag x) (attach-tag 'rectangular x))

  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))

  (define (make-from-mag-ang x y)
    ((get 'make-from-mag-ang 'rectangular) x y))

  (define (add z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))

  (define (mul z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z2) (angle z2))))

  (define (div z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z2) (angle z2))))

  (define (=zero? z)
    (and (equal? 0 (real-part z)) (equal? 0 (imag-part z))))
  
  (define (tag z) (attach-tag 'complex z))

  (put 'add '(complex complex) (lambda (z1 z2) (tag (add z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div z1 z2))))
  (put 'make-from-real-imag 'complex 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex 
       (lambda (x y) (tag (make-from-mag-ang x y))))
  (put '=zero?  '(complex) =zero?)
  (put 'negate '(complex) 
       (lambda (z) 
         (make-from-real-imag (- (real-part z))
                              (- (imag-part z)))))
  'done)



(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? x y) (and (variable? x) (variable? y) (equal? x y)))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))

  (define (negate-poly p1)
    (make-poly (variable p1)
               (map (lambda (x) (list (car x) (negate (cadr x))))
                    (term-list p1))))

  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))

  (define (=zero?-poly p1)
    (define (myand l)
      (cond ((null? l) #t)
            ((car l) (myand (cdr l)))
            (else #f)))
    (or (null? (term-list p1))
        (myand (map (lambda (x) (cadr x)) (term-list p1)))))

  ; Representing term lists

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                      t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                      t2 (add-terms L1 (rest-terms L2))))
                   (else
                     (adjoin-term
                       (make-term (order t1)
                                  (add (coeff t1) (coeff t2)))
                       (add-terms (rest-terms L1)
                                  (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
       (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial) 
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial)
       =zero?-poly)
  (put 'negate '(polynomial)
       negate-poly)

  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)


;; Constructor
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;;; Some basic testing

(install-rational-package)
(install-scheme-number-package)
(install-rectangular-package)
(install-complex-package)
(install-polynomial-package)

;;; test cases
(assert (equal? (add 5 4) 9))
(assert (equal? (=zero? 0) #t))
(assert (equal? (=zero? 1) #f))
(assert (=zero? (make-rational 0 5)))
(assert (not (=zero? (make-rational 1 5))))
(assert (=zero? (sub (make-rational 1 2) (make-rational 4 8))))
(assert (not (=zero? (make-complex-from-real-imag 0 4))))
(assert (=zero? (make-complex-from-real-imag 0 0)))


(define z1 (make-complex-from-real-imag 5 3))
(define z2 (make-complex-from-mag-ang 1 4))
(printf "~A~N" (add z1 z2))

(printf "~A~N" (sub (make-rational 1 2)
                    (make-rational 1 3)))


(define p1 (make-polynomial 'x '((1 -1))))
(define p2 (make-polynomial 'x '((0 1))))
(printf "~A~N" (sub p1 p2))
