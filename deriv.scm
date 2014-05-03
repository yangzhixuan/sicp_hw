; 1. 算术运算符支持多参数: ok
; 2. 基本初等函数的导数: ok
; 3. 对多项式进行化简：ok
; 4. 将中缀表达式翻译成前缀: 50%
; 5. 打印出相应的LaTeX公式、调用gnuplot绘制图像


(define (deriv original-expr var)

  ;---------------------------------------------------
  ; 各类谓词，用于判断表达式的类型
  (define (variable? expr) (symbol? expr))

  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (equal? v1 v2)))

  ; (add-predicate (sub? '+) (sub? '-)) ---> (begin (define ) )
  (define-syntax add-predicate
    (syntax-rules ()
      ((add-predicate) (void))
      ((add-predicate (name sym) more ...)
       (begin (define (name expr)
                (and (pair? expr) (equal? sym (car expr))))
              (add-predicate more ...)))))

  (add-predicate (sum? '+) (sub? '-) (product? '*)
                 (div? '/) (exponential? '**) (sin? 'sin)
                 (cos? 'cos) (log? 'log))

  (define (first-argument expr)
    (cadr expr))

  (define (second-argument expr)
    (caddr expr))

  (define (rest-argument expr)
    (cddr expr))
  ;---------------------------------------------------

  ;---------------------------------------------------
  ; 多项式相关函数，用于对多项式的表达式化简为标准形式
  (define (sum-poly poly1 poly2)
    (cond ((null? poly1) poly2)
          ((null? poly2) poly1)
          (else (cons (+ (car poly1) (car poly2)) (sum-poly (cdr poly1) (cdr poly2))))))

  (define (mul-poly poly1 poly2)
    (cond ((null? poly2) (list 0))
          (else (sum-poly (map (lambda (x) (* x (car poly2))) poly1)
                          (cons 0 (mul-poly poly1 (cdr poly2)))))))

  (define (expr->polynomial expr)
    (cond ((number? expr) (list expr))
          ((variable? expr) (list 0 1))
          (else (let ((first-poly (expr->polynomial (first-argument expr)))
                      (second-poly (expr->polynomial (second-argument expr))))
                  (cond ((sum? expr) (sum-poly first-poly second-poly))
                        ((product? expr) (mul-poly first-poly second-poly))
                        ((sub? expr) (sum-poly first-poly (mul-poly (list -1) second-poly)))
                        ((exponential? expr) 
                         (let loop ((k (car second-poly)))
                          (if (= k 0)
                            (list 1)
                            (mul-poly first-poly (loop (- k 1))))))
                        (else (error "can't convert to poly --- " expr)))))))

  (define (polynomial->expr poly)
    (define (recur rest k)
      (if (null? rest)
        0
        (list '+ 
              (list '* (car rest) (list '** var k)) 
              (recur (cdr rest) (+ k 1)))))
    (simplify (recur poly 0)))

  (define (simplify-poly expr)
    (polynomial->expr (expr->polynomial expr)))

  (define (polynomial? expr)
    (cond ((or (same-variable? expr var) (number? expr)) #t)
          ((and (or (product? expr) (sum? expr) (sub? expr))
                (polynomial? (first-argument expr))
                (polynomial? (second-argument expr))) #t)
          ((and (exponential? expr)
                (polynomial? (first-argument expr))
                (number? (second-argument expr))
                (>= (second-argument expr) 0)) #t)
          (else #f)))
  ;---------------------------------------------------


  ;---------------------------------------------------
  ; 化简表达式：1. 将多参形式化成单参形式，2. 执行明显的运算
  (define (simplify expr)
    (define (simplify-product a1 a2)
      (cond ((equal? a1 1) a2)
            ((equal? a2 1) a1)
            ((or (equal? a1 0) (equal? a2 0)) 0)
            ((and (number? a1) (number? a2)) (* a1 a2))
            (else (list '* a1 a2))))

    (define (simplify-add a1 a2)
      (cond ((equal? a1 0) a2)
            ((equal? a2 0) a1)
            ((and (number? a1) (number? a2)) (+ a1 a2))
            (else (list '+ a1 a2))))

    (define (simplify-exponential a1 a2)
      (cond ((equal? a2 0) 1)
            ((equal? a2 1) a1)
            ((and (number? a1) (number? a2)) (expt a1 a2))
            (else (list '** a1 a2))))

    (define (simplify-sub a1 a2)
      (cond ((equal? a2 0) a1)
            ((and (number? a1) (number? a2)) (- a1 a2))
            ((equal? a1 a2) 0)
            (else (list '- a1 a2))))

    (define (simplify-div a1 a2)
      (cond ((equal? a2 1) a1)
            ((and (number? a1) (number? a2)) (/ a1 a2))
            ((equal? a1 a2) 1)
            ((number? a2) (list '* (/ 1 a2) a1))
            (else (list '/ a1 a2))))

    (define (simplify-sin a1)
      (cond ((number? a1) (sin a1))
            (else (list 'sin a1))))

    (define (simplify-cos a1)
      (cond ((number? a1) (cos a1))
            (else (list 'cos a1))))

    (define (simplify-log a1)
      (cond ((number? a1) (log a1))
            (else (list 'log a1))))

    (cond ((product? expr)
           (cond ((= (length expr) 1) 1)
                 ((= (length expr) 2) (simplify (first-argument expr)))
                 (else (simplify-product 
                         (simplify (first-argument expr)) 
                         (simplify (cons '* (rest-argument expr)))))))
          ((sum? expr)
           (cond ((= (length expr) 1) 0)
                 ((= (length expr) 2) (simplify (first-argument expr)))
                 (else (simplify-add 
                         (simplify (first-argument expr)) 
                         (simplify (cons '+ (rest-argument expr)))))))

          ((sub? expr)
           (cond ((= (length expr) 2) (simplify (list '- 0 (first-argument expr))))
                 (else (simplify-sub 
                         (simplify (first-argument expr)) 
                         (simplify (cons '+ (rest-argument expr)))))))

          ((div? expr)
           (cond ((= (length expr) 2) (simplify (list '/ 1 (first-argument expr))))
                 (else (simplify-div
                         (simplify (first-argument expr)) 
                         (simplify (cons '* (rest-argument expr)))))))

          ((exponential? expr) 
           (simplify-exponential 
             (simplify (first-argument expr))
             (simplify (second-argument expr))))

          ((sin? expr)
           (simplify-sin 
             (simplify (first-argument expr))))

          ((cos? expr)
           (simplify-cos 
             (simplify (first-argument expr))))

          ((log? expr)
           (simplify-log 
             (simplify (first-argument expr))))

          (else expr)))
  ;---------------------------------------------------

  (define expr (simplify original-expr))
  (printf "simplify ~A to ~A~N" original-expr expr)

  ;---------------------------------------------------
  ; 求导的法则
  (define result 
    (simplify 
      (cond ((number? expr) 0)

            ((variable? expr)
             (if (same-variable? var expr) 1 0))

            ((sum? expr)
             (list '+ (deriv (first-argument expr) var)
                   (deriv (second-argument expr) var)))

            ((sub? expr)
             (list '- (deriv (first-argument expr) var)
                   (deriv (second-argument expr) var)))

            ((div? expr)
             (list '/
                   (list '-
                         (list '* (deriv (first-argument expr) var) (second-argument expr))
                         (list '* (first-argument expr) (deriv (second-argument expr) var)))
                   (list '** (second-argument expr) 2)))

            ((product? expr)
             (let ((first (first-argument expr))
                   (second (second-argument expr)))
               (list '+  (list '* (deriv first var) second)
                     (list '* (deriv second var) first))))

            ((exponential? expr)
             (let ((u (first-argument expr))
                   (n (second-argument expr)))
               (cond ((number? n)
                      (list '* n (list '** u (- n 1)) (deriv u var)))
                     ((number? u)
                      (list '* (log u) (list '** u n) (deriv n var)))
                     (else (error "can't handle this exp yet --- " expr)))))

            ((log? expr)
             (let ((u (first-argument expr)))
              (list '/ (deriv u var) u)))

            ((sin? expr)
             (let ((u (first-argument expr)))
              (list '* (list 'cos u) (deriv u var))))

            ((cos? expr)
             (let ((u (first-argument expr)))
              (list '* (list '- (list 'sin u)) (deriv u var))))

            (else
              (error "unknown expression type" expr)))))
  ;---------------------------------------------------

  ;---------------------------------------------------
  ; 如果结果由多项式构成的话，将多项式化为标准形式
  (cond ((polynomial? result) (simplify-poly result))
        ((and (= (length result) 3) 
              (polynomial? (first-argument result))
              (polynomial? (second-argument result)))
         (simplify (list (car result)
                         (simplify-poly (first-argument result))
                         (simplify-poly (second-argument result)))))
        (else result)))


(define ** expt)

; (eval-at (deriv '(** x 5) 'x) ('x 2))
(define (eval-at expr . args)
  (eval `(let ,args ,expr)))

; (expr->lambda (deriv '(** x 5)) '(x y))
(define (expr->lambda expr vars)
  (eval `(lambda ,vars ,expr)))


(define (S->infix expr)
  (cond ((not (pair? expr)) expr)
        (else (list (S->infix (cadr expr)) (car expr) (S->infix (caddr expr))))))

(define (infix->S expr)
  (cond ((not (pair? expr)) expr)
        (else (list (cadr expr) (infix->S (car expr)) (infix->S (caddr expr))))))
