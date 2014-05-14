(load "misc.scm")
(load "properties.scm")

(define (vertical-box . args)
  (parse-args args contents properties)

  (define (to-html)
    (apply
      make-div
      (cons `(("style" ,(string-append "width:100%;"
                                       (css-handler properties))))
            (map (lambda (c) (string-append (c 'to-html) "\n")) contents))))

  (lambda (msg)
    (match msg
           ['type 'content]
           ['to-html (to-html)])))


(define (fixed-width-n-columns-box width-list contents-list properties)
  (define (to-html)
    (let ([num-cols (length contents-list)])
     (define columns
       (map (lambda (content width)
              (make-div `(("style" ,(format "height:100%; width:~A%; float:left;" 
                                            (exact-floor (* width 100)))))
                        (content 'to-html)))
            contents-list width-list))
     (define clear-float-div
       (make-div `(("style" "float:clear"))))

     (apply make-div (cons `(("style" ,(string-append "height:100%; width:100%"
                                                      (css-handler properties))))
                           (append columns (list clear-float-div))))))
  (lambda (msg)
    (match msg
           ['type 'content]
           ['to-html (to-html)])))

(define (n-columns-box . args)
  (parse-args args contents properties)
  (let ([len (length contents)])
    (fixed-width-n-columns-box (build-list len (lambda (x) (/ 1.0 len)))
                               contents
                               properties)))


(define (fixed-height-n-rows-box height-list contents-list properties)
  (define (to-html)
    (define columns
      (map (lambda (content height)
             (make-div `(("style" ,(format "height:~A%; width:100%;" 
                                           (exact-floor (* 100 height)))))
                       (content 'to-html)))
           contents-list height-list))

    (apply make-div (cons `(("style" 
                             ,(string-append "height:100%; width:100%"
                                             (css-handler properties))))
                          columns)))
  (lambda (msg)
    (match msg
           ['type 'content]
           ['to-html (to-html)])))

(define (n-rows-box . args)
  (parse-args args contents properties)
  (let ([len (length contents)])
    (fixed-height-n-rows-box (build-list len (lambda (x) (/ 1.0 len)))
                               contents
                               properties)))
