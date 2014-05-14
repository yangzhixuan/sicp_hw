(load "misc.scm")
(load "properties.scm")
(load "layouts.scm")


;---------------impress function; the main routine----------------------

(define (content? obj)
  (eq? (obj 'type) 'content))

(define (impress . args)
  (parse-args args contents properties css-properties)

  ; default value for properties
  (define default-properties
    (list (impress.js-path "js/impress.js")
          (css-path "css/default.css")
          (charset "utf-8")
          (viewpoint "width=device-width, minimum-scale=1, maximum-scale=1, user-scalable=no")
          (output-filename (current-output-port))
          (slide-flow-style "matrix")))

  ; map property into html tags
  (define (property-handler property)
    (let ([key (property 'key)] 
          [val (property 'val)])
      (match key
             ['title (make-title val)]
             ['css-path (make-css-link val)]
             ['charset (make-meta-charset val)]
             ['viewpoint (make-meta-viewpoint val)]
             [other ""])))

  ; use the default value if some property isn't specified
  (set! properties (merge-properties properties default-properties))

  ; set slides' position if a slide-flow style is specified
  (let* ([style (lookup-table properties 'slide-flow-style)]
         [handler (assoc (style 'val) slide-flow-style-handlers)])
    (if handler
      (apply (cadr handler) (cons contents (style 'additional)))
      (error "impress: unknown slide-flow style: " (style 'val))))

  ; write to file if specified
  (let* ([name ((lookup-table properties 'output-filename) 'val)]
         [port (if (string? name) (open-output-file name #:exists 'replace) name)])
    (display 
      (make-html
        (apply make-head
               (append (map property-handler properties)
                       (list (make-style (css-all-selector (css-handler css-properties))))))
        (make-body
          (apply make-div 
                 (cons `(("id" "impress"))
                       (map (lambda (x) (x 'to-html)) contents)))
          (make-outside-script ((lookup-table properties 'impress.js-path) 'val))
          (make-inside-script "impress().init();")))
      port)
    (newline port)
    (when (string? name) (close-output-port port)))

  #t)

;--------------------- slide-flow handler routines------------------------------
(define slide-flow-style-handlers '())

(define (add-slide-flow-style-handler! name proc)
  (set! slide-flow-style-handlers 
    (cons (list name proc)
          slide-flow-style-handlers)))

(define (position-specified? content)
  (let ([get (content 'get-property)])
   (or (get 'data-x) 
       (get 'data-y)
       (get 'data-z))))

(define (not-p p)
  (lambda (x) (not (p x))))

(define (matrix-style contents [num-cols 5])
  (let* ([contents (filter (not-p position-specified?) contents)]
         [size (length contents)])
    (let loop ([count 0]
               [rest contents])
      (unless (null? rest)
        (((car rest) 'set-property!) (x-coordinate (* 1000 (remainder count num-cols))))
        (((car rest) 'set-property!) (y-coordinate (* 1000 (quotient count num-cols))))
        (loop (+ count 1) (cdr rest))))))

(add-slide-flow-style-handler! "matrix" matrix-style)


(define (circle-style contents radius)
  (let* ([contents (filter (not-p position-specified?) contents)]
         [size (length contents)])

    (let loop ([count 0]
               [rest contents])
      (unless (null? rest)
        ; shift the coordinate to avoid a bug of chrome, not necessary for firefox
        (((car rest) 'set-property!) (x-coordinate 
                                       (+ 50000 (* radius  
                                                   (cos (* pi -2 (/ count size)))))))
        (((car rest) 'set-property!) (y-coordinate 
                                       (+ 50000 (* radius 
                                                   (sin (* pi -2 (/ count size)))))))
        (loop (+ count 1) (cdr rest))))))

(add-slide-flow-style-handler! "circle" circle-style)

;---------------------- slide --------------------------------------------------
(define (slide . args)
  (parse-args args contents properties css-properties)

  ; map property to div property pair
  (define (property-handler p)
    (let ([key (p 'key)]
          [val (p 'val)])
      (cond [(regexp-match-exact? (regexp "data-.+") 
                                  (symbol->string key))
             (list key val)]
            [else (error "slide: unknown property " key " " val)])))

  (define (to-html)
    (make-div
      (append (list `("class" "step slide")
                    `("style" ,(css-handler css-properties)))
              (map property-handler properties))
      ((apply vertical-box contents) 'to-html)))

  (define (set-property! new-property)
    ;(printf "set property: ~A = ~A~N" (new-property 'key) (new-property 'val))
    (set! properties 
      (merge-properties properties (list new-property))))

  (lambda (msg)
    (match msg 
           ['to-html  (to-html)]
           ['type 'content]
           ['set-property! set-property!]
           ['get-property (lambda (key) (lookup-table properties key))]
           [other (error "slide: unknown message " msg)])))

(define (text str . css-properties)
  (lambda (msg)
    (match msg
           ['to-html 
            (make-p `(("style" ,(css-handler css-properties)))
                    (make-span '()
                               str))]
           ['type 'content])))

(define (strong-text str . css-properties)
  (lambda (msg)
    (match msg
           ['to-html
            (make-p `(("style" ,(css-handler css-properties))) 
                    (make-strong '() 
                                 (make-span '() str)))]
           ['type 'content])))


(define (general-list make-list . args)
  (parse-args args contents properties css-properties)

  (define (to-html)
    (define items-html
      (map (lambda (x)
             (make-li '() (x 'to-html)))
           contents))

    (apply make-list
           (cons `(("style" ,(css-handler css-properties)))
                  items-html)))
  
  (lambda (msg)
    (match msg
           ['to-html (to-html)]
           ['type 'content])))

(define (ordered-list . args)
  (apply general-list (cons make-ol args)))

(define (unordered-list . args)
  (apply general-list (cons make-ul args)))

(define (empty-content)
  (lambda (msg)
    (match msg
           ['type 'content]
           ['to-html ""])))

(define (slide-cover title [subtitle ""] [author ""])
  (slide (n-rows-box 
                     (empty-content)
                     (strong-text title (font-size "54px") (text-align "center"))
                     (text subtitle (font-size "32px") (text-align "center") (css-style "font-style: italic"))
                     (text author (font-size "24px") (text-align "right"))
                     (empty-content) 
                     (empty-content))))

