(define (mylayout top-left top-right bottom)
  (top-bottom-box
    (left-right-box 
      top-left 
      top-right)
    bottom))

(define third-slide
  (slide
    (text "Thank You!")))

(impress
  (slide
    (head "impress.scm: write slides in scheme!"
           (type 'headtitle))
    (text "yang zhixuan" 
          (position 'right))
    (text "2014.5.1"
          (position 'right)
          (italic #t))

    (x-coordinate 0)
    (y-coordinate 0)
    (class "cover"))

  (slide
    (head "quick start!")
    (left-right-box
      (vertical-box
        (ordered-list
          (text "write in scheme")
          (text "generate with impress.scm")
          (text "play with google chrome")))
      (vertical-box
        (code-snippte "mockup.scm")
        (code-snippte "demo.html")
        (image "demo.png")))

  third-slide

  output-filename "mockup.html"))

