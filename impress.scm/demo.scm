(load "impress.scm")

(define (mylayout content)
  (fixed-height-n-rows-box
    (list 0.2 0.6 0.2)
    (list (empty-content) content (empty-content))
    '()))

(impress
  (font-size "24px")
  (slide-cover "impress.scm!"
               "elegant slides written in scheme" 
               "yang zhixuan")
  (slide
    (mylayout 
      (ordered-list 
        (text "write down what you want to present in impress.scm")
        (text "generate htmls automatically")
        (text "play the slides in Google Chrome!")))
    (scale "3"))

  (slide
    (mylayout
      (text "you can (easily) extend impress.scm with your own layout, javascript library, ..."))
    (z-rotate "90"))

  (slide-cover
    "thank you!")
  
  (slide-flow-style 
    "circle"  3000)
  
  (output-filename "templates/demo.html"))
