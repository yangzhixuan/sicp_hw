(load "impress.scm")

(define (mylayout top-left top-right bottom)
  (n-rows-box (n-columns-box top-left top-right)
              bottom))

(impress (font-size "24px")
         (slide 
           (text "First text" (font-size "150%")) 
           (text "Second text"))
         (slide
           (strong-text "new slide"))
         (slide
           (n-columns-box
             (text "col1")
             (text "col2")
             (text "col3"))
           (font-size "30px")
           (background-color 100 200 100 0.6))
         (slide
           (n-rows-box
             (text "row1")
             (text "row2"
                   (background-color 233 233 255)
                   (border-radius "12px"))
             (text "row3")))
         (slide
           (mylayout (text "I") (text "love") (text "scheme")))
         (slide
           (ordered-list
             (text "first item")
             (text "second item")
             (text "third item")
             (unordered-list
               (text "inner first")
               (text "inner second"))))
         (slide)
         (slide)
         (slide)
         (slide)
         (slide)
         (slide)
         (slide)
         (slide)

         (title "new title") 
         (output-filename "templates/test.html")
         (slide-flow-style "matrix" 5))

(define prop-list
  (list (title "old-title")
        (css-path "this.css")))

(define new-list
  (list (title "new-title")))

(print-properties-list (merge-properties new-list prop-list))

