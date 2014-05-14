(load "impress.scm")


; I will add this layout into the library!
(define (mylayout . content)
  (fixed-width-n-columns-box
    (list 0.1 0.8 0.1)
    (list (empty-content) (apply vertical-box content) (empty-content))
    '()))

(impress
  (output-filename "templates/word2vec_notes.html")
  (font-size "24px")
  (slide-cover "word2vec: notes on implementation"
               ""
               "yang zhixuan")

  ; outline
  (slide 
    (font-size "22px")
    (mylayout
      (strong-text "Story line:")
      (ordered-list (text "train word vectors with applications")
                    (text "by application, we use language models or simplified language models")
                    (text "Mikolov proposed two simplified language model for training word vectors: CBOW and skip-gram")
                    (text "Mikolov observed skip-gram outperforms all the other models(NNLM, RNNLM, CBOW...)")
                    (text "He introduced several extensions for training skip-gram: negative sampling and subsampling")
                    (text "He open-sourced an extremely optimized C program for training skip-gram and CBOW: word2vec"))))
  
  (slide
    (mylayout
      (unordered-list (text "NNLM: predicate(by neural network) the probability of words as being the next word after a word sequence")
                      (text "CBOW: predicate(by linear soft-max regression with shared project layer) the probability of words as being the middle word before and after a word sequence")
                      (text "skip-gram: predicate (by linear soft-max regression) the probability of words observed around a word"))
      (strong-text "Comparison:")
      (unordered-list (text "skip-gram outperforms all the other models especially in semantics?")
                      (text "how to train word vectors when copora is limited?"))
      ))

  (slide
    (mylayout
      (strong-text "Extensions")
      (unordered-list (vertical-box (text "Hierarchical soft-max")
                                    (strong-text "ontology tree for hierarchical soft-max?"))
                      (vertical-box (text "Negative sampling")
                                    (text "negative sampling works better when the dimension of vector is small?"))
                      (vertical-box (text "Subsampling for frequent words")))))
  
  (slide-cover "" "This slides is written by scheme!"))
