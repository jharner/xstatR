(def iron (make-variable
           '(61 175 111 124 130 173 169 169 160 224 257 333 199)
           :name 'iron :role 'X))
(def aluminum (make-variable
               '(13 21 24 23 64 38 33 61 39 71 112 88 54)
               :name 'aluminum :role 'X))
(def absorbtion (make-variable
                 '(4 18 14 18 26 26 21 30 28 36 65 62 40)
                 :name 'absorbtion :role 'Y))

(def sediments (make-dataset (list iron aluminum absorbtion)
                             :dataset-name 'sediments))
