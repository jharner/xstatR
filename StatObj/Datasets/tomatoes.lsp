(def fertilizer (make-variable
                 '(A A B B A B B B A A B) :name 'fertilizer))

(def yield (make-variable
            '(29.9 11.4 26.6 23.7 25.3 28.5 14.2 17.9 16.5 21.1 24.3)
            :name 'yield))

(def tomatoes (make-dataset (list fertilizer yield)
                            :dataset-name 'tomatoes))