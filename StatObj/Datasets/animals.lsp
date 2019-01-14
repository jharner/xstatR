(def species (make-variable
              '(FOX FOX FOX FOX FOX FOX FOX FOX FOX FOX FOX FOX COYOTE
                    COYOTE COYOTE COYOTE COYOTE COYOTE COYOTE COYOTE
                    COYOTE COYOTE COYOTE COYOTE)
              :name 'species))
(def animal (make-variable
             '(1 1 1 1 2 2 2 2 3 3 3 3 1 1 1 1 2 2 2 2 3 3 3 3)
             :name 'animal))
(def miles (make-variable
            '(0 0 5 3 3 1 5 4 4 3 6 2 4 2 7 8 5 4 6 6 7 5 8 9)
             :name 'miles))
(def season (make-variable
             '(FALL WINTER SPRING SUMMER FALL WINTER SPRING SUMMER FALL
                    WINTER SPRING SUMMER FALL WINTER SPRING SUMMER FALL
                    WINTER SPRING SUMMER FALL WINTER SPRING SUMMER)
             :name 'season))
(def animals (make-dataset (list species animal miles season)
                           :dataset-name 'animals))