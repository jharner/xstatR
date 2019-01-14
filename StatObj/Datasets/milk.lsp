(def diet1 (make-variable '(44 44 56 46 47 38 58 53 49 35 46 30 41)
                          :var-name 'DIET1))
(def diet2 (make-variable '(35 47 55 29 40 39 32 41 42 57 51 39)
                          :var-name 'DIET2))

(def milk-yield (make-collection 'MILK-YIELD '(diet1 diet2)))