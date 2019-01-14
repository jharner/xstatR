;; Marketing specialist studying the waiting times in minutes at checkout
;; lines in three large slef-service deartment stores
;; Exam. 14-6 p. 453: Elementary Statistics by Bluman

(def store (make-variable
            '(A A A A A A B B B B B B C C C C C C) :name 'Store))

(def wait (make-variable
           '(3 2 5 6 3 1 5 8 9 6 2 5 1 3 4 2 7  3) :name 'Wait))

(def marketing (make-dataset (list store wait) :dataset-name 'Marketing))

