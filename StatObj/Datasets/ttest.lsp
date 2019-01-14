;; Hand #202

(def patient '(1 2 3 4 5 6 7 8 9 10))

(def D '(0.7 -1.6 -0.2 -1.2 -1.0 3.4 3.7 0.8 0 2.0))

(def L '(1.9 0.8 1.1 0.1 -0.1 4.4 5.5 1.6 4.6 3.4))

(def L-D (- L D))

(display-var-browser '(patient D L L-D))
