;; Hand #150

(def ratio '(0.693	0.662	0.690	0.606	0.570 0.749	0.672	0.628	0.609	0.844 0.654	0.615	0.668	0.601	0.576 0.670	0.606	0.611	0.553	0.933))

(def log-ratio (log ratio))

(def recip-ratio (/ ratio))

(def recip-sqr-ratio (/ (sqrt ratio)))

(display-var-browser '(ratio log-ratio recip-ratio recip-sqr-ratio))