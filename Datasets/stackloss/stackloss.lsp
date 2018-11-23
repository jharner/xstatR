(setf stack-data (read-data-obs "xstatR/Datasets/stackloss/stackloss.txt"))	
(setf stack-loss (make-dataset 'stack-loss (first stack-data) (second stack-data)))

(make-scatplot 'air-flow 'stack-loss stack-loss)
(make-scatplot 'water-temp 'stack-loss stack-loss)