(require "multivariate")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Canonical Correlation Prototype                           ;
;;;;  computes canonical correlations between X- and Y-variables;
;;;;  and builds a dataset for the canonical biplot.             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto cancorr-proto '(udv column-weights y-can-coeff x-can-coeff
                              y-can-scores x-can-scores)
  '(instances) multivariate-data-proto "Canonical Correlations")

(defmeth cancorr-proto :new (dataset &rest args)
  (let ((cancorr-data (apply #'call-next-method dataset args)))
    (setf (slot-value 'instances)
          (cons cancorr-data (slot-value 'instances)))
    cancorr-data))

;;;
;;;Constructor Function
;;;
    
(defun make-cancorr (dataset data-browser &key name the-type robust
                              tolerance tuning-value iterations)
  (let ((cancorr nil)
        (x-variables (send dataset :variables-of-role 'X)))
    (if x-variables
        (let* ((cancorr-count
                (+ (length (send cancorr-proto :slot-value 'instances)) 1))
               (name (if name name  
                         (concatenate 'string "COR "
                                      (format nil "~s" cancorr-count)))))
          (setf cancorr (send cancorr-proto :new dataset :name name
                              :the-type the-type
                              :robust robust :tolerance tolerance
                              :tuning-value tuning-value
                              :iterations iterations))))
    (when cancorr
          (send *data-tree* :register-dataset dataset cancorr))))

;(setf *mycancorr* cancorr))))

;;;;
;;;; Slot Accessors
;;;;

(defmeth cancorr-proto :udv ()
  (slot-value 'udv))

(defmeth cancorr-proto :column-weights ()
  (slot-value 'column-weights))

(defmeth cancorr-proto :x-can-scores ()
  (slot-value 'x-can-scores))

(defmeth cancorr-proto :y-can-scores ()
  (slot-value 'y-can-scores))

(defmeth cancorr-proto :x-can-coeff ()
  (slot-value 'x-can-coeff))

(defmeth cancorr-proto :y-can-coeff ()
  (slot-value 'y-can-coeff))

;;;
;;; Computes the canonical correlations and canonical vectors
;;;

(defmeth cancorr-proto :compute ()
  (let* ((cov-matrices (send self :preprocess))
         (row-weightm (send self :find-row-weightmatrix cov-matrices)) 
         (stilda (second cov-matrices))
         (column-weightm (send self :find-column-weightmatrix cov-matrices))
         (sstar (matmult column-weightm (matmult stilda row-weightm))) 
         (udv (sv-decomp sstar))
         (ustar (matmult (inverse column-weightm) (first udv)))
         (vstar (matmult (inverse row-weightm) (third udv)))
         (b (matmult column-weightm
                     (matmult (first udv) (diagonal (second udv)))))
         (c (matmult row-weightm (third udv))))
    (setf (slot-value 'y-can-coeff) b)
    (setf (slot-value 'x-can-coeff) c)
    (setf (slot-value 'udv) (list ustar (second udv) vstar))))

(defmeth cancorr-proto :find-row-weightmatrix (cov-matrices)
  (symmetric-root-inverse (first cov-matrices)))

(defmeth cancorr-proto :find-column-weightmatrix (cov-matrices)
  (symmetric-root-inverse (third cov-matrices)))

(defmeth cancorr-proto :preprocess ()
  (let* ((centered-matrix (if (slot-value 'robust)
                              (send self :robust-summaries)
                              (send self :numerical-summaries)))
         (num-x-vars (slot-value 'num-x-vars))
         (num-y-vars (slot-value 'num-y-vars))
         (num-z-vars (slot-value 'num-z-vars))
         (num-vars (array-dimension centered-matrix 1))
         (num-obs (array-dimension centered-matrix 0))
         (i (iseq num-x-vars))
         (j (iseq num-x-vars (+ num-x-vars num-y-vars (- 1))))
         (k (iseq (+ num-x-vars num-y-vars)))
         (h (iseq (+ num-x-vars num-y-vars) (- num-vars 1)))
         (l (iseq num-obs))
         (covariance-matrix
          (if (or (> num-z-vars 0) (eql (slot-value 'the-type) "custom"))
              (let* ((x-ymatrix (select centered-matrix l k))
                     (tilda (if (eql num-z-vars 0) 
                                x-ymatrix
                                (second (project x-ymatrix
                                             (select centered-matrix l h)))))
                     (column-weights (send self :find-column-weights))
                     (row-weights (send self :find-row-weights))
                     (star (matmult (diagonal row-weights)
                                    (matmult tilda
                                             (diagonal column-weights)))))
                (matmult (transpose star) star))
              (case (slot-value 'the-type)
                (corr (slot-value 'correlation))
                (cov (slot-value 'covariance)))))
         (sxx (select covariance-matrix i i))
         (sxy (select covariance-matrix i j))
         (syy (select covariance-matrix j j)))
    (setf (slot-value 'y-can-scores) (select centered-matrix l j))
    (setf (slot-value 'x-can-scores) (select centered-matrix l i)) 
    (list sxx (transpose sxy) syy)))

(defmeth cancorr-proto :find-row-weights ()
  (let* ((n (send self :num-obs-unmasked))
         (weight-variable (slot-value 'weight-variable))
         (robust (slot-value 'robust))
         (row-weights 
          (cond (robust (slot-value 'row-weights))
            (weight-variable (send (eval weight-variable) :values))  
            (t (make-array n :initial-element 1)))))
   (setf (slot-value 'row-weights) row-weights)))

(defmeth cancorr-proto :find-column-weights ()
  (let ((num-x-vars (slot-value 'num-x-vars))
        (num-y-vars (slot-value 'num-y-vars))
        (column-weights (slot-value 'column-weights)))
    (if column-weights column-weights
        (make-array (+ num-x-vars num-y-vars) :initial-element 1)))) 


;;;;
;;;; Creates the scores for the canonical variables and calls 
;;;;        the constructor function                                            
;;;;

(defmeth cancorr-proto :add-new-variables ()
  (let* ((y-can-scores (slot-value 'y-can-scores))
         (x-can-scores (slot-value 'x-can-scores))
         (b (slot-value 'y-can-coeff))
         (c (slot-value 'x-can-coeff)))
    (setf y-can-scores (matmult y-can-scores b))
    (setf x-can-scores (matmult x-can-scores c))
    (send self :make-variables x-can-scores "X-Can") 
    (send self :make-variables y-can-scores "Y-Can" )
    (setf (slot-value 'y-can-scores) y-can-scores)
    (setf (slot-value 'x-can-scores) x-can-scores)))

;;;;
;;;; Creates a biplot dataset by computing the g and h matrices and
;;;;     the scores and using their rows as observations
;;;;

(defmeth cancorr-proto :make-biplot-dataset (alpha)
  (let* ((udv (slot-value 'udv))
         (ustar (first udv))
         (d (second udv))
         (vstar (third udv))
         (dataset (slot-value 'owner))
         (dataset-name (send dataset :name))
         (g-values (column-list 
                    (matmult ustar (diagonal (^ d alpha)))))
         (h-values (column-list 
                    (matmult vstar 
                            (diagonal (^ d (- 1 alpha))))))
         (num-x-vars (slot-value 'num-x-vars))
         (num-y-vars (slot-value 'num-y-vars))
         (coeff-list nil)
         (f 0)
         (coeff
          (dolist (g-col g-values (reverse coeff-list))
                  (setf coeff-list
                        (cons (concatenate 'list g-col
                                           (select h-values f)) coeff-list))
                  (setf f (+ f 1))))
         (max-coeff (max coeff))
         (scores (column-list (slot-value 'y-can-scores)))
         (max-scores (max scores))
         (fact (/ max-scores max-coeff))
         (coeff (map-elements #'* fact coeff))
         (varscores nil)
         (value-list
          (dotimes (i (length g-values) (reverse varscores))
                   (setf varscores
                         (cons (concatenate 'list (select scores i)
                                            (select coeff i))  varscores))))
         (vars nil)
         (i 0)
         (variable-list
          (dolist (var value-list  (reverse vars))
                  (setf i (+ i 1))
                  (setf vars
                        (cons (make-variable (coerce var 'list)
                                             :name (format nil "COR ~d" i))
                              vars))
                  (if (<= i num-x-vars)
                      (send (first vars) :role 'X)
                      (send (first vars) :role 'Y))))
         (label nil)
         (labels-var (send self :label-variable))
         (labels (if labels-var (select (send (eval labels-var) :values)
                                        (which mask))
                     (iseq (length (first scores)))))
         (names (concatenate 'list
                             (send self :get-names-of-role 'X)
                             (send self :get-names-of-role 'Y)))
         (label-var (make-variable (concatenate 'list labels names)
                                   :name "LABEL" :type 'C :role 'L))
         
        
         (var-list (cons label-var variable-list))
         (biplot-dataset (send dataset-proto :new var-list :name
                               (format nil "CC- ~a" dataset-name)))
         (old-color (select (slot-value 'color)
                            (which (slot-value 'mask))))
         (color (concatenate 'list old-color
                             (repeat 'black (+ num-x-vars num-y-vars))))
         (old-state (select (slot-value 'state) 
                            (which (slot-value 'mask))))
         (state (concatenate 'list old-state
                             (repeat 'invisible (+ num-x-vars num-y-vars)))) 
         (old-symbol (select (slot-value 'symbol)
                            (which (slot-value 'mask))))
         (symbol (concatenate 'list old-symbol
                              (repeat 'cross num-x-vars)
                              (repeat 'diamond num-y-vars))))
    (send biplot-dataset :color color)
    (send biplot-dataset :symbol symbol)
    (send biplot-dataset :state state)
    biplot-dataset))

;    (setf *bipl* biplot-dataset)))

;;;;
;;;;  Cancorr Report
;;;;

(defmeth cancorr-proto :make-matrix-list ()
  (let* ((var-names (send self :get-names-by-role))
         (x-variable-names (second var-names))
         (y-variable-names (third var-names))
         (variable-names
          (concatenate 'list x-variable-names y-variable-names))
         (num-x-vars (slot-value 'num-x-vars))
         (num-y-vars (slot-value 'num-y-vars))
         (no-of-pairs (min num-x-vars num-y-vars))
         (j (iseq (+ num-x-vars num-y-vars)))
         (means (select (slot-value 'means) j))
         (standard-deviations (select (slot-value 'standard-deviations) j))
         (covariance (select (slot-value 'covariance) j j))
         (correlation (select (slot-value 'correlation) j j))
         (correlation-values (second (slot-value 'udv)))
         (x-can-coeff (slot-value 'x-can-coeff))
         (y-can-coeff (slot-value 'y-can-coeff))
         (x-can-names nil)
         (y-can-names nil)
         (x-components-names
          (dotimes (i no-of-pairs (reverse x-can-names))
                   (setf x-can-names
                         (cons (format nil "X-Can ~d" (+ i 1)) x-can-names)))) 
         (y-components-names
          (dotimes (i no-of-pairs (reverse y-can-names))
                   (setf y-can-names
                         (cons (format nil "Y-Can ~d" (+ i 1)) y-can-names)))) 
         (cov-corr (make-array (list (array-dimension correlation 0)
                                     (array-dimension correlation 0)))))
    (dotimes (i (array-dimension correlation 0))
            (dotimes (j (array-dimension correlation 0))
                    (setf (aref cov-corr i j)
                          (if (> j i)  (aref correlation i j)
                              (aref covariance i j)))))
    (let ((mean-mat
           (send matrix-proto :new "Means"
                 (coerce means 'vector) :column-labels variable-names))
        (std-mat (send matrix-proto :new "Standard Deviations"
                       (coerce standard-deviations 'vector)
                       :column-labels variable-names))
        (cov-corr-mat (send matrix-proto
                            :new "Covariance/Correlation Matrix" cov-corr
                            :row-labels variable-names
                            :column-labels variable-names))
        (correl-mat (send matrix-proto
                          :new "Canonical Correlations"
                          (coerce correlation-values 'vector)))
         (xcan-mat (send matrix-proto
                         :new "X-Canonicals" x-can-coeff
                         :row-labels x-variable-names
                         :column-labels x-components-names))
         (ycan-mat (send matrix-proto
                         :new "Y-Canonicals" y-can-coeff
                         :row-labels y-variable-names
                         :column-labels y-components-names)))
      (list mean-mat std-mat cov-corr-mat correl-mat xcan-mat ycan-mat ))))

(defmeth cancorr-proto :make-stat-menu (browser)
  (let* ((cancorr (send browser :dataset))
         (icon-view (send browser :icon-view))
         (menu (send stat-menu-proto :new browser)))
    (send menu :append-items
          (send dash-item-proto :new)
          (send menu-item-proto :new "Show Report"
                :action #'(lambda ()
                            (send icon-view :register-view
                                  (send report-proto :new cancorr))))
          (send menu-item-proto :new "Canonical Biplot"
                :action
                #'(lambda () 
                    (let* ((num-obs-points (send cancorr :num-obs-unmasked))
                           (alpha (get-value-dialog
                                   "Choose an alpha between 0 and 1:"
                                   :initial 1)))
                      (if (and alpha (and  (>= (first alpha) 0)
                                           (<= (first alpha) 1)))
                          (make-xyplot (send cancorr :make-biplot-dataset
                                             (first alpha))
                                       icon-view num-obs-points)
                          (message-dialog
                           "Incorrect value: alpha must be between
                                0 and 1"))))))
    menu))

(defmeth cancorr-proto :make-stat-options-menu (browser)
  nil)
