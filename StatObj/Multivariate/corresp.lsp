;;;;
;;;; Correspondence Analysis Prototype: does ordinary correspondence
;;;; analysis and builds dataset for biplots 

(defproto corresp-proto () '(instances) princomp-proto  "Correspondence Analysis")



;;;
;;;Constructor Function
;;;
      
(defun make-correspondence (dataset data-browser)
  (let* ((corresp nil)
         (x-variables (send dataset :variables-of-role 'X)))
    (if x-variables 
        (let* ((count
                (+ (length (send cancorresp-proto :slot-value 'instances)) 1))
               (name (concatenate 'string "CCA "
                                  (format nil "~s" count))))
          (setf corresp (send cancorresp-proto :new dataset :name name)))
        (let* ((count
                (+ (length (send corresp-proto :slot-value 'instances)) 1))
               (name (concatenate 'string "CA "
                                  (format nil "~s" count))))
          (setf corresp (send corresp-proto :new dataset :name name))))
    (when corresp 
          (setf *mycorr* corresp) 
          (send *data-tree* :register-dataset dataset corresp))))


;;;
;;; Computes principal components and the corresponding eigenvalues and
;;;     canonical correlations between the components and variables
;;;

(defmeth corresp-proto :compute ()
  (call-next-method)
  (let* ((y-udv (slot-value 'y-udv))
         (u (first y-udv))
         (d (second y-udv))
         (v (third y-udv))
         (i (iseq (array-dimension u 0)))
         (j (iseq (array-dimension v 0)))
         (no-of-components (- (array-dimension u 1) 1))
         (k (iseq  1 no-of-components))
         (ustar (matmult (diagonal (/ (slot-value 'row-weights)))
                         (first y-udv)))
         (vstar (matmult (diagonal (/ (slot-value 'y-column-weights)))
                         (third y-udv)))
         (ustar-reduced (select ustar i k))
         (d-reduced (select d k k))
         (vstar-reduced (select vstar j k)))
    (setf (slot-value 'no-of-components) no-of-components) 
    (setf (slot-value 'y-udv) (list ustar-reduced d-reduced vstar-reduced)))) 

(defmeth corresp-proto :find-row-weights (&optional y)
 (let ((rowsums (reduce #'+ (column-list y ))))
  (setf (slot-value 'row-weights) (/ (sqrt rowsums)))))

(defmeth corresp-proto :find-column-weights (&optional y)
  (let ((colsums (reduce #'+ (row-list y ))))
    (setf (slot-value 'y-column-weights) (/ (sqrt colsums)))))  
  
(defmeth corresp-proto :preprocess ()
  (let ((y (send self :make-data-matrix-of-role 'Y))
        (z (send self :make-data-matrix-of-role 'Z)))
    (setf (slot-value 'num-y-vars) (array-dimension y 1))
    (setf (slot-value 'num-z-vars) (if z (array-dimension z 1) 0))
    (if z (second (project y z)) y)))

(defmeth corresp-proto :add-new-variables ()
  (send self :make-variables (first (slot-value 'y-udv)) "Corresp" ))

;;;;
;;;;Corresp Report
;;;;

(defmeth corresp-proto :make-matrix-list ()
  (let* ((variable-names (send self :get-names-by-role))
         (y-names (third variable-names))
         (corresp-names nil)
         (y-udv (slot-value 'y-udv))
         (eigenvalues (^ (second y-udv) 2))
         (comp-coefficients (third y-udv))
         (no-of-components (slot-value 'no-of-components)))
    (dotimes (i no-of-components)
             (setf corresp-names
                   (cons (format nil "CA ~d" (+ i 1)) corresp-names)))
    (let* ((corresp-names (reverse corresp-names))
           (corresp-val (send matrix-proto :new "Eigenvalues"
                        (coerce eigenvalues 'vector)
                        :column-labels corresp-names))
           (corresp-coeff (send matrix-proto
                                :new "Correspondent Variable Coefficients"
                                comp-coefficients
                                :row-labels y-names
                                :column-labels corresp-names)))
      (list corresp-val corresp-coeff))))

(defmeth corresp-proto :make-stat-menu (browser)
  (let* ((corresp (send browser :dataset))
         (icon-view (send browser :icon-view))
         (menu (send stat-menu-proto :new browser)))
    (send menu :append-items
          (send dash-item-proto :new)
          (send menu-item-proto :new "Show Report"
                :action #'(lambda ()
                            (send icon-view :register-view
                                  (send report-proto :new corresp))))
          (send menu-item-proto :new "CA Biplot"
                :action
                #'(lambda () 
                    (let ((num-obs-points (send corresp :num-obs-unmasked))
                          (alpha (get-value-dialog
                                  "Choose an alpha between 0 and 1:"
                                  :initial 1)))
                      (if (and alpha (and  (>= (first alpha) 0)
                                           (<= (first alpha) 1)))
                          (make-xyplot (send corresp :make-biplot-dataset
                                             (first alpha) :type "CA")
                                       icon-view num-obs-points)
                          (message-dialog
                           "Incorrect value: alpha must be between
                                0 and 1"))))))
    menu))

(defmeth corresp-proto :make-stat-options-menu (browser)
  nil)
