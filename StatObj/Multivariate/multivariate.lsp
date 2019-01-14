(provide "multivariate")

;;;;
;;;;  Contains a prototype for computing multivariate numerical summaries and
;;;;      a few related functions from linear algebra
;;;; 

;;;;
;;;; Multivariate Numerical Summaries: gives numerical summaries of a list of
;;;;      variables ordered by role, X-variables first, and observations
;;;;

(defproto multivariate-data-proto
  '(owner the-type robust tolerance tuning-value iterations
          means standard-deviations covariance correlation
          mahalanobis row-weights num-x-vars num-y-vars num-z-vars)
 () dataset-proto "Multivariate Data")

;;;
;;;  Builds object and initializes slot-values calls methods for doing
;;;      numerical summaries and eigenanalysis
;;;

(defmeth multivariate-data-proto :isnew
  (dataset &key name the-type robust tolerance tuning-value iterations)
  (let* ((label-variable (send dataset :label-variable))
         (x-variables (send dataset :variables-of-role 'X))
         (y-variables (send dataset :variables-of-role 'Y))
         (z-variables (send dataset :variables-of-role 'Z))
         (mask (setf (slot-value 'mask) (copy-list (send dataset :mask)))))
    (setf (slot-value 'variables)
          (remove nil (append x-variables y-variables z-variables)))
    (if label-variable
        (setf (slot-value 'variables)
              (cons label-variable (slot-value 'variables))))
    (setf (slot-value 'mask) (copy-list (send dataset :mask)))
    (setf (slot-value 'color) (send dataset :color))
    (setf (slot-value 'symbol) (send dataset :symbol))
    (setf (slot-value 'state) (send dataset :state))
    (setf (slot-value 'owner) dataset)
    (setf (slot-value 'name) name)
    (setf (slot-value 'the-type) (if the-type the-type 'corr))
    (setf (slot-value 'robust) robust)
    (setf (slot-value 'tolerance) tolerance)
    (setf (slot-value 'tuning-value) tuning-value)
    (setf (slot-value 'iterations) iterations)
    (send self :compute)
    (send self :add-new-variables)))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth multivariate-data-proto :owner ()
  (slot-value 'owner))

(defmeth multivariate-data-proto :means ()
  (slot-value 'means))

(defmeth multivariate-data-proto :row-weights ()
  (slot-value 'row-weights))

(defmeth multivariate-data-proto :standard-deviations ()
  (slot-value 'standard-deviations))

(defmeth multivariate-data-proto :covariance ()
  (slot-value 'covariance))

(defmeth multivariate-data-proto :correlation ()
  (slot-value 'correlation))

(defmeth multivariate-data-proto :mahalanobis ()
  (slot-value 'mahalanobis))


(defmeth multivariate-data-proto :num-x-vars ()
  (slot-value 'num-x-vars))

(defmeth multivariate-data-proto :num-y-vars ()
  (slot-value 'num-y-vars))

(defmeth multivariate-data-proto :num-z-vars ()
  (slot-value 'num-z-vars))



;;;
;;; Computes ordinary numerical summaries (means, standard
;;;     deviations, covariance and correlation matrices, centered
;;;     data matrix, mahalanobis distances), optionally weights can
;;;     be specified
;;;

(defmeth multivariate-data-proto :numerical-summaries
  (&key data-matrix row-weights)
  (let* ((x (send self :make-data-matrix-of-role 'X))
         (y (send self :make-data-matrix-of-role 'Y))
         (z (send self :make-data-matrix-of-role 'Z))
         (weightvar (slot-value 'weight-variable))
         (mask (slot-value 'mask))
         (weights (if weightvar (select (send (eval weightvar) :values)
                                        (which mask))))
         (num-x-vars (if x  (array-dimension x 1) 0))
         (num-y-vars (array-dimension y 1))
         (num-z-vars (if z (array-dimension z 1) 0))
         (data-matrix (bind-matrices x y z))
         (num-obs (array-dimension data-matrix 0))
         (num-vars (array-dimension data-matrix 1))
         (j (make-array num-obs :initial-element 1))
         (row-weights (if row-weights row-weights
                          (if weights weights j)))
         (means (/ (matmult row-weights data-matrix) (sum row-weights)))
         (centered-matrix (/ (matmult (diagonal row-weights)
                                      (- data-matrix (outer-product j means)))
                             (sqrt (* (/ ( - num-obs 1) num-obs) 
                                      (inner-product
                                       row-weights row-weights)))))
         (i (iseq num-obs))
         (cov (matmult (transpose centered-matrix) centered-matrix)) 
         (cov-inverse (inverse cov))
         (standard-deviations (sqrt (diagonal cov)))
         (std-inv (diagonal (/ standard-deviations)))
         (corr (matmult (matmult std-inv cov) std-inv))
         (mahal
          (let ((mahal nil)
                (vars (iseq 0 (- num-vars 1))))
            (dotimes (i num-obs (* (- num-obs 1) mahal))
                     (setf mahal (cons 
                                  (aref (matmult
                                         (matmult
                                          (select centered-matrix i vars) 
                                          cov-inverse)
                                         (transpose
                                          (select centered-matrix i vars)))
                                        0 0)
                                  mahal))))))
    (setf (slot-value 'row-weights) row-weights)
    (setf (slot-value 'num-x-vars) num-x-vars)
    (setf (slot-value 'num-y-vars) num-y-vars)
    (setf (slot-value 'num-z-vars) num-z-vars)
    (setf (slot-value 'means) means)
    (setf (slot-value 'standard-deviations) standard-deviations)
    (setf (slot-value 'covariance) cov)
    (setf (slot-value 'correlation) corr)
    (setf (slot-value 'mahalanobis) (sqrt mahal))
    centered-matrix))

(defun bind-matrices (x y z)
  (let* ((num-obs (array-dimension y 0))
         (num-y-vars (array-dimension y 1))
         (ytv (make-array (* num-obs num-y-vars)
                          :displaced-to (transpose y))))
    (if x
        (let* ((num-x-vars  (array-dimension x 1))
               (xtv (make-array (* num-obs num-x-vars)
                                :displaced-to (transpose x))))
          (if z 
              (let* ((num-z-vars (array-dimension z 1))
                     (ztv (make-array (* num-obs num-z-vars)
                                      :displaced-to (transpose z)))
                     (num-vars (+ num-x-vars num-y-vars num-z-vars))
                     (data-vector
                      (make-array
                       (* num-obs (+ num-x-vars num-y-vars num-z-vars))
                       :initial-contents (concatenate 'list xtv ytv ztv))))
                (transpose (make-array (list num-vars num-obs)
                                       :displaced-to data-vector))) 
              (transpose (make-array ( list (+ num-x-vars num-y-vars) num-obs)
                                     :displaced-to
                                     (make-array
                                      (* num-obs (+ num-x-vars num-y-vars)) 
                                      :initial-contents
                                      (concatenate 'list xtv ytv ))))))
        (if z 
            (let* ((num-z-vars (array-dimension z 1))
                   (ztv (make-array (* num-obs num-z-vars) :displaced-to z))
                   (num-vars (+ num-y-vars num-z-vars))
                   (data-vector
                    (make-array (* num-obs (+ num-y-vars num-z-vars)) 
                                :initial-contents
                                (concatenate 'list ytv ztv))))
              (transpose (make-array (list num-vars num-obs)
                                     :displaced-to data-vector)))
            y))))

;;
;;  Computes robust summaries with specified tolerance,
;;      iteration number and tuning value for the weightfunction
;;

(defmeth multivariate-data-proto :robust-summaries
  (&optional weights)
  (let* ((centered-matrices
          (send self :numerical-summaries :row-weights weights))
         (data-matrix (elt centered-matrices 3))
         (num-vars (array-dimension data-matrix 1))
         (row-weights (slot-value 'row-weights))
         (tuning-value (slot-value 'tuning-value))
         (iterations (slot-value 'iterations))
         (tolerance (slot-value 'tolerance))) 
    (do ((old-weights row-weights new-weights)
         (new-weights (huber-weights (slot-value 'mahalanobis)
                                     tuning-value num-vars)
                      (huber-weights (slot-value 'mahalanobis) 
                                     tuning-value num-vars))
         (i 1 (+ i 1)))
        ((or (< (sqrt (sum (^ (- old-weights new-weights) 2))) tolerance)
             (> i iterations)) centered-matrices)
        (setf centered-matrices
              (send self :numerical-summaries
                    :data-matrix data-matrix :row-weights new-weights)))))

;;
;; Huber-weightfunction for the robust summaries
;;

(defun huber-weights (d tuning-value num-vars)
  (setf k (sqrt (chisq-quant tuning-value num-vars)))         
  (if-else (> d k) (/ k d) 1))

;;;
;;; Creates the derived variables and adds them to the variable list
;;;    
;;;

(defmeth multivariate-data-proto :make-variables (scores name)
  (let* ((scores (column-list scores))
         (vars nil)
         (i 1))
    (dolist (var scores)
            (setf vars (cons (make-variable (coerce var 'list) 
                                            :name (concatenate 'string name
                                                    (format nil "~d" i)) 
                                            :type 'N) vars))
            (setf i (+ i 1)))
    (dolist (var vars)
            (send var :owner self))
    (setf vars (reverse vars))
    (setf (slot-value 'variables)
          (concatenate 'list (slot-value 'variables) vars))))



;;
;;  Plots the Mahalanobis-distances between observations
;;

(defmeth multivariate-data-proto :plot-mahal ()
  (let* ((num-obs (send self :num-obs-unmasked)))
    (plot-points (iseq 1 num-obs) (slot-value 'mahalanobis))))

;;
;; Plots the weights of observations for robust summaries
;;

(defmeth multivariate-data-proto :plot-weights ()
  (let* ((num-obs (send self :num-obs-unmasked)))
    (plot-points (iseq 1 num-obs) (slot-value 'weights))))

;;;
;;;  Numerical Functions: provides a few useful functions from linear algebra
;;;

(defun gen-sv-decomp (y &optional row-weights col-weights)
  (let* ((n (array-dimension y 0))
         (p (array-dimension y 1))
         (row-weights (if row-weights row-weights
                          (make-array n :initial-element 1)))
         (col-weights (if col-weights col-weights
                          (make-array p :initial-element 1)))
         (y (matmult (diagonal row-weights) y (diagonal col-weights)))
         (svd (sv-decomp y)))
    (list (matmult (diagonal (/ row-weights)) (first svd)) (second svd)
          (matmult (diagonal (/ col-weights)) (third svd)))))

(defun symmetric-root-inverse (s)
    (let* ((p (array-dimension s 0))
           (eigvec (transpose
                    (make-array (list p p)
                                :initial-contents (eigenvectors s))))
           (eigval (eigenvalues s))
           (s-1/2 (matmult eigvec (diagonal (/ (sqrt eigval )))
                           (transpose eigvec))))
      s-1/2))

(defun scale-matrix (cols &optional (center t) (scale t))
  (let* ((y (if (matrixp cols)
                cols
                (apply #'bind-columns cols)))
         (n (array-dimension y 0))
         (p (array-dimension y 1))
         (j (make-array n :initial-element 1))
         (centers (if center
                     (if (eql center t)
                         (mapcar #'mean (column-list y))
                         center)))
         (scales (if scale
                    (if (eql scale t)
                        (mapcar #'standard-deviation (column-list y))
                        scale))))
    (If centers (setf y (/ (- y (outer-product j centers)) (sqrt (- n 1)))))
    (if (and scales (and (> scales 0)))
        (setf y (matmult y (diagonal (/ scales)))))
    y))

(defun standardize (y)
  (let ((y-bar (mean y))
        (s (standard-deviation y)))
    (/ (- y y-bar) s)))

(defun norm (y)
  (sqrt (inner-product y y)))

(defun uniform (dim)
  (loop (let* ((u (uniform-rand dim))
               (tu (- (* 2 u) 1))
               (sstu (sum (* tu tu))))
          (if (and (> sstu 0) (<= sstu 1))
              (return tu)))))
;;
;; Projection operators
;;

(defun projection-matrix (cols)
  (let* ((x (if (matrixp cols) cols
                (apply #'bind-columns cols)))
         (svd (sv-decomp x))
         (u (first svd))
         (s-vals (second svd))
         (basis (select (column-list u) (which (> s-vals 0.0))))
         (u1 (apply #'bind-columns basis)))
    (matmult u1 (transpose u1))))

(defun project (y-cols x-cols)
  (let* ((y (if (matrixp y-cols) y-cols
                (apply #'bind-columns y-cols)))
         (x (if (matrixp x-cols) x-cols
                (apply #'bind-columns x-cols)))
         (y-hat (matmult (projection-matrix x) y)))
    (list y-hat (- y y-hat))))

(defun negative-values (x)
  (let ((neg nil))
    (dolist (value x)
            (if (< value 0) (setf neg t)))
    neg))

