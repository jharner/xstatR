;;;;
;;;;   xyPlot-Data Prototype
;;;;       is the data model for the xy-plot, a dynamic graphics
;;;;       prototype
;;;;

(defproto xyplot-data-proto
  '(owner states a0 b0 at bt a1 b1 home a0-pr b0-pr a1-star b1-star project
          alpha beta teta delta history geod)
  '(instances) dataset-proto "xy-Plot model")

;;;
;;;  Initializes the slot values , builds the states vector
;;;      and computes the starting position of the plot
;;;

(defmeth xyplot-data-proto :new (dataset &rest args)
  (let ((xyplot-data (apply #'call-next-method dataset args)))
    (setf (slot-value 'instances)
          (cons xyplot-data (slot-value 'instances)))
    xyplot-data))

(defmeth xyplot-data-proto :isnew (dataset)
  (let* ((x-vars (send dataset :variables-of-role 'X))
         (y-vars (send dataset :variables-of-role 'Y))
         (z-vars (send dataset :variables-of-role 'Z))
         (variable-list
          (remove nil (append x-vars y-vars z-vars)))
         (num-vars (length variable-list))
         (x-var nil)
         (y-var nil)
         (states (if (not x-vars)
                     (progn (setf x-var 0)
                            (setf y-var 1)
                            (repeat 'A num-vars))
                     (progn (setf x-var 0)
                            (setf y-var (length y-vars))
                            (mapcar
                             #'(lambda (x)
                                 (let ((state (send (eval x) :role)))
                                   (if (eql state 'Z) (setf state 'O) state)))
                                    variable-list))))
         (a (make-array (list num-vars num-vars) :initial-element 0))
         (a0 (make-array num-vars :initial-element 0))
         (b0 (make-array num-vars :initial-element 0))
         (a1 (make-array num-vars :initial-element 0))
         (b1 (make-array num-vars :initial-element 0))
         (str nil))
    (when (and x-var y-var)
          (setf (slot-value 'owner) dataset)
          (setf (slot-value 'variables) variable-list)
          (dolist (z-var (reverse z-vars))
                  (setf str (concatenate 'string
                        (format nil " ~s" (send z-var :name)) str)))
          (if z-vars
              (setf str (concatenate 'string (format nil " |") str)))
          (dolist (x-var (reverse x-vars))
                  (setf str (concatenate 'string
                        (format nil " ~s" (send x-var :name)) str)))
          (if x-vars
              (setf str (concatenate 'string (format nil " ~~ ") str)))
          (dolist (y-var (reverse y-vars))
                  (setf str (concatenate 'string
                        (format nil " ~s" (send y-var :name)) str)))
          (setf str (concatenate 'string (format nil "2-dProj: ") str))
          (setf (slot-value 'name) str)
          (setf (slot-value 'mask) (copy-list (send dataset :mask)))
          (setf (slot-value 'symbol) (send dataset :slot-value 'symbol))
          (setf (slot-value 'color) (send dataset :slot-value 'color))
          (setf (slot-value 'state) (send dataset :slot-value 'state))
          (setf (select a0 x-var) 1)
          (setf (select b0 y-var) 1)
          (setf (slot-value 'a0) (coerce a0 'vector))
          (setf (slot-value 'b0) (coerce b0 'vector))
          (setf (slot-value 'at) (coerce a0 'vector))
          (setf (slot-value 'bt) (coerce b0 'vector))
          (setf (slot-value 'a1) (coerce a1 'vector))
          (setf (slot-value 'b1) (coerce b1 'vector))
          (setf (slot-value 'history) (list (list a0 b0)))
          (setf (slot-value 'states) states)
          (setf (select a 0 x-var) 1)
          (setf (select a 1 y-var) 1)
          (setf (slot-value 'home) a))))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth xyplot-data-proto :owner ()
  (slot-value 'owner))

(defmeth xyplot-data-proto :history (&optional history)
  (if history (setf (slot-value 'history) history))
  (slot-value 'history))
 

(defmeth xyplot-data-proto :a0 (&optional a0)
  (if a0 (setf (slot-value 'a0) a0))
  (slot-value 'a0))

(defmeth xyplot-data-proto :b0 (&optional b0)
  (if b0 (setf (slot-value 'b0) b0))
  (slot-value 'b0))

(defmeth xyplot-data-proto :at (&optional at)
  (if at (setf (slot-value 'at) at))
  (slot-value 'at))

(defmeth xyplot-data-proto :bt (&optional bt)
  (if bt (setf (slot-value 'bt) bt))
  (slot-value 'bt))

(defmeth xyplot-data-proto :a1 (&optional a1)
  (if a1 (setf (slot-value 'a1) a1))
  (slot-value 'a1))

(defmeth xyplot-data-proto :b1 (&optional b1)
  (if b1 (setf (slot-value 'b1) b1))
  (slot-value 'b1))

(defmeth xyplot-data-proto :home (&optional home)
  (if home (setf (slot-value 'home) home))
  (slot-value 'home))


(defmeth xyplot-data-proto :states (&optional states)
  (if states (setf (slot-value 'states) states))
  (slot-value 'states))

(defmeth xyplot-data-proto :geod (&optional geod)
  (if geod (setf (slot-value 'geod) geod))
  (slot-value 'geod))

;;;
;;; Computes two vectors that specify a random plane which
;;;     satisfies the  orthogonality constraints and forms with the
;;;     present projection plane an angle that is less than that given
;;;     by the slot-value neighborhood.                                          
;;;

(defmeth xyplot-data-proto :local-planes (neighborhood)
  (let* ((variables (send self :variables))
         (num-vars (length variables))
         (states (slot-value 'states))
         (a0 (slot-value 'a0))
         (b0 (slot-value 'b0))
         (a1* (make-array num-vars :initial-element 0))
         (b1* (make-array num-vars :initial-element 0))
         (a1 (make-array num-vars :initial-element 0))
         (b1 (make-array num-vars :initial-element 0))
         (a-vector (make-array num-vars :initial-element 0))
         (x-vector (make-array num-vars :initial-element 0))
         (y-vector (make-array num-vars :initial-element 0)))
    (dotimes (j num-vars)
            (setf var-state (select states j))
            (case var-state
              (A (setf (select a-vector j) 1))
              (X (setf (select x-vector j) 1))
              (Y (setf (select y-vector j) 1))))
    (let* ((a-dim (sum a-vector))
          (x-dim (sum x-vector))
          (y-dim (sum y-vector))
          (a0-ap (do* ((j 0 (+ j 1))
                      (complies t (or (= (select a-vector j) 1) 
                                      (< (abs (select a0 j)) 1e-04))))
                     ((or (not complies) (= j (- num-vars 1)))  complies)))
           (b0-ap (do* ((j 0 (+ j 1))
                        (complies t (or (= (select a-vector j) 1) 
                                        (< (abs (select b0 j)) 1e-04))))
                       ((or (not complies) (= j (- num-vars 1)))  complies)))
           (a0-xp (do* ((j 0 (+ j 1))
                      (complies t (or (= (select x-vector j) 1) 
                                      (< (abs (select a0 j)) 1e-04))))
                     ((or (not complies) (= j (- num-vars 1))) complies)))
           (b0-yp (do* ((j 0 (+ j 1))
                      (complies t (or (= (select y-vector j) 1) 
                                      (< (abs (select b0 j)) 1e-04))))
                     ((or (not complies) (= j (- num-vars 1)))  complies)))
          (bau (or (and (> a-dim 1) (= x-dim 0) (= y-dim 0) a0-ap b0-ap)
                   (and (= a-dim 0) (> x-dim 0) (> y-dim 0) a0-xp b0-yp))))  
                  
      (cond ((not bau) (message-dialog
                        "Orthogonality constraints not properly constructed")
                       (send self :move-it 0))
            ((and (> a-dim 1) (= x-dim 0) (= y-dim 0))
             (progn
              (setf (slot-value 'geod) t)
              (let* ((a (uniform a-dim))
                     (b (uniform a-dim))
                     (pos 0))
                (dotimes (j num-vars)
                         (if (= (select a-vector j) 1)  
                             (progn
                              (setf (select a1 j) (select a pos))
                              (setf pos (+ pos 1)))))
                (setf pos 0)
                (dotimes (j num-vars)
                         (if (= (select a-vector j) 1)
                         (progn
                              (setf (select b1 j) (select b pos))
                              (setf pos (+ pos 1))))))))
           ((and (= a-dim 0) (> x-dim 0) (> y-dim 0))
            (progn
             (setf (slot-value 'geod) nil)
             (let* ((x (uniform x-dim))
                    (y (uniform y-dim))
                    (pos 0))
               
               (dotimes (j num-vars)
                        (if (= (select x-vector j) 1)
                            (progn
                             (setf (select a1 j) (select x pos))
                             (setf pos (+ pos 1)))))
               (setf pos 0)
               (dotimes (j num-vars)
                        (if (= (select y-vector j) 1)
                            (progn
                             (setf (select b1 j) (select y pos))
                             (setf pos (+ pos 1)))))))))
   (if bau    
       (let* ((theta (* (uniform-rand 2) neighborhood))
              (a1 (/ a1 (norm a1)))
              (a1* (- a1 (* (inner-product a1 a0) a0)))
              (a1* (if (= (norm a1*) 0) a1* (/ a1* (norm a1*))))
              (b1 (/ b1 (norm b1))) 
              (b1* (- b1 (* (inner-product b1 b0) b0)))
              (b1* (if (= (norm b1*) 0) b1* (/ b1* (norm b1*)))))
  ;       (print theta)
         (cond 
           ((or (and (> (abs (inner-product a1* b0)) (- 1 1e-04))                          
                     (> (abs (inner-product b1* a0)) (- 1 1e-04)))
                (and (= (norm a1*) 0) (= (norm b1*) 0))) 
            (message-dialog "There is only one plane")
            (send self :move-it 0))
           (t 
            (let* ((a1 (if (or (> (abs (inner-product a1* b0)) (- 1 1e-04))
                               (= (norm a1*) 0)) a0
                           (+ (* a0 (cos (first theta)))
                              (* a1* (sin (first theta))))))
                   (b1 (if (or (> (abs (inner-product b1* a0)) (- 1 1e-04))
                               (= (norm b1*) 0)) b0
                           (+ (* b0 (cos (second theta)))
                              (* b1* (sin (second theta))))))
                   (b1-orthog (- b1 (* (inner-product b1 a1) a1)))
                   (b1 (/ b1-orthog (norm b1-orthog))))
              (setf (slot-value 'a1) a1)
              (setf (slot-value 'b1) b1)))))))))

;;;
;;; Computes two vectors which form a random plane that satisfies
;;;     the orthogonality constraints. This will be the target plane.
;;;
   
(defmeth xyplot-data-proto :planes ()
  (let* ((variables (send self :variables))
         (num-vars (length variables))
         (states (slot-value 'states))
         (a0 (slot-value 'a0))
         (b0 (slot-value 'b0))
         (a1 (make-array num-vars :initial-element 0))
         (b1 (make-array num-vars :initial-element 0))
         (a-vector (make-array num-vars :initial-element 0))
         (x-vector (make-array num-vars :initial-element 0))
         (y-vector (make-array num-vars :initial-element 0)))
    (dotimes (j num-vars)
            (setf var-state (select states j))
            (case var-state
              (A (setf (select a-vector j) 1))
              (X (setf (select x-vector j) 1))
              (Y (setf (select y-vector j) 1))))
    (setf a-dim (sum a-vector))
    (setf x-dim (sum x-vector))
    (setf y-dim (sum y-vector))
    (cond
      ((and (> a-dim 1) (= x-dim 0) (= y-dim 0))
       (progn
        (setf (slot-value 'geod) t)
        (let* ((a (uniform a-dim))
               (b (uniform a-dim))
               (pos 0))
          (dotimes (j num-vars)
                   (if (= (select a-vector j) 1)
                       (progn
                        (setf (select a1 j) (select a pos))
                        (setf pos (+ pos 1)))))
          (setf pos 0)
          (dotimes (j num-vars)
                   (if (= (select a-vector j) 1)
                       (progn
                        (setf (select b1 j) (select b pos))
                        (setf pos (+ pos 1))))))))
      ((and (= a-dim 0) (> x-dim 0) (> y-dim 0))
       (progn
        (setf (slot-value 'geod) nil)
        (let* ((x (uniform x-dim))
               (y (uniform y-dim))
               (pos 0))
          (dotimes (j num-vars)
                   (if (= (select x-vector j) 1)
                       (progn
                        (setf (select a1 j) (select x pos))
                        (setf pos (+ pos 1)))))
          (setf pos 0)
          (dotimes (j num-vars)
                   (if (= (select y-vector j) 1)
                       (progn
                        (setf (select b1 j) (select y pos))
                        (setf pos (+ pos 1))))))))
      (t (message-dialog
          "Orthogonality constraints not properly constructed")))
    (let* ((a1 (/ a1 (norm a1)))
           (b1-orthog (- b1 (* (inner-product b1 a1) a1)))
           (b1 (/ b1-orthog (norm b1-orthog))))
      (setf (slot-value 'a1) a1)
      (setf (slot-value 'b1) b1)
      )))

;;;
;;; Computes the principal components for the target plane and
;;;     the two principal angles between that and the original plane.
;;;
                                 
(defmeth xyplot-data-proto  :principal-vectors  ()
  (let* ((u0 (transpose (bind-rows (slot-value 'a0) (slot-value 'b0))))
         (u1 (transpose (bind-rows (slot-value 'a1) (slot-value 'b1))))
         (svd (sv-decomp (matmult (transpose u0) u1)))
         (u (column-list (matmult u0 (first svd))))
         (u-x (first u))
         (u-y (second u))
         (v (column-list (matmult u1 (third svd))))
         (v-x (first v))
         (v-y (second v))
         (theta (coerce (realpart (acos (second svd))) 'list))
         (alpha (first theta))
         (beta (second theta)))
    ; (print "principal angles")
    ; (print (acos (second svd))) 
    (list u-x u-y v-x v-y alpha beta (first svd) (third svd))))

;;;
;;;  Computes the rotation angles and vector-pairs for geodesic
;;;      interpolation between two planes and puts them into the
;;;      corresponding slot-values. Takes as argument the list
;;;      returned by the principal-vectors method                                 ;;;

(defmeth xyplot-data-proto :geod-interpolate (princomp number-of-steps)
   (let* ((a0 (first princomp))
         (b0 (second princomp))
         (a1 (third princomp))
         (b1 (elt princomp 3))
         (alpha (elt princomp 4))
         (beta (elt princomp 5))
         (y-matrix (elt princomp 6))
         (z-matrix (elt princomp 7))
         (rotp (> (* (determinant y-matrix) (determinant z-matrix)) 0))
         (rot (matmult y-matrix (transpose z-matrix)))
         (project (transpose y-matrix)))  
    (unless rotp
           ;(print "yzt is symmetric")  
            (setf (slot-value 'b1) (- (slot-value 'b1)))  	
            (setf rot (matmult y-matrix 
                        (matmult (transpose z-matrix) '#2a((1 0) (0 -1)) ))))
    (when (< (determinant y-matrix) 0)
          ;(print "y is symmetric")
          ;(terpri)
          (setf b0 (- b0))
          (setf b1 (- b1))
          (setf project (matmult '#2a((1 0) (0 -1)) (transpose y-matrix))))
;    (print "a0new, b0new")
;    (print a0)
;    (print b0)
;    (print "Desired a1, b1")
;    (print (slot-value 'a1))
;    (print (slot-value 'b1))
    (let*((a1-orthog (- a1 (* (inner-product a1 a0) a0)))
          (b1-orthog (- b1 (* (inner-product b1 b0) b0)))
          (a1-star (if (> (norm a1-orthog) 0)
                       (/ a1-orthog (norm a1-orthog))
                       a1))
          (b1-star (if (> (norm b1-orthog) 0)
                      (/ b1-orthog (norm b1-orthog))
                      b1))
          (delta (sqrt (+ (^ alpha 2) (^ beta 2))))
          (alpha-p (if (= delta 0) 0 (/ alpha delta)))
          (beta-p (if (= delta 0) 0 (/ beta delta)))
          (teta (if (< (aref rot 0 1) 0) (- (acos (aref rot 0 0)))
                    (acos (aref rot 0 0)))))
      (setf (slot-value 'a1-star) a1-star)
      (setf (slot-value 'b1-star) b1-star)
      (setf (slot-value 'delta) (/ delta number-of-steps))
      (setf (slot-value 'alpha) alpha-p)
      (setf (slot-value 'beta) beta-p)
      (setf (slot-value 'teta) (/ teta number-of-steps))
      (setf (slot-value 'a0-pr) a0)
      (setf (slot-value 'b0-pr) b0)
      (setf (slot-value 'project) project)
;    (format t "teta= ~g" teta)
;    (print "desired a1pr, b1pr")
;    (print a1)
;    (print b1)
      )))

;;;
;;; Requires as argument the number of the current iteration and
;;;     returns the transformation matrix corresponding to this step
;;;

(defmeth xyplot-data-proto :do-one-geod-rotation (current-iteration)
  (let* ((delta (slot-value' delta))
         (dt (* current-iteration delta))
         (alpha-p (slot-value 'alpha))
         (beta-p (slot-value 'beta))
         (teta (slot-value 'teta))
         (project (slot-value 'project))
         (a0 (slot-value 'a0-pr))
         (b0 (slot-value 'b0-pr))
         (a1-star (slot-value 'a1-star))
         (b1-star (slot-value 'b1-star))
         (at (if (= alpha-p 0) a0
                 (+ (* (cos (* dt alpha-p)) a0)
                    (* (sin (* dt alpha-p)) a1-star))))
         (bt (if (= beta-p 0) b0
                 (+ (* (cos (* dt beta-p)) b0)
                    (* (sin (* dt beta-p)) b1-star))))
         (c (cos (* current-iteration teta)))
         (s (sin (* current-iteration teta)))
         (rot (bind-rows (list c s) (list (- s) c))) 
         (proj (matmult (matmult (bind-columns at bt) project)
                        rot))
         (axt  (first (column-list proj)))
         (ayt (second (column-list proj)))
         (num-vars (send self :num-var))
         (m (make-array (list num-vars num-vars) :initial-element 0)))
    (dotimes (j num-vars)
             (setf (aref m 0 j) (aref axt j))
             (setf (aref m 1 j) (aref ayt j)))
    (setf (slot-value 'at) axt)
    (setf (slot-value 'bt) ayt)
    ;(print "actual a1new, b1new")
;    (print atf)
;    (print btf)
;    (print "actual a1old, b1old")
 ;   (print (slot-value 'at))
 ;   (print (slot-value 'bt))
 ;   (terpri)
    m))


(defmeth xyplot-data-proto :interpolate (number-of-steps)
  (let* ((a0 (slot-value 'a0))
         (b0 (slot-value 'b0))
         (a1 (slot-value 'a1))
         (b1 (slot-value 'b1))
         (alpha (acos (inner-product a0 a1)))
         (beta (acos (inner-product b0 b1))))
    (when (> alpha 1.5709) (setf a1 (- a1)) (setf alpha (- pi alpha)))
    (when  (> beta 1.5709) (setf b1 (- b1)) (setf beta (- pi beta)))
    (let* (
         (a1-orthog (- a1 (* (inner-product a1 a0) a0)))
         (b1-orthog (- b1 (* (inner-product b1 b0) b0)))
         (a1-star (if (> (norm a1-orthog) 0)
                      (/ a1-orthog (norm a1-orthog))
                      a1))
         (b1-star (if (> (norm b1-orthog) 0)
                      (/ b1-orthog (norm b1-orthog))
                      b1))
          (delta (sqrt (+ (^ alpha 2) (^ beta 2))))
          (alpha-p (if (= delta 0) 0 (/ alpha delta)))
          (beta-p (if (= delta 0) 0 (/ beta delta))))
      (setf (slot-value 'a1) a1)
      (setf (slot-value 'b1) b1)
    (setf (slot-value 'a1-star) a1-star)
    (setf (slot-value 'b1-star) b1-star)
    (setf (slot-value 'delta) (/ delta number-of-steps))
    (setf (slot-value 'alpha) alpha-p)
    (setf (slot-value 'beta) beta-p)
     )))
;;;
;;; Requires as argument the number of the current iteration and 
;;;     returns the transformation matrix corresponding to this step
;;;

(defmeth xyplot-data-proto :do-one-rotation (current-iteration)
  (let* ((delta (slot-value' delta))
         (dt (* current-iteration delta))
         (alpha-p (slot-value 'alpha))
         (beta-p (slot-value 'beta))
         (a0 (slot-value 'a0))
         (b0 (slot-value 'b0))
         (a1-star (slot-value 'a1-star))
         (b1-star (slot-value 'b1-star))
         (at (if (= alpha-p 0) a0
                 (+ (* (cos (* dt alpha-p)) a0)
                    (* (sin (* dt alpha-p)) a1-star))))
         (bt (if (= beta-p 0) b0
                 (+ (* (cos (* dt beta-p)) b0)
                    (* (sin (* dt beta-p)) b1-star))))
         (num-vars (send self :num-var))
         (m (make-array (list num-vars num-vars) :initial-element 0)))
    (dotimes (j num-vars)
             (setf (aref m 0 j) (aref at j))
             (setf (aref m 1 j) (aref bt j)))
    (setf (slot-value 'at) at)
    (setf (slot-value 'bt) bt)
    m))

;;;
;;;  Inserts the current plane (given by the vectors a0 and b0)
;;;      in the history list. If the history list is too long deletes
;;;      the oldest record
;;;

 (defmeth xyplot-data-proto :insert-in-history ()
    (setf (slot-value 'history)  (cons (list (slot-value 'a0)
                                   (slot-value 'b0)) (slot-value  'history)))
    (if (= (length (slot-value 'history)) 100 ) 
	(setf (slot-value 'history)  (reverse (rest (reverse
                                                  (slot-value 'history)))))))

 
              
          