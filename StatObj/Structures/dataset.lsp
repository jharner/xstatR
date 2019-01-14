;;;;
;;;; Dataset Prototype: a list of variables with additional information about
;;;;     the observations. Allows selection of a subset of variables (using
;;;;     variable selecion methods) and masking of some observations (using
;;;;     mask); can define optional label and weight variables; has methods
;;;;     for building numeric matrices from a dataset delimited by the
;;;;     variables selected and mask
;;;;

(defproto dataset-proto '(name z-names owner description variables
                               x-variables y-variables z-variables
                               z-mask label-variable weight-variable
                               mask symbol color state))

;;;
;;; Build dataset object and set default slots
;;;

(defmeth dataset-proto :isnew (variables &rest args)
  (send self :variables variables)
  (let ((num-obs (length (send (first variables) :values))))
    (setf (slot-value 'mask) (repeat nil num-obs))
    (setf (slot-value 'z-mask) (repeat nil num-obs))
    (setf (slot-value 'symbol) (repeat 'disk num-obs))
    (setf (slot-value 'color) (repeat 'black num-obs))
    (setf (slot-value 'state) (repeat 'normal num-obs)))
  (apply #'call-next-method args))

;;;
;;; Constructor Function: makes a new dataset object from  a list of variables 
;;;     and adds the new item to the data menu
;;;

(defun make-dataset (variables &key (dataset-name nil))
  (let* ((dataset nil)
         (value-lists (mapcar #'(lambda (var)
                                  (send var :values)) variables))
         (var-length (length (first value-lists)))
         (check-valuelists (dolist (var value-lists t)
                                   (if (not (eql var-length (length var)))
                                       (return nil)))))
    (if (not check-valuelists)
        (message-dialog "Variables not all the same length")
        (progn
         (setf dataset (send dataset-proto :new variables :name dataset-name))
         (dolist (var variables)
                 (send var :owner dataset))
         (send dataset :set-label-variable)
         (send *data-tree* :register-dataset nil dataset)
         dataset))))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth dataset-proto :name (&optional (name nil set-name))
  (if set-name (setf (slot-value 'name) name))
  (slot-value 'name))

(defmeth dataset-proto :z-names (&optional (z-names nil set-z-names))
  (if set-z-names (setf (slot-value 'z-names) z-names))
  (slot-value 'z-names))

(defmeth dataset-proto :owner (&optional (owner nil set-owner))
  (if set-owner (setf (slot-value 'owner) owner))
  (slot-value 'owner))

(defmeth dataset-proto :description (&optional (desc nil set-desc))
  (if set-desc (setf (slot-value 'description) desc))
  (slot-value 'description))

(defmeth dataset-proto :variables (&optional (variables nil set-variables))
  (if set-variables (setf (slot-value 'variables) variables))
  (slot-value 'variables))

(defmeth dataset-proto :label-variable
  (&optional (label-variable nil set-label-variable))
  (if set-label-variable (setf (slot-value 'label-variable) label-variable))
  (slot-value 'label-variable))

(defmeth dataset-proto :x-variables (&optional (variables nil set-variables))
  (if set-variables (setf (slot-value 'x-variables) variables))
  (slot-value 'x-variables))

(defmeth dataset-proto :y-variables (&optional (variables nil set-variables))
  (if set-variables (setf (slot-value 'y-variables) variables))
  (slot-value 'y-variables))

(defmeth dataset-proto :z-variables (&optional (variables nil set-variables))
  (if set-variables (setf (slot-value 'z-variables) variables))
  (slot-value 'z-variables))

(defmeth dataset-proto :z-mask (&optional z-mask)
  (if z-mask (setf (slot-value 'z-mask) z-mask))
  (slot-value 'z-mask))

(defmeth dataset-proto :weight-variable
  (&optional (weight-variable nil set-weight-variable))
  (if set-weight-variable
      (setf (slot-value 'weight-variable) weight-variable))
  (slot-value 'weight-variable))

(defmeth dataset-proto :mask (&optional mask)
  (if mask (setf (slot-value 'mask) mask))
  (slot-value 'mask))

(defmeth dataset-proto :symbol (&optional symbol)
  (if symbol (setf (slot-value 'symbol) symbol))
  (slot-value 'symbol))

(defmeth dataset-proto :color (&optional color)
  (if color (setf (slot-value 'color) color))
  (slot-value 'color))

(defmeth dataset-proto :state (&optional state)
  (if state (setf (slot-value 'state) state))
  (slot-value 'state))

;;;
;;; Other methods
;;;

(defmeth dataset-proto :add-variable (var)
  (let ((variables (send self :variables)))
    (setf variables (reverse (cons var (reverse variables))))
    (send self :variables variables)))

(defmeth dataset-proto :delete-variable (var)
  nil)

(defmeth dataset-proto :evaluate (expression)
  (send (first (slot-value 'variables)) :values))

;;;
;;; Methods for determining the number of variables and the number of (masked)
;;;     observations
;;;

(defmeth dataset-proto :num-var ()
  (length (slot-value 'variables)))

(defmeth dataset-proto :num-obs ()
  (length (send (first (slot-value 'variables)) :values)))

(defmeth dataset-proto :num-obs-unmasked ()
  (let* ((mask (slot-value 'mask))
         (not-mask (mapcar #'(lambda (val) (not val)) mask)))
    (length (which not-mask))))

;;;
;;; Methods for finding the active variables, active variables by role,
;;;     or active variables of a certain role
;;;

(defmeth dataset-proto :active-variables ()
  (let ((variables (slot-value 'variables))
        (active-variables nil))
    (dolist (var variables)
            (if (send var :active)
                (setf active-variables
                      (cons var active-variables))))
    (reverse active-variables)))

(defmeth dataset-proto :variables-by-role ()
  (let ((active-variables (send self :active-variables))
        (x-variables nil)
        (y-variables nil)
        (z-variables nil)
        (label-variable (send self :label-variable))
        (weight-variable (send self :weight-variable)))
    (dolist (var active-variables)
            (let ((role (send var :role)))
              (case role
                ('X (setf x-variables (cons var x-variables)))
                ('Y (setf y-variables (cons var y-variables)))
                ('Z (setf z-variables (cons var z-variables))))))
    (list label-variable (reverse x-variables) (reverse y-variables)
          (reverse z-variables) weight-variable)))

(defmeth dataset-proto :variables-of-role (role)
  (let ((active-variables (send self :active-variables))
        (variables nil))
    (dolist (var active-variables)
            (if (eql (send var :role) role)
                (setf variables (cons var variables))))
    (reverse variables)))

(defmeth dataset-proto :z-type ()
  (let ((z-variables (send self :variables-of-role 'Z))
        (z-type 'N))
    (when z-variables
          (if (kind-of-p (first z-variables) categoric-variable-proto)
              (setf z-type 'C))
          (dolist (z-var z-variables)
                  (if (not (eql (send z-var :type) z-type))
                      (setf z-type nil))))
    z-type))

;;;
;;; Methods for clearing all roles
;;;

(defmeth dataset-proto :clear-roles ()
  (let ((variables (send self :variables)))
    (dolist (var variables)
            (send var :role nil))))

(defmeth dataset-proto :clear-role (role)
  (let ((variables (send self :variables-of-role role)))
    (dolist (var variables)
            (send var :role nil))))

;;;
;;; Methods for getting selected variable names, selected variable names by
;;;     role, or selected variable names of a certain role
;;;

(defmeth dataset-proto :get-names ()
  (let ((variables (send self :active-variables)))
    (mapcar #'(lambda (x) (send x :name)) variables)))

(defmeth dataset-proto :get-names-by-role ()
  (let* ((variables (send self :variables-by-role))
         (label-name "Obs")
         (x-vars (elt variables 1))
         (y-vars (elt variables 2))
         (z-vars (elt variables 3))
         (x-names (mapcar #'(lambda (x) (send x :name)) x-vars))
         (y-names (mapcar #'(lambda (x) (send x :name)) y-vars))
         (z-names (mapcar #'(lambda (x) (send x :name)) z-vars))
         (weight-name "Weight"))
    (list label-name x-names y-names z-names weight-name)))

(defmeth dataset-proto :get-names-of-role (role)
  (let* ((variables (send self :variables-of-role role))
         (names (mapcar #'(lambda (x) (send x :name)) variables)))
    names))

;;;
;;; Methods for setting a label or weight variable
;;;

(defmeth dataset-proto :set-label-variable (&optional variable)
  (let* ((active-variables (send self :active-variables))
         (label-variable (send self :label-variable))
         (n (send (car active-variables) :data-length))
         (duplicate nil))
    (dolist (var active-variables)
            (if (or (eql 'L (send var :role))
                    (eql var variable))
                (if (and (not (eql label-variable var)) (not duplicate))
                    (if (< (length (send var :make-unique-values)) n)
                        (message-dialog "Values are not unique")
                        (progn
                         (if label-variable
                             (send label-variable :role nil))
                         (setf label-variable var)
                         (send self :label-variable var)
                         (send var :role 'L)
                         (send var :set-type 'C)
                         (setf duplicate t)))
                    (send var :role nil))))))

(defmeth dataset-proto :set-weight-variable (&optional variable)
  (let ((active-variables (send self :active-variables))
        (weight-variable (send self :weight-variable))
        (duplicate nil))
    (dolist (var active-variables)
            (if (or (eql 'W (send var :role))
                    (eql var variable))
                (if (and (not (eql weight-variable var)) (not duplicate))
                    (if (negative-values (send var :values))
                        (message-dialog "Values are Nonnegative")
                        (progn
                         (if weight-variable
                             (send weight-variable :role nil))
                         (setf weight-variable var)
                         (send self :weight-variable var)
                         (send var :role 'W)
                         (send var :set-type 'N)
                         (setf duplicate t)))
                    (send var :role nil))))))

;;;
;;; Methods for creating a matrix from the variable list of a certain role
;;;     eliminating masked observations
;;;

(defmeth dataset-proto :make-data-matrix-of-role (role)
  (let* ((variables (send self :variables-of-role role))
         (mask (slot-value 'mask))
         (not-mask (mapcar #'(lambda (val) (not val)) mask))
         (num-obs (send self :num-obs-unmasked))
         (num-vars (length variables))
         (value-lists nil))
    (if variables
        (dolist (var variables)
                (let* ((values (select (send var :values) (which not-mask))))
                  (if values (setf value-lists (cons values value-lists))))))
    (if value-lists
        (permute-array
               (make-array (list num-vars num-obs)
                           :initial-contents (reverse value-lists)) '(1 0)))))

(defmeth dataset-proto :make-header () nil)
