
;; Classes to implement the observer/observable design pattern

(defproto observable '(observers))

(defmeth observable :add-observer (obj)
  (if obj
    (let ((olist (slot-value 'observers)))
      (setf (slot-value 'observers) (cons obj olist))
    )
  )
)

(defmeth observable :remove-observer (obj)
  (let ((olist (slot-value 'observers)))
    (setf (slot-value 'observers) (remove obj olist))
  )
)

(defmeth observable :fireUpdateEvent (extraInfo)
  (dolist (obj (slot-value 'observers))
    (send obj :update self extraInfo)
  )
)

;; The observer proto is nothing more than a tag indicating a object
;; implements the update method

(defproto observer ())

(defmeth observer :update (obj extraInfo)
  (format t "Object ~a has changed. ExtraInfo: ~a\n" obj extraInfo)
)

;; Observation Proto

(defproto obs-proto '(label state symbol color) () observable)

(defmeth obs-proto :attribute (attrName &optional (value nil set))
  (cond
    (set
      (setf (slot-value attrName) value)
      (send self :fireUpdateEvent attrName)
    )
  )
  (slot-value attrName)
)

(defmeth obs-proto :isnew (label)
  (send self :attribute 'label label)
  (send self :attribute 'state 'normal)
  (send self :attribute 'symbol 'disk)
  (send self :attribute 'color 'black)
)

; Slot accessors

(defmeth obs-proto :label () (send self :attribute 'label))
(defmeth obs-proto :state () (send self :attribute 'state))
(defmeth obs-proto :symbol () (send self :attribute 'symbol))
(defmeth obs-proto :color () (send self :attribute 'color))

;; Var proto

(defproto var-proto '(name type) () ())

(defmeth var-proto :isnew (name &key type)
  (setf (slot-value 'name) name)
  (if type (send self :type type) (send self :type 'N))
)

(defmeth var-proto :name () (slot-value 'name))

(defmeth var-proto :type (&optional (type nil set-type))
  (if set-type (setf (slot-value 'type) type))
  (slot-value 'type)
)

;;
;; Dataset Prototype
;;

(defproto dataset-proto '(name observations variables) ()
  (list observable observer))

(defmeth dataset-proto :isnew (name obsList variable-names)
  (send self :name name)
  (if obsList (dolist (obs obslist) (send obs :add-observer self)))
  (setf (slot-value 'observations) obsList)
  (let ((variables nil))
    (dolist (vName variable-names)
      (setf variables (cons (send var-proto :new vName) variables))
    )
    (setf (slot-value 'variables) (reverse variables))
  )
)

(defmeth dataset-proto :print (&optional (stream t))
  (format stream "Dataset: ~a, proto-name ~a, variables: ~a"
    (send self :name)
    (send self :slot-value 'proto-name) 
    (send self :variable-names))
)

(defmeth dataset-proto :update (obj extraInfo)
  (send self :fireUpdateEvent (list obj extraInfo))
)

; Slot Accessors and Mutators

(defmeth dataset-proto :name (&optional (name nil set-name))
  (if set-name (setf (slot-value 'name) name))
  (slot-value 'name)
)
  
(defmeth dataset-proto :observations ()
  (slot-value 'observations)
)
  
(defmeth dataset-proto :variables ()
  (slot-value 'variables)
)

(defmeth dataset-proto :variable-names ()
  (mapcar #'(lambda (var) (send var :name)) (send self :variables))
)
; Reader functions for tab-delimited files
	
(defun read-dataset (file)
  (let* (
      (data-list (read-data-columns file))
      (var-name-list nil)
      (obs-list nil)
      (n (- (length (first data-list)) 1))
      (m (length data-list))
      (obs-temp nil)
    )

    (dolist (var data-list)
      (setf var-name-list (cons (first var) var-name-list))
    )

    (dotimes (i n)
      (setf obs-temp (send obs-proto :new (format nil "~d" (+ i 1))))
      (dotimes (j m)
	(setf var (select var-name-list j))
	(send obs-temp :add-slot (select var-name-list j)
              (select (select data-list j) (+ i 1)))
      )
      (setf obs-list (cons obs-temp obs-list))
    )

    (send dataset-proto :new file (reverse obs-list) (reverse var-name-list))
  )
)

; Other methods

(defmeth dataset-proto :values (var)
  (let* (
      (var-names (send self :variable-names))
      (pos (position var var-names))
    )
    
    (mapcar #'(lambda (x) (send x :attribute (select var-names pos)))
	    (send self :observations))
  )
)

(defmeth dataset-proto :type-of-var (var)
  (let ((pos (position var (send self :variable-names))))
    (send (select (send self :variables) pos) :type)
  )
)

(defmeth dataset-proto :subset (name col-names rows)
  (setf col-names (intersection col-names (send self :variable-names)))
  (send dataset-proto :new name
        (select (send self :observations) rows) col-names)
)
