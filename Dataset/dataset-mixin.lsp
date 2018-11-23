(defproto obs-wrapper-proto '(owner index) () (list observable))

(defmeth obs-wrapper-proto :isnew (owner index)
  (setf (slot-value 'owner) owner)
  (setf (slot-value 'index) index)
)

(defmeth obs-wrapper-proto :attribute (attrName &optional (value nil set))
  (let* (
      (owner (slot-value 'owner))
      (obs (elt (send (send owner :dataset) :observations) (slot-value 'index)))
    )
    (cond
      ((not (find attrName (send owner :mixedin-var-names)))
        (if set
          (send obs :attribute attrName value)
          (send obs :attribute attrName)
        )
      )
      (t
        (if set
          (send owner :mixedin-value (slot-value 'index) attrName value)
          (send owner :mixedin-value (slot-value 'index) attrName)
        )
      )
    )
  )
)

(defmeth obs-wrapper-proto :label () (send self :attribute 'label))
(defmeth obs-wrapper-proto :state () (send self :attribute 'state))
(defmeth obs-wrapper-proto :symbol () (send self :attribute 'symbol))
(defmeth obs-wrapper-proto :color () (send self :attribute 'color))

(defproto dataset-mixin-proto '(name dataset observations) ()
  (list observable observer))

(defmeth dataset-mixin-proto :isnew (name dataset)
  (setf (slot-value 'name) name)
  (setf (slot-value 'dataset) dataset)
  (send dataset :add-observer self)
  (setf (slot-value 'observations)
        (mapcar #'(lambda (i) (send obs-wrapper-proto :new self i))
                (iseq (length (send dataset :observations)))))
)

(defmeth dataset-mixin-proto :dataset ()
  (slot-value 'dataset)
)

(defmeth dataset-mixin-proto :name ()
  (slot-value 'name)
)

(defmeth dataset-mixin-proto :observations ()
  (slot-value 'observations)
)

(defmeth dataset-mixin-proto :variables ()
  (combine (send (send self :dataset) :variables) (send self :mixedin-vars))
)

(defmeth dataset-mixin-proto :variable-names ()
  (mapcar #'(lambda (var) (send var :name)) (send self :variables))
)

(defmeth dataset-mixin-proto :values (var)
  (if (find var (send self :mixedin-var-names))
    (send self :mixedin-values var)
    (send (send self :dataset) :values var)
  )
)

(defmeth dataset-mixin-proto :type-of-var (var)
  (let ((pos (position var (send self :variable-names))))
    (send (select (send self :variables) pos) :type)
  )
)

(defmeth dataset-mixin-proto :subset (name col-names rows)
  (setf col-names (intersection col-names (send self :variable-names)))
  (send dataset-proto :new name
        (select (send self :observations) rows) col-names)
)

(defmeth dataset-mixin-proto :mixedin-var-names ()
  (mapcar #'(lambda (var) (send var :name)) (send self :mixedin-vars))
)

;The set function is not supported yet
(defmeth dataset-mixin-proto :mixedin-value (index varName
                                               &optional (value nil set))
  (elt (send self :values varNames) index)
)

(defmeth dataset-mixin-proto :mixedin-vars ()
  (format t "Need to be overwritten by subclasses\n")
)

(defmeth dataset-mixin-proto :mixedin-values (var)
  (format t "Need to be overwritten by subclasses\n")
)

(defmeth dataset-mixin-proto :update (object extraInfo)
  (format t "Need to be overwritten by sub-class\n")
)
