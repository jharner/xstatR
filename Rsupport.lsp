;; High level interface to the embedded R

;; R engine prototype

(defproto Rengine-proto '(version))

(defmeth Rengine-proto :isnew ()
  (send self :slot-value 'version 
      (elt (first (callr "R.Version()$version.string")) 0)
  )
)

(defmeth Rengine-proto :print (&optional (stream t))
  (format stream "#<Embedded ~a>" (send self :slot-value 'version))
)

(defmeth Rengine-proto :data (name &key package)
  (if package
    (callr (concatenate 'string "library(" package ")"))
  )

  (callr (concatenate 'string "data(" name ")"))
  (let* (
      (rawdata (callr name))
      (retval (send self :convert rawdata))
    )
    
    (if (and (objectp retval) (send retval :has-slot 'name :own t))
      (send retval :slot-value 'name (intern (string-upcase name)))
    )
    retval
  )
)

(defmeth Rengine-proto :convert (rObjInfo)
  (let (
      (classAttr (getRObjAttr rObjInfo "class"))
      (dim (getRObjAttr rObjInfo "dim"))
      (dPart nil)
      (newdPart nil)
      (converter nil)
    )

    (cond
      ((= (length rObjInfo) 1)
        (setf dPart (elt rObjInfo 0))
        (if (listp dPart)
          (dotimes (i (length dPart))
            (setf tmp (send self :convert (elt dPart i)))
            (setf (elt dPart i) tmp)
          )
        )
        dPart
      )
      (classAttr
         (setf converter (find-symbol (string-upcase (elt classAttr 0))))
         (if converter (funcall converter rObjInfo) retval)
      )
      (dim
        (setf newdPart (make-array (coerce dim 'list)
                                   :displaced-to (elt rObjInfo 0)))
        (cons newdPart (rest rObjInfo))
      )
      (t rObjInfo)
    )
  )
)

(defmeth Rengine-proto :save (varName data &key attr attrNames)
  (let (
      (tmp (list data))
    )

    (if (and attr attrNames) (setf tmp (append (append tmp attr) attrNames)))
    (savetor varName tmp)
  )
)

(defmeth Rengine-proto :save-as-logical (varName data)
  (send self :save varName data)
  (send self :call (concatenate 'string varName " <- as.logical(" varName ")"))
)

(defmeth Rengine-proto :save-dataset (varName dataset)
  (let* (
      (obs (send dataset :observations))
      (attrNames (list (vector "names" "row.names" "class")))
      (attrs (list (list (vector "data.frame"))))
      (rowNames (coerce (mapcar #'(lambda (x) (send x :label)) obs) 'vector))
      (names (send dataset :variable-names))
      (dataPart nil)
    )

    (setf attrs (cons (list rowNames) attrs))
    (dotimes (i (length names))
      (setf dataPart (cons (list (coerce (send dataset :values (elt names i))
                                         'vector)) dataPart))
    )
    (setf names (coerce (mapcar #'(lambda (v) (symbol-name v)) names) 'vector))
    (setf attrs (cons (list names) attrs))
    (savetor varName (list (reverse dataPart) attrs attrNames))
  )
)

(defmeth Rengine-proto :call (statement &key asis)
  (let (
      (retval (callr statement))
    )
    (if asis retval (send self :convert retval))
  )
)

(defun getRObjAttr (robj attrName)
  (let (
      (attr nil)
      (attrNames nil)
      (index nil)
    )
    
    (cond 
      ((< (length robj) 3) (return-from getRObjAttr nil))
      ((not attrName) (return-from getRObjAttr nil))
    )

    (setf attr (elt robj 1))
    (setf attrNames (first (elt robj 2)))
    (setf index (position attrName attrNames :test #'equal))
    (cond
      (index (elt (elt attr index) 0))
      (t NIL)
    )
  )
)

(defun getRObjNamedValue (robj name)
  (let (
      (names (getRObjAttr robj "names"))
      (dataPart (elt robj 0))
      (index nil)
    )
  
    (setf index (position name names :test #'equal))
    (if index
      (elt dataPart index)
      NIL
    )
  )
)

(defun data.frame (rawdata)
  (let (
      (headers nil)
      (dPart (elt rawdata 0))
      (colNames (getRObjAttr rawdata "names"))
      (tmp nil)
      (obs nil)
      (obsTemp nil)
    )

    (dotimes (i (length colNames))
      (setf headers (cons (intern (string-upcase (elt colNames i))) headers))
      (setf tmp (elt (elt dPart i) 0))
      (setf (elt dPart i) tmp)
    )

    (dotimes (i (length (elt dPart 0)))
      (setf obsTemp (send obs-proto :new (format nil "~d" (+ i 1))))
      (dotimes (j (length headers))
        (send obsTemp :add-slot (elt headers j) (elt (elt dPart j) i))
      )
      (setf obs (cons obsTemp obs))
    )
    (send dataset-proto :new "Rdataset" (reverse obs) (reverse headers))
  )
)

(defun load-dataset (name &rest args &key rEngine package)
  (let ((dataset nil))
    (if rEngine
      (setf dataset (send rEngine :data name :package package))
      (setf dataset (read-dataset name))
    )
    (send dataset-browser-proto :new dataset) 
  )
)

