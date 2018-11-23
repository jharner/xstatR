;;
;; Plot-mixin prototype and generic plot functions
;;

(defproto plot-mixin '(variable-names dataset) () (list observer))

(defmeth plot-mixin :isnew (variable-names dataset &rest args)
  (apply #'call-next-method (length variable-names)
         :variable-labels (mapcar #'string variable-names) args)
  (setf (slot-value 'variable-names) variable-names)
  (send self :dataset dataset)
)

; Plot-mixin slot accessors

(defmeth plot-mixin :observations ()
  (let ((dataset (send self :dataset)))
    (if dataset (send dataset :observations) nil)
  )
)

(defmeth plot-mixin :variable-names () (slot-value 'variable-names))

(defmeth plot-mixin :dataset (&optional (dataset nil set))
  (if set
    (let ((oldData (slot-value 'dataset)))
      (if oldData (send oldData :remove-observer self))
      (setf (slot-value 'dataset) dataset)
      (if dataset (send dataset :add-observer self))
      (send self :needs-adjusting t)
      (send self :adjust-screen)
    )
  )
  (slot-value 'dataset)
)

(defmeth plot-mixin :remove ()
  (send self :dataset nil)
  (call-next-method)
)

(defmeth plot-mixin :update (obj extraInfo)
  (send self :needs-adjusting t)
)

(defmeth plot-mixin :adjust-screen ()
  (if (send self :needs-adjusting)
    (let (
        (vars (slot-value 'variable-names))
        (dataset (send self :dataset))
        (obs (send self :observations))
      )

      (send self :clear-points :draw nil)
      (cond
        ((and dataset obs)
          (send self :add-points
                (mapcar #'(lambda (v) (send dataset :values v)) vars))
          (dotimes (i (length obs))
            (send self :point-label i (send (elt obs i) :label))
            (send self :point-state i (send (elt obs i) :state))
            (send self :point-color i (send (elt obs i) :color))
            (send self :point-symbol i (send (elt obs i) :symbol))
          )
        )
      )
      (send self :needs-adjusting nil)
      (send self :redraw-content)
    )
  )
)

(defmeth plot-mixin :erase-selection ()
  (let ((obs (send self :observations)))
    (dolist (i (send self :selection))
      (send (elt obs i) :attribute 'state 'invisible)
    )
  )
  (synchronize-graphs)
)

(defmeth plot-mixin :show-all-points ()
  (dolist (obs (send self :observations))
    (send obs :attribute 'state 'normal)
  )
  (synchronize-graphs)
)

(defmeth plot-mixin :focus-on-selection ()
  (let* (
      (obs (send self :observations))
      (shwoing (send self :points-showing))
      (selection (send self :selection))
    )

    (dolist (i (set-difference showing selection))
      (send (elt obs i) :attribute 'state 'invisible)
    )
  )
  (synchronize-graphs)
)

(defmeth plot-mixin :menu-template ()
  (remove 'link (call-next-method))
)

(defmeth plot-mixin :unselect-all-points ()
  (let ((obs (send self :observations)))
    (dolist (i (send self :selection))
      (send (elt obs i) :attribute 'state 'normal)
    )
  )
  (send self :adjust-screen)
)

(defmeth plot-mixin :adjust-points-in-rect (left top width height state)
  (let (
      (points (send self :points-in-rect left top width height))
      (selection (send self :selection))
      (obs (send self :observations))
    )

    (case state
          (selected
            (dolist (i (set-difference points selection))
              (send (elt obs i) :attribute 'state 'selected)
            )
          )
          (hilited
            (let* (
                (points (set-difference points selection))
                (hilited (send self :points-hilited))
                (new (set-difference points hilited))
                (old (set-difference hilited points))
              )
              (dolist (i new) (send (elt obs i) :attribute 'state 'hilited))
              (dolist (i old) (send (elt obs i) :attribute 'state 'normal))
            )
          )
    )
  )
  (synchronize-graphs)
)

(defmeth plot-mixin :set-selection-symbol ()
"Method args: ()
Open dialog to set symbol used to plot selected points."
  (let* (
      (symbols (remove-if #'(lambda (x) (member x '(dot1 dot2 dot3 dot4)))
                          *plot-symbols*))
      (i (choose-item-dialog "Symbol for selected points"
                             (mapcar #'string symbols)))
      (symbol (nth i symbols))
      (obs (send self :observations))
    )
    (when i
      (if (send self :depth-cuing) (send self :depth-cuing nil))
      (dolist (i (send self :selection))
        (send (elt obs i) :attribute 'symbol symbol)
      )
    )
  )
  (synchronize-graphs)
)

(defmeth plot-mixin :set-selection-color ()
"Method args: ()
Open dialog to set color used to plot selected points."
  (let (
      (c (choose-item-dialog "Color for selected points"
                             (cons "None" (mapcar #'string *colors*))))
      (obs (send self :observations))
    )
    (when c
      (setf c (if (= c 0) nil (nth (- c 1) *colors*)))
      (dolist (i (send self :selection))
        (send (elt obs i) :attribute 'color c)
      )
    )
  )
  (synchronize-graphs)
)

(defun synchronize-graphs ()
  (dolist (g (active-windows))
    (if (kind-of-p g plot-mixin) (send g :adjust-screen))
  )
)

;;
;; Scatterplot proto
;;

(defproto scatplot-proto () () (list plot-mixin scatterplot-proto))

; Constructor function

(defun make-scatplot (x-var y-var dataset)
  (let* (
      (var-names (send dataset :variable-names))
      (x-pos (position x-var var-names))
      (y-pos (position y-var var-names))
      (graph (send scatplot-proto :new (select var-names (list x-pos y-pos))
                   dataset))
    )

    (send graph :use-color t)
    (send graph :new-menu)
    (send graph :adjust-to-data)
    graph
  )
)
