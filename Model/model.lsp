;;
;; Model Prototypes
;;

(provide "model")
(setf *model-tree* nil)

(defproto rect-proto '(x y width height) nil nil "Rectangular area")

(defmeth rect-proto :isnew (x y w h)
  (slot-value 'x x)
  (slot-value 'y y)
  (slot-value 'width w)
  (slot-value 'height h)
)

(defmeth rect-proto :print (&optional (stream t))
  (format stream "<#Rectangular area: ~d, ~d, ~d, ~d>" (slot-value 'x)
          (slot-value 'y) (slot-value 'width) (slot-value 'height))
)

(defmeth rect-proto :fall-in (x y)
  (let (
      (left (slot-value 'x))
      (right (+ (slot-value 'x) (slot-value 'width)))
      (top (slot-value 'y))
      (bottom (+ (slot-value 'y) (slot-value 'height)))
    )
    (and (> x left) (< x right) (> y top) (< y bottom))
  )
)

;; tree structure used to organize models into a tree
(defproto tree-node-proto '(value frame-rect parent descendents))

(defmeth tree-node-proto :print (&optional (stream t))
  (format t "<Tree node, value: ~a>" (slot-value 'value))
)

(defmeth tree-node-proto :isnew (v &key parent)
  (send self :slot-value 'value v)
  (if parent
      (send parent :add-descendent self))
)

(defmeth tree-node-proto :add-descendent (sub-node)
  (if (slot-value 'descendents)
    (slot-value 'descendents (combine (slot-value 'descendents)
                                      (list sub-node)))
    (slot-value 'descendents (list sub-node))
  )
  (send sub-node :slot-value 'parent self)
)

(defmeth tree-node-proto :del-descendent (sub-node)
  (remove sub-node (slot-value 'descendent))
 (send sub-node :slot-value 'parent nil)
)

(defmeth tree-node-proto :search (value)
  (cond
   ((equal value (slot-value 'value)) self)
   (t
     (do (
           (result nil (send (first nodes) :search value))
           (nodes (slot-value 'descendents) (rest nodes))
         )
         ((or result (not nodes)) result)
     )
   )
  )
)

(defmeth tree-node-proto :duplicate ()
  (let (
    (newRoot (send tree-node-proto :new (slot-value 'value)))
    )

    (dolist (subnode (slot-value 'descendents) newRoot)
      (send newRoot :add-descendent (send subnode :duplicate))
    )
  )
)

(defmeth tree-node-proto :frame-rect (&optional (rect nil set))
  (if set (setf (slot-value 'frame-rect) rect))
  (slot-value 'frame-rect)
)

(defproto linear-model-proto '(engine formula rObjRef weights offset vars)
  () (list dataset-mixin-proto) "Linear Models")

(defmeth linear-model-proto :isnew (engine formula dataset &key name weights
                                    offset)
  (call-next-method name dataset)
  (let (
      (suffix (format nil "~d" (address-of self)))
    )
    
    (slot-value 'engine engine)
    (slot-value 'formula formula)
    (slot-value 'rObjRef (concatenate 'string "lm" suffix))
    (slot-value 'weights weights)
    (slot-value 'offset offset)
    (let (
        (var-names (list "RESIDUALS" "FITTED-VALUES"))
        (prefix (string-upcase (concatenate 'string name "-")))
      )
      (setf (slot-value 'vars)
            (mapcar #'(lambda (v)
                        (send var-proto :new
                              (intern (concatenate 'string prefix v))))
                    var-names))
    )
    (send self :compute)
  )
)

(defmeth linear-model-proto :mixedin-vars ()
  (slot-value 'vars)
)

(defmeth linear-model-proto :mixedin-values (varName)
  (let (
      (retval nil)
      (name-string (symbol-name varName))
    )

    (setf name-string
          (subseq name-string (+ 1 (length (send self :name)))))
    (case (intern name-string)
      ('residuals
        (setf retval (send self :residuals))
      )
      ('fitted-values
        (setf retval (send self :fitted-values))
      )
    )
    retval
  )
)

(defmeth linear-model-proto :update (object extraInfo)
  (let (
      (dataset (send self :dataset))
      (statusAttr '(label state symbol color))
      (index nil)
      (origObs nil)
      (attrName nil)
      (newExtraInfo nil)
    )
    (cond
      ((not extraInfo) (send self :compute))
      (t
        (setf origObs (elt extraInfo 0))
        (setf attrName (elt extraInfo 1))
        (setf index (position origObs (send dataset :observations)))
        (if (not (find attrName statusAttr)) (send self :compute))
        (setf newExtraInfo (list (elt (send self :observations) index)
                                 attrName))
      )
    )
    (send self :fireUpdateEvent newExtraInfo)
  )
)

(defmeth linear-model-proto :compute ()
  (let (
      (dataset (send self :dataset))
      (weights (slot-value 'weights))
      (offset (slot-value 'offset))
      (engine (slot-value 'engine))
      (lmcall (concatenate 'string "lm.lispr <- lm(" (slot-value 'formula)
                           ", data=lm.data.lispr"))
    )

    (send engine :save-dataset "lm.data.lispr" dataset)
    (cond (weights
        (send engine :save "lm.weights" weights)
        (setf lmcall (concatenate 'string lmcall ", weights=lm.weights"))
      )
    )
    (cond (offset
        (send engine :save "lm.offset" offset)
        (setf lmcall (concatenate 'string lmcall ", offset=lm.offset"))
      )
    )
    (setf lmcall (concatenate 'string lmcall ")"))
    (send engine :call lmcall :asis t)
    (send engine :call (concatenate 'string (slot-value 'rObjRef)
        " <- list(lmObj=lm.lispr, summary=summary(lm.lispr), "
        "influence=influence(lm.lispr))") :asis t)
  )
)

(defmeth linear-model-proto :name ()
  (if (slot-value 'name) (slot-value 'name) "Linear")
)

(defmeth linear-model-proto :coefficients ()
  (send (slot-value 'engine) :call (concatenate 'string (slot-value 'rObjRef)
      "$lmObj$coefficients"))
)

(defmeth linear-model-proto :coeff_i ()
  (send (slot-value 'engine) :call (concatenate 'string (slot-value 'rObjRef)
      "$influence$coefficients"))
)

(defmeth linear-model-proto :cov-unscaled ()
  (send (slot-value 'engine) :call (concatenate 'string (slot-value 'rObjRef)
      "$summary$cov.unscaled"))
)

(defmeth linear-model-proto :residuals ()
  (let (
      (engine (slot-value 'engine))
      (retval nil)
    )
    
    (setf retval (send engine :call (concatenate 'string (slot-value 'rObjRef)
        "$lmObj$residuals") :asis t))
    (elt retval 0)
  )
)

(defmeth linear-model-proto :df-residual ()
  (let (
      (engine (slot-value 'engine))
      (retval nil)
    )
    
    (setf retval (send engine :call (concatenate 'string (slot-value 'rObjRef)
        "$lmObj$df.residual") :asis t))
    (elt (elt retval 0) 0)
  )
)

(defmeth linear-model-proto :fitted-values ()
  (let (
      (engine (slot-value 'engine))
      (retval nil)
    )
    
    (setf retval (send engine :call (concatenate 'string (slot-value 'rObjRef)
        "$lmObj$fitted.values") :asis t))
    (elt retval 0)
  )
)

(defmeth linear-model-proto :sigma ()
  (let (
      (engine (slot-value 'engine))
      (retval nil)
    )
    
    (setf retval (send engine :call (concatenate 'string (slot-value 'rObjRef)
        "$summary$sigma") :asis t))
    (elt (elt retval 0) 0)
  )
)

(defmeth linear-model-proto :sigma-i ()
  (let (
      (engine (slot-value 'engine))
      (retval nil)
    )
    
    (setf retval (send engine :call (concatenate 'string (slot-value 'rObjRef)
        "$influence$sigma") :asis t))
    (elt (elt retval 0) 0)
  )
)

(defmeth linear-model-proto :r-squared ()
  (let (
      (engine (slot-value 'engine))
      (retval nil)
    )
    
    (setf retval (send engine :call (concatenate 'string (slot-value 'rObjRef)
        "$summary$r.squared") :asis t))
    (elt (elt retval 0) 0)
  )
)

(defmeth linear-model-proto :adjusted-r-squared ()
  (let (
      (engine (slot-value 'engine))
      (retval nil)
    )
    
    (setf retval (send engine :call (concatenate 'string (slot-value 'rObjRef)
        "$summary$adj.r.squared") :asis t))
    (elt (elt retval 0) 0)
  )
)

(defmeth linear-model-proto :hat ()
  (let (
      (engine (slot-value 'engine))
      (retval nil)
    )
    
    (setf retval (send engine :call (concatenate 'string (slot-value 'rObjRef)
        "$influcence$hat") :asis t))
    (elt retval 0)
  )
)

(defmeth linear-model-proto :cooks-distance ()
  (let (
      (engine (slot-value 'engine))
      (retval nil)
    )
    
    (setf retval (send engine :call (concatenate 'string "cooks.distance("
        (slot-value 'rObjRef) "$lmObj)") :asis t))
    (elt retval 0)
  )
)

;(defmeth linear-model-proto :dataset ()
;  (send (slot-value 'engine) :call (concatenate 'string (slot-value 'rObjRef)
;      "$lmObj$model"))
;)

(setf *model-tree* (send tree-node-proto :new linear-model-proto))

(defproto glm-proto () () linear-model-proto)

(defmeth glm-proto :name ()
  "Generalized linear"
)

(send tree-node-proto :new glm-proto
                      :parent (send *model-tree* :search linear-model-proto)) 

(defproto nlm-proto () () linear-model-proto)

(defmeth nlm-proto :name ()
  "Nonlinear"
)

(send tree-node-proto :new nlm-proto
                      :parent (send *model-tree* :search linear-model-proto))

(defproto model-win-proto '(root dataset yvar xvars selectedNode row-gap col-gap
                            startx starty titleHeight modelMenu
                            browser) () graph-window-proto)

(defmeth model-win-proto :isnew (yvar xvars browser &rest args)
  (apply #'call-next-method args)
  (slot-value 'browser browser)
  (slot-value 'dataset (send browser :dataset))
  (slot-value 'yvar yvar)
  (slot-value 'xvars xvars)
  (slot-value 'root (send *model-tree* :duplicate))
  (slot-value 'modelMenu (send model-menu-proto :new self))
  (send self :title (format nil "Models (data: ~s)"
                            (send (slot-value 'dataset) :name)))
  (slot-value 'row-gap 15)
  (slot-value 'col-gap 30)
  (slot-value 'startx 5)
  (slot-value 'starty 5)
  (slot-value 'titleHeight (+ (* 2 (+  3 (send self :text-ascent)
                                    (send self :text-descent))) 5)) 
  (let (
      (x (slot-value 'startx))
      (y (+ (slot-value 'starty) (slot-value 'titleHeight)))
    )
    
    (send self :rearrange x y (slot-value 'root))
  )
)

(defmeth model-win-proto :selectedNode (&optional (node nil set))
  (if set
    (let* (
        (value (if node (send node :slot-value 'value) nil))
        (browser (slot-value 'browser))
      )
      (setf (slot-value 'selectedNode) node)
      (if (or (not value) (send *model-tree* :search value)) 
        (send browser :dataset (slot-value 'dataset))
        (send browser :dataset value)
      )
      (send self :redraw)
    )
  )
  (slot-value 'selectedNode)
)

(defmeth model-win-proto :rearrange (x y node)
  (let (
      (value (send node :slot-value 'value))
      (children (send node :slot-value 'descendents))
      (width (+ (send self :text-width "Generalized linear") 6))
      (height (+ 6 (send self :text-ascent) (send self :text-descent)))
      (nx x)
      (ny y)
    )

    (send node :frame-rect (send rect-proto :new x y width height))
    (cond
      ((> (length children) 0)
        (setf nx (+ x (+ width (send self :slot-value 'col-gap))))
        (dolist (subnode children ny)
          (setf ny (send self :rearrange nx ny subnode))
        )
      )
      (t (+ y (+ height (slot-value 'row-gap))))
    )
  )
)

(defmeth model-win-proto :redraw ()
  (send self :erase-window)
  (let (
      (x (slot-value 'startx))
      (y (+ 3 (slot-value 'starty) (send self :text-ascent)))
      (text (concatenate 'string "Response: "
                         (symbol-name (slot-value 'yvar))))
    )
    (send self :draw-string text x y)
    (setf y (+ y 3 (send self :text-ascent) (send self :text-descent)))
    (setf text "Predictors: ")
    (dolist (var (slot-value 'xvars))
      (setf text (concatenate 'string text (symbol-name var) "  "))
    )
    (send self :draw-string text x y)
  )
  (send self :draw-tree (slot-value 'root))
)

(defmeth model-win-proto :draw-tree (node)
  (let (
      (value (send node :slot-value 'value))
      (children (send node :slot-value 'descendents))
      (x (send (send node :frame-rect) :slot-value 'x))
      (y (send (send node :frame-rect) :slot-value 'y))
      (width (send (send node :frame-rect) :slot-value 'width))
      (height (send (send node :frame-rect) :slot-value 'height))
    )

    (if (send *model-tree* :search value)
      (send self :line-width 2)
      (send self :line-width 1)
    )

    (if (equal (send self :selectedNode) node)
      (send self :draw-color 'RED)
      (send self :draw-color 'BLACK)
    )
    (send self :frame-rect x y width height)
    (send self :draw-string
          (send self :fitted-string (- width 6) (send value :name))
          (+ x 3) (+ y (+ 3 (send self :text-ascent))))
    (send self :draw-color 'BLACK)

    (cond
      ((> (length children) 0)
        (dolist (subnode children)
          (send self :draw-edge y (send subnode :frame-rect))
          (send self :draw-tree subnode)
        )
      )
      (t (+ y (+ height (slot-value 'row-gap))))
    )
  )
)

(defmeth model-win-proto :fitted-string (width origText)
  (let (
      (omittedSign "...")
      (signWidth (send self :text-width "..."))
    )

    (do (
        (len (length origText) (length fittedText))
        (fittedText origText (subseq fittedText 0 (- len 3)))
        (textWidth (send self :text-width origText)
                   (+ (send self :text-width fittedText) signWidth))
      )
      ((or (< len 0) (<= textWidth width))
       (if (eq fittedText origText)
           origText (concatenate 'string fittedText "...")))
    )
  )
)

(defmeth model-win-proto :draw-edge (y rect)
  (let (
      (x (- (send rect :slot-value 'x) (send self :slot-value 'col-gap)))
      (vadj (floor (/ (+ 6 (+ (send self :text-ascent)
                              (send self :text-descent))) 2)))
    )

    (send self :line-width 1)
    (send self :draw-line x (+ y vadj)
          (send rect :slot-value 'x) (+ (send rect :slot-value 'y) vadj))
  )
)

(defmeth model-win-proto :do-click (x y m1 m2)
  (let* (
      (menu (slot-value 'modelMenu))
      (items  (send menu :items))
      (node (send self :searchNode (slot-value 'root) x y))
      (value (if node (send node :slot-value 'value) nil))
      (is-a-model (send *model-tree* :search value))
    )

    (send self :selectedNode node)
    (cond
      ((and node m2)
        (send (elt items 0) :enabled is-a-model)
        (send (elt items 1) :enabled (not is-a-model))
        (send (slot-value 'modelMenu) :popup (+ x 2) (+ y 2) self)
      )
    )
  )
)

(defmeth model-win-proto :searchNode (n x y)
  (let ((children (send n :slot-value 'descendents)))
    (cond
      ((send (send n :frame-rect) :fall-in x y) n)
      (children
        (do (
            (retval nil (send self :searchNode (first nodelist) x y))
            (nodelist children (rest nodelist))
          )
          ((or retval (not nodelist)) retval)
        )
      )
      (t nil)
    )
  )
)

(defmeth model-win-proto :create-model (name formula)
  (format t "In create-model\n")
  (let (
      (fo (format nil "~a ~~ ~a" (slot-value 'yvar) formula))
      (m (send (send self :selectedNode) :slot-value 'value))
    )
    (send self :add-model (send m :new R fo
                                (send self :slot-value 'dataset) :name name))
  )
)

(defmeth model-win-proto :add-model (m)
  (format t "Add-model\n")
  (cond ((not (send (send self :slot-value 'root) :search m))
      (send tree-node-proto :new m
            :parent (send self :selectedNode))
      (let (
          (x (slot-value 'startx))
          (y (+ (slot-value 'starty) (slot-value 'titleHeight)))
        )
        (send self :rearrange x y (slot-value 'root))
      )
      (send self :redraw)
    )
  )
)

(defmeth model-win-proto :plot-fittedvalues-residuals ()
  (let* (
      (m (send (send self :selectedNode) :slot-value 'value))
      (yvar (intern (string-upcase (concatenate 'string (send m :name) "-"
                                                "RESIDUALS"))))
      (xvar (intern (string-upcase (concatenate 'string (send m :name) "-"
                                                "FITTED-VALUES"))))       
    )
    (make-scatplot xvar yvar m)
  ) 
)

(defproto model-menu-proto () () menu-proto "Model Menu")

(defmeth model-menu-proto :isnew (model-win)
  (call-next-method "Model Menu")
  (send self :append-items
      (send menu-item-proto :new "Create Model..."
          	:action #'(lambda () (send model-def-info-proto :new model-win)))
      (send dash-item-proto :new)
      (send menu-item-proto :new "Residual Plot"
          	:action #'(lambda () (send model-win :plot-fittedvalues-residuals)))
      (send menu-item-proto :new "Added Variable Plot"
      		:action #'(lambda () nil))
      (send menu-item-proto :new "Partial Residual Plot"
      		:action #'(lambda () nil))
      (send dash-item-proto :new)
      (send menu-item-proto :new "Show Report"
      		:action #'(lambda () nil))
  )
)

(defproto model-def-info-proto '(model-win nameEditor formulaEditor
    cancelButton okButton dialog) () nil "Define model dialog")

(defmeth model-def-info-proto :isnew (model-win)
  (slot-value 'model-win model-win)
  (slot-value 'nameEditor (send edit-text-item-proto :new "" :text-length 20))
  (slot-value 'formulaEditor
               (send edit-text-item-proto :new "" :text-length 20))
  (slot-value 'cancelButton (send button-item-proto :new "Cancel"
      :action #'(lambda ()
        (let ((dialog (send self :slot-value 'dialog)))
          (send dialog :modal-dialog-return nil)
        )
      )))
  (slot-value 'okButton (send button-item-proto :new "OK"
      :action #'(lambda ()
        (let (
            (dialog (send self :slot-value 'dialog))
            (model-win (send self :slot-value 'model-win))
            (name (send (send self :slot-value 'nameEditor) :text))
            (formula (send (send self :slot-value 'formulaEditor) :text))
          )
          (send model-win :create-model name formula)
          (send dialog :modal-dialog-return nil)
        ))))
  
  (slot-value 'dialog (send modal-dialog-proto :new (list
      (list (list (send text-item-proto :new "Name:")
                  (send text-item-proto :new (format nil"Formula: ~s ~~"
                      (send model-win :slot-value 'yvar))))
            (list (slot-value 'nameEditor) (slot-value 'formulaEditor))
      )
      (list (slot-value 'cancelButton) (slot-value 'okButton)))))
  (send (slot-value 'dialog) :modal-dialog)
)
