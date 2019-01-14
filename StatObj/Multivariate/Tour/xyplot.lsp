;;;;
;;;;   Contains the xyPlot Prototype for dynamic graphics,
;;;;       a biplot-proto which inherits from the first and
;;;;       displays variables and observations in the same space,
;;;;       along with a menu proto
;;;;

(defproto xyplot-proto '(name plot-menu move-it neighborhood backsteps depth
                              coming-back number-of-steps status-overlay
                              variable-overlays var-line-overlays)
  nil (list graph-proto graph-mixin) "xyPlot")

;;;
;;;  Slot Accessors and Mutators
;;;

(defmeth xyplot-proto :plot-menu (&optional plot-menu)
  (if plot-menu (setf (slot-value 'plot-menu) plot-menu))
  (slot-value 'plot-menu))

(defmeth xyplot-proto :variable-overlays (&optional variable-overlays)
  (if variable-overlays
      (setf (slot-value 'variable-overlays) variable-overlays))
  (slot-value 'variable-overlays))

(defmeth xyplot-proto :var-line-overlays (&optional var-line-overlays)
  (if var-line-overlays
      (setf (slot-value 'var-line-overlays) var-line-overlays))
  (slot-value 'var-line-overlays))

(defmeth xyplot-proto :status-overlay (&optional status-overlay)
  (if status-overlay (setf (slot-value 'status-overlay) status-overlay))
  (slot-value 'status-overlay))

(defmeth xyplot-proto :neighborhood (&optional neighborhood)
  (if neighborhood (setf (slot-value 'neighborhood) neighborhood))
  (slot-value 'neighborhood))

(defmeth xyplot-proto :move-it (&optional move-it)
  (if move-it (setf (slot-value 'move-it) move-it))
  (slot-value 'move-it))   

(defmeth xyplot-proto :number-of-steps (&optional number-of-steps)
  (if number-of-steps (setf (slot-value 'number-of-steps) number-of-steps))
  (slot-value 'number-of-steps))

(defmeth xyplot-proto :backsteps (&optional backsteps)
  (if backsteps (setf (slot-value 'backsteps) backsteps))
  (slot-value 'backsteps))

(defmeth xyplot-proto :depth (&optional depth)
  (if depth (setf (slot-value 'depth) depth))
  (slot-value 'depth)) 

(defmeth xyplot-proto :coming-back ()
  (slot-value 'coming-back))
 
(defmeth xyplot-proto :name ()
  (slot-value 'name))

;;;
;;; xyPlot Constructor
;;;

(defun make-2dProj (dataset data-browser)
  (let* ((xydata (send xyplot-data-proto :new dataset))
         (title (format nil "2-dProj: ~a" (send dataset :name)))
         (xyplot (xyplot xydata title)))
    (send *data-tree* :register-dataset-with-plot dataset xydata xyplot)))

(defun xyplot (xydata title)
  (let* ((variables (send xydata :variables))
         (states (send xydata :states))
         (num-vars (length variables))
         (biplot nil)
         (plot (if biplot
                   (send biplot-proto
                         :new  num-vars
                         :num-observation-points num-observation-points 
                         :title title  :dataset xydata
                         :scale-type 'variable :size '(450 345))
                   (send xyplot-proto :new num-vars :title title
                         :name (coerce
                                (select (coerce title 'list)
                                        (iseq (+ (position #\- title) 2)
                                              (- (length title) 1))) 'string)
                         :dataset xydata :scale-type 'variable
                         :size '(450 345))))
         (plot-menu (if biplot
                        (send biplot-menu-proto :new plot)
                        (send xyplot-menu-proto :new plot)))
         (has-color (send plot :use-color t))
         (menu (send menu-overlay-proto :new  0 'xytype "xyPlot"))
         (label (send label-button-overlay-proto :new 7))
         (select (send select-button-overlay-proto :new 8))
         (brush (send brush-button-overlay-proto :new 9))
         (symbol (send symbol-button-overlay-proto :new 11))
         (color (if has-color
                    (send color-button-overlay-proto :new 15)))
         (stop (send stop-button-overlay-proto :new 18))
         (left-margin (* (ceiling (/ num-vars 5)) 50))
         (width (send plot :canvas-width))
         (height (send plot :canvas-height))
         (left-mask (send mask-overlay-proto
                          :new 0 left-margin 25 height ))
         (top-mask (send mask-overlay-proto :new 0 width 0 25 ))
         (status-overlay (send status-overlay-proto :new left-margin 
                               "Waiting for the next command"))
         (pos 0)
         (var-overlays nil)
         (var-line-overlays nil))
    (send plot :number-of-steps 20)
    (send plot :depth 0)
    (send plot :plot-menu plot-menu)
    (send plot :add-overlay top-mask)
    (send plot :add-overlay left-mask)
    (dolist (var variables)
            (let ((var-overlay (send variable-overlay-proto :new 
                                     pos var (select states pos)))
                  (var-line-overlay
                   (send variable-line-overlay-proto :new pos )))
              (send plot :add-overlay var-overlay)
              (send plot :add-overlay var-line-overlay)
              (setf var-overlays (cons var-overlay var-overlays))
              (setf var-line-overlays
                    (cons var-line-overlay var-line-overlays))
              (setf pos (+ pos 1)))) 
    (send plot :variable-overlays (reverse var-overlays))
    (send plot :var-line-overlays (reverse var-line-overlays))
    (send plot :move-it 0)
    (send plot :add-overlay status-overlay)
    (send plot :status-overlay status-overlay)
    (send plot :add-overlay label)
    (send plot :add-overlay select)
    (send plot :add-overlay brush)
    (send plot :add-overlay symbol)
    (if has-color (send plot :add-overlay color))
    (send plot :add-overlay stop)
    (send plot :add-overlay menu)
    (send plot :margin left-margin 40 0 0 :draw nil)
    (send plot :transformation (send xydata :home))
    (let ((a0 (send xydata :a0))
          (b0 (send xydata :b0))
          (pos 0))
      (dolist (overlay (reverse var-line-overlays))
              (if (> (select a0 pos) 0) (send overlay :redraw))
              (if (> (select b0 pos) 0) (send overlay :redraw))
              (setf pos (+ pos 1))))
    (send plot :adjust-to-data)
    plot))

;;;  xyplot controllers

(defmeth xyplot-proto :redraw-content ()
  (let* ((xydata (slot-value 'dataset))
         (variables (send xydata :variables))
         (num-vars (send xydata :num-var))
         (mask (send xydata :mask))
         (not-mask (mapcar #'(lambda (val) (not val)) mask))
         (symbols (send xydata :symbol))
         (colors (send xydata :color))
         (states (send xydata :state))
         (num-obs (send xydata :num-obs))
         (values nil)
         (var-values (dolist (var variables (reverse values))
                             (setf values
                                   (cons (select (send (eval var) :values)
                                                 (which not-mask)) values)))))
    (send self :clear-points :draw nil)
    (send self :add-points var-values :draw nil)
    (send self :point-state (iseq num-obs) (select states (which not-mask)))
    (send self :point-symbol (iseq num-obs) (select symbols (which not-mask)))
    (send self :point-color (iseq num-obs) (select colors (which not-mask)))
    (call-next-method)))

(defmeth xyplot-proto :adjust-points-in-rect (a b c d e)
  (call-next-method a b c d e)
  (let* ((dataset (slot-value 'dataset))
         (num-obs (send dataset :num-obs-unmasked))
         (point-states (send data :state)))
   (dotimes (i num-obs)
            (setf (select point-states i) (send self :point-state i)))
    (send data :state point-states) 
    (mapcar #'(lambda (x) (send x :redraw))
            (send *data-tree* :get-related-plots self))))

(defmeth xyplot-proto :close ()
  (send self :hide-window))

(defmeth xyplot-proto :interpolate ()
  (let* ((xydata (slot-value 'dataset))
         (geod (send xydata :geod))
         (number-of-steps (slot-value 'number-of-steps)))
  (if geod  
      (send xydata :geod-interpolate
            (send xydata :principal-vectors) number-of-steps) 
      (send xydata :interpolate number-of-steps))
    (dotimes (i number-of-steps)
            (let ((m (if geod (send xydata :do-one-geod-rotation (+ i 1))
                         (send xydata :do-one-rotation (+ i 1)))))
              (send self :transformation m)
              (dolist (var-line (slot-value 'var-line-overlays))
                      (send var-line :redraw))))
    (send xydata :a0 (send xydata :a1))
    (send xydata :b0 (send xydata :b1))
    (send xydata :at (send xydata :a1))
    (send xydata :bt (send xydata :b1))
    (dolist (var-line (slot-value 'var-line-overlays))
            (send var-line :redraw))))

(defmeth xyplot-proto :scan ()
  (let ((xydata (slot-value 'dataset))
        (status (slot-value 'status-overlay)))
    (send xydata :planes)
    (send status :info  "Interpolating between random planes")
    (send status :redraw)
    (send self :interpolate) 
    (send xydata :insert-in-history)))
 
(defmeth xyplot-proto :local-scan ()
  (let* ((size (slot-value 'neighborhood))
         (xydata (slot-value 'dataset))
         (a0 (send xydata :a0))
         (b0 (send xydata :b0))
         (status (slot-value 'status-overlay)))
    (send xydata :local-planes size)
    (if (= (slot-value 'move-it) 2)
        (progn
         (send status :info
               "Interpolating to target plane")
         (send status :redraw)
         (send self :interpolate)
         (send xydata :insert-in-history)
         (send xydata :a1 a0)
         (send xydata :b1 b0)
         (send status :info
               "Interpolating back to original plane")
         (send status :redraw)
         (send self :interpolate)
         (send xydata :insert-in-history))
        (progn
         (send self :idle-on nil)
         (send status :info "Waiting for the user")
         (send status :redraw)))))
   
(defmeth xyplot-proto :cycle ()
   (let* ((xydata (slot-value 'dataset))
         (a0 (send xydata :a0))
         (b0 (send xydata :b0))
         (status (slot-value 'status-overlay)))
     (send status :info "Cycling between two planes")
     (send status :redraw)
     (send self :interpolate)
     (send xydata :insert-in-history)
     (send xydata :a1 a0)
     (setf xydata :b1 b0)
     (when (not (send self :idle-on))
          (send status :info "Waiting for the user")
          (send status :redraw))))
 
(defmeth xyplot-proto :backtrack ()
  (let* ((xydata (slot-value 'dataset))
         (a0 (send xydata :a0))
         (b0 (send xydata :b0))
         (status (slot-value  'status-overlay))
         (old-depth (slot-value 'depth))
         (depth (if (slot-value 'coming-back) (- old-depth 1) 
                    ( + old-depth 1)))
         (plane (elt (send xydata :history) depth)))
    (send xydata :a1 (first plane))
    (send xydata :b1 (second plane))
    (send status :info
          (format nil "Backtracking from plane ~d to plane ~d"
                  old-depth depth))
    (send status :redraw)
    (send self :interpolate)
    (send self :depth depth)
    (when (or (= depth (slot-value 'backsteps))
              (= depth  (- (length (send xydata :history)) 1)))
          (setf (slot-value 'coming-back) t))
    (when (= depth 0) 
          (setf (slot-value 'coming-back) nil) 
          (send self :idle-on nil)
          (send self :move-it 0))
    (when (not (send self :idle-on))
          (send status :info "Waiting for the user")
          (send status :redraw))))


(defmeth xyplot-proto :do-idle ()
  (let ((flag (send self :move-it)))   
    (case flag
	  (1 (send self :scan))
	  (2 (send self :local-scan))
	  (3 (send self :cycle))
	  (4 (send self :backtrack))
	  (t (send self :idle-on nil) (send self :move-it 0)))))

;;;;
;;;;Biplot prototype
;;;;

(defproto biplot-proto '(num-observation-points) nil xyplot-proto "Biplot")

(defmeth biplot-proto :show-variables ()
  (let* ((num-obs (slot-value 'num-observation-points))
         (num-points (send self :num-points))
         (index (iseq num-obs (- num-points 1)))
         (xydata (slot-value 'dataset))
         (states (send xydata :state))
         (new-states (setf (select states index)
                          (repeat 'normal (- num-points num-obs))))
         (linestarts nil)
         (variables (send xydata :variables))
         (lines (dolist (var variables (reverse linestarts))
                        (let ((lstart '(0))
                              (varl (select (send var :values) index)))
                          (setf linestarts
                                (cons 
                                 (dolist (val varl (reverse lstart))  
                                         (setf lstart
                                               (cons '0 (cons val lstart))))
                                 linestarts))))))
    (send xydata :state states)
    (send self :add-lines lines)))

;;;
;;; xyPlot Menu and Items
;;;

(defproto xyplot-menu-proto '(plot) nil menu-proto "Plot")

(defmeth xyplot-menu-proto :isnew (plot)
  (call-next-method "xyPlot")
  (setf (slot-value 'plot) plot)
  (send self :append-items
        (send menu-item-proto :new "Redraw"
              :action #'(lambda () (send plot :redraw)))
        (send dash-item-proto :new)
        (send menu-item-proto :new "Scan"
              :action #'(lambda () (send plot :move-it 1)
                          (send plot :idle-on t)))
        (send menu-item-proto :new "Local Scan"
              :action #'(lambda () 
                          (let ((nbh (first (get-value-dialog
                      "Size of the neighborhood to be explored (in radians) :"
                                             :initial 0.5))))
                            (cond ((and nbh (> nbh 0))
                                   (send plot :move-it 2)
                                   (send plot :neighborhood nbh)
                                   (send plot :idle-on t))
                              (t (message-dialog
                              "Size of neighborhood can not be 0 or nil"))))))
        (send menu-item-proto :new "Cycle"
              :action #'(lambda () (send plot :move-it 3)
                          (send (send plot :dataset) :planes)
                          (send plot :idle-on t)))
        (send menu-item-proto :new "Backtrack"
              :action #'(lambda () 
                          (cond ((>(length (send (send plot
                                                       :dataset) :history)) 1) 
                                 (let ((bstep (first (get-value-dialog 
                                          "Number of steps to backtrack
                                          (maximum is 99) :" :initial 3))))
                                   (cond ((and bstep (> bstep 0))
                                          (send plot :depth 0)
                                          (send plot :move-it 4) 
                                          (send plot :backsteps bstep)
                                          (send plot :idle-on t))
                                     (t (message-dialog
                                      "Number of steps can't be nil or 0")))))
                            (t (message-dialog
                        "You have no history, backtracking is impossible")))))
        (send menu-item-proto :new "Rotate"
              :action #'(lambda () (send plot :move-it 5)
                          (send plot :idle-on t)))
        (send dash-item-proto :new)
        (send menu-item-proto :new "Hide Plot"
              :action #'(lambda () (send plot :hide-window)))
        (send menu-item-proto :new "Remove Plot"
              :action #'(lambda () (send plot :close)))))


(defproto biplot-menu-proto nil nil xyplot-menu-proto "Biplot Menu")

(defmeth biplot-menu-proto :isnew (plot)
  (call-next-method plot)
  (send self :append-items
        (send dash-item-proto :new)
        (send menu-item-proto :new "Show Variables"
              :action #'(lambda () 
                          (send plot :show-variables)
                          (send plot :adjust-to-data)))))


