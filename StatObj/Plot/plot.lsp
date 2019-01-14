;;;;
;;;;  plot-data Prototype
;;;;      is the data model for the plot
;;;;

(defproto plot-data-proto '(x-jittered) () dataset-proto "Plot Model")

;;;
;;;  Instance initialization method for plot-data-proto
;;;

(defmeth plot-data-proto :isnew (dataset &rest args)
  (reset-graphics-buffer)
  (let* ((args (first args))
         (label-variable (first (send dataset :variables-of-role 'L)))
         (z-variables (send dataset :variables-of-role 'Z))
         (z-vars (if (> (length z-variables) 3)
                     (select z-variables '(0 1 2)) z-variables))
         (str nil))
    (dolist (z-var (reverse z-vars))
            (setf str (concatenate 'string
                                   (format nil " ~s"
                                           (send z-var :name)) str)))
    (setf (slot-value 'name)
          (concatenate 'string
                       (slot-value 'name)
                       (if z-vars (format nil " |")) str))
    (setf (slot-value 'z-names) str)
    (setf (slot-value 'z-variables) z-vars)
    (setf (slot-value 'variables) (append args z-variables))
    (when label-variable
          (setf (slot-value 'label-variable) label-variable)
          (setf (slot-value 'variables)
                (cons label-variable (slot-value 'variables))))
    (setf (slot-value 'owner) dataset)
    (setf (slot-value 'mask) (copy-list (send dataset :mask)))
    (setf (slot-value 'z-mask) (copy-list (send dataset :z-mask)))
    (setf (slot-value 'symbol) (send dataset :slot-value 'symbol))
    (setf (slot-value 'color) (send dataset :slot-value 'color))
    (setf (slot-value 'state) (send dataset :slot-value 'state))))

;;; Slot Accessors and Mutators

(defmeth plot-data-proto :x-jittered (&optional x)
  (if x (setf (slot-value 'x-jittered) x))
  (slot-value 'x-jittered))

;;; Other Methods

(defmeth plot-data-proto :get-variable-values (type conditioning)
  (let* ((var (with-input-from-string (s (format nil ":~s-variables" type))
                                      (read s)))
         (z-vars (send self :z-variables))
         (mask (send self :mask))
         (z-mask (send self :z-mask))
         (or-mask (map-elements #'vector-or mask z-mask)))
    (if (and z-vars conditioning)
        (select (send (send self var) :values)
                (which (mapcar #'(lambda (val) (not val)) or-mask)))
        (select (send (send self var) :values)
                (which (mapcar #'(lambda (val) (not val)) mask))))))

(defmeth plot-data-proto :jitter (var)
  (let* ((values (send var :values)))
    (setf (slot-value 'x-jittered)
          (+ values (/ (- (uniform-rand (length values)) .5) 20)))))

;;;;
;;;;   plot Prototype
;;;;       is the (coplot) graphics view of plot-data
;;;;

(defproto plot-proto '(title plot-menu name-list summaries visible fit) ()
  (list graph-proto graph-mixin) "Plot")

(defmeth plot-proto :isnew (&rest args)
  (let* ((plot (apply #'call-next-method args))
         (data (slot-value 'dataset))
         (z-vars (send data :z-variables))
         (offset (+ *line-height* 10))
         (top-height (+ (if z-vars (* 2 offset) offset) 2))
         (width (send plot :canvas-width))
         (has-color (send plot :use-color t))
         (mask (send mask-overlay-proto
                           :new 0 width 0 top-height))
         (menu (send menu-overlay-proto :new 0 'plot "Plot"))
         (label (send label-button-overlay-proto :new 7))
         (select (send select-button-overlay-proto :new 8))
         (brush (send brush-button-overlay-proto :new 9))
         (symbol (send symbol-button-overlay-proto :new 11))
         (color (if has-color
                    (send color-button-overlay-proto :new 15)))
         (link (send link-overlay-proto :new width))
         (names (send name-list-overlay-proto :new width)))
    (send plot :margin 0 top-height 0 0 :draw nil)
    (send plot :add-overlay mask)
    (send plot :add-overlay menu)
    (send plot :add-overlay label)
    (send plot :add-overlay select)
    (send plot :add-overlay brush)
    (send plot :add-overlay symbol)
    (if has-color
        (send plot :add-overlay color))
    (send plot :add-overlay link)
    (send plot :add-overlay names)
    (if z-vars
        (send plot :add-sliders z-vars data plot))
    (send plot :menu nil)
    plot))

(defun synchronize-plots (plot)
  (let* ((dataset (send plot :dataset))
         (ancestor (send *data-tree* :get-ancestor dataset))
         (plot-datasets
          (rest (element-seq (send *data-tree*
                                   :get-dataset-subtree ancestor)))))
    (dolist (plot (active-windows))
            (if (kind-of-p plot plot-proto)
                (let ((dataset (send plot :dataset)))
                  (when (member dataset plot-datasets)
                        (send plot :needs-adjusting t)
                        (send plot :adjust-screen))))
            (if (kind-of-p plot data-list-proto)
                (send plot :redraw))
            (if (find "multivariate" *modules* :test #'equal)
                (if (kind-of-p plot xyplot-proto)
                    (let ((dataset (send plot :dataset)))
                      (when (member dataset plot-datasets)
                            (send plot :needs-adjusting t)
                            (send plot :adjust-screen))))))))

;;;
;;; Constucts a generic yPlot for each y variable
;;;

(defun make-yPlot (dataset)
  (let ((y-variables (send dataset :variables-of-role 'Y)))
    (if (eql (send dataset :z-type) nil)
        (message-dialog "Z-variables are of mixed types")
        (dolist (y-var y-variables)
                (case (send y-var :type)
                  ('N (make-histogram y-var dataset))
                  ('C (message-dialog
                       (concatenate 'string
                        (format nil "Dynamic barChart for ")
                        (format nil  "~s" (send y-var :name))
                        (format nil " is not yet available.")))))))))

;;;
;;; Constucts a generic xyPlot for each y variable
;;;

(defun make-xyPlot (dataset)
  (let ((x-variables (send dataset :variables-of-role 'X))
        (y-variables (send dataset :variables-of-role 'Y)))
    (if (eql (send dataset :z-type) nil)
        (message-dialog "Z-variables are of mixed types")
        (dolist (x-var x-variables)
                (if (eql (send x-var :type) 'C)
                    (dolist (y-var y-variables)
                            (case (send y-var :type)
                              ('N (make-dotplot x-var y-var dataset))
                              ('C (message-dialog
                                   (concatenate 'string
                                    (format nil "Dynamic mosaicPlot for ")
                                    (format nil  "~s vs " (send x-var :name))
                                    (format nil  "~s~%" (send y-var :name))
                                    (format nil "is not yet available."))))))
                    (dolist (y-var y-variables)
                            (case (send y-var :type)
                              ('N (make-scatplot x-var y-var dataset))
                              ('C (message-dialog
                                   (concatenate 'string
                                    (format nil "Dynamic probPlot for ")
                                    (format nil  "~s vs " (send x-var :name))
                                    (format nil  "~s~%" (send y-var :name))
                                    (format nil
                                            "is not yet available.")))))))))))

;;;
;;;  Slot Accessors and Mutators
;;;

(defmeth plot-proto :title (&optional title)
  (if title (setf (slot-value 'title) title))
  (slot-value 'title))

(defmeth plot-proto :plot-menu (&optional plot-menu)
  (if plot-menu (setf (slot-value 'plot-menu) plot-menu))
  (slot-value 'plot-menu))

(defmeth plot-proto :name-list (&optional (name-list nil set-name-list))
  (if set-name-list (setf (slot-value 'name-list) name-list))
  (slot-value 'name-list))

(defmeth plot-proto :summaries (&optional (summaries nil set-summaries))
  (if set-summaries (setf (slot-value 'summaries) summaries))
  (slot-value 'summaries))

(defmeth plot-proto :visible (&optional (visible nil set-visible))
  (if set-visible (setf (slot-value 'visible) visible))
  (slot-value 'visible))

(defmeth plot-proto :fit (&optional (fit nil set-fit))
  (if set-fit (setf (slot-value 'fit) fit))
  (slot-value 'fit))


;;;
;;;   Plot-proto Redraw Methods
;;;

(defmeth plot-proto :redraw ()
  (let* ((overlays (slot-value 'overlays))
         (width (send self :canvas-width)))
    (if overlays
        (dolist (overlay overlays)
                (if (kind-of-p overlay link-overlay-proto)
                    (send overlay :width width))
                (if (kind-of-p overlay name-list-overlay-proto)
                    (send overlay :width width))))
    (call-next-method)))

(defmeth plot-proto :adjust-plot ()
  nil)

(defmeth plot-proto :redraw-bottom ()
  (let* ((summaries (send self :summaries))
         (margin (send self :margin))
         (width (first (send self :size)))
         (height (second (send self :size)))
         (bottomless-height (- height (fourth margin)))
         (lines 0)
         (line-height *line-height*))
    (dolist (summary summaries)
            (if (send summary :visible)
                (setf lines (+ lines (send summary :lines)))
                (setf lines (+ lines 1))))
    (send self :margin 0 (second margin) 0 (* lines line-height) :draw nil)
    (send self :size width (+ bottomless-height (* lines line-height)))))

(defmeth plot-proto :adjust-screen ()
  (if (send self :needs-adjusting)
      (let* ((dataset (slot-value 'dataset))
             (num-obs (send dataset :num-obs))
             (z-vars (send dataset :z-variables))
             (label-variable (send dataset :label-variable))
             (num-dim (if (kind-of-p self hist-proto)
                          (- (send self :num-variables) 1)
                          (send self :num-variables)))
             (variables (if label-variable
                            (select (send dataset :variables)
                                    (iseq 1 num-dim))
                            (select (send dataset :variables)
                                    (iseq num-dim))))
             (contents (send self :content-rect))
             (mask (send dataset :mask))
             (z-mask (send dataset :z-mask))
             (or-mask (map-elements #'vector-or mask z-mask))
             (not-mask (mapcar #'(lambda (val) (not val)) or-mask))
             (label-values (if label-variable
                               (send label-variable :values)
                               (iseq 1 num-obs))))
        (send self :clear-points :draw nil)
        (when (< 0 num-obs)
              (send self :start-buffering)
              (send self :add-points
                    (mapcar #'(lambda (var) (send var :values)) variables)
                    :draw nil)
              (send self :point-state (iseq num-obs) (send dataset :state))
              (if z-vars
                  (send self :point-state (which or-mask) 'invisible))
              (send self :point-color (which not-mask)
                    (select (send dataset :color) (which not-mask)))
              (send self :point-symbol (iseq num-obs) (send dataset :symbol))
              (send self :point-label (which not-mask)
                    (mapcar #'(lambda (value)
                                (format nil "~a" value))
                            (select label-values (which not-mask))))
              (send self :redraw-content)
              (send self :buffer-to-screen (first contents) (second contents)
                    (+ (third contents) 1) (fourth contents))
              (send self :needs-adjusting nil)))))

(defmeth plot-proto :unselect-all-points ()
  (let* ((dataset (slot-value 'dataset))
         (states (send dataset :state))
         (points-selected (send self :selection))
         (points-hilited (send self :points-hilited))
         (points (union points-selected points-hilited)))
    (when points
          (dolist (i points)
                  (setf (select states i) 'normal))
          (send dataset :state states))))

(defmeth plot-proto :adjust-points (slot type)
  (let* ((dataset (slot-value 'dataset))
         (slot-values (send dataset slot))
         (points-selected (send self :selection)))
    (when points-selected
          (dolist (i points-selected)
                  (setf (select slot-values i) type))
          (send dataset slot slot-values)
          (synchronize-plots self))))

(defmeth plot-proto :adjust-points-in-rect (x y width height state)
  (let* ((dataset (slot-value 'dataset))
         (num-obs (send dataset :num-obs))
         (states (send dataset :state))
         (points (send self :points-in-rect x y width height))
         (points-selected (send self :points-selected))
         (points-hilited (send self :points-hilited)))
    (when points
          (case state
            (selected
             (dolist (i (set-difference points points-selected))
                     (setf (select states i) 'selected)))
            (hilited
             (let* ((points (set-difference points points-selected))
                    (new (set-difference points points-hilited))
                    (old (set-difference points-hilited points)))
               (if new
                   (dolist (i new)
                           (setf (select states i) 'hilited)))
               (if old
                   (dolist (i old)
                           (setf (select states i) 'normal))))))
          (send dataset :state states))
    (synchronize-plots self)))

;;;
;;;  Other Methods
;;;

(defmeth plot-proto :close ()
  (let ((name-list (send self :name-list)))
    (send self :hide-window)
    (if name-list (send name-list :remove))
    (send self :name-list nil)))

;;;
;;;  Method to add the slider-overlays
;;;

(defmeth plot-proto :add-sliders (z-vars dataset plot)
  (let* ((num-vars (if (<= (length z-vars) 3) (length z-vars) 3)))
    (dotimes (i num-vars)
             (let* ((name (format nil "~a"
                                  (send (select z-vars i) :name)))
                    (pos (list (+ (* (+ i 1) 5) (* 120 i)) 35))
                    (dims (list 10 120))
                    (cond-var (select z-vars i))
                    (new-shingles
                     (send cond-var
                           :shingles (send cond-var :make-shingles 0)))
                    (range
                     (if (equal (type-of new-shingles) 'array)
                         (list (select new-shingles 0 0)
                               (select new-shingles 0 1))
                         (list (select new-shingles 0) nil)))
                    (new-slider
                     (if (equal (type-of new-shingles) 'array) 
                         (send variable-slider-overlay-proto
                               :new name pos dims range 0 cond-var dataset)
                         (send cat-variable-slider-overlay-proto
                               :new name pos dims range 0 cond-var dataset))))
               (send plot :add-overlay new-slider)))))

;;;
;;;  plot-menu-proto:
;;;      inherits from menu-proto and adds the slot 'plot'.
;;;

(defproto plot-menu-proto '(plot) nil menu-proto "Plot")

;;;
;;;  Initilization method for plot-menu-proto
;;;

(defmeth plot-menu-proto :isnew (plot)
  (setf (slot-value 'plot) plot)
  (let* ((message-item (send menu-item-proto :new "Messages"))
         (dash-item (send dash-item-proto :new))
         (redraw-item (send menu-item-proto :new "Redraw"
                            :action #'(lambda () (send plot :redraw))))
         (brush-item (send menu-item-proto :new "Resize Brush"
                           :action #'(lambda () (send plot :resize-brush))))
         (dash-item (send dash-item-proto :new))
         (rescale-item
          (send menu-item-proto :new "Rescale"
                :action #'(lambda () (send plot :adjust-to-data))))
         (options-item (send menu-item-proto :new "Options..."
                             :action #'(lambda () (send plot :set-options))))
         (dash-item (send dash-item-proto :new))
         (erase-selection
          (send menu-item-proto :new "Erase Selection"
                :action #'(lambda () (send plot :erase-selection))))
         (focus-on-selection
          (send menu-item-proto :new "Focus on Selection"
                :action #'(lambda () (send plot :focus-on-selection))))
         (show-all
          (send menu-item-proto :new "Show All"
                :action #'(lambda () (send plot :show-all-points)))))
    (call-next-method "Plot")
    (send self
          :append-items message-item dash-item redraw-item brush-item
          options-item)))
