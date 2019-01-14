;;;;
;;;; Definition of scatplot-data-proto: inherits from plot-data-proto
;;;;

(defproto scatplot-data-proto '() '(instances) plot-data-proto
  "Scatplot Model")

(defmeth scatplot-data-proto :new (dataset &rest args)
  (let ((scatplot-data (apply #'call-next-method dataset args)))
    (setf (slot-value 'instances)
          (cons scatplot-data (slot-value 'instances)))
    scatplot-data))

(defmeth scatplot-data-proto :isnew (dataset &rest args)
  (setf (slot-value 'name)
        (concatenate 'string
                     (format nil "ScatPlot: ")
                     (format nil "~s" (send (second args) :name))
                     (format nil " ~~ ~s" (send (first args) :name))))
  (setf (slot-value 'x-variables) (first args))
  (setf (slot-value 'y-variables) (second args))
  (call-next-method dataset args))

;;; Other Methods

(defmeth scatplot-data-proto :compute-regfit (conditioning)
  (let* ((x (send self :get-variable-values 'X conditioning))
         (y (send self :get-variable-values 'Y conditioning))
         (n (length x))
         (b (/ (ccp x y) (css x)))
         (a (- (mean y) (* b (mean x))))
         (s-r (sqrt (/ (- (css y) (* (^ b 2) (css x))) (- n 2))))
         (calc-t (/ b (/ s-r (sqrt (css x)))))
         (p (t-cdf calc-t (- n 2))))
    (list a b calc-t (* 2 (min (- 1 p) p)) x s-r)))

(defmeth scatplot-data-proto :compute-corr (conditioning)
  (let* ((x (send self :get-variable-values 'X conditioning))
         (y (send self :get-variable-values 'Y conditioning))
         (n (length x))
         (corr (/ (ccp x y) (sqrt (* (css x) (css y)))))
         (t-value (* corr (sqrt (/ (- n 2) (- 1 (^ corr 2))))))
         (p (t-cdf t-value (- n 2))))
    (list corr t-value (* 2 (min (- 1 p) p)))))

;;;;
;;;; Definition of scatplot-proto: inherits from plot proto
;;;;

(defproto scatplot-proto
  '(smooth-algorithm smooth-parameters smooth-controls) ()
  plot-proto "Scatter Plot")

(defmeth scatplot-proto :isnew (&rest args)
  (let* ((scatplot (apply #'call-next-method args))
         (scatdata (slot-value 'dataset))
         (overlays (slot-value 'overlays))
         (c-rect (send scatplot :content-rect))
         (x-var (send scatdata :x-variables))
         (y-var (send scatdata :y-variables))
         (x-values (send x-var :values))
         (y-values (send y-var :values))
         (x-name (format nil "~a" (send x-var :name)))
         (y-name (format nil "~a" (send y-var :name)))
         (x-int (send scatplot :text-width
                      (with-output-to-string (s) (prin1 (max x-values) s))))
         (y-int (send scatplot :text-width
                      (with-output-to-string (s) (prin1 (max y-values) s))))
         (x-range (get-nice-range (min x-values) (max x-values)
                                  (min (floor (/ (third c-rect) x-int)) 6)))
         (y-range (get-nice-range (min y-values) (max y-values)
                                  (min (floor (/ (fourth c-rect) y-int)) 6)))
         (menu (send scatplot-menu-proto :new scatplot))
         (regfit (send regfit-overlay-proto :new :lines 4))
         (corr (send corr-overlay-proto :new :lines 3)))
    (dolist (overlay overlays)
            (if (kind-of-p overlay menu-overlay-proto)
                (send overlay :string "ScatPlot")))
    (send scatplot :add-overlay regfit)
    (send scatplot :add-overlay corr)
    (send scatplot :summaries (list regfit corr))
    (send scatplot :variable-label '(0 1) (list x-name y-name))
    (send scatplot :plot-menu menu)
    (send scatplot :x-axis t t 0 :draw nil)
    (send scatplot :y-axis t t 0 :draw nil)
    (let* ((rect (send scatplot :content-rect))
           (x-range (get-nice-range (min x-values) (max x-values)
                                    (min (floor (/ (third rect) x-int)) 6)))
           (y-range (get-nice-range (min y-values) (max y-values)
                                    (min (floor (/ (fourth rect) y-int)) 6))))
      (send scatplot :range 0 (first x-range) (second x-range) :draw nil)
      (send scatplot :range 1 (first y-range) (second y-range) :draw nil)
      (send scatplot :x-axis t t (third x-range) :draw nil)
      (send scatplot :y-axis t t (third y-range) :draw nil)) 
    (send scatplot :smooth-algorithm 0)
    (send scatplot :smooth-parameters nil)
    (send scatplot :smooth-controls nil)
    scatplot))

;;;
;;; Constucts scatplots for each x and y variable
;;;

(defun make-scatplot (x-var y-var dataset)
  (let* ((scat-data (send scatplot-data-proto :new dataset x-var y-var))
         (title (send scat-data :name))
         (scat-plot (send scatplot-proto :new 2 :title title
                          :dataset scat-data :size '(400 300) :show nil))
         (z-variables (send scat-data :z-variables)))
    (when (not z-variables)
          (send scat-plot :needs-adjusting t)
          (send scat-plot :adjust-screen))
    (send scat-plot :redraw-bottom)
    (send *data-tree* :register-dataset-with-plot
          dataset scat-data scat-plot)
    (send scat-plot :show-window)
    scat-data))

;;;
;;;  Slot Accessors and Mutators
;;;

(defmeth scatplot-proto :smooth-algorithm (&optional smooth-algorithm)
  (if smooth-algorithm (setf (slot-value 'smooth-algorithm) smooth-algorithm))
  (slot-value 'smooth-algorithm))

(defmeth scatplot-proto :smooth-parameters (&optional smooth-parameters)
  (if smooth-parameters
      (setf (slot-value 'smooth-parameters) smooth-parameters))
  (slot-value 'smooth-parameters))

(defmeth scatplot-proto :smooth-controls (&optional smooth-controls)
  (if smooth-controls (setf (slot-value 'smooth-controls) smooth-controls))
  (slot-value 'smooth-controls))

;;
;; Redraw method for the scatplot
;;

(defmeth scatplot-proto :redraw-content ()
  (if (not (= (send self :smooth-algorithm) 0))
      (send self :add-smoothing-line))
  (call-next-method))

(defmeth scatplot-proto :redraw-lines ()
  (let ((summaries (slot-value 'summaries)))
    (send self :clear-lines :draw nil)
    (if (send (first summaries) :visible) (send self :reg-fit))
    (send self :redraw-content)))

;;;
;;;  Other Methods
;;;

(defmeth scatplot-proto :adjust-plot ()
  nil)

(defmeth scatplot-proto :reg-fit ()
  (let* ((dataset (slot-value 'dataset))
         (z-vars (send dataset :z-variables))
         (regfit (send dataset :compute-regfit z-vars))
         (a (first regfit))
         (b (second regfit))
         (x (fifth regfit))
         (x-min (min x))
         (x-max (max x))
         (x-seq (rseq x-min x-max 30))
         (y-hat (+ a (* b x-seq)))
         (sq-dev (^ (- x-seq (mean x)) 2))
         (error (* (t-quant .975 (- (length x) 2)) (sixth regfit)
                   (sqrt (+ (/ 1 (length x)) (/ sq-dev (css x))))))
         (num-lines (send self :num-lines)))
    (send self :add-lines (list (list x-min x-max)
                                (list (+ a (* b x-min)) (+ a (* b x-max))))
          :draw nil)
    (send self :add-lines (list x-seq (+ y-hat error))
          :type 'dashed :draw nil)
    (send self :add-lines (list x-seq (- y-hat error))
          :type 'dashed :draw nil)
    (send self :linestart-color (iseq num-lines (+ num-lines 1)) 'blue)
    (setf num-lines (+ num-lines 1))
    (send self :linestart-color (iseq num-lines (+ num-lines 59)) 'blue)))

(defmeth scatplot-proto :corr-fit ()
  (let* ((dataset (slot-value 'dataset))
         (z-vars (send dataset :z-variables))
         (corr (send dataset :compute-corr z-vars))
         (num-lines (send self :num-lines)))
    nil))

(defmeth scatplot-proto :add-smoothing-line ()
  (let* ((param1 (select (send self :smooth-parameters) 0))
         (param2 (select (send self :smooth-parameters) 1))
         (param3 (select (send self :smooth-parameters) 2))
         (dataset (slot-value 'dataset))
         (x-var (send (send dataset :x-variables) :values))
         (y-var (send (send dataset :y-variables) :values))
         (visible-x nil)
         (visible-y nil)
         (states (send dataset :state))
         (num-obs (length states))
         (mask (send dataset :mask))
         (z-mask (send dataset :z-mask))
         (or-mask (map-elements #'vector-or mask z-mask)))
    (dotimes (i num-obs)
             (when (not (select or-mask i))
                   (setf visible-x (cons (select x-var i) visible-x))
                   (setf visible-y (cons (select y-var i) visible-y))))
    (setf x-range (- (max visible-x) (min visible-x)))
    (cond 
      ((and (> (length visible-x) 1) (= (send self :smooth-algorithm) 1))
       (send self :add-lines (lowess visible-x visible-y :f param1)))
      ((and (> (length visible-x) 1) (= (send self :smooth-algorithm) 2))
       (send self :add-lines
             (kernel-smooth visible-x visible-y :type param3 :x-vals param2
                            :width (* param1 x-range)))))))

(defmeth scatplot-proto :remove-smoothing-line ()
  (send self :redraw-lines))

(defmeth scatplot-proto :add-lowess-controls ()
  (let* ((dataset (slot-value 'dataset))
         (x-var (send (send dataset :x-variables) :values))
         (x-max (max x-var))
         (x-min (min x-var))
         (x-range (- x-max x-min))
         (z-vars (send dataset :z-variables))
         (mask-overlay-height (if z-vars 50 25))
         (frac-slider (send slider-overlay-proto
                            :new "F" (list 260 8) (list 50 10)
                            5 (list .1 1) 1 .1 .1)))
    (send self :add-overlay frac-slider)
    (send self :smooth-controls (list frac-slider nil nil))
    (send self :smooth-parameters (list .5 2 (* .02 x-range)))
    (send self :smooth-algorithm 1)
    (send self :redraw)))

(defmeth scatplot-proto :add-kernel-controls ()
  (let* ((dataset (slot-value 'dataset)) 
         (x-var (send (send dataset :x-variables) :values))
         (x-max (max x-var))
         (x-min (min x-var))
         (x-range (- x-max x-min))
         (z-vars (send dataset :z-variables))
         (mask-overlay-height (if z-vars 50 25))
         (type-slider (send kernel-overlay-proto
                            :new "W" (list 260 8) (list 53 10) 10
                            (list .05 1) 1 .05 .05)))
    (send self :add-overlay type-slider)
    (send self :smooth-controls (list type-slider nil nil))
    (send self :smooth-parameters (list .5 30 'g))
    (send self :smooth-algorithm 2)
    (send self :redraw)))

(defmeth scatplot-proto :remove-smoothing-controls ()
  (let* ((dataset (slot-value 'dataset))
         (z-vars (send dataset :z-variables))
         (mask-overlay-height (if z-vars 50 25)))
    (send self :delete-overlay (select (send self :smooth-controls) 0))
    (send self :smooth-algorithm 0)
    (send self :smooth-parameters nil)
    (send self :smooth-controls nil)
    (send self :remove-smoothing-line)
    (send self :redraw)))

;;;
;;;  Method to add or remove a lowess line from a plot
;;;

(defmeth scatplot-proto :add-remove-lowess ()
  (let* ((plot-menu (send self :plot-menu))
         (items (send plot-menu :items)))
    (cond
      ((send (select items 6) :mark)
       (send self :smooth-algorithm 0)
       (send self :remove-smoothing-controls)
       (mark-items (select items 6) (select items 7)))
      ((not (send (select items 6) :mark))
       (if (= (send self :smooth-algorithm) 2)
           (send self :remove-smoothing-controls))
       (send self :smooth-algorithm 1)
       (send self :add-lowess-controls)
       (mark-items (select items 6) (select items 7))))))

;;;
;;;  Method to add or remove a kernel line from a plot
;;;

(defmeth scatplot-proto :add-remove-kernel ()
  (let* ((plot-menu (send self :plot-menu))
         (items (send plot-menu :items)))
    (cond
      ((send (select items 7) :mark)
       (send self :smooth-algorithm 0)
       (send self :remove-smoothing-controls)
       (mark-items (select items 7) (select items 6)))
      ((not (send (select items 7) :mark))
       (if (= (send self :smooth-algorithm) 1)
           (send self :remove-smoothing-controls))
       (send self :smooth-algorithm 2)
       (send self :add-kernel-controls)
       (mark-items (select items 7) (select items 6))))))

;;;;
;;;; Definition of scatplot-menu-proto: inherits from plot-menu-proto
;;;;

(defproto scatplot-menu-proto '() nil plot-menu-proto "Scatplot Menu")

(defmeth scatplot-menu-proto :isnew (plot)
  (let* ((lowess (send menu-item-proto :new "Lowess"
                       :action #'(lambda () (send plot :add-remove-lowess))))
         (kernel (send menu-item-proto :new "Kernel"
                       :action #'(lambda () (send plot :add-remove-kernel))))
         (hide (send menu-item-proto :new "Hide Plot"
                     :action #'(lambda () (send plot :hide-window)))))
    (call-next-method plot)
    (send self :append-items (send dash-item-proto :new) lowess kernel
          (send dash-item-proto :new) hide)))

;;
;; Definition of scatplot-model-menu-proto: inherits from menu-proto             ;;

(defproto scatplot-model-menu-proto '() nil menu-proto
  "ScatPlot Model Menu")

(defmeth scatplot-model-menu-proto :isnew (master-view)
  (call-next-method "ScatPlot-Model")
  (send self :append-items
        (send menu-item-proto :new "Messages")
        (send dash-item-proto :new)
        (send menu-item-proto :new "Show ScatPlot"
              :action #'(lambda () 
                          (send master-view :show-view scatplot-proto)))
        (send menu-item-proto :new "Hide ScatPlot" 
              :action #'(lambda () (send master-view
                                         :hide-view scatplot-proto)))
        (send dash-item-proto :new)
        (send menu-item-proto :new "Delete ScatPlot" 
              :action #'(lambda () 
                          (send master-view :delete-model)))))

;;;
;;;  Plot helper function to mark and unmark menu items
;;;

(defun mark-items (item-to-mark item-to-unmark)
  (if (send item-to-mark :mark)
      (send item-to-mark :mark nil)
      (send item-to-mark :mark t))
  (if (send item-to-unmark :mark)
      (send item-to-unmark :mark nil)))


        
              