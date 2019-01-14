;;;;
;;;; Definition of dotplot-data-proto: inherits from plot-data-proto
;;;;

(defproto dotplot-data-proto '(x-name x-levels ci-level ref-level)
  '(instances) plot-data-proto "DotPlot Dataset")

(defmeth dotplot-data-proto :new (dataset &rest args)
  (let ((dotplot-data (apply #'call-next-method dataset args)))
    (setf (slot-value 'instances)
          (cons dotplot-data (slot-value 'instances)))
    dotplot-data))

(defmeth dotplot-data-proto :isnew (dataset &rest args)
  (let* ((x-var (first args))
         (x-values (+ (send x-var :make-formal-values) 1))
         (formal-levels (make-variable x-values :name 'formal-levels
                                       :role 'X)))
    (setf (slot-value 'name)
          (concatenate 'string
                       (format nil "DotPlot: ")
                       (format nil "~s" (send (second args) :name))
                       (format nil " ~~ ~s" (send (first args) :name))))
    (setf (slot-value 'x-variables) formal-levels)
    (setf (slot-value 'x-name) (send x-var :name))
    (setf (slot-value 'x-levels) (send x-var :actual-levels))
    (setf (slot-value 'ref-level) (first (slot-value 'x-levels)))
    (setf (slot-value 'y-variables) (second args))
    (setf (slot-value 'ci-level) 0.95)
    (call-next-method dataset (list formal-levels (second args)))))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth dotplot-data-proto :x-name (&optional (name nil set-name))
  (if set-name (setf (slot-value 'x-name) name))
  (slot-value 'x-name))

(defmeth dotplot-data-proto :x-levels (&optional (levels nil set-levels))
  (if set-levels (setf (slot-value 'x-levels) levels))
  (slot-value 'x-levels))

(defmeth dotplot-data-proto :ci-level (&optional ci-level)
  (if ci-level (setf (slot-value 'ci-level) ci-level))
  (slot-value 'ci-level))

(defmeth dotplot-data-proto :ref-level (&optional ref-level)
  (if ref-level (setf (slot-value 'ref-level) ref-level))
  (slot-value 'ref-level))

;;; Other Methods

(defmeth dotplot-data-proto :compute-moments (conditioning)
  (let* ((x-values (send self :get-variable-values 'X conditioning))
         (x-levels (sort (remove-duplicates x-values) #'<))
         (y-values (send self :get-variable-values 'Y conditioning))
         (data nil))
    (dolist (level x-levels)
            (setf data (cons (select y-values (which (= level x-values)))
                             data)))
    (let* ((data (reverse data))
           (n (mapcar #'length data))
           (mean (mapcar #'mean data))
           (std-dev (mapcar #'standard-deviation data))
           (std-err (/ std-dev (sqrt n))))
      (list n mean std-dev std-err))))

(defmeth dotplot-data-proto :compute-test (conditioning)
  (let* ((moments (send self :compute-moments conditioning))
         (n (first moments))
         (num-df (- (length (send self :x-levels)) 1))
         (denom-df (sum (- n 1)))
         (means (second moments))
         (grand-mean (/ (inner-product n means) (sum n)))
         (between (/ (inner-product n (^ (- means grand-mean) 2)) num-df))
         (std-devs (third moments))
         (within (/ (inner-product (- n 1) (^ std-devs 2)) denom-df))
         (f-value (/ between within))
         (p-value (- 1 (f-cdf f-value num-df denom-df))))
    (list f-value p-value num-df denom-df)))

(defmeth dotplot-data-proto :compute-diffs (conditioning)
  (let* ((moments (send self :compute-moments conditioning))
         (n (first moments))
         (denom-df (sum (- n 1)))
         (ci-level (slot-value 'ci-level))
         (t-tab (t-quant (- 1 (/ (- 1 ci-level) 2)) denom-df))
         (means (second moments))
         (std-devs (third moments))
         (within (/ (inner-product (- n 1) (^ std-devs 2)) denom-df))
         (x-levels (slot-value 'x-levels))
         (ref-position (position (slot-value 'ref-level) x-levels))
         (comp-positions (which (/= ref-position (iseq (length x-levels)))))
         (mean-diffs (- (select means ref-position)
                        (select means comp-positions)))
         (error-terms (* t-tab (sqrt within)
                         (sqrt (+ (/ (select n ref-position))
                                  (/ (select n comp-positions)))))))
    (list mean-diffs error-terms)))

;;;;
;;;; Definition of dotplot-proto: inherits from scatplot-proto
;;;;

(defproto dotplot-proto '(ci-menu) () plot-proto "Dot Plot")

(defmeth dotplot-proto :isnew (&rest args)
  (let* ((dotplot (apply #'call-next-method args))
         (dotdata (slot-value 'dataset))
         (overlays (slot-value 'overlays))
         (x-var (first (send dotdata :variables-of-role 'X)))
         (y-var (first (send dotdata :variables-of-role 'Y)))
         (x-values (send x-var :values))
         (y-values (send y-var :values))
         (x-name (format nil "~a" (send dotdata :x-name)))
         (y-name (format nil "~a" (send y-var :name)))
         (num-levels (length (send x-var :make-unique-values)))
         (y-int (send dotplot :text-width
                      (with-output-to-string (s) (prin1 (max y-values) s))))
         (menu (send dotplot-menu-proto :new dotplot))
         (ci-menu (send ci-menu-proto :new dotplot))
         (f-test (send f-test-overlay-proto :new :lines 3))
         (means (send means-overlay-proto :new :lines (+ num-levels 1)))
         (comparisons (send comparisons-overlay-proto
                            :new :lines (+ num-levels 2))))
    (dolist (overlay overlays)
            (if (kind-of-p overlay menu-overlay-proto)
                (send overlay :string "DotPlot")))
    (send dotplot :add-overlay f-test)
    (send dotplot :add-overlay means)
    (send dotplot :add-overlay comparisons)
    (send dotplot :summaries (list f-test means comparisons))
    (send dotplot :variable-label '(0 1) (list x-name y-name))
    (send dotplot :plot-menu menu)
    (send dotplot :ci-menu ci-menu)
    (send dotplot :x-axis t t 0 :draw nil)
    (send dotplot :y-axis t t 0 :draw nil)
    (let* ((rect (send dotplot :content-rect))
           (y-range (get-nice-range (min y-values) (max y-values)
                                    (min (floor (/ (fourth rect) y-int)) 6))))
      (send dotplot :range 0 .5 (+ num-levels .5) :draw nil)
      (send dotplot :range 1 (first y-range) (second y-range) :draw nil)
      (send dotplot :x-axis t t 0 :draw nil)
      (send dotplot :y-axis t t (third y-range) :draw nil))
    dotplot))

;;;
;;; Constucts scatplots for each x and y variable
;;;

(defun make-dotplot (x-var y-var dataset)
  (let* ((dot-data (send dotplot-data-proto :new dataset x-var y-var))
         (title (send dot-data :name))
         (dot-plot (send dotplot-proto :new 2 :title title
                         :dataset dot-data :size '(400 300) :show nil))
         (z-variables (send dot-data :z-variables)))
    (when (not z-variables)
          (send dot-plot :needs-adjusting t)
          (send dot-plot :adjust-screen))
    (send dot-plot :redraw-bottom)
    (send *data-tree* :register-dataset-with-plot
          dataset dot-data dot-plot)
    (send dot-plot :show-window)
    dot-data))

;;;
;;;  Slot Accessors and Mutators
;;;

(defmeth dotplot-proto :ci-menu (&optional (ci-menu nil set-ci-menu))
  (if set-ci-menu (setf (slot-value 'ci-menu) ci-menu))
  (slot-value 'ci-menu))

;;
;; Redraw methods for the dotplot
;;

(defmeth dotplot-proto :redraw-background ()
  (call-next-method)
  (let* ((levels (send (slot-value 'dataset) :x-levels))
         (k (length levels))
         (origin (send self :content-origin))
         (y-origin (second origin))
         (width (third (send self :content-rect)))
         (increment (round (/ width (* 2 k))))
         (x-pos (+ (first origin) increment))
         (y-pos (+ y-origin 15)))
    (dolist (level levels)
            (let* ((str (format nil "~s" level))
                   (tw (send self :text-width str))
                   (adj (- (position level levels) 1))
                   (x-start (- (+ x-pos increment) adj))
                   (x-adj (- x-pos (round (/ tw 2)) adj)))
              (send self :draw-line x-start y-origin x-start (+ y-origin 5))
              (send self :draw-string str x-adj y-pos)
              (setf x-pos (+ x-pos (* 2 increment)))))))

(defmeth dotplot-proto :redraw-lines ()
  (let ((summaries (slot-value 'summaries)))
    (send self :clear-lines :draw nil)
    (send self :grand-mean-fit)
    (if (send self :fit) (send self :means-fit))
    (if (send (second summaries) :visible) (send self :means-se))
    (send self :redraw-content)))

;;;
;;;  Other Methods
;;;

(defmeth dotplot-proto :means-fit ()
  (let* ((dataset (send self :dataset))
         (z-vars (send dataset :z-variables))
         (means (second (send dataset :compute-moments z-vars))))
    (send self :add-lines (list (+ (iseq (length means)) 1) means))
    (send self :linestart-color
          (iseq (- (send self :num-lines) (length means))
                (- (send self :num-lines) 1)) 'green)))

(defmeth dotplot-proto :grand-mean-fit ()
  (let* ((dataset (send self :dataset))
         (num-levels (length (send dataset :x-levels)))
         (z-vars (send dataset :z-variables))
         (moments (send dataset :compute-moments z-vars))
         (n (first moments))
         (means (second moments))
         (grand-mean (/ (inner-product n means) (sum n))))
    (send self :add-lines (list (list 0 (+ num-levels 0.5))
          (list grand-mean grand-mean)))))

(defmeth dotplot-proto :means-se ()
  (let* ((dataset (send self :dataset))
         (x-levels (send dataset :x-levels))
         (z-vars (send dataset :z-variables))
         (moments (send dataset :compute-moments z-vars))
         (means (second moments))
         (std-errs (fourth moments))
         (i 0))
    (dolist (level x-levels nil)
            (let ((x1 (- (+ i 1) 0.05))
                  (x2 (+ (+ i 1) 0.05))
                  (x-se (- (elt means i) (elt std-errs i)))
                  (x (elt means i))
                  (x+se (+ (elt means i) (elt std-errs i))))
              (send self :add-lines
                    (list (list (+ i 0.99) (+ i 1.01))
                          (list x x)))
              (send self :linestart-width
                    (iseq (- (send self :num-lines) 2)
                          (- (send self :num-lines) 1)) 2)
              (send self :add-lines (list (list x1 x2) (list x-se x-se)))
              (send self :add-lines (list (list x1 x2) (list x+se x+se)))
              (send self :add-lines (list (repeat (+ i 1) 2)
                                          (list x-se x+se)))
              (send self :linestart-color
                    (iseq (- (send self :num-lines) 8)
                          (- (send self :num-lines) 1)) 'green))
            (setf i (+ i 1)))))

;;; Mark menu items

(defmeth dotplot-proto :set-menu ()
  (let* ((plot-menu (send self :plot-menu))
         (items (send plot-menu :items)))
    (send (select items 6) :mark (send self :fit))))

;;;;
;;;; Definition of dotplot-menu-proto: inherits from plot-menu-proto
;;;;

(defproto dotplot-menu-proto '() nil plot-menu-proto "Dotplot Menu")

(defmeth dotplot-menu-proto :isnew (plot)
  (let* ((fit (send menu-item-proto :new "Fit Means"
                       :action #'(lambda ()
                                   (send plot :fit (not (send plot :fit)))
                                   (send plot :redraw-lines)
                                   (send plot :set-menu)))))
    (call-next-method plot)
    (send self :append-items (send dash-item-proto :new) fit)))


              