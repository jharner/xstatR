;;;;
;;;; Summary Overlay Prototype
;;;;

(defproto summary-overlay-proto '(visible levels lines offset)
  nil graph-overlay-proto)

;;; Slot Accessors and Mutators

(defmeth summary-overlay-proto :visible (&optional (visible nil set-visible))
  (if set-visible (setf (slot-value 'visible) visible))
  (slot-value 'visible))

(defmeth summary-overlay-proto :levels (&optional (levels nil set-levels))
  (if set-levels (setf (slot-value 'levels) levels))
  (slot-value 'levels))

(defmeth summary-overlay-proto :lines (&optional lines)
  (if lines (setf (slot-value 'lines) lines))
  (slot-value 'lines))

(defmeth summary-overlay-proto :offset (&optional offset)
  (if offset (setf (slot-value 'offset) offset))
  (slot-value 'offset))

;;; Other Methods

(defmeth summary-overlay-proto :do-click (x y m1 m2)
  (let* ((plot (send self :graph))
         (height (- (send plot :canvas-height) (slot-value 'offset)))
         (line-height *line-height*))
    (when (and (< 2 x 14)
               (< height y (+ height line-height)))
          (send plot :while-button-down #'(lambda (x y) nil))
          (send self :visible (not (send self :visible)))              
          (send plot :redraw-bottom))))

(defmeth summary-overlay-proto :redraw ()
  (if self
  (let* ((plot (send self :graph))
         (dataset (send plot :dataset))
         (mask (send dataset :mask))
         (z-mask (send dataset :z-mask))
         (or-mask (map-elements #'vector-or mask z-mask))
         (not-mask (mapcar #'(lambda (val) (not val)) or-mask))
         (z-vars (send dataset :z-variables))
         (summaries (send plot :summaries))
         (lines 0)
         (levels nil))
    (dolist (summary (reverse summaries))
            (if (send summary :visible)
                (setf lines (+ lines (send summary :lines)))
                (setf lines (+ lines 1)))
            (if (equal summary self) (return)))
    (send self :offset (* lines *line-height*))
    (send plot :erase-rect 0
          (- (send plot :canvas-height) (slot-value 'offset))
          (send plot :canvas-width) (slot-value 'offset))
    (if (send self :visible)
        (send self :redraw-hide)
        (send self :redraw-reveal))
    (dolist (z-var (reverse z-vars))
            (let* ((level (first (select (send z-var :values)
                                         (which not-mask)))))
              (setf levels (concatenate 'string
                                        (format nil " ~s" level) levels))))
    (send self :levels levels)
    (send plot :redraw-lines))))

(defmeth summary-overlay-proto :redraw-hide ()
  (let* ((plot (send self :graph))
         (height (- (send plot :canvas-height) (slot-value 'offset))))
    (send plot :frame-poly (list (list 3 (+ height 5)) (list 10 0)
                                 (list -5 5) (list -5 -5)) nil)))

(defmeth summary-overlay-proto :redraw-reveal ()
  (let* ((plot (send self :graph))
         (height (- (send plot :canvas-height) (slot-value 'offset))))
    (send plot :frame-poly (list (list 5 (+ height 3)) (list 0 10)
                                 (list 5 -5) (list -5 -5)) nil)))

;;;;
;;;; Moment Overlay Prototype
;;;;

(defproto moment-overlay-proto '() nil summary-overlay-proto)

(defmeth moment-overlay-proto :redraw ()
  (call-next-method)
  (let* ((plot (send self :graph))
         (height (- (send plot :canvas-height)
                    (+ (slot-value 'offset) *descent*)))
         (line-height *line-height*))
    (send plot :draw-string (format nil "Moments:")
          (+ line-height 5) (+ height line-height))
    (when (send self :visible)
          (let* ((dataset (send plot :dataset))
                 (z-vars (send dataset :z-variables))
                 (indent (* 8 line-height))
                 (levels (slot-value 'levels))
                 (moments (send dataset :compute-moments nil)))
            (when z-vars
                  (let ((z-moments (send dataset :compute-moments z-vars)))
                    (send plot :draw-string
                          (format nil "  ~d" (first z-moments))
                          indent (+ height (* 2 line-height)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (second z-moments))
                          indent (+ height (* 3 line-height)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (third z-moments))
                          indent (+ height (* 4 line-height)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (fourth z-moments))
                          indent (+ height (* 5 line-height)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (fifth z-moments))
                          indent (+ height (* 6 line-height)))
                    (send plot :draw-string (format nil "for ~a" levels)
                          (+ indent (round (* .25 line-height)))
                          (+ height line-height))
                    (setf indent (+ indent (* 6 line-height)))
                    (send plot :draw-string (format nil "Overall")
                          (+ indent (round (* .25 line-height)))
                          (+ height line-height))))
            (send plot :draw-string (format nil "N:")
                  (* 2 line-height) (+ height (* 2 line-height)))
            (send plot :draw-string (format nil "  ~d" (first moments))
                  indent (+ height (* 2 line-height)))
            (send plot :draw-string (format nil "Mean:")
                  (* 2 line-height) (+ height (* 3 line-height)))
            (send plot :draw-string (format nil "~v,vg" 12 5 (second moments))
                  indent (+ height (* 3 line-height)))
            (send plot :draw-string (format nil "Std Dev:")
                  (* 2 line-height) (+ height (* 4 line-height)))
            (send plot :draw-string (format nil "~v,vg" 12 5 (third moments))
                  indent (+ height (* 4 line-height)))
            (send plot :draw-string (format nil "Std Err:")
                  (* 2 line-height) (+ height (* 5 line-height)))
            (send plot :draw-string (format nil "~v,vg" 12 5 (fourth moments))
                  indent (+ height (* 5 line-height)))
            (send plot :draw-string (format nil "Skewness:")
                  (* 2 line-height) (+ height (* 6 line-height)))
            (send plot :draw-string (format nil "~v,vg" 12 5 (fifth moments))
                  indent (+ height (* 6 line-height)))))))

;;;;
;;;; Test Overlay Prototype
;;;;

(defproto test-overlay-proto '() nil summary-overlay-proto)

(defmeth test-overlay-proto :do-click (x y m1 m2)
  (let* ((plot (send self :graph))
         (dataset (send plot :dataset))
         (null-value (send dataset :null-value))
         (text-w1 (send plot :text-width "Ho: µ = "))
         (text-w2 (send plot :text-width
                        (format nil "[ ~5f ]" null-value)))
         (height (- (send plot :canvas-height)
                    (+ (slot-value 'offset) *descent*))))
    (if (and (< (+ (* 2 *line-height*) text-w1) x
                (+ (* 2 *line-height*) text-w1 text-w2 5))
             (< (+ height *line-height* (round (/ *line-height* 4)) 2) y
                (+ height (* 2 *line-height*) 2)))
        (let ((null-value (get-value-dialog "Enter the null value:")))
          (if null-value
              (if (numberp (first null-value))
                  (progn
                   (send dataset :null-value (first null-value))
                   (send plot :redraw-overlays)
                   (send plot :redraw-content))
                  (message-dialog "Null value must be numeric."))))
        (call-next-method x y m1 m2))))

(defmeth test-overlay-proto :redraw ()
  (call-next-method)
  (let* ((plot (send self :graph))
         (height (- (send plot :canvas-height)
                    (+ (slot-value 'offset) *descent*)))
         (line-height *line-height*))
    (send plot :draw-string (format nil "t-test:")
          (+ line-height 5) (+ height line-height))
    (when (send self :visible)
          (let* ((dataset (send plot :dataset))
                 (test (send dataset :compute-test nil))
                 (null-value (send dataset :null-value))
                 (z-vars (send dataset :z-variables))
                 (indent (* 8 line-height))
                 (levels (slot-value 'levels)))
            (send plot :draw-string (format nil "Ho: µ = [ ~5f ]" null-value)
                  (* 2 line-height) (+ height (* 2 line-height)))
            (when z-vars
                  (let ((z-test (send dataset :compute-test z-vars)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (first z-test))
                          indent (+ height (* 3 line-height)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (second z-test))
                          indent (+ height (* 4 line-height)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (third z-test))
                          indent (+ height (* 5 line-height)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (fourth z-test))
                          indent (+ height (* 6 line-height)))
                    (send plot :draw-string (format nil "for ~a" levels)
                          (+ indent (round (* .25 line-height)))
                          (+ height line-height))
                    (setf indent (+ indent (* 6 line-height)))
                    (send plot :draw-string (format nil "Overall")
                          (+ indent (round (* .25 line-height)))
                          (+ height line-height))))
            (send plot :draw-string (format nil "t-value:")
                  (* 2 line-height) (+ height (* 3 line-height)))
            (send plot :draw-string (format nil "~v,vg" 12 5 (first test))
                  indent (+ height (* 3 line-height)))
            (send plot :draw-string (format nil "P( |t| > t-value):")
                  (* 2 line-height) (+ height (* 4 line-height)))
            (send plot :draw-string (format nil "~v,vg" 12 5 (second test))
                  indent (+ height (* 4 line-height)))
            (send plot :draw-string (format nil "P( t > t-value) :")
                  (* 2 line-height) (+ height (* 5 line-height)))
            (send plot :draw-string (format nil "~v,vg" 12 5 (third test))
                  indent (+ height (* 5 line-height)))
            (send plot :draw-string (format nil "P( t < t-value) :")
                  (* 2 line-height) (+ height (* 6 line-height)))
            (send plot :draw-string (format nil "~v,vg" 12 5 (fourth test))
                  indent (+ height (* 6 line-height)))))))

;;;;
;;;; CI Overlay Prototype
;;;;

(defproto ci-overlay-proto '() nil summary-overlay-proto)

(defmeth ci-overlay-proto :do-click (x y m1 m2)
  (let* ((plot (send self :graph))
         (text-w1 (send plot :text-width "Level: "))
         (text-w2 (send plot :text-width "[ 95% ]"))
         (height (- (send plot :canvas-height)
                    (+ (slot-value 'offset) *descent*))))
    (if (and (< (+ (* 2 *line-height*) text-w1) x
                (+ (round (/ (* 7 *line-height*) 2)) text-w1 text-w2))
             (< (+ height *line-height* (round (/ *line-height* 4)) 2) y
                (+ height (* 2 *line-height*) 2)))
        (send (send plot :ci-menu) :popup
              (+ (* 2 *line-height*) text-w1)
              (+ height *line-height* (round (/ *line-height* 4)) 2) plot)
        (call-next-method x y m1 m2))))

(defmeth ci-overlay-proto :redraw ()
  (call-next-method)
  (let* ((plot (send self :graph))
         (height (- (send plot :canvas-height)
                    (+ (slot-value 'offset) *descent*)))
         (line-height *line-height*))
    (send plot :draw-string (format nil "Confidence Interval:")
          (+ line-height 5) (+ height line-height))
    (when (send self :visible)
          (let* ((dataset (send plot :dataset))
                 (ci (send dataset :compute-ci nil))
                 (ci-level (round (* 100 (send dataset :ci-level))))
                 (z-vars (send dataset :z-variables))
                 (indent (* 8 line-height))
                 (text-w1 (send plot :text-width "Level: "))
                 (text-w2 (send plot :text-width
                                (format nil " ~d% " ci-level)))
                 (levels (slot-value 'levels)))
            (send plot :draw-string
                  (format nil "Level:  ~d% " ci-level)
                  (* 2 line-height) (+ height (* 2 line-height)))
            (send plot :frame-rect (+ (* 2 line-height) text-w1)
                  (+ height line-height (round (/ line-height 4)) 2)
                  (+ line-height text-w2 5) (round (/ (* 3 line-height) 4)))
            (send plot :draw-line (+ (* 2 line-height) text-w1 4)
                  (+ height (* 2 line-height) 2)
                  (+ (* 3 line-height) text-w1 text-w2 4)
                  (+ height (* 2 line-height) 2))
            (send plot :draw-line
                  (+ (round (/ (* 6 line-height) 2)) text-w1 text-w2 5)
                  (+ height line-height (round (/ line-height 4)) 4)
                  (+ (round (/ (* 6 line-height) 2)) text-w1 text-w2 5)
                  (+ height (* 2 line-height) 2))
            (send plot :paint-poly
                  (list
                   (list (+ (* 2 line-height) text-w2 text-w1 2)
                         (+ height line-height (round (/ line-height 4)) 4))
                   (list 12 0) (list -6 6) (list -6 -6)) nil)   
            (when z-vars
                  (let ((z-ci (send dataset :compute-ci z-vars)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (first z-ci))
                          indent (+ height (* 3 line-height)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (second z-ci))
                          indent (+ height (* 4 line-height)))
                    (send plot :draw-string (format nil "for ~a" levels)
                          (+ indent (round (* .25 line-height)))
                          (+ height line-height))
                    (setf indent (+ indent (* 6 line-height)))
                    (send plot :draw-string (format nil "Overall")
                          (+ indent (round (* .25 line-height)))
                          (+ height line-height))))
            (send plot :draw-string (format nil "Lower Limit:")
                  (* 2 line-height) (+ height (* 3 line-height)))
            (send plot :draw-string (format nil "~v,vg" 12 5 (first ci))
                  indent (+ height (* 3 line-height)))
            (send plot :draw-string (format nil "Upper Limit:")
                  (* 2 line-height) (+ height (* 4 line-height)))
            (send plot :draw-string (format nil "~v,vg" 12 5 (second ci))
                  indent (+ height (* 4 line-height)))
            (send plot :ci-band)))))

;;;;
;;;; Quantile Overlay Prototype
;;;;

(defproto quantile-overlay-proto '() nil summary-overlay-proto)

(defmeth quantile-overlay-proto :redraw ()
  (call-next-method)
  (let* ((plot (send self :graph))
         (height (- (send plot :canvas-height)
                    (+ (slot-value 'offset) *descent*)))
         (line-height *line-height*))
    (send plot :draw-string (format nil "Quantiles:")
          (+ line-height 5) (+ height line-height))
    (when (send self :visible)
          (let* ((dataset (send plot :dataset))
                 (quant (send dataset :compute-quantiles nil))
                 (z-vars (send dataset :z-variables))
                 (indent (* 8 line-height))
                 (levels (slot-value 'levels)))
            (when z-vars
                  (let ((z-quant (send dataset :compute-quantiles z-vars)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (first z-quant))
                          indent (+ height (* 2 line-height)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (second z-quant))
                          indent (+ height (* 3 line-height)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (third z-quant))
                          indent (+ height (* 4 line-height)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (fourth z-quant))
                          indent (+ height (* 5 line-height)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (fifth z-quant))
                          indent (+ height (* 6 line-height)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (sixth z-quant))
                          indent (+ height (* 7 line-height)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (seventh z-quant))
                          indent (+ height (* 8 line-height)))
                    (send plot :draw-string (format nil "for ~a" levels)
                          (+ indent (round (* .25 line-height)))
                          (+ height line-height))
                    (setf indent (+ indent (* 6 line-height)))
                    (send plot :draw-string (format nil "Overall")
                          (+ indent (round (* .25 line-height)))
                          (+ height line-height))))
            (send plot :draw-string (format nil "Minimun:")
                  (* 2 line-height) (+ height (* 2 line-height)))
            (send plot :draw-string (format nil "~v,vg" 12 5 (first quant))
                  indent (+ height (* 2 line-height)))
            (send plot :draw-string (format nil "Lower Quartile:")
                  (* 2 line-height) (+ height (* 3 line-height)))
            (send plot :draw-string (format nil "~v,vg" 12 5 (second quant))
                  indent (+ height (* 3 line-height)))
            (send plot :draw-string (format nil "Median:")
                  (* 2 line-height) (+ height (* 4 line-height)))
            (send plot :draw-string
                  (format nil "~v,vg" 12 5 (third quant))
                  indent (+ height (* 4 line-height)))
            (send plot :draw-string (format nil "Upper Quantile:")
                  (* 2 line-height) (+ height (* 5 line-height)))
            (send plot :draw-string (format nil "~v,vg" 12 5 (fourth quant))
                  indent (+ height (* 5 line-height)))
            (send plot :draw-string (format nil "Maximun:")
                  (* 2 line-height) (+ height (* 6 line-height)))
            (send plot :draw-string (format nil "~v,vg" 12 5 (fifth quant))
                  indent (+ height (* 6 line-height)))
            (send plot :draw-string (format nil "IQR:")
                  (* 2 line-height) (+ height (* 7 line-height)))
            (send plot :draw-string (format nil "~v,vg" 12 5 (sixth quant))
                  indent (+ height (* 7 line-height)))
            (send plot :draw-string (format nil "Range:")
                  (* 2 line-height) (+ height (* 8 line-height)))
            (send plot :draw-string (format nil "~v,vg" 12 5 (seventh quant))
                  indent (+ height (* 8 line-height)))))))

;;;;
;;;; RegFit Overlay Prototype
;;;;

(defproto regfit-overlay-proto '() nil summary-overlay-proto)

(defmeth regfit-overlay-proto :redraw ()
  (call-next-method)
  (let* ((plot (send self :graph))
         (height (- (send plot :canvas-height)
                    (+ (slot-value 'offset) *descent*)))
         (line-height *line-height*))
    (send plot :draw-string (format nil "Linear Fit:")
          (+ line-height 5) (+ height line-height))
    (when (send self :visible)
          (let* ((dataset (send plot :dataset))
                 (reg (send dataset :compute-regfit nil))
                 (z-vars (send dataset :z-variables))
                 (indent (round (* 8.5 line-height)))
                 (levels (slot-value 'levels)))
            (when z-vars
                  (let ((z-reg (send dataset :compute-regfit z-vars)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (first z-reg))
                          indent (+ height (* 2 line-height)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (second z-reg))
                          indent (+ height (* 3 line-height)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (third z-reg))
                          indent (+ height (* 4 line-height)))
                    (send plot :draw-string
                          (format nil "(~v,vg)" 10 4 (fourth z-reg))
                          (+ indent (round (* 4.5 line-height)))
                          (+ height (* 4 line-height)))
                    (send plot :draw-string (format nil "for ~a" levels)
                          (+ indent (round (* .25 line-height)))
                          (+ height line-height))
                    (setf indent (+ indent (round (* 9.5 line-height))))
                    (send plot :draw-string (format nil "Overall")
                          (+ indent (round (* .25 line-height)))
                          (+ height line-height))))
            (send plot :draw-string (format nil "Intercept:")
                  (* 2 line-height) (+ height (* 2 line-height)))
            (send plot :draw-string (format nil "~v,vg" 12 5 (first reg))
                  indent (+ height (* 2 line-height)))
            (send plot :draw-string (format nil "Slope:")
                  (* 2 line-height) (+ height (* 3 line-height)))
            (send plot :draw-string (format nil "~v,vg" 12 5 (second reg))
                  indent (+ height (* 3 line-height)))
            (send plot :draw-string (format nil "~v,vg" 12 5 (third reg))
                  indent (+ height (* 4 line-height)))
            (send plot :draw-string (format nil "(~v,vg)" 10 4 (fourth reg))
                  (+ indent (round (* 4.5 line-height)))
                  (+ height (* 4 line-height)))
            (send plot :draw-string (format nil "Calc-t (P-value):")
                  (round (* 2.5 line-height))
                  (+ height (* 4 line-height)))))))

;;;;
;;;; Correlation Overlay Prototype
;;;;

(defproto corr-overlay-proto '() nil summary-overlay-proto)

(defmeth corr-overlay-proto :redraw ()
  (call-next-method)
  (let* ((plot (send self :graph))
         (height (- (send plot :canvas-height)
                    (+ (slot-value 'offset) *descent*)))
         (line-height *line-height*))
    (send plot :draw-string (format nil "Correlation:")
          (+ line-height 5) (+ height line-height))
    (when (send self :visible)
          (let* ((dataset (send plot :dataset))
                 (corr (send dataset :compute-corr nil))
                 (z-vars (send dataset :z-variables))
                 (indent (round (* 8.5 line-height)))
                 (levels (slot-value 'levels)))
            (when z-vars
                  (let ((z-corr (send dataset :compute-corr z-vars)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (first z-corr))
                          indent (+ height (* 2 line-height)))
                    (send plot :draw-string
                          (format nil "~v,vg" 12 5 (second z-corr))
                          indent (+ height (* 3 line-height)))
                    (send plot :draw-string
                          (format nil "(~v,vg)" 10 4 (third z-corr))
                          (+ indent (round (* 4.5 line-height)))
                          (+ height (* 3 line-height)))
                    (send plot :draw-string (format nil "for ~a" levels)
                          (+ indent (round (* .25 line-height)))
                          (+ height line-height))
                    (setf indent (+ indent (round (* 9.5 line-height))))
                    (send plot :draw-string (format nil "Overall")
                          (+ indent (round (* .25 line-height)))
                          (+ height line-height))))
            (send plot :draw-string (format nil "Pearson:")
                  (* 2 line-height) (+ height (* 2 line-height)))
            (send plot :draw-string (format nil "~v,vg" 12 5 (first corr))
                  indent (+ height (* 2 line-height)))
            (send plot :draw-string (format nil "~v,vg" 12 5 (second corr))
                  indent (+ height (* 3 line-height)))
            (send plot :draw-string (format nil "(~v,vg)" 10 4 (third corr))
                  (+ indent (round (* 4.5 line-height)))
                  (+ height (* 3 line-height)))
            (send plot :draw-string (format nil "Calc-t (P-value):")
                  (round (* 2.5 line-height))
                  (+ height (* 3 line-height)))))))

