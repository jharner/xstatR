;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Conditioning Variable Slider Overlay Prototype            ;
; 'label' is the name of conditioning variable              ;
; 'pos' is the upper left hand corner of the slider's box   ;
;    (first pos) is the x coordinate                        ;
;    (second pos) is the y corrdinate                       ;
; 'dimensions' are the height and width of the slider's box ;
;    (first dimension) is the height                        ;
;    (second dimension) is the width                        ;
; 'range' is the range of the current slice                 ;               
; As of 5/29/95 this file is under renovation               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    

(defproto variable-slider-overlay-proto
  '(label pos dimensions slice-num range click cond-var cond-vars dataset)
  nil graph-overlay-proto)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This method initializes the slots that belong to ;
; variable-slider-overlay-proto                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth variable-slider-overlay-proto :isnew
  (label pos dims range slice-num cond-var dataset)
  (setf (slot-value 'dimensions) dims)
  (setf (slot-value 'label) label)
  (setf (slot-value 'pos ) pos)
  (setf (slot-value 'click) 0)
  (setf (slot-value 'cond-var) cond-var)
  (setf (slot-value 'range) range)
  (setf (slot-value 'slice-num) slice-num)
  (setf (slot-value 'dataset) dataset)
  (setf (slot-value 'cond-vars) (send dataset :variables-of-role 'Z))
  (send cond-var :current-shingle 0))

(defmeth variable-slider-overlay-proto :cond-vars
  (&optional (dataset nil set-cond-vars))
  (if set-cond-vars (setf (slot-value 'cond-vars) cond-vars))
  (slot-value 'cond-vars))

(defmeth variable-slider-overlay-proto :dataset
  (&optional (dataset nil set-dataset))
  (if set-dataset (setf (slot-value 'dataset) dataset))
  (slot-value 'dataset))

(defmeth variable-slider-overlay-proto :slice-num
  (&optional (slice-num nil set-slice-num))
  (if set-slice-num (setf (slot-value 'slice-num) slice-num))
  (slot-value 'slice-num))

(defmeth variable-slider-overlay-proto :range
  (&optional (range nil set-range))
  (if set-range (setf (slot-value 'range) range))
  (slot-value 'range))

(defmeth variable-slider-overlay-proto :cond-var
  (&optional (cond-var nil set-cond-var))
  (if set-cond-var (setf (slot-value 'cond-var) cond-var))
  (slot-value 'cond-var))

(defmeth variable-slider-overlay-proto :click
  (&optional (click nil set-click))
  (if set-dimension (setf (slot-value 'click) click))
  (slot-value 'click))

(defmeth variable-slider-overlay-proto :pos (&optional (pos nil set-pos))
  (if set-pos (setf (slot-value 'pos) pos))
  (slot-value 'pos))

(defmeth variable-slider-overlay-proto :dimensions
  (&optional (dimensions nil set-dimensions))
  (if set-dimension (setf (slot-value 'dimensions) dimensions))
  (slot-value 'dimensions))

(defmeth variable-slider-overlay-proto :label
  (&optional (label nil set-label))
  (if set-label (setf (slot-value 'label) label))
  (slot-value 'label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This method draws the variable-slider                ;
; The rectangle, indicator, label, and range are drawn ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth variable-slider-overlay-proto :redraw ()
	(let* ((plot (send self :graph))
        (data (send self :dataset))
        (position (slot-value 'pos))
        (x-pos (first position))
        (y-pos (second position))
        (dimens (slot-value 'dimensions))
        (height (first dimens))
        (width (second dimens))
        (title (format nil "~,5a" (slot-value 'label)))
        (rang (slot-value 'range))
        (upper (second rang))
        (lower (first rang))
        (rangstring (format nil "(~,2f, ~,2f)" lower upper)))
   (send plot :clear-lines :draw nil)
   (send self :calculate-mask)
   (send plot :erase-rect x-pos (- y-pos 11) width 22)
   (send plot :frame-rect x-pos y-pos width height)
   (send plot :draw-string title x-pos (- y-pos (send plot :text-descent)))
   (send plot :draw-string rangstring
         (- (+ x-pos width) (send plot :text-width rangstring))
         (- y-pos (send plot :text-descent)))
   (send self :draw-indicator)
   (send plot :redraw-lines)
   (send plot :adjust-plot)
   (send plot :needs-adjusting t)
   (send plot :adjust-screen)))

(defmeth variable-slider-overlay-proto :calculate-mask ()
  (let* ((dataset (send self :dataset))
         (num-obs (send dataset :num-obs))
         (mask (repeat nil num-obs))
         (cond-vars (send self :cond-vars))
         (plot (send self :graph)))
    (dotimes (i (length cond-vars) mask)
             (let* ((cond-var (select cond-vars i))
                    (values (send cond-var :values))
                    (shing-num (send cond-var :current-shingle))
                    (lower (select (send cond-var :shingles) shing-num 0))
                    (upper (select (send cond-var :shingles) shing-num 1)))
               (dotimes (j (length values) mask)
                        (setf (select mask j)
                              (if (and (and (>= (select values j) lower)
                                            (<= (select values j) upper)) 
                                       (equal nil (select mask j)))
                                  nil 'T)))))
    (send dataset :z-mask mask)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Draws the indicator bar according to the range of the slice ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth variable-slider-overlay-proto :draw-indicator ()
(let* ((plot (send self :graph))
       (cond-var (slot-value 'cond-var))
       (max (max (send cond-var :values)))
       (min (min (send cond-var :values))) 
       (position (slot-value 'pos))
       (slider-xl (first position))
       (slider-xr (+ slider-xl 120))
       (indic-y (second position))
       (slider-dimens (slot-value 'dimensions))
       (indic-height (first slider-dimens))
       (rang (slot-value 'range))
       (lower (first rang))
       (upper (second rang))
       (slope (/ (- slider-xr slider-xl) (- max min)))
       (intercept (- slider-xr (* slope max)))
       (indic-xl (floor (+ (* lower slope) intercept)))
       (indic-xr (floor (+ (* upper slope) intercept)))
       (indic-width (- indic-xr indic-xl)))
   (send plot :paint-rect indic-xl indic-y indic-width indic-height)))

(defmeth variable-slider-overlay-proto :in-slider ( x y )
  (let* ((position (slot-value 'pos))
         (x-pos (first position))
         (y-pos (second position))
         (dimens (slot-value 'dimensions))
         (height (first dimens))
         (width (second dimens)))
    (and (< x-pos x (+ x-pos width)) (< y-pos y (+ y-pos width)))))

(defmeth variable-slider-overlay-proto :do-click ( click-x click-y mod1 mod2)
  (let ((plot (send self :graph)))
    (when (send self :in-slider click-x click-y)
          (send self :handle-click click-x click-y)
          (synchronize-plots plot))))

(defmeth variable-slider-overlay-proto :in-indicator ( x y )
 (let* ((plot (send self :graph))
        (cond-var (slot-value 'cond-var))
        (max (max (send cond-var :values)))
        (min (min (send cond-var :values))) 
        (position (slot-value 'pos))
        (x-pos (first position))
        (slider-xl x-pos)
        (slider-xr (+ slider-xl 120))
        (indic-y (second position))
        (slider-dimens (slot-value 'dimensions))
        (indic-height (first slider-dimens))
        (rang (slot-value 'range))
        (lower (first rang))
        (upper (second rang))
        (slope (/ (- slider-xr slider-xl) (- max min)))
        (intercept (- slider-xr (* slope max)))
        (indic-xl (floor (+ (* lower slope) intercept)))
        (indic-xr (floor (+ (* upper slope) intercept)))
        (indic-width (- indic-xr indic-xl)))
   (and (> x indic-xl) (> y indic-y) (< x indic-xr )
        (< y (+ indic-y indic-height)))))  

(defmeth variable-slider-overlay-proto :right-of-indicator ( x y )
(let* ((plot (send self :graph))
       (cond-var (slot-value 'cond-var))
       (max (max (send cond-var :values)))
       (min (min (send cond-var :values))) 
       (position (slot-value 'pos))
       (x-pos (first position))
       (slider-xl x-pos)
       (slider-xr (+ slider-xl 120))
       (indic-y (second position))
       (slider-dimens (slot-value 'dimensions))
       (indic-height (first slider-dimens))
       (rang (slot-value 'range))
       (lower (first rang))
       (upper (second rang))
       (slope (/ (- slider-xr slider-xl) (- max min)))
       (intercept (- slider-xr (* slope max)))
       (indic-xl (floor (+ (* lower slope) intercept)))
       (indic-xr (floor (+ (* upper slope) intercept)))
       (indic-width (- indic-xr indic-xl)))
  (and (> x indic-xr) (> y indic-y) (< x (+ x-pos 120)) (< y (+ indic-y indic-height)))))

(defmeth variable-slider-overlay-proto :move-indicator ( x )
(let* ((plot (send self :graph))
       (cond-var (slot-value 'cond-var))
       (slice-num (slot-value 'slice-num))
       (shingles (send cond-var :shingles))
       (max (max (send cond-var :values)))
       (min (min (send cond-var :values))) 
       (position (slot-value 'pos))
       (x-pos (first position))
       (slider-xl x-pos)
       (slider-xr (+ slider-xl 120))
       (indic-y (second position))
       (slider-dimens (slot-value 'dimensions))
       (indic-height (first slider-dimens))
       (rang (slot-value 'range))
       (lower (first rang))
       (upper (second rang))
       (slope (/ (- slider-xr slider-xl) (- max min)))
       (intercept (- slider-xr (* slope max)))
       (indic-xl (floor (+ (* lower slope) intercept)))
       (indic-xr (floor (+ (* upper slope) intercept)))
       (indic-width (- indic-xr indic-xl)))
  (if (> x 0) (setf (slot-value 'range) 
                    (list (select shingles (+ slice-num 1) 0)
                          (select shingles (+ slice-num 1) 1)))
      (setf (slot-value 'range) 
            (list (select shingles (- slice-num 1) 0)
                  (select shingles (- slice-num 1) 1))))
  (if (> x 0) (setf (slot-value 'slice-num) (+ slice-num 1))
      (setf (slot-value 'slice-num) (- slice-num 1)))
  (if (> x 0) (send cond-var :current-shingle (+ slice-num 1))
      (send cond-var :current-shingle (- slice-num 1)))
  (send plot :erase-rect indic-xl (+ indic-y 1) indic-width
        (- indic-height 2))
  (send self :redraw)))

(defmeth variable-slider-overlay-proto :handle-right-click ( x y )
  (let* ((plot (send self :graph)))
    (send plot :while-button-down #'(lambda (x y)
                                      (pause 5)
                                      (if (send self :right-of-indicator x y)
                                          (send self :move-indicator 20)))
          nil)))
                                  

(defmeth variable-slider-overlay-proto :left-of-indicator ( x y )
(let* ((plot (send self :graph))
       (cond-var (slot-value 'cond-var))
       (max (max (send cond-var :values)))
       (min (min (send cond-var :values))) 
       (position (slot-value 'pos))
       (x-pos (first position))
       (slider-xl x-pos)
       (slider-xr (+ slider-xl 120))
       (indic-y (second position))
       (slider-dimens (slot-value 'dimensions))
       (indic-height (first slider-dimens))
       (rang (slot-value 'range))
       (lower (first rang))
       (upper (second rang))
       (slope (/ (- slider-xr slider-xl) (- max min)))
       (intercept (- slider-xr (* slope max)))
       (indic-xl (floor (+ (* lower slope) intercept)))
       (indic-xr (floor (+ (* upper slope) intercept)))
       (indic-width (- indic-xr indic-xl)))
  (and (< x indic-xl) (> y indic-y) (> x x-pos) (< y (+ indic-y indic-height)))))

(defmeth variable-slider-overlay-proto :handle-left-click ( x y )
(let* ((plot (send self :graph)))
(send plot :while-button-down #'(lambda (x y)
                                (pause 5)
                                (if (send self :left-of-indicator x y) (send self :move-indicator -20))) nil)))

(defun double-click-time ()
  (* .75 internal-time-units-per-second))    

(defun collect-values ()
  (let* ((num-shing (with-input-from-string
                     (s (send shingles-edit-text :text)) (read s)))
         (overlap (/ (with-input-from-string
                      (s (send overlap-edit-text :text)) (read s)) 100)))
    (make-new-shingles num-shing overlap slider the-var)
    (send slider :range (list (select (send the-var :shingles) 0 0)
                              (select (send the-var :shingles) 0 1)))
    (send slider :slice-num 0)
    (send shingle-dialog :remove)
    (send shingle-dialog :modal-dialog-return 1)))

(defun cancel-values ()
  (send shingle-dialog :remove)
  (send shingle-dialog :modal-dialog-return 2))

(defun set-default-values ()
  (send shingles-edit-text :text "6")
  (send overlap-edit-text :text "50")
  (send shingle-dialog :modal-dialog-return 3))

(defun make-new-shingles (num-shingles overlap slider cond-var)
  (send cond-var :frac-overlap overlap)
  (send cond-var :shingles (send cond-var :make-shingles num-shingles))
  (send cond-var :current-shingle 0))

(defmeth variable-slider-overlay-proto :show-dialog ()
(let* ((cond-var (slot-value 'cond-var))
       (plot (send self :graph))
       (x-pos (first (slot-value 'pos)))
       (y-pos (second (slot-value 'pos)))
       (slice-range (slot-value 'range))
       (var-max (max (send cond-var :values)))
       (var-min (min (send cond-var :values)))
       (var-name (slot-value 'label))
       (overlap (format nil "~d"
                        (floor (* 100 (send cond-var :frac-overlap)))))
       (num-shingles (format nil "~d"
                             (first (array-dimensions (send cond-var
                                                            :shingles)))))
       (name-static-text (send text-item-proto :new var-name))
       (range-string (format nil "Range = ( ~,2f, ~,2f )"
                             var-min var-max) :text-length 15)
       (range-static-text (send text-item-proto :new range-string))
       (current-range-string (format nil "Current Range = ( ~,2f, ~,2f )"
                                     (first slice-range) (second slice-range)) 
                             :text-length 15)
       (current-range-static-text (send text-item-proto :new
                                        current-range-string))
       (blank-static-text (send text-item-proto :new "" ))
       (cancel-button (send modal-button-proto :new "Cancel"
                            :action #'cancel-values))
       (ok-button (send modal-button-proto :new "OK"
                        :action #'collect-values))
       (default-button (send modal-button-proto
                             :new "Default" :action #'set-default-values))
       (shingles-static-text (send text-item-proto :new "# Shingles:"))
       (overlap-static-text (send text-item-proto :new "% Overlap:")))
       (setf the-var cond-var)
       (setf slider self)
       (setf shingles-edit-text (send edit-text-item-proto
                                      :new num-shingles :text-length 1))
       (setf overlap-edit-text (send edit-text-item-proto
                                     :new overlap :text-length 2))
       (setf items-list
             (list (list (list name-static-text
                               shingles-static-text overlap-static-text)
                         (list blank-static-text
                               shingles-edit-text overlap-edit-text) 
                         (list default-button cancel-button ok-button))))
       (setf shingle-dialog (send modal-dialog-proto
                                  :new items-list :default-button ok-button))
       (do ((reply 3 (send shingle-dialog :modal-dialog nil)))
           ((not (= reply 3)) (send self :redraw)))))

       
(defmeth variable-slider-overlay-proto :handle-indicator-click
  ( click-x click-y )
  (let* ((prev-click (slot-value 'click))
         (current-click (get-internal-real-time)))
    (setf (slot-value 'click) current-click)
    (if (<= (- current-click prev-click) (double-click-time))
        (send self :show-dialog))))


(defmeth variable-slider-overlay-proto :handle-click ( click-x click-y)
  (if (send self :in-indicator click-x click-y)
      (send self :handle-indicator-click click-x click-y))
  (if (send self :right-of-indicator click-x click-y)
      (send self :handle-right-click click-x click-y))
  (if (send self :left-of-indicator click-x click-y)
      (send self :handle-left-click click-x click-y )))

;
; Categorical Conditioning Slider
;

(defproto cat-variable-slider-overlay-proto '() nil
  variable-slider-overlay-proto)

(defmeth cat-variable-slider-overlay-proto :redraw ()
  (let* ((plot (send self :graph))
         (data (send self :dataset))
         (position (slot-value 'pos))
         (slider-x (first position))
         (slider-y (second position))
         (dimensions (slot-value 'dimensions))
         (height (first dimensions))
         (width (second dimensions))
         (title (format nil "~5,a" (slot-value 'label)))
         (value (first (slot-value 'range)))
         (range-string (if (or (equal (type-of value) 'symbol)
                               (equal (type-of value) 'string))
                           (format nil "~a" value)
                           (format nil "~,2f" value))))
    (send plot :clear-lines :draw nil)
    (send self :calculate-mask)
    (send plot :erase-rect slider-x (- slider-y 11) width 22)
    (send plot :frame-rect slider-x slider-y width height)
    (send plot :draw-string title slider-x
          (- slider-y (send plot :text-descent)))
    (send plot :draw-string range-string
          (- (+ slider-x width) (send plot :text-width range-string))
          (- slider-y (send plot :text-descent)))
    (send self :draw-indicator)
    (send plot :redraw-lines)
    (send plot :adjust-plot)
    (send plot :needs-adjusting t)
    (send plot :adjust-screen)))

(defmeth cat-variable-slider-overlay-proto :calculate-mask ()
  (let* ((dataset (send self :dataset))
         (num-obs (send dataset :num-obs))
         (mask (repeat nil num-obs))
         (cond-vars (send self :cond-vars)))
    (dolist (the-var cond-vars)
            (let* ((values (send the-var :values))
                   (shingle-number (send the-var :current-shingle))
                   (shingle-value
                    (select (send the-var :shingles) shingle-number)))
              (dotimes (j (length values))
                       (if (and (equal (select values j) shingle-value) 
                                (equal nil (select mask j)))
                           (setf (select mask j) nil)
                           (setf (select mask j) t)))))
    (send dataset :z-mask mask)))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Draws the indicator bar according to the range of the slice ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth cat-variable-slider-overlay-proto :draw-indicator ()
  (let* ((plot (send self :graph))
         (indicator-left (send self :indicator-left))
         (indicator-right (+ indicator-left 5))
         (indicator-height (first (slot-value 'dimensions)))
         (indicator-top (second (slot-value 'pos)))
         (indicator-width 5))
    (send plot :paint-rect
          indicator-left indicator-top indicator-width indicator-height)))

(defmeth cat-variable-slider-overlay-proto :indicator-left ()
  (let* ((cond-var (send self :cond-var))
         (num-shingles (length (send cond-var :shingles)))
         (shingle-number (send self :slice-num)))
    (ceiling (+ (* (/ 115 (- num-shingles 1)) shingle-number)
                (first (slot-value 'pos))))))


(defmeth cat-variable-slider-overlay-proto :in-indicator ( x y )
  nil)
  

(defmeth cat-variable-slider-overlay-proto :right-of-indicator ( x y )
  (let* ((plot (send self :graph))
         (indicator-left (send self :indicator-left))
         (indicator-right (+ indicator-left 5))
         (indicator-height (first (slot-value 'dimensions)))
         (indicator-top (second (slot-value 'pos)))
         (indicator-bottom (+ indicator-top indicator-height))
         (slider-right (+ (first (slot-value 'pos)) 120)))
    (and (> x indicator-right) (< x slider-right)
         (> y indicator-top) (< y indicator-bottom))))

(defmeth cat-variable-slider-overlay-proto :left-of-indicator ( x y )
  (let* ((plot (send self :graph))
         (indicator-left (send self :indicator-left))
         (indicator-right (+ indicator-left 5))
         (indicator-height (first (slot-value 'dimensions)))
         (indicator-top (second (slot-value 'pos)))
         (indicator-bottom (+ indicator-top indicator-height))
         (slider-left (first (slot-value 'pos))))
    (and (> x slider-left) (< x indicator-left)
         (> y indicator-top) (< y indicator-bottom))))

(defmeth cat-variable-slider-overlay-proto :move-indicator ( x )
  (let* ((plot (send self :graph))
         (cond-var (send self :cond-var))
         (shingle-number (send self :slice-num))
         (shingles (send cond-var :shingles))
         (indicator-left (send self :indicator-left))
         (indicator-right (+ indicator-left 5))
         (indicator-height (first (slot-value 'dimensions)))
         (indicator-top (second (slot-value 'pos)))
         (indicator-bottom (+ indicator-top indicator-height)))
    (cond 
      ((> x 0)
       (setf (slot-value 'range)
             (list (select shingles (+ shingle-number 1)) nil))
       (setf (slot-value 'slice-num) (+ shingle-number 1))
       (send cond-var :current-shingle (+ shingle-number 1)))
      ((< x 0)
       (setf (slot-value 'range)
             (list (select shingles (- shingle-number 1)) nil))
       (setf (slot-value 'slice-num) (- shingle-number 1))
       (send cond-var :current-shingle (- shingle-number 1))))
    (send plot :erase-rect indicator-left
          (+ indicator-top 1) 5 (- indicator-height 2))
    (send self :redraw)))
   
(defmeth cat-variable-slider-overlay-proto :handle-indicator-click
  ( click-x click-y )
  nil)

  



