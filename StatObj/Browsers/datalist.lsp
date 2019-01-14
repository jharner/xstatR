;;;;
;;;;  This file contains data-list-proto, a view for datasets created from ;;;;    dataset-proto
;;;;

;;;;                                                              
;;;; Data List Prototype: Provides a list view of a dataset prototype                                                            ;;;;

(defproto data-list-proto '(plot) '(instances)
  (list graph-window-proto graph-mixin))

(defmeth data-list-proto :new (label &rest args)
  (let ((data-list (apply #'call-next-method label args)))
    (setf (slot-value 'instances)
          (cons data-list (slot-value 'instances)))
    data-list))

;;;
;;; Constructor and Display Function
;;;

(defun display-data-list (plot)
  (let* ((size (screen-size))
         (dataset (send plot :dataset))
         (num-obs (send dataset :num-obs))
         (height (+ (* num-obs 16) 14))
         (list-browser (send data-list-proto :new
                             :plot plot
                             :location (list 20 42)
                             :size (list 160 (min 334 height))
                             :title (concatenate
                                     'string (format nil "~s Labels"
                                                     (send dataset :name))))))
    (send list-browser :use-color t)
    (send list-browser :has-v-scroll height)
    (send list-browser :v-scroll-incs 16 80)
    (send *data-tree* :register-dataset-view dataset list-browser)
    (send plot :name-list list-browser)
    (send list-browser :redraw)
    list-browser))

;;;
;;; Other Methods               
;;;

(defmeth data-list-proto :redraw ()
  (let* ((plot (slot-value 'plot))
         (dataset (send plot :dataset))
         (color (send dataset :color))
         (symbol (send dataset :symbol))
         (state (send dataset :state))
         (mask (send dataset :mask))
         (z-mask (send dataset :z-mask))
         (or-mask (map-elements #'vector-or mask z-mask))
         (not-mask (mapcar #'(lambda (val) (not val)) or-mask))
         (size (send self :size))
         (view-rect (send self :view-rect))
         (y-delta (second view-rect))
         (first-obs (+ (floor (/ (- y-delta 16) 16)) 1))
         (last-obs (+ (floor (/ (- (fourth view-rect) 14) 16)) first-obs))
         (label-variable (send dataset :label-variable))
         (obs (send dataset :num-obs))
         (width (first size))
         (height (+ y-delta (fourth view-rect)))
         (values (if label-variable
                     (send (eval label-variable) :values)
                     (iseq 1 obs))))
    (send self :start-buffering)
    (send self :erase-rect 0 0 width height)
    (send self :draw-line 0 (+ y-delta 14) width (+ y-delta 14))
    (dotimes (i obs nil)
             (if (<= first-obs i last-obs)
                 (progn
                  (let ((position (+ (* (+ i 1) 16) 16)))
                    (send self
                          :draw-text (format nil "~a" (select values i))
                          30 (- position 4) 0 0)
                    (when (select not-mask i)
                          (send self :draw-color (select color i))
                          (send self :draw-symbol (select symbol i)
                                (member (select state i)
                                        (list 'selected 'hilited))
                                10 (- position 7))
                          (send self :draw-color 'black))))))
    (send self :buffer-to-screen 0 0 width height)))

(defmeth data-list-proto :close ()
  (let ((plot (slot-value 'plot)))
    (send plot :name-list nil)
    (call-next-method)))

;;;
;;; The main controller for the data-list: receives and processes
;;;     all the mouse clicks on the data-list
;;;

(defmeth data-list-proto :do-click (x y m1 m2)
  (let* ((plot (slot-value 'plot))
         (dataset (slot-value 'dataset)))
    (send plot :unselect-all-points)
    (synchronize-plots plot)))