;;;;
;;;; Data Sets View Prototype
;;;; 

(defproto data-view-proto '(data-sets subviews focus)
  nil graph-window-proto)

(defmeth data-view-proto :isnew (&rest args)
  (send self :title "Data Sets")
  (setf (slot-value 'data-sets) data-sets)
  (apply #'call-next-method args))

;;;
;;; Constructor and Display Function
;;;

(defun display-data-view ()
 (let* ((screen-size (screen-size))
        (screen-width (car screen-size))
        (screen-height (cadr screen-size))
        (width 300)
        (height (floor (* (/ 1 3) screen-height)))
        (data-view (send data-view-proto :new
                        	:location (list 4 42)
                        	:size (list width height) :go-away t)))
   (send data-view :has-v-scroll 100)
   (send data-view :v-scroll-incs 10 40)
   (send data-view :use-color t)
   data-view))

;;;;
;;;; Slot Accessors and Mutators
;;;;

(defmeth data-view-proto :data-sets ()
  (slot-value 'data-sets))

(defmeth data-view-proto :subviews (&optional subviews)
  (if subviews (setf (slot-value 'subviews) subviews))
  (slot-value 'subviews))

(defmeth data-view-proto :focus (&optional focus)
  (if focus (setf (slot-value 'focus) focus))
  (slot-value 'focus))

;;;;
;;;; Other Methods
;;;;

(defmeth data-view-proto :close ()
  (send self :hide-window))

(defmeth data-view-proto :register-dataset (dataset)
  (let ((icon-view (send data-subview-proto :new
                   	:data-view self :dataset dataset)))
    (setf (slot-value 'data-sets) (cons dataset :add-dataset dataset)
    (send self :redraw)))
    
;;
;;  Prototype for the dataset subviews                                             
;;
    
(defproto data-subview-proto '(data-view dataset h-position v-position))

;;
;; Slot Accessors and Mutators
;;

(defmeth data-subview-proto :data-view ()
  (slot-value 'data-view))

(defmeth data-subview-proto :dataset (&optional dataset)
  (if dataset (setf (slot-value 'dataset) dataset))
  (slot-value 'dataset))

(defmeth data-subview-proto :h-position (&optional h-position)
  (if h-position (setf (slot-value 'h-position) h-position))
  (slot-value 'h-position))

(defmeth data-subview-proto :v-position (&optional v-position)
  (if v-position (setf (slot-value 'v-position) v-position))
  (slot-value 'v-position))

;;
;; Other Methods
;;

(defmeth data-tree-subview-proto :redraw (h-position v-position)
  (let* ((data-view (slot-value 'data-view))
         (focus (send data-view :focus))
         (name (send (slot-value 'dataset) :name)))
    (flet ((draw-line (x-left y-top x-right y-bottom)
                      (send data-view :draw-line
                            (+ h-position x-left) (+ v-position y-top)
                            (+ h-position x-right) (+ v-position y-bottom))))
      (if (eql (slot-value 'dataset) focus)
          (progn
           (send data-view :line-width 2)
           (send data-view :draw-color 'red)
           (draw-line 12 3 18 3)
           (draw-line 12 6 18 6)
           (draw-line 12 9 18 9)
           (send data-view :line-width 1)
           (send data-view :draw-color 'black))
          (progn
           (draw-line 12 3 18 3)
           (draw-line 12 6 18 6)
           (draw-line 12 9 18 9))))
    (send (slot-value 'data-view) :draw-string (format nil "~15a" name) 
          (+ h-position 27) (+ v-position 11))))

(defmeth data-subview-proto :in-subview (x y)
  (let ((h-position (slot-value 'h-position))
        (v-position (slot-value 'v-position)))
    (and (<= h-position x (+ h-position 120))
         (<= (+ v-position 2)  y (+ v-position 12)))))

(defmeth data-subview-proto :do-click (x y m1 m2)
  (let* ((h-position (slot-value 'h-position))
         (data-view (slot-value 'data-view))
         (dataset (slot-value 'dataset)))
  	(send data-view :focus dataset)
   	(send data-view :redraw)))
        
