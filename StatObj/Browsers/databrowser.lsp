;;;;
;;;;  This file contains two prototypes: the data-browser-proto,
;;;;    a view for datasets created from dataset-proto, and the
;;;;    data-browser-subview-proto for subviews of the data browser,
;;;;    representing variables.
;;;;

;;;;                                                                    
;;;; Data Browser Prototype: Provides a list view of a dataset prototype                                                            ;;;;

(defproto data-browser-proto
  '(subviews icon-view data-menu stat-menu stat-options-menu)
  '(instances) (list graph-window-proto graph-mixin))

(defmeth data-browser-proto :new (dataset icon-view browser &rest args)
  (let ((data-browser
         (apply #'call-next-method dataset icon-view browser args)))
    (setf (slot-value 'instances)
          (cons data-browser (slot-value 'instances)))
    data-browser))

(defmeth data-browser-proto :isnew (dataset icon-view browser &rest args)
  (setf (slot-value 'dataset) dataset)
  (setf (slot-value 'icon-view) icon-view)
  (if browser
      (let ((variables (send dataset :variables))
            (subviews nil)
            (position 1))
        (dolist (var variables)
                (setf subviews
                      (cons (send data-browser-subview-proto
                                  :new :variable var
                                  :position position :data-browser self)
                            subviews))
                (setf position (+ 1 position)))
        (setf (slot-value 'subviews) (reverse subviews))))
  (apply #'call-next-method args))

;;;
;;;  Constructor and Display Function
;;;

(defun display-data-browser (dataset icon-view)
  (let* ((size (send *data-tree* :size))
         (location (send *data-tree* :location))
         (num-views (send (slot-value 'dataset) :num-var))
         (height (+ (* num-views 16) 24))
         (h-position (send icon-view :h-position))
         (v-position (send icon-view :v-position))
         (start-horizontal (+ (car location) (car size) h-position 2))
         (start-vertical (- (+ (cadr location) v-position) 2))
         (browser t)
         (data-browser (send data-browser-proto
                             :new dataset icon-view browser
                             :location (list start-horizontal start-vertical)
                             :size (list 240 (max 184 height))
                             :title (concatenate
                                     'string (format nil "~s Data"
                                                     (send dataset :name))))))
    (send data-browser :data-menu
          (send  dataset-menu-proto :new data-browser))
    (send data-browser :stat-menu (send stat-menu-proto :new data-browser))
    (send data-browser :stat-options-menu
          (send stat-options-menu-proto :new data-browser))
    (send data-browser :has-v-scroll height)
    (send data-browser :v-scroll-incs 16 80)
    (send data-browser :use-color t)
    (send data-browser :redraw)
    data-browser))

;;;
;;;  Slot Accessors and Mutators                         
;;;

(defmeth data-browser-proto :subviews (&optional subviews)
  (if subviews (setf (slot-value 'subviews) subviews))
  (slot-value 'subviews))

(defmeth data-browser-proto :icon-view (&optional icon-view)
  (if icon-view (setf (slot-value 'icon-view) icon-view))
  (slot-value 'icon-view))

(defmeth data-browser-proto :data-menu (&optional data-menu)
  (if data-menu (setf (slot-value 'data-menu) data-menu))
  (slot-value 'data-menu))

(defmeth data-browser-proto :stat-menu (&optional stat-menu)
  (if stat-menu (setf (slot-value 'stat-menu) stat-menu))
  (slot-value 'stat-menu))

(defmeth data-browser-proto :stat-options-menu (&optional stat-options-menu)
  (if stat-options-menu (setf (slot-value 'stat-options-menu)
                              stat-options-menu))
  (slot-value 'stat-options-menu))

;;;
;;; Other Methods                   
;;;

(defmeth data-browser-proto :close ()
  (send self :hide-window))

(defmeth data-browser-proto :dispose (data-browser)
  (setf (slot-value 'instances)
        (remove data-browser (slot-value 'instances)))
  (send self :delete-self))

(defmeth data-browser-proto :redraw ()
  (let* ((subviews (slot-value 'subviews))
         (delta (second (send self :scroll)))
         (first-view (floor (/ (- delta 16) 16)))
         (size (send self :size))
         (width (first size))
         (num-views (send (slot-value 'dataset) :num-var))
         (height (max (second size) (+ (* num-views 16) 24))))
    (send self :start-buffering)
    (send self :erase-rect 0 0 width height)
    (send self :draw-line 12 (+ delta 3) 18 (+ delta 3))
    (send self :draw-line 12 (+ delta 6) 18 (+ delta 6))
    (send self :draw-line 12 (+ delta 9) 15 (+ delta 9))
    (send self :draw-line 32 (+ delta 3) 38 (+ delta 3))
    (send self :draw-line 32 (+ delta 9) 38 (+ delta 9))
    (send self :draw-line (- width 33) (+ delta 3) (- width 33) (+ delta 9))
    (send self :draw-line (- width 30) (+ delta 3) (- width 30) (+ delta 9))
    (send self :draw-line (- width 27) (+ delta 3) (- width 27) (+ delta 9))
    (send self :draw-line 0 (+ delta 14) width (+ delta 14))
    (dotimes (i num-views)
             (if (> i first-view) (send (select subviews i) :redraw)))
    (send self :buffer-to-screen 0 0 width height)))

;;;
;;;  The main controller for the databrowser: receives and
;;;      processes all the mouse clicks on the data-browser
;;;

(defmeth data-browser-proto :do-click (x y m1 m2)
  (let* ((dataset (slot-value 'dataset))
         (num-x (length (send dataset :variables-of-role 'X)))
         (num-y (length (send dataset :variables-of-role 'Y)))
         (num-z (length (send dataset :variables-of-role 'Z)))
         (delta (second (send self :scroll)))
         (width (first (send self :size)))
         (data-tree-view (send (slot-value 'icon-view) :data-tree-view))
         (view-list (send data-tree-view :get-all-views dataset))
         (subviews (slot-value 'subviews))
         (menu-items (send (slot-value 'stat-menu) :items)))
    (send data-tree-view :focus dataset)
    (send data-tree-view :redraw)
    (cond
      ((and (<= 12 x 18) (<= y (+ delta 14)))
       (let ((items (send (slot-value 'data-menu) :items)))
         (send (select items 2) :enabled
               (find "statcalc" *modules* :test #'equal))
         (send (slot-value 'data-menu) :popup 12 delta  self)))
      ((and (<= 32 x 38) (<= y (+ delta 14)))
       (if (eq m2 t)
           (progn
            (let ((items (send (slot-value 'stat-options-menu) :items)))
              (send (select items 2) :enabled (>= num-y 2))
              (send (select items 3) :enabled
                    (and (>= num-x 2) (>= num-y 2)))
              (send (select items 4) :enabled 
                    (and (>= num-x 2) (>= num-y 1)))
              (send (send stat-options-menu-proto :new self)
                    :popup 32 delta self)))
           (progn
            (let ((items (send (slot-value 'stat-menu) :items)))
              (send (select items 2) :enabled (>= num-y 1))
              (send (select items 3) :enabled (and (>= num-x 1) (>= num-y 1)))
              (send (select items 4) :enabled (and (>= num-x 1) (>= num-y 1)))
              (send (select items 6)
                    :enabled (and (>= num-x 1) (>= num-y 1)
                                  (find "model" *modules* :test #'equal)))
              (send (select items 8)
                    :enabled (and (>= (+ num-x num-y num-z) 3)
                                  (>= (+ num-x num-y) 2) (>= num-y 1)
                                  (find "multivariate" *modules*
                                        :test #'equal)))
              (send (select items 10)
                    :enabled (and (>= num-y 2) (find "multivariate" *modules*
                                                     :test #'equal)))
              (send (select items 11)
                    :enabled (and (>= num-y 2) (find "multivariate" *modules*
                                                     :test #'equal)))
              (send (select items 12)
                    :enabled (and (>= num-x 2) (>= num-y 2)
                                  (find "multivariate" *modules*
                                        :test #'equal)))
              (send (slot-value 'stat-menu) :popup 32 delta self)))))
      ((and (<= (- width 33) x (- width 27)) (<= y (+ delta 14)))
       (progn
        (let ((table (dolist (view view-list)
                             (if  (kind-of-p view data-table-proto)
                                  (return view)))))
          (if table
              (send table :show-window)
              (send *data-tree* :register-dataset-view dataset
                    (display-data-table dataset (slot-value 'icon-view)))))
        (send self :hide-window)))
      (t (dolist (subview subviews)
                 (if (send subview :in-subview x y)
                     (send  subview :do-click x y m1 m2)))))
    (send self :redraw)))

;;;;                                                                       
;;;; Data Browser Subview Prototype: an iconic view for variables                                                           ;;;;

(defproto data-browser-subview-proto '(data-browser variable position))

;;;
;;; Other Methods               
;;;

(defmeth data-browser-subview-proto :in-subview (x y)
  (let ((start (+ (* (slot-value 'position) 16) 8)))
    (and (<= start y (+ start 16)))))

(defmeth data-browser-subview-proto :redraw ()
   (let* ((variable (slot-value 'variable))
          (data-browser (slot-value 'data-browser))
          (start (+ (* (slot-value 'position) 16) 8))
          (role (send (eval variable) :role))
          (type (send (eval variable) :type))
          (name (send (eval variable) :name)))
     (send data-browser :draw-string (format nil "~s" name)
           32 (+ start 11))
     (if (eql type 'C) (send data-browser :line-type 'dashed))
     (if (not (or (eql role 'L) (eql role 'W)))
         (progn
          (send data-browser :draw-line 16 (+ start 8) 23 (+ start 8))
          (send data-browser :draw-line 16 (+ start 1) 16 (+ start 7))
          (send data-browser :draw-line 16 (+ start 8) 12 (+ start 12))))
     (send data-browser :line-width 2)
     (send data-browser :draw-color 'red)
     (case role
       ('L (send data-browser :frame-rect 12 (+ start 4) 10 7))
       ('X (send data-browser :draw-line 16 (+ start 8) 22 (+ start 8)))
       ('Y (send data-browser :draw-line 16 (+ start 1) 16 (+ start 7)))
       ('Z (send data-browser :draw-line 15 (+ start 8) 11 (+ start 12))))
     (send data-browser :line-width 1)
     (send data-browser :draw-color 'black)
     (send data-browser :line-type 'solid)))

;;;
;;; The main controller for the variable subview: receives and
;;;     processes all the mouse clicks
;;;

(defmeth data-browser-subview-proto :do-click (x y m1 m2)
  (let* ((start (+ (* (slot-value 'position) 16) 8))
         (data-browser (slot-value 'data-browser))
         (dataset (send data-browser :dataset))
         (variable (slot-value 'variable))
         (type (send (eval variable) :type))
         (role (send (eval variable) :role)))
    (if (<= 10 x 24)
        (if (eq m2 t)
            (send (send type-menu-proto :new self)
                  :popup 10 start data-browser)
            (if (not (or (eql role 'L) (eql role 'W)))
                (cond
                  ((and (<= 17 x 23) (<= (+ start 7) y ( + start 9)))
                   (send (eval variable) :role 'X))
                  ((and (<= 15 x 17) (<= start y (+ start 6)))
                   (send (eval variable) :role 'Y))
                  ((and (<= 10 x 16) (<= (+ start 8) y (+ start 12)))
                   (send (eval variable) :role 'Z))
                  (t (send (eval variable) :role nil))))))
    (send self :redraw)))
