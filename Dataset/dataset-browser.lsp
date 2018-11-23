;;
;; Data Browser and dependencies
;;

;;
;; Stat Menu and Items
;;

(defproto stat-menu-proto () () menu-proto "Stat Menu")

(defmeth stat-menu-proto :isnew (browser)
  (call-next-method "Stat")
  (send self :append-items
        (send menu-item-proto :new "Plot")
        (send dash-item-proto :new)
        (send menu-item-proto :new "yPlot"
              :action #'(lambda () (send browser :yPlot)))
        (send menu-item-proto :new "xyPlot"
              :action #'(lambda () (send browser :xyPlot)))
        (send dash-item-proto :new)
        (send menu-item-proto :new "y~x...Model"
              :action #'(lambda () (send browser :show-model-win)))
  )
)

;;
;; Var Menu and Items
;;

(defproto var-menu-proto () () menu-proto "Variable Menu")

(defmeth var-menu-proto :isnew (browser)
  (call-next-method "Variable")
  (let ((dataset (send browser :dataset)))
    (send self :append-items
          (send menu-item-proto :new "Var")
          (send dash-item-proto :new)
          (send menu-item-proto :new "New Variable..."
                :action #'(lambda () nil))
          (send dash-item-proto :new)
          (send menu-item-proto :new "Clear Roles"
                :action #'(lambda () nil))
          (send menu-item-proto :new "Clear X Roles"
                :action #'(lambda () nil))
          (send menu-item-proto :new "Clear Y Roles"
                :action #'(lambda () nil)))))
                
(defproto variable-view-proto '(browser variable position role))

(defmeth variable-view-proto :isnew (browser var position)
  (setf (slot-value 'browser) browser)
  (setf (slot-value 'variable) var)
  (setf (slot-value 'position) position)
)

(defmeth variable-view-proto :role (&optional (role nil set))
  (if set (setf (slot-value 'role) role))
  (slot-value 'role)
)

(defmeth variable-view-proto :variable ()
  (slot-value 'variable)
)

(defmeth variable-view-proto :is-in (x y)
  (let* (
      (win (slot-value 'browser))
      (offset (send win :slot-value 'toolbar-height))
      (line-height (send win :slot-value 'line-height))
      (startY (+ (* (slot-value 'position) line-height) offset))
    )
    (<= startY y (+ startY line-height))
  )
)

(defmeth variable-view-proto :redraw ()
  (let* (
      (win (slot-value 'browser))
      (offset (send win :slot-value 'toolbar-height))
      (line-height (send win :slot-value 'line-height))
      (startY (+ (* (slot-value 'position) line-height) offset))
      (startX 32)
      (role (send self :role))
      (textAscent (send win :text-ascent))
    )
    (send win :draw-string (format nil "~a" (slot-value 'variable))
                                   startX (+ startY textAscent))
    (if (not (eql role 'L))
         (progn
          (send win :draw-line 12 (+ startY 10) 21 (+ startY 10))
          (send win :draw-line 12 (+ startY 2) 12 (+ startY 10))))
     (send win :line-width 2)
     (send win :draw-color 'red)
     (case role
       ('L (send win :frame-rect 12 (+ startY 2) 8 8))
       ('X (send win :draw-line 13 (+ startY 10) 21 (+ startY 10)))
       ('Y (send win :draw-line 12 (+ startY 2) 12 (+ startY 10))))
     (send win :line-width 1)
     (send win :draw-color 'black)
     (send win :line-type 'solid)
  )
)

(defmeth variable-view-proto :do-click (x y m1 m2)
  (let* (
      (win (slot-value 'browser))
      (line-height (send win :slot-value 'line-height))
      (offset (send win :slot-value 'toolbar-height))
      (startY (+ (* (slot-value 'position) line-height) offset))
      (variable (slot-value 'variable))
      (role (send self :role))
    )
    
    (if (<= 11 x 22)
      (if (eq m2 t)
        (format t "Popup type menu")
        (if (not (eql role 'L))
          (cond
            ((and (<= 13 x 21) (<= (+ startY 9) y ( + startY 11)))
              (send self :role 'X)
            )
            ((and (<= 11 x 13) (<= (+ startY 2) y (+ startY 10)))
              (send self :role 'Y)
            )
            (t (send self :role nil))
          )
        )
      )
    )
  )
)

(defproto dataset-browser-proto '(dataset var-views var-menu stat-menu
                                  type-menu toolbar-height line-height)
  () (list graph-window-proto))

(defmeth dataset-browser-proto :isnew (dataset &rest args)
  (apply #'call-next-method args)
  (send self :v-scroll-incs 16 80)
  (send self :use-color t)
  (setf (slot-value 'var-menu) (send var-menu-proto :new self))
  (setf (slot-value 'stat-menu) (send stat-menu-proto :new self))
  (setf (slot-value 'toolbar-height) 16)
  (setf (slot-value 'line-height)
        (+ 3 (send self :text-ascent) (send self :text-descent)))
  (send self :dataset dataset)
)

(defmeth dataset-browser-proto :dataset (&optional (dataset nil set))
  (cond
    (set
      (setf (slot-value 'dataset) dataset)
      (let* (
          (vars (send dataset :variable-names))
          (nvars (length vars))
          (height (+ (* nvars 16) 24))
          (varViews nil)
        )

        (send self :size 300 (max 184 height))
        (send self :title (format nil "Dataset: ~s" (send dataset :name)))
        (send self :has-v-scroll height)
        (dotimes (i nvars)
          (setf varViews (cons (send variable-view-proto :new self
                                     (elt vars i) i) varViews))
        )
        (setf (slot-value 'var-views) (reverse varViews))
        (send self :redraw)
      )
    )
  )
  (slot-value 'dataset)
)

(defmeth dataset-browser-proto :variables-of-role (role)
  (let ((varList nil))
    (dolist (varView (slot-value 'var-views))
      (if (equal role (send varView :role))
        (setf varList (cons (send varView :variable) varList)))
    )
    (if varList (reverse varList) varList)
  )
)

(defmeth dataset-browser-proto :redraw ()
  (let* (
      (var-views (slot-value 'var-views))
      (delta (second (send self :scroll)))
      (first-view (floor (/ (- delta (slot-value 'toolbar-height))
                            (slot-value 'line-height))))
      (size (send self :size))
      (width (first size))
      (nvars (length (slot-value 'var-views)))
      (height (max (second size) (* nvars (slot-value 'line-height))))
    )
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

    (dotimes (i nvars)
             (if (> i first-view) (send (select var-views i) :redraw)))
    (send self :buffer-to-screen 0 0 width height)
  )
)

(defmeth dataset-browser-proto :do-click (x y m1 m2)
  (let* (
      (num-x (length (send self :variables-of-role 'X)))
      (num-y (length (send self :variables-of-role 'Y)))
      (delta (second (send self :scroll)))
      (width (first (send self :size)))
      (var-views (slot-value 'var-views))
    )
    
    (cond
      ((and (<= 12 x 18) (<= y (+ delta (slot-value 'toolbar-height))))
        (send (slot-value 'var-menu) :popup (+ x 2) (+ y 2) self)
      )
      ((and (<= 32 x 38) (<= y (+ delta (slot-value 'toolbar-height))))
        (let ((items (send (slot-value 'stat-menu) :items)))
          (send (select items 2) :enabled (>= num-y 1))
          (send (select items 3) :enabled (and (>= num-x 1) (>= num-y 1)))
          (send (select items 4) :enabled
                                 (and (find "model" *modules* :test #'equal)
                                 (>= num-x 1) (>= num-y 1)))
        )
        (send (slot-value 'stat-menu) :popup (+ x 2) (+ y 2) self)
      )
      (t
        (dolist (var var-views)
          (if (send var :is-in x y) (send var :do-click x y m1 m2)))
        (send self :redraw)
      )
    )
  )
)

(defmeth dataset-browser-proto :yPlot ()
  (let (
      (dataset (send self :dataset))
      (y-variables (send self :variables-of-role 'Y))
    )

    (dolist (y y-variables)
      (case (send dataset :type-of-var y)
            ('N (message-dialog
                 (concatenate 'string
                              (format nil "Dynamic histPlot for ")
                              (format nil  "~s" y)
                              (format nil " is not yet available."))))
            ('C (message-dialog
             	 (concatenate 'string
                              (format nil "Dynamic barChart for ")
                              (format nil  "~s" y)
                              (format nil " is not yet available."))))
      )
    )
  )
)

(defmeth dataset-browser-proto :xyPlot()
  (let (
      (dataset (send self :dataset))
      (x-variables (send self :variables-of-role 'X))
      (y-variables (send self :variables-of-role 'Y))
    )
  
    (dolist (x x-variables)
      (if (eql (send dataset :type-of-var x) 'C)
        (dolist (y y-variables)
          (case (send dataset :type-of-var y)
                ('N (message-dialog
                     (concatenate 'string
                                  (format nil "Dynamic dotPlot for ")
                                  (format nil  "~s vs " x)
                                  (format nil  "~s~%" y)
                                  (format nil "is not yet available."))))
                ('C (message-dialog
                     (concatenate 'string
                                  (format nil "Dynamic mosaicPlot for ")
                               	  (format nil  "~s vs " x)
                               	  (format nil  "~s~%" y)
                              	  (format nil "is not yet available."))))
          )
        )
        (dolist (y y-variables) 
          (case (send dataset :type-of-var y)
                ('N (make-scatplot x y dataset))
                ('C (message-dialog
                     (concatenate 'string
                                  (format nil "Dynamic logitPlot for ")
                                  (format nil  "~s vs " (send x-var :name))
                                  (format nil  "~s~%" (send y-var :name))
                                  (format nil "is not yet available."))))
          )
        )
      )
    )
  )
)

(defmeth dataset-browser-proto :show-model-win ()
  (let (
      (ylist (send self :variables-of-role 'Y))
      (xlist (send self :variables-of-role 'X))
    )
    (if (and ylist xlist (> (length ylist) 0))
      (send model-win-proto :new (elt ylist 0) xlist self)
    )
  )
)
