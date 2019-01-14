;;;;
;;;;  Contains the data-tree-proto, a tree structure holding datasets
;;;;      according to their dependency relationships; data-tree-view-proto,
;;;;      administrator of all models (and their views) created in a session;
;;;;      dictionary proto, a list of list for keeping track of dependencies
;;;;      between models and their views
;;;;

;;;;
;;;;  Prototype for the data tree                                          
;;;;

(defproto data-tree-proto '(tree) '(instances) nil)

(defmeth data-tree-proto :new ()
  (let* ((data-tree  (call-next-method)))
    (setf (slot-value 'instances)
          (cons data-tree (slot-value 'instances)))
    data-tree))

(defmeth data-tree-proto :isnew ()
  (setf (slot-value 'tree) (cons nil ())))

;;;
;;;  Slot Accessors and Mutators
;;;

(defmeth data-tree-proto :tree (&optional (tree nil set-tree))
  (if set-tree (setf (slot-value 'tree) tree))
  (slot-value 'tree))

;;;
;;; Other Methods
;;;

(defmeth data-tree-proto :add-dataset (parent dataset)
  (let ((tree (slot-value 'tree)))
    (setf (slot-value 'tree)
          (add-dataset tree parent dataset))))

(defmeth data-tree-proto :find-dataset-subtree (dataset)
  (let ((tree (slot-value 'tree)))
    (find-dataset-subtree (cons tree ()) dataset)))

(defmeth data-tree-proto :find-dataset-descendants (dataset)
  (let ((subtree (send self :find-dataset-subtree dataset)))
    (rest subtree)))

(defmeth data-tree-proto :delete-dataset-subtree (dataset)
  nil)

(defmeth data-tree-proto :hide-dataset-descendants (dataset)
  nil)

(defmeth data-tree-proto :show-dataset-descendants (dataset)
  nil)

;;;
;;;  Data Tree Proto Helper Functions
;;;

(defun find-dataset-subtree (tree dataset)
  (cond 
    ((endp tree) nil)
    ((eq (first (first tree)) dataset)
     (first tree))
    (t (find-dataset-subtree
        (append (rest (first tree)) (rest tree)) dataset))))

(defun add-dataset (tree parent dataset)
  (cond
    ((endp tree) nil)
    ((or (symbolp (first tree)) (kind-of-p (first tree) dataset-proto))
     (if (eq (first tree) parent)
         (cons (first tree)
               (reverse (cons (list dataset) (reverse (rest tree)))))
         (cons (first tree)
               (add-dataset (rest tree) parent dataset))))
    (t
     (cons (add-dataset (first tree) parent dataset)
           (add-dataset (rest tree) parent dataset)))))

;;;;
;;;; Data Tree View Prototype
;;;;     The Data Tree View is the administrator of all the
;;;;     models and views created in a session
;;;;

(defproto data-tree-view-proto '(data-tree dictionary subviews focus)
  nil graph-window-proto)

(defmeth data-tree-view-proto :isnew (&rest args)
  (send self :title "Data Tree")
  (setf (slot-value 'data-tree) (send data-tree-proto :new))
  (setf (slot-value 'dictionary) (send dictionary-proto :new))
  (apply #'call-next-method args))

;;;
;;; Constructor and Display Function
;;;

(defun display-data-tree ()
 (let* ((screen-size (screen-size))
        (screen-width (car screen-size))
        (screen-height (cadr screen-size))
        (width 300)
        (height (floor (* (/ 1 3) screen-height)))
        (data-tree-view (send data-tree-view-proto :new
                        :location (list 4 42)
                        :size (list width height) :go-away t)))
   (send data-tree-view :has-v-scroll 100)
   (send data-tree-view :v-scroll-incs 10 40)
   (send data-tree-view :use-color t)
   data-tree-view))

;;;;
;;;; Slot Accessors and Mutators
;;;;

(defmeth data-tree-view-proto :data-tree ()
  (slot-value 'data-tree))

(defmeth data-tree-view-proto :dictionary ()
  (slot-value 'dictionary))

(defmeth data-tree-view-proto :subviews (&optional subviews)
  (if subviews (setf (slot-value 'subviews) subviews))
  (slot-value 'subviews))

(defmeth data-tree-view-proto :focus (&optional focus)
  (if focus (setf (slot-value 'focus) focus))
  (slot-value 'focus))

;;;;
;;;; Other Methods
;;;;

(defmeth data-tree-view-proto :close ()
  (send self :hide-window))

(defmeth data-tree-view-proto :register-dataset (parent dataset)
  (let ((icon-view (send data-tree-subview-proto :new
                         :data-tree-view self :dataset dataset)))
    (send (slot-value 'data-tree) :add-dataset parent dataset)
    (send (slot-value 'dictionary) :add-dataset-view dataset icon-view)
    (send self :redraw)))

(defmeth data-tree-view-proto :unregister-datasets (parent)
  (send (slot-value 'data-tree) :delete-dataset-subtree parent))

(defmeth data-tree-view-proto :register-dataset-view (dataset view)
  (let ((dictionary (send self :dictionary)))
    (send dictionary :add-dataset-view dataset view)))

(defmeth data-tree-view-proto :unregister-dataset-view (dataset view)
  (send (slot-value 'dictionary) :delete-view dataset view))

(defmeth data-tree-view-proto :unregister-all-views (dataset)
  (send (slot-value 'dictionary) :delete-dataset-views dataset))

(defmeth data-tree-view-proto :register-dataset-with-plot
  (parent dataset plot)
  (let ((dictionary (send self :dictionary)))
    (send self :register-dataset parent dataset)
    (send dictionary :add-dataset-view dataset plot)
    (send self :redraw)))

(defmeth data-tree-view-proto :delete-subview (subview)
  (let* ((subviews (slot-value 'subviews))
         (found-it nil))
    (dolist (view subviews)
            (if found-it 
                (let ((v-position (send view :v-position)))
                  (send view :v-position (- v-position 20))))
            (if (eq view subview) 
                (progn
                 (setf found-it t)
                 (setf subviews (remove view subviews)))))
    (setf (slot-value 'v-position) (- (slot-value 'v-position) 20))  
    (setf (slot-value 'subviews) subviews)))

(defmeth data-tree-view-proto :get-dataset-subtree (parent)
  (send (slot-value 'data-tree) :find-dataset-subtree parent))

(defmeth data-tree-view-proto :get-all-views (dataset)
  (send (slot-value 'dictionary) :get-all-views dataset))

(defmeth data-tree-view-proto :get-all-plots (plot)
  (let* ((dataset (send plot :dataset))
         (views (send self :get-all-views dataset))
         (plots (remove-if
                 #'(lambda (x)
                     (or (eql x plot) (not (kind-of-p x graph-proto))))
                 views)))
    plots))

(defmeth data-tree-view-proto :get-views-of-kind (dataset view-kind)
  (send (slot-value 'dictionary) :get-views-of-kind dataset view-kind))

(defmeth data-tree-view-proto :get-icon-view (dataset)
  (send (slot-value 'dictionary) :get-icon-view dataset))

(defmeth data-tree-view-proto :get-plot-views (dataset)
  (send (slot-value 'dictionary) :get-plot-views dataset))


(defmeth data-tree-view-proto :get-related-views (view)
  (let* ((dataset (send view :dataset))
         (ancestor (send self :get-ancestor dataset)))
    (send self :get-dependent-views ancestor)))

(defmeth data-tree-view-proto :get-ancestor (dataset)
  (if (send dataset :has-slot 'owner)
      (if (send dataset :owner)
          (send self :get-ancestor (send dataset :owner))
          dataset)
      dataset))

(defmeth data-tree-view-proto :get-dependent-views (ancestor)
  (let ((subtree (send self :get-dataset-subtree ancestor))
        (views nil)
        (new-views nil))
    (if subtree
        (setf views
              (dolist (dataset subtree new-views)
                      (setf new-views
                            (concatenate 'list new-views
                                         (send self :get-dependent-views
                                               dataset))))))
    (setf views (concatenate 'list views (send self :get-all-views ancestor)))
    views))

(defmeth data-tree-view-proto :get-related-plots (plot)
  (let* ((views (send self :get-related-views plot))
         (plots (remove-if
                 #'(lambda (x)
                     (or (eql x plot) (not (kind-of-p x graph-proto))))
                 views)))
    plots))

(defmeth data-tree-view-proto :delete-subview (subview)
  (let* ((subviews (slot-value 'subviews))
         (found-it nil))
    (dolist (view subviews)
            (if found-it 
                (let ((v-position (send view :v-position)))
                  (send view :v-position (- v-position 20))))
            (if (eq view subview) 
                (progn
                 (setf found-it t)
                 (setf subviews (remove view subviews)))))
    (setf (slot-value 'v-position) (- (slot-value 'v-position) 20))  
    (setf (slot-value 'subviews) subviews)))

(defmeth data-tree-view-proto :redraw ()
  (let* ((data-tree (send (slot-value 'data-tree) :tree))
         (datasets (rest data-tree))
         (size (send self :size))
         (width (first size))
         (height (second size)))
    (reset-graphics-buffer)
    (send self :start-buffering)
    (send self :erase-rect 0 0 width height)
    (send self :draw-line 0 14 width 14)
    (if datasets
        (traverse-data-tree data-tree 0 0))
    (send self :buffer-to-screen 0 0 width height)))

(defmeth data-tree-view-proto :open-triangle (h-position v-position)
  (send self :draw-line 3 (+ v-position 2) 12 (+ v-position 2))
  (send self :draw-line 2 (+ v-position 2) 7 (+ v-position 7))
  (send self :draw-line 13 (+ v-position 2) 8 (+ v-position 7)))

(defmeth data-tree-view-proto :closed-triangle (h-position v-position)
  (send self :draw-line 8 (+ v-position 2) 8 (+ v-position 11))
  (send self :draw-line 8 (+ v-position 2) 13 (+ v-position 7))
  (send self :draw-line 13 (+ v-position 7) 8 (+ v-position 12)))

(defmeth data-tree-view-proto :do-click (x y m1 m2)
  (let* ((data-tree (send (slot-value 'data-tree) :tree))
         (datasets (rest data-tree))
         (view (if datasets
                   (find-icon-view (cons data-tree ()) x y))))
    (if view (send view :do-click x y m1 m2))))

;;;
;;;
;;;

(defun traverse-data-tree (tree depth count)
  (cond
    ((endp tree) count)
    ((or (symbolp (first tree)) (kind-of-p (first tree) dataset-proto))
     (if (kind-of-p (first tree) dataset-proto)
         (progn
          (let* ((dictionary (send *data-tree* :dictionary))
                 (icon-view (send dictionary :get-icon-view (first tree)))
                 (h-position (+ 10 (* depth 15)))
                 (v-position (+ 24 (* count 16))))
            (send icon-view :h-position h-position)
            (send icon-view :v-position v-position)
            (if (rest tree)
                (send icon-view :terminal nil)
                (send icon-view :terminal t))
            (send icon-view :redraw  h-position v-position))
          (traverse-data-tree (rest tree) (+ depth 1) (+ count 1)))
         (traverse-data-tree (rest tree) depth count)))
    (t
     (progn
      (setf count
            (traverse-data-tree (first tree) depth count))
      (traverse-data-tree (rest tree) depth count)))))

(defun find-icon-view (tree x y)
  (cond
    ((endp tree) nil)
    ((kind-of-p (first (first tree)) dataset-proto)
     (progn
      (let* ((dictionary (send *data-tree* :dictionary))
             (icon-view (send dictionary
                              :get-icon-view (first (first tree)))))
        (if (send icon-view :in-subview x y)
            icon-view
            (find-icon-view (append (rest (first tree)) (rest tree)) x y)))))
    (t (find-icon-view (append (rest (first tree)) (rest tree)) x y))))

;;;;
;;;;  Prototype for the subviews of data-tree, representing objects                                             
;;;;

(defproto data-tree-subview-proto
  '(data-tree-view dataset visible terminal h-position v-position))

;;;;
;;;; Slot Accessors and Mutators
;;;;

(defmeth data-tree-subview-proto :data-tree-view ()
  (slot-value 'data-tree-view))

(defmeth data-tree-subview-proto :dataset (&optional dataset)
  (if dataset (setf (slot-value 'dataset) dataset))
  (slot-value 'dataset))

(defmeth data-tree-subview-proto :visible (&optional visible)
  (if visible (setf (slot-value 'visible) visible))
  (slot-value 'visible))

(defmeth data-tree-subview-proto :terminal
  (&optional (terminal nil set-terminal))
  (if set-terminal (setf (slot-value 'terminal) terminal))
  (slot-value 'terminal))

(defmeth data-tree-subview-proto :h-position (&optional h-position)
  (if h-position (setf (slot-value 'h-position) h-position))
  (slot-value 'h-position))

(defmeth data-tree-subview-proto :v-position (&optional v-position)
  (if v-position (setf (slot-value 'v-position) v-position))
  (slot-value 'v-position))

;;;
;;; Other Methods
;;;

(defmeth data-tree-subview-proto :delete-view (view-kind)
  (let* ((dataset (slot-value 'dataset))
         (view-list (send (slot-value 'data-tree-view)
                          :get-views-of-kind dataset view-kind)))
    (dolist (view view-list) 
            (send view :close)
            (if (eql data-browser-proto view-kind)
                (send (slot-value 'data-tree-view)
                      :browser-count (- (send (slot-value 'data-tree-view)
                                              :browser-count) 1))))))

(defmeth data-tree-subview-proto :delete-dataset ()
  (let* ((dataset (slot-value 'dataset))
         (data-tree-view (slot-value 'data-tree-view))
         (datasets (send data-tree-view :get-descendents dataset)))
    (dolist (dataset datasets)
            (send (send data-tree-view :get-icon-view dataset)
                  :delete-dataset))
    (dolist (view (send data-tree-view :get-all-views dataset))
            (send view :close)
            (if (kind-of-p view data-browser-proto)
                (send data-tree-view
                      :browser-count
                      (- (send data-tree-view :browser-count) 1))))
    (send data-tree-view :unregister-all-views dataset)
    (send data-tree-view :unregister-from-descendents dataset)))

(defmeth data-tree-subview-proto :hide-view (view-kind)
    (let* ((dataset (slot-value 'dataset))
           (view-list (send (slot-value 'data-tree-view)
                            :get-views-of-kind dataset view-kind)))
    (dolist (view view-list) 
            (send view :hide-window))))

(defmeth data-tree-subview-proto :show-plot-views (view-kind)
   (let* ((dataset (slot-value 'dataset))
          (view-list (send (slot-value 'data-tree-view)
                           :get-plot-views dataset)))
     (dolist (view view-list) 
             (send view :show-window))))

(defmeth data-tree-subview-proto :close ()
  (send self :delete-self)
  (send (slot-value 'data-tree-view) :delete-subview self))

(defmeth data-tree-subview-proto :redraw (h-position v-position)
  (let* ((data-tree-view (slot-value 'data-tree-view))
         (focus (send data-tree-view :focus))
         (name (send (slot-value 'dataset) :name)))
    (if (not (slot-value 'terminal))
        (send data-tree-view :open-triangle h-position v-position))
    (flet ((draw-line (x-left y-top x-right y-bottom)
                      (send data-tree-view :draw-line
                            (+ h-position x-left) (+ v-position y-top)
                            (+ h-position x-right) (+ v-position y-bottom))))
      (if (eql (slot-value 'dataset) focus)
          (progn
           (send data-tree-view :line-width 2)
           (send data-tree-view :draw-color 'red)
           (draw-line 12 3 18 3)
           (draw-line 12 6 18 6)
           (draw-line 12 9 18 9)
           (send data-tree-view :line-width 1)
           (send data-tree-view :draw-color 'black))
          (progn
           (draw-line 12 3 18 3)
           (draw-line 12 6 18 6)
           (draw-line 12 9 18 9))))
    (send (slot-value 'data-tree-view) :draw-string (format nil "~15a" name) 
          (+ h-position 27) (+ v-position 11))))

(defmeth data-tree-subview-proto :in-subview (x y)
  (let ((h-position (slot-value 'h-position))
        (v-position (slot-value 'v-position)))
    (and (<= h-position x (+ h-position 120))
         (<= (+ v-position 2)  y (+ v-position 12)))))

(defmeth data-tree-subview-proto :do-click (x y m1 m2)
  (let* ((h-position (slot-value 'h-position))
         (data-tree-view (slot-value 'data-tree-view))
         (dataset (slot-value 'dataset))
         (view-list (send data-tree-view :get-all-views dataset)))
    (if (<= (+ h-position 12) x (+ h-position 22))
        (let ((browser
               (dolist (view view-list)
                       (if  (kind-of-p view data-browser-proto)
                            (return view)))))
          (if browser
              (send browser :show-window)
              (send data-tree-view :register-dataset-view dataset
                    (display-data-browser dataset self)))
          (send data-tree-view :focus dataset)
          (send data-tree-view :redraw))
        (progn
         (send self :show-plot-views dataset)
         (if (find "model" *modules* :test #'equal)
             (dolist (view view-list)
                     (if  (kind-of-p view model-browser-proto)
                          (send view :show-window))))
         (if (find "multivariate" *modules* :test #'equal)
             (if (kind-of-p dataset multivariate-data-proto)
                 (send data-tree-view :register-dataset-view
                       dataset (send report-proto :new dataset))))))))

;;;;
;;;; The identity dictionary is the link between models and their views. It ;;;;     is a list of lists, with the first element in each list the model ;;;;     and the rest of the elements its views
;;;;

(defproto dictionary-proto '(dataset-view-list))

(defmeth dictionary-proto :add-dataset-view (dataset view)
	(let* ((dataset-view-list (slot-value 'dataset-view-list))
        (found-it nil)
        (dictionary nil)
        (new-list (dolist
                   (dataset-views dataset-view-list (reverse dictionary))
                   (if (eq (first dataset-views) dataset)
                       (progn
                        (setf dataset-views
                              (reverse (cons view (reverse dataset-views))))
                        (setf found-it t)))
                   (setf dictionary (cons dataset-views dictionary)))))
   (unless found-it
           (setf new-list
                 (reverse (cons (list dataset view) (reverse new-list)))))
   (setf (slot-value 'dataset-view-list) new-list)))

(defmeth dictionary-proto :delete-view (dataset view)
  (let* ((dataset-view-list (slot-value 'dataset-view-list))
         (dictionary nil)
         (new-list (dolist (dataset-views dataset-view-list
                                          (reverse dictionary))
                           (if (eq (first dataset-views) dataset) 
                               (setf dataset-views
                                     (remove view dataset-views)))
                           (setf dictionary
                                 (cons dataset-views dictionary)))))
    (setf (slot-value 'dataset-view-list) new-list)))

(defmeth dictionary-proto :delete-dataset-views (dataset)
 (let* ((dataset-view-list (slot-value 'dataset-view-list))
        (new-list (dolist (dataset-views dataset-view-list dataset-view-list)
                          (if (eq (first dataset-views) dataset)
                              (progn (setf dataset-view-list
                                           (remove dataset-views
                                                   dataset-view-list))
                                     (return dataset-view-list))))))
   (setf (slot-value 'dataset-view-list) new-list)))

(defmeth dictionary-proto :get-all-views (dataset)
  (dolist (dataset-views (slot-value 'dataset-view-list))
          (if (eq (first dataset-views) dataset)
              (return (rest dataset-views)))))

(defmeth dictionary-proto :get-plot-views (dataset)
  (let ((views (send self :get-all-views dataset)))
    (remove-if-not #'(lambda (plot) (kind-of-p plot graph-proto)) views)))


(defmeth dictionary-proto :get-views-of-kind (dataset view-kind)
  (let ((view-list nil))
    (dolist (dataset-views (slot-value 'dataset-view-list))
            (if (eq (first dataset-views) dataset)
                (return (dolist (view (rest dataset-views)
                                      (reverse view-list))
                                (if (kind-of-p view view-kind) 
                                    (setf view-list
                                          (cons view view-list)))))))))

(defmeth dictionary-proto :get-icon-view (dataset)
 (dolist (dataset-views (slot-value 'dataset-view-list) icon)
         (if (eq (first dataset-views) dataset)
             (return (second dataset-views)))))

