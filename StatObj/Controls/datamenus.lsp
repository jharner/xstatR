;;;
;;; Data Browser Menus and Items
;;;

(defproto stat-menu-proto () nil menu-proto "Stat-Menu")

(defmeth stat-menu-proto :isnew (browser)
  (call-next-method "Stat")
  (let* ((dataset (send browser :dataset)))
    (send self :append-items
          (send menu-item-proto :new "Messages")
          (send dash-item-proto :new)
          (send menu-item-proto :new "y|zPlot"
                :action #'(lambda () (make-yPlot dataset)))
          (send menu-item-proto :new "y~x|zPlot"
                :action #'(lambda () (make-xyPlot dataset)))
          (send menu-item-proto :new "y~x...|zPlot"
                :action #'(lambda () (message-dialog "Under Construction")))
          (send dash-item-proto :new)
          (send menu-item-proto :new "y~x...Model"
                :action #'(lambda () (make-model dataset browser)))
          (send dash-item-proto :new)
          (send menu-item-proto :new "2-dProj"
                :action #'(lambda () (make-2dProj dataset browser)))
          (send dash-item-proto :new)
          (send menu-item-proto :new "PrinComp"
                :action #'(lambda () (make-princomp dataset browser)))
          (send menu-item-proto :new "Correspondence"
                :action #'(lambda () (make-correspondence dataset browser)))
          (send menu-item-proto :new "CanCorr"
                :action #'(lambda () (make-cancorr dataset browser))))))

(defproto stat-options-menu-proto ()
  nil menu-proto "Stat-Options-Menu")

(defmeth stat-options-menu-proto :isnew (browser)
  (call-next-method "Stat-Options")
  (let ((dataset (send browser :dataset)))
    (send self :append-items
          (send menu-item-proto :new "Messages")
          (send dash-item-proto :new)
          (send menu-item-proto :new "PrinComp..."
                :action
                #'(lambda () 
                    (let* ((analysis-options
                            (send (send multivariate-options-dialog-proto
                                        :new)
                                  :slot-value 'options-list)))
                      (if analysis-options
                          (let* ((name (first analysis-options))
                                 (the-type (second analysis-options))
                                 (is-robust (third analysis-options)) 
                                 (tolerance (fourth analysis-options))
                                 (tuning-value (fifth analysis-options))
                                 (num-iterations (sixth analysis-options)))
                            (make-princomp  dataset browser :name name
                                            :the-type the-type
                                            :robust is-robust
                                            :tolerance tolerance 
                                            :tuning-value tuning-value 
                                            :iterations num-iterations))))))
          (send menu-item-proto :new "Discrim..."
                :action #'(lambda () nil))
          (send menu-item-proto :new "CanCorr..."
                :action
                #'(lambda () 
                    (let* ((analysis-options
                            (send (send multivariate-options-dialog-proto
                                        :new)
                                  :slot-value 'options-list)))
                      (if analysis-options
                          (let* ((name (first analysis-options))
                                 (the-type (second analysis-options))
                                 (is-robust (third analysis-options)) 
                                 (tolerance (fourth analysis-options))
                                 (tuning-value (fifth analysis-options))
                                 (num-iterations (sixth analysis-options)))
                            (make-cancorr dataset browser :name name
                                          :the-type the-type
                                          :robust is-robust
                                          :tolerance tolerance 
                                          :tuning-value tuning-value 
                                          :iterations num-iterations)))))))))

;;;;
;;;; Dataset Menus and Items
;;;;

(defproto dataset-menu-proto () () menu-proto "Dataset Menu")

(defmeth dataset-menu-proto :isnew (browser)
  (call-next-method "Dataset")
  (let ((dataset (send browser :dataset)))
    (send self :append-items
          (send menu-item-proto :new "Messages"
                :action #'(lambda () ()))
          (send dash-item-proto :new)
          (send menu-item-proto :new "New Variable..."
                :action #'(lambda () (make-calculator dataset)))
          (send dash-item-proto :new)
          (send menu-item-proto :new "Clear Roles"
                :action #'(lambda () (send dataset :clear-roles)))
          (send menu-item-proto :new "Clear X Roles"
                :action #'(lambda () (send dataset :clear-role 'X)))
          (send menu-item-proto :new "Clear Y Roles"
                :action #'(lambda () (send dataset :clear-role 'Y)))
          (send menu-item-proto :new "Clear Z Roles"
                :action #'(lambda () (send dataset :clear-role 'Z))))))

;;;
;;; Variable Type Menu Prototype
;;;

(defproto type-menu-proto '() nil menu-proto "Type Menu")

(defmeth type-menu-proto :isnew (subview)
  (call-next-method "Variable")
  (let* ((var (eval (send subview :slot-value 'variable)))
         (values (send var :values))
         (type-item (send menu-item-proto :new "Variable Type"))
         (dash (send dash-item-proto :new))
         (numeric (send menu-item-proto :new "Numeric" :action
                        #'(lambda () (send var :set-type 'N))))
         (categoric (send menu-item-proto :new "Categoric" :action
                          #'(lambda () (send var :set-type 'C))))
         (dash (send dash-item-proto :new))
         (label (send menu-item-proto :new "Label" :action
                      #'(lambda () (let ((owner (send var :owner)))
                                     (send owner :set-label-variable var)))))
         (weight (send menu-item-proto :new "Weight" :action
                       #'(lambda ()
                           (let ((owner (send var :owner)))
                             (send owner :set-weight-variable var))))))
    (send self :append-items type-item dash numeric categoric dash
          label weight)))

