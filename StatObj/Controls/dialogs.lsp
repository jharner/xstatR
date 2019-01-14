;;;;
;;;;  Dataset-state-dialog-proto: allows the user to change data values and
;;;;      other dataset attributes
;;;;

(defproto dataset-state-dialog-proto '(dataset)
  nil modal-dialog-proto "Dataset State Dialog")

(defmeth dataset-state-dialog-proto :isnew (dataset)
  (let* ((screen-size (screen-size))
         (screen-width (car screen-size))
         (screen-height (cadr screen-size))
         (desc-contents (if (eql (send dataset :description) nil)
                            ""
                            (send dataset :description)))
         (variables (send dataset :variables))
         (row-value-index nil)
         (col-value-index nil)
         (dataset-label (send text-item-proto :new "Dataset"
                             :location '(5 5)))
         (dataset-name (send text-item-proto :new
                             (format nil "~s" (send dataset :name))
                    :editable t
                    :location '(5 28) :size '(120 15)))
         (desc-label (send text-item-proto :new "Description"
                          :location '(5 57)))
         (desc (send text-item-proto :new desc-contents
                     :editable t :location '(5 85) :size '(155 97)))
         (values-label (send text-item-proto :new "Values"
                            :location '(185 5)))
         (values (send list-item-proto :new
                  (permute-array
                   (make-array (list (+ (send dataset :num-var) 1)
                                     (send dataset :num-obs))
                    :initial-contents
                               (let ((datal (list (map-elements
                                       #'(lambda (x) (format nil "~s" x))
                                       (iseq 1 (send dataset :num-obs))))))
                        (dolist (var variables datal)
                         (setf datal (cons (map-elements
                                #'(lambda (x) (format nil "~s" x))
                                (send (eval var) :values)) datal)))
                                 (reverse datal))) '(1 0))
                    :columns 3
                    :location '(185 25) :size '(260 160)))
         (value-label (send text-item-proto :new "Value"
                            :location '(280 195)))
         (value (send text-item-proto :new "" :editable t
                      :location '(348 195) :size '(80 15)))
         (cancel (send modal-button-proto :new "Cancel"
                       :location '(210 225) :size '(70 20)
                       :action #'(lambda () nil)))
         (apply (send button-item-proto :new "Apply"
                      :location '(290 225) :size '(70 20)))
         (ok (send modal-button-proto :new "OK"
                   :location '(370 225) :size '(70 20))))
    (send values :action #'(lambda (double)
                             (let* ((selection (send values :selection))
                                    (row-index (car selection))
                                    (col-index (cadr selection)))
                               (setf row-value-index row-index)
                               (setf col-value-index col-index)
                               (if (not (eql col-index 0))
                                (send value :text
                                  (format nil "~s" (select
                                       (send
                                     (eval (select variables (- col-index 1)))
                                          :values) row-index)))))))
    (send apply :action
          #'(lambda ()
              (let ((new-value (send value :text)))
                (send values :set-text (list row-value-index
                                             col-value-index) new-value)
                (setf (select (send (eval (select vars (- col-value-index 1)))
                                    :values) row-value-index)
                      (read (make-string-input-stream new-value))))))
    (send ok :action #'(lambda ()
                         (send dataset :description
                               (send desc :text))
                         (let ((data (send data :text)))
                           (send dataset :name
                                 (read (make-string-input-stream data))))
                         t))
    (call-next-method
     (list dataset-label dataset-name desc-label desc values-label values
           value-label value cancel apply ok)
     :title "Dataset State"
     :location (list (/ (- screen-width 450) 2)
                     (/ (- screen-height 250) 2))
     :size '(450 250)
     :default-button ok)
    (unwind-protect (send self :modal-dialog)
                    (send self :remove))))

;;;;
;;;;Variable-State Dialog Prototype
;;;;

(defproto variable-state-dialog-proto '(var) ()
  modal-dialog-proto)

(defmeth variable-state-dialog-proto :isnew (var &rest args)
  (setf (slot-value 'var) var)
  (let* ((screen-size (screen-size))
         (screen-width (car screen-size))
         (screen-height (cadr screen-size))
         (type (send var :type))
         (owner (send var :owner))
         (label-variable (if owner (send owner :label-variable)))
         (labels (if label-variable
                     (send (eval label-variable) :values)
                     (iseq 1 (send var :data-length))))
         (desc-contents (if (eql (send var :description) nil)
                            ""
                            (send var :description)))
         (value-indices nil)
         (name-label (send text-item-proto :new "Variable" :location '(5 5)))
         (name (send text-item-proto :new (format nil "~s"
                                                  (send var :name))
                     :editable t
                     :location '(5 28) :size '(160 15)))
         (desc-label (send text-item-proto :new "Description"
                           :location '(187 5)))
         (desc (send text-item-proto :new desc-contents :editable t
                     :location '(187 28) :size '(147 50)))
         (values-label (send text-item-proto :new "Labels        Values"
                             :location '(5 70)))
         (values (send list-item-proto
                       :new (bind-columns
                             (map-elements
                              #'(lambda (x) (format nil "~s" x)) labels)
                             (map-elements
                              #'(lambda (x) (format nil "~s" x))
                              (send var :values)))
                       :columns 2
                       :location '(5 90) :size '(165 160)))
         (value-label (send text-item-proto :new "Value"
                            :location '(185 200)))
         (value (send text-item-proto :new "" :editable t
                      :location '(240 200) :size '(95 15)))
         (reset (send button-item-proto :new "Reset"
                           :location '(185 230) :size '(70 20)))
         (apply (send button-item-proto :new "Apply"
                      :location '(265 230) :size '(70 20)))
         (cancel (send modal-button-proto :new "Cancel"
                       :location '(185 260) :size '(70 20)
                       :action #'(lambda () nil)))
         (ok (send modal-button-proto :new "OK"
                   :location '(265 260) :size '(70 20))))
    (send values :action
          #'(lambda (double)
              (let ((index (car (send values :selection))))
                (setf value-indices index)
                (send value :text
                      (format nil "~s" (select (send var :values) index))))))
    (if args
        (case type
          (N (send (cadr args) :action
                   #'(lambda (double)
                       (let ((index (send (cadr args) :selection)))
                         (send value :text (format nil "~s"
                            (select (car (send var :unique-values))
                                    index)))))))
          (C (send (cadr args) :action
                   #'(lambda (double)
                       (let ((index (send (cadr args) :selection)))
                         (send value :text (format nil "~s"
                            (select (send var :actual-levels) index)))))))))
    (send reset :action
          #'(lambda ()
              (let ((i 0)) 
                (if levels
                    (progn (setf new-levels (send var :make-levels))
                           (dolist (j new-levels nil)
                                   (send levels :set-text i
                                         (with-output-to-string (s)
                                                                (prin1 j s)))
                                   (setf i (+ i 1))))
                    (let ((l (send var :levels))
                          (i 0)
                          (null-text ""))
                      (dolist (j l nil)
                              (send levels :set-text i
                                (with-output-to-string (s)
                                                       (prin1 null-text s)))
                              (setf i (+ i 1))))))))
    (send apply :action
          #'(lambda ()
              (let ((new-value (send value :text)))
                (if (not (equalp new-value ""))
                    (progn (send values :set-text (list value-indices 1)
                                 new-value)
                           (send var :values (setf (select (send var :values)
                                                           value-indices)
                            (read (make-string-input-stream new-value)))))))))
    (send ok :action
          #'(lambda ()
              (let* ((name (send name :text)))
                (send var :name (read (make-string-input-stream name)))
                (send var :description (send desc :text))
                t)))
    (let ((items (list name-label name desc-label desc values-label values
                       value-label value cancel reset apply ok)))
      (if args (setf items (append args items)))
      (call-next-method items
                        :title "Variable State"
                        :location (list (/ (- screen-width 450) 2)
                                        (/ (- screen-height 340) 2))
                        :size '(340 285)
                        :default-button ok))
    (unwind-protect (send self :modal-dialog)
                    (send self :remove))))

;;;
;;; Numeric-variable-state Dialog Prototype
;;;

(defproto numeric-variable-state-dialog-proto '() nil
  variable-state-dialog-proto)

(defmeth numeric-variable-state-dialog-proto :isnew (var)
  (let ((unique-values (send var :unique-values)))
    (if unique-values
        (let* ((unique-name (send text-item-proto :new "Unique Values"
                                  :location '(185 175)))
               (unique-values (send var :unique-values))
               (unique (send list-item-proto :new
                             (map-elements #'(lambda (x) (format nil "~s" x))
                                           unique-values)
                             :location '(185 90) :size '(110 80))))
          (call-next-method var unique-name unique))
        (call-next-method var))))

;;;
;;; Categoric-variable-state Dialog Prototype
;;;

(defproto categoric-variable-state-dialog-proto '() nil
  variable-state-dialog-proto)

(defmeth categoric-variable-state-dialog-proto :isnew (var)
  (let* ((levels-name (send text-item-proto :new "Levels"
                            :location '(185 175)))
         (levels (send list-item-proto :new
                       (map-elements #'(lambda (x) (format nil "~s" x))
                                     (send var :actual-levels))
                       :location '(185 90) :size '(110 80))))
    (call-next-method var levels-name levels)))
