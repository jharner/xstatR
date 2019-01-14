;;;
;;; Analysis Option Dialog
;;;     lets the user specify options like robustness or using
;;;     covariance or correlation matrix for analysis
;;;

(defproto multivariate-options-dialog-proto '(options-list)
 nil modal-dialog-proto "Analysis Options Dialog")

(defmeth multivariate-options-dialog-proto :isnew ()
  (let* ((dialog self)
         (screen-size (screen-size))
         (screen-width (car screen-size))
         (screen-height (cadr screen-size))
         (stat-name (send text-item-proto :new "Analysis Name"
                          :location '(5 5)))
         (stat (send text-item-proto :new (format nil "Options") :editable t
                     :location '(170 5) :size '(120 15)))
         (type-name (send text-item-proto :new "Type" :location '(5 35)))
         (type (send choice-item-proto :new
                     (list "Correlation" "Covariance" "Custom")
                     :location '(18 50)))
         (robust (send toggle-item-proto :new "Robust Analysis"
                       :location '(170 35)))
         (tuning-value-name (send text-item-proto :new "Tuning Constant"
                          :location '(170 65)))
         (tuning-value (send text-item-proto :new "0.9" :editable t
                             :location '(295 65) :size '(50 15)))
         (tolerance-name (send text-item-proto :new "Tolerance"
                          :location '(170 95)))
         (tolerance (send text-item-proto :new "0.0001" :editable t
                             :location '(295 95) :size '(50 15)))
         (iterations-name (send text-item-proto :new "Iterations"
                                :location '(170 125)))
         (iterations (send text-item-proto :new "5" :editable t
                             :location '(295 125) :size '(50 15)))
         (customize (send modal-button-proto :new "Customize"
                       :location '(5 170) :size '(90 20)
                       :action #'(lambda () nil)))
         (cancel (send modal-button-proto :new "Cancel"
                       :location '(170 170) :size '(70 20)
                       :action #'(lambda () (send dialog :options-list nil))))
         (run (send modal-button-proto :new "Run"
                    :location '(280 170) :size '(70 20)
                    :action #'(lambda ()
                      (let* ((name (send stat :text))
                             (the-type (case (send type :value)
                                         (0 'corr)
                                         (1 'cov)
                                         (2 'custom)))
                             (is-robust (send robust :value))
                             (the-tuning-value
                              (read (make-string-input-stream
                                     (send tuning-value :text))))
                             (the-tolerance (read (make-string-input-stream
                                                   (send tolerance :text))))
                             (num-iterations
                              (read (make-string-input-stream
                                     (send iterations :text)))))
                      (send dialog :options-list
                              (list name the-type is-robust the-tolerance
                                    the-tuning-value num-iterations))))))) 
    
  (call-next-method
           (list stat-name stat type-name type robust
                 tuning-value-name tuning-value
                 tolerance-name tolerance
                 iterations-name iterations customize cancel run)
           :title "Analysis Options"
           :location (list (/ (- screen-width 450) 2)
                           (/ (- screen-height 340) 2))
           :size '(375 210)
           :default-button run)
  
    (unwind-protect (send self :modal-dialog)
                    (send self :remove))))

(defmeth multivariate-options-dialog-proto :options-list (&optional options-list)
  (if options-list (setf (slot-value 'options-list) options-list)
      (slot-value 'options-list)))

