(provide ":StatIns:arrbtn.lsp")
(require ":StatIns:cntls.lsp")
(require ":StatIns:symbols.lsp")

;;
;; Arrow button for increasing and decreasing
;;

(defproto bitmap-btn-proto '(hilite-bm unhilite-bm) () cntl-proto)

(defmeth bitmap-btn-proto :isnew (lft tp wind)
  (call-next-method lft tp 11 11 "Oops" 1 wind))

(defmeth bitmap-btn-proto :hilite-bm (&optional hilite-bm)
  (if hilite-bm (setf (slot-value 'hilite-bm) hilite-bm))
  (slot-value 'hilite-bm))

(defmeth bitmap-btn-proto :unhilite-bm (&optional unhilite-bm)
  (if unhilite-bm (setf (slot-value 'unhilite-bm) unhilite-bm))
  (slot-value 'unhilite-bm))

(defmeth bitmap-btn-proto :draw-cntl-hilited ()
  (let* ((bm (send self :hilite-bm))
         (lft (send self :lft))
         (tp (send self :tp))
         (w (send self :wind)))
    (send self :frame-cntl)
    (send w :draw-bitmap bm (+ lft 2) (+ tp 2))))

(defmeth bitmap-btn-proto :draw-cntl-unhilited ()
  (let* ((bm (send self :unhilite-bm))
         (lft (send self :lft))
         (tp (send self :tp))
         (w (send self :wind)))
    (send self :frame-cntl)
    (send w :draw-bitmap bm (+ lft 2) (+ tp 2))))

(defmeth bitmap-btn-proto :released-in (x y m1 m2)
  (send self :hilite 0)
  (send self :redraw))

(defmeth bitmap-btn-proto :no-mod-click (x y m1 m2)
  (send self :hilite 0)
  (send self :redraw))
;;
;; Param btn proto
;;

(defproto param-btn-proto '(distn paramnum) () bitmap-btn-proto)

(defmeth param-btn-proto :distn (&optional distn)
  (if distn (setf (slot-value 'distn) distn))
  (slot-value 'distn))

(defmeth param-btn-proto :paramnum (&optional paramnum)
  (if paramnum (setf (slot-value 'paramnum) paramnum))
  (slot-value 'paramnum))

(defmeth param-btn-proto :isnew (lft tp wind distn paramnum)
  (send self :distn distn)
  (send self :paramnum paramnum)
  (call-next-method lft tp wind))

;;
;; param arrow btn proto
;;

(defproto param-arr-proto '() () param-btn-proto)

(defmeth param-arr-proto :press-action ()
  (let* ((w (send self :wind))
         (s-pdf (send w :s-pdf))
         (paramtxt (send w :params))
         (probtxt (if (= s-pdf 0) (select (send w :prob-btns) 1)))
         (distn (send w :distn))
         (paramnum (send self :paramnum))
         (incr (select (send distn :param-incrs) paramnum))
         (param (select (send distn :param-values) paramnum))
         (pdf (send w :pdf))
         (newparam (send self :incr-param param incr)))
    (pause 3)
    (setf (select (send distn :param-values) paramnum) newparam)
    (send distn :check-params)
    (send w :clear-lines :draw nil)
    (send (select paramtxt paramnum) :redraw)
    (cond 
      ((and (= s-pdf 0) (= pdf 1)) 
       (send w :add-pdf nil)
       (send probtxt :redraw))
      ((and (= s-pdf 0) (= pdf 0)) 
       (send w :add-cdf nil)
       (send (select paramtxt 2) :redraw))
      ((= s-pdf 1)
       (send (select paramtxt 4) :redraw)
       (send w :add-pdf nil)
       (send w :add-sampling-pdf nil)))
    (send w :redraw-content)))

(defmeth param-arr-proto :incr-param (param incr)
  nil)

(defproto ci-param-arr-proto '() () param-btn-proto)

(defmeth ci-param-arr-proto :press-action ()
  (let* ((w (send self :wind))
         (distn (send w :distn))
         (paramnum (send self :paramnum))
         (txt (select (send w :param-txt) paramnum))
         (incr (select (send distn :param-incrs) paramnum))
         (param (select (send distn :param-values) paramnum))
         (newparam (send self :incr-param param incr)))
    (pause 3)
    (setf (select (send distn :param-values) paramnum) newparam)
    (send distn :ci-check-params)
    (send txt :redraw)
    (send distn :add-new-conf-intervals nil)))

(defmeth ci-param-arr-proto :incr-param (param incr)
  nil)

;;
;; Power delta arrow buttons
;;

(defproto power-arr-proto '(add) () param-btn-proto)

(defmeth power-arr-proto :add (&optional add)
  (if add (setf (slot-value 'add) add))
  (slot-value 'add))

(defmeth power-arr-proto :press-action ()
  (let* ((w (send self :wind))
         (paramnum (send self :paramnum))
         (val (select (send w :params) paramnum))
         (incr (select (send w :param-incr) paramnum))
         (newparam (+ val (* incr (send self :add))))
         (txt (select (send w :param-txt) paramnum)))
    (pause 3)
    (send self :update newparam)
    (send txt :redraw)
    (send w :add-distns nil)
    (send w :redraw-content)))

;;
;; Delta power buttons
;;

(defproto delta-proto '() () power-arr-proto)

(defmeth delta-proto :isnew (lft top wind distn)
  (call-next-method lft top wind distn 0))

(defmeth delta-proto :update (new-value)
 (let* ((w (send self :wind))
        (distn (send self :distn)))
  (setf (select (send distn :param-values) 0) new-value)
  (send distn :check-params)
  (setf (select (send w :params) (send self :paramnum)) (select (send distn :param-values) 0))))

(defproto lft-delta-proto '() () delta-proto)

(defmeth lft-delta-proto :isnew (lft top wind distn)
  (send self :hilite-bm lftarrh)
  (send self :unhilite-bm lftarru)
  (send self :add -1)
  (call-next-method lft top wind distn))

(defproto rgt-delta-proto '() () delta-proto)

(defmeth rgt-delta-proto :isnew (lft top wind distn)
  (send self :hilite-bm rtarrh)
  (send self :unhilite-bm rtarru)
  (send self :add 1)
  (call-next-method lft top wind distn))

;;
;; Sigma Power buttons
;;

(defproto sigma-proto '() () power-arr-proto)

(defmeth sigma-proto :isnew (lft top wind distn)
  (call-next-method lft top wind distn 1))

(defmeth sigma-proto :update (new-value)
 (let* ((w (send self :wind))
        (hyp-distn (send w :hyp-distn))
        (true-distn (send w :true-distn))
        (n (select (send w :params) 2)))
  (setf (select (send hyp-distn :param-values) 1) (/ new-value (sqrt n)))
  (setf (select (send true-distn :param-values) 1) (/ new-value (sqrt n)))
  (send hyp-distn :check-params)
  (send true-distn :check-params)
  (setf (select (send w :params) (send self :paramnum)) (* (select (send true-distn :param-values) 1) (sqrt n)))))

(defproto lft-sigma-proto '() () sigma-proto)

(defmeth lft-sigma-proto :isnew (lft top wind distn)
  (send self :hilite-bm lftarrh)
  (send self :unhilite-bm lftarru)
  (send self :add -1)
  (call-next-method lft top wind distn))

(defproto rgt-sigma-proto '() () sigma-proto)

(defmeth rgt-sigma-proto :isnew (lft top wind distn)
  (send self :hilite-bm rtarrh)
  (send self :unhilite-bm rtarru)
  (send self :add 1)
  (call-next-method lft top wind distn))

;;
;; n power buttons
;;

(defproto n-proto '() () power-arr-proto)

(defmeth n-proto :isnew (lft top wind distn)
  (call-next-method lft top wind distn 2))

(defmeth n-proto :update (new-value)
 (let* ((w (send self :wind))
        (hyp-distn (send w :hyp-distn))
        (true-distn (send w :true-distn))
        (hyp-s (select (send hyp-distn :param-values) 1))
        (true-s (select (send true-distn :param-values) 1))
        (s (select (send w :params) 1))
        (n (if (<= new-value 0) 1 (ceiling new-value))))
  (setf (select (send hyp-distn :param-values) 1) (/ s (sqrt n)))
  (setf (select (send true-distn :param-values) 1) (/ s (sqrt n)))
  (send hyp-distn :check-params)
  (send true-distn :check-params)
  (setf (select (send w :params) (send self :paramnum)) (round (^ (/ s (select (send true-distn :param-values) 1)) 2)))))

(defproto lft-n-proto '() () n-proto)

(defmeth lft-n-proto :isnew (lft top wind distn)
  (send self :hilite-bm lftarrh)
  (send self :unhilite-bm lftarru)
  (send self :add -1)
  (call-next-method lft top wind distn))

(defproto rgt-n-proto '() () n-proto)

(defmeth rgt-n-proto :isnew (lft top wind distn)
  (send self :hilite-bm rtarrh)
  (send self :unhilite-bm rtarru)
  (send self :add 1)
  (call-next-method lft top wind distn))

;;
;; alpha power buttons
;;

(defproto alpha-proto '() () power-arr-proto)

(defmeth alpha-proto :isnew (lft top wind distn)
  (call-next-method lft top wind distn 3))

(defmeth alpha-proto :update (new-value)
 (let* ((w (send self :wind))
        (incr (select (send w :param-incr) (send self :paramnum)))
        (true-distn (send w :true-distn))
        (hyp-distn (send w :hyp-distn))
        (nv (if (< new-value 0.0000001) incr new-value))
        (nv (if (> nv .999999) .999999 nv)))
  (setf (select (send w :params) (send self :paramnum)) nv)))

(defproto lft-alpha-proto '() () alpha-proto)

(defmeth lft-alpha-proto :isnew (lft top wind distn)
  (send self :hilite-bm lftarrh)
  (send self :unhilite-bm lftarru)
  (send self :add -1)
  (call-next-method lft top wind distn))

(defproto rgt-alpha-proto '() () alpha-proto)

(defmeth rgt-alpha-proto :isnew (lft top wind distn)
  (send self :hilite-bm rtarrh)
  (send self :unhilite-bm rtarru)
  (send self :add 1)
  (call-next-method lft top wind distn))


;;
;; Left arrow button
;;

(defproto lft-arr-proto '() () param-arr-proto)

(defmeth lft-arr-proto :isnew (lft tp wind distn paramnum)
  (send self :hilite-bm lftarrh)
  (send self :unhilite-bm lftarru)
  (call-next-method lft tp wind distn paramnum))

(defmeth lft-arr-proto :incr-param (param incr)
  (- param incr))

(defproto ci-lft-arr-proto '() () ci-param-arr-proto)

(defmeth ci-lft-arr-proto :isnew (lft tp wind distn paramnum)
  (send self :hilite-bm lftarrh)
  (send self :unhilite-bm lftarru)
  (call-next-method lft tp wind distn paramnum))

(defmeth ci-lft-arr-proto :incr-param (param incr)
  (- param incr))

;;
;; Left first parameter button
;;

(defproto lft-p0-proto '() () lft-arr-proto)

(defmeth lft-p0-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 0))

(defproto ci-lft-p0-proto '() () ci-lft-arr-proto)

(defmeth ci-lft-p0-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 0))

;;
;; Left second parameter button
;;

(defproto lft-p1-proto '() () lft-arr-proto)

(defmeth lft-p1-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 1))

(defproto ci-lft-p1-proto '() () ci-lft-arr-proto)

(defmeth ci-lft-p1-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 1))


;;
;; Right arrow button
;;


(defproto rt-arr-proto '() () param-arr-proto)

(defmeth  rt-arr-proto :isnew (lft tp wind distn paramnum)
  (send self :hilite-bm rtarrh)
  (send self :unhilite-bm rtarru)
  (call-next-method lft tp wind distn paramnum))

(defmeth  rt-arr-proto :incr-param (param incr)
  (+ param incr))

(defproto ci-rt-arr-proto '() () ci-param-arr-proto)

(defmeth  ci-rt-arr-proto :isnew (lft tp wind distn paramnum)
  (send self :hilite-bm rtarrh)
  (send self :unhilite-bm rtarru)
  (call-next-method lft tp wind distn paramnum))

(defmeth  ci-rt-arr-proto :incr-param (param incr)
  (+ param incr))

;;
;; Right first parameter button
;;

(defproto rt-p0-proto '() () rt-arr-proto)

(defmeth rt-p0-proto :isnew (lft tp wind distn)
 (call-next-method lft tp wind distn 0))

(defproto ci-rt-p0-proto '() () ci-rt-arr-proto)

(defmeth ci-rt-p0-proto :isnew (lft tp wind distn)
 (call-next-method lft tp wind distn 0))


;;
;; Right second parameter button
;;

(defproto rt-p1-proto '() () rt-arr-proto)

(defmeth rt-p1-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 1))

(defproto ci-rt-p1-proto '() () ci-rt-arr-proto)

(defmeth ci-rt-p1-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 1))



(defproto param-sym-proto '() () param-btn-proto)

(defmeth param-sym-proto :isnew (lft tp wind distn paramnum)
 (call-next-method lft tp wind distn paramnum)
 (send self :wth 11)) 

(defmeth param-sym-proto :draw-cntl-hilited ()
  (let* ((w (send self :wind))
         (distn (send w :distn))
         (paramnum (send self :paramnum))
         (lft (send self :lft))
         (tp (send self :tp))
         (wth (send self :wth))
         (ht (send self :ht))
         (bm (select (send distn :param-symbols) paramnum))) 
    (send w :draw-mode 'xor)
    (send w :paint-rect lft tp wth ht)
    (send w :draw-mode 'normal)
    (send self :frame-cntl)))

(defmeth param-sym-proto :draw-cntl-unhilited ()
  (let* ((w (send self :wind))
         (distn (send w :distn))
         (paramnum (send self :paramnum))
         (lft (send self :lft))
         (tp (send self :tp))
         (wth (send self :wth))
         (ht (send self :ht))
         (bm (select (send distn :param-symbols) paramnum)))
    (send w :erase-rect lft tp wth ht)
    (send w :draw-bitmap bm (+ lft 2) (+ tp 2))
    (send self :frame-cntl)))

(defmeth param-sym-proto :released-in (x y m1 m2)
  (let* ((w (send self :wind)))
    (send self :show-dialog)
    (call-next-method x y m1 m2)))
  

(defmeth param-sym-proto :no-mod-click (x y m1 m2)
  (let* ((w (send self :wind)))
    (send self :show-dialog)
    (call-next-method x y m1 m2)))

(defmeth param-sym-proto :show-dialog ()
  (let* ((w (send self :wind))
         (distn (send w :distn))
         (probtns (send w :prob-btns))
         (param-num (send self :paramnum))
         (paramtxt (send w :params))
         (param-value (format nil "~,5f" (select (send distn :param-values) param-num)))
         (param-item (send edit-text-item-proto :new param-value :text-length 20 :value 't))
         (param-title (send text-item-proto :new (select (send distn :param-names) param-num)))
         (equal-item (send text-item-proto :new " ="))
         (equal2-item (send text-item-proto :new "="))
         (incr (format nil "~,5f" (select (send distn :param-incrs) param-num)))
         (incr-item (send edit-text-item-proto :new incr :text-length 20 :value nil))
         (incr-title (send text-item-proto :new "Incr"))
         (ok (send modal-button-proto :new "OK" :action
                   #'(lambda ()
                       (list (send param-item :text) (send incr-item :text)))))
         (cancel (send button-item-proto :new "Cancel" :action
                       #'(lambda ()
                           (let ((dialog (send cancel :dialog)))
                             (send dialog :modal-dialog-return nil)))))
         (dlg (send modal-dialog-proto :new (list (list param-title equal-item param-item)
                                                  (list incr-title equal2-item incr-item)
                                                  (list ok cancel)) :default-button ok))
         (pdf (send w :pdf))
         (s-pdf (send w :s-pdf)))
   (setf params (send dlg :modal-dialog))
   (cond (params
         (setf (select (send distn :param-values) param-num) (with-input-from-string (s (select params 0)) (read s)))
         (setf (select (send distn :param-incrs) param-num) (with-input-from-string (s (select params 1)) (read s)))
         (send distn :check-params)
         (send w :clear-lines :draw nil)
         (send (select paramtxt param-num) :redraw)
         (cond 
           ((and (= s-pdf 0) (= pdf 0)) 
            (send (select paramtxt 2) :redraw)
           (send w :add-cdf 't)))
         (cond 
           ((and (= s-pdf 0) (= pdf 1))
            (send (select probtns 1) :redraw)
            (send w :add-pdf 't)))
         (cond 
           ((= s-pdf 1)
            (send (select paramtxt 4) :redraw)
            (send w :add-sampling-pdf 't)))
         (send w :redraw-content)))))

(defproto ci-param-sym-proto '() () param-sym-proto)

(defmeth ci-param-sym-proto :show-dialog ()
  (let* ((w (send self :wind))
         (distn (send w :distn))
         (param-num (send self :paramnum))
         (txt (select (send w :param-txt) param-num))
         (param-value (format nil "~,5f" (select (send distn :param-values) param-num)))
         (param-item (send edit-text-item-proto :new param-value :text-length 20 :value 't))
         (param-title (send text-item-proto :new (select (send distn :param-names) param-num)))
         (equal-item (send text-item-proto :new " ="))
         (equal2-item (send text-item-proto :new "="))
         (incr (format nil "~,5f" (select (send distn :param-incrs) param-num)))
         (incr-item (send edit-text-item-proto :new incr :text-length 20 :value nil))
         (incr-title (send text-item-proto :new "Incr"))
         (ok (send modal-button-proto :new "OK" :action
                   #'(lambda ()
                       (list (send param-item :text) (send incr-item :text)))))
         (cancel (send button-item-proto :new "Cancel" :action
                       #'(lambda ()
                           (let ((dialog (send cancel :dialog)))
                             (send dialog :modal-dialog-return nil)))))
         (dlg (send modal-dialog-proto :new (list (list param-title equal-item param-item)
                                                  (list incr-title equal2-item incr-item)
                                                  (list ok cancel)) :default-button ok)))
   (setf params (send dlg :modal-dialog))
   (cond (params
         (setf (select (send distn :param-values) param-num) (with-input-from-string (s (select params 0)) (read s)))
         (setf (select (send distn :param-incrs) param-num) (with-input-from-string (s (select params 1)) (read s)))
         (send distn :ci-check-params)
         (send txt :redraw)
         (send distn :add-new-conf-intervals T)))))

(defproto power-sym-proto '() () param-sym-proto)

(defmeth power-sym-proto :show-dialog ()
  (let* ((w (send self :wind))
         (hyp-distn (send w :hyp-distn))
         (true-distn (send w :true-distn))
         (param-num (send self :paramnum))
         (param-value (format nil "~,5f" (select (send w :params) param-num)))
         (param-item (send edit-text-item-proto :new param-value :text-length 20 :value 't))
         (param-title (send text-item-proto :new (select (send w :param-names) param-num)))
         (equal-item (send text-item-proto :new " ="))
         (equal2-item (send text-item-proto :new "="))
         (incr (format nil "~,5f" (select (send w :param-incr) param-num)))
         (incr-item (send edit-text-item-proto :new incr :text-length 20 :value nil))
         (incr-title (send text-item-proto :new "Incr"))
         (ok (send modal-button-proto :new "OK" :action
                   #'(lambda ()
                       (list (send param-item :text) (send incr-item :text)))))
         (cancel (send button-item-proto :new "Cancel" :action
                       #'(lambda ()
                           (let ((dialog (send cancel :dialog)))
                             (send dialog :modal-dialog-return nil)))))
         (dlg (send modal-dialog-proto :new (list (list param-title equal-item param-item)
                                                  (list incr-title equal2-item incr-item)
                                                  (list ok cancel)) :default-button ok)))
   (setf params (send dlg :modal-dialog))
   (cond (params
         (send self :update params)
         (send hyp-distn :check-params)
         (send true-distn :check-params) 
         (send w :add-distns T)))))

(defmeth power-sym-proto :draw-cntl-hilited ()
  (let* ((w (send self :wind))
         (paramnum (send self :paramnum))
         (lft (send self :lft))
         (tp (send self :tp))
         (wth (send self :wth))
         (ht (send self :ht))
         (bm (select (send w :param-symbols) paramnum))) 
    (send w :draw-mode 'xor)
    (send w :paint-rect lft tp wth ht)
    (send w :draw-mode 'normal)
    (send self :frame-cntl)))

(defmeth power-sym-proto :draw-cntl-unhilited ()
  (let* ((w (send self :wind))
         (paramnum (send self :paramnum))
         (lft (send self :lft))
         (tp (send self :tp))
         (wth (send self :wth))
         (ht (send self :ht))
         (bm (select (send w :param-symbols) paramnum)))
    (send w :erase-rect lft tp wth ht)
    (send w :draw-bitmap bm (+ lft 2) (+ tp 2))
    (send self :frame-cntl)))

(defmeth power-sym-proto :update (params)
  (let* ((w (send self :wind))
         (param-num (send self :paramnum))
         (txt (select (send w :param-txt) param-num)))
    (send txt :redraw)
    (setf (select (send w :param-incr) param-num) (with-input-from-string (s (select params 1)) (read s)))))

(defproto delta-sym-proto '() () power-sym-proto)

(defmeth delta-sym-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 0))

(defmeth delta-sym-proto :update (params)
  (let* ((w (send self :wind))
         (true-distn (send w :true-distn))
         (new-value (with-input-from-string (s (select params 0)) (read s))))
    (setf (select (send true-distn :param-values) 0) new-value)
    (send true-distn :check-params)
    (setf (select (send w :params) (send self :paramnum)) (select (send true-distn :param-values) 0))
    (call-next-method params)))

(defproto sigma-sym-proto '() () power-sym-proto)

(defmeth sigma-sym-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 1))

(defmeth sigma-sym-proto :update (params)
  (let* ((w (send self :wind))
         (true-distn (send w :true-distn))
         (hyp-distn (send w :hyp-distn))
         (new-value (with-input-from-string (s (select params 0)) (read s)))
         (n (select (send w :params) 2)))
    (setf (select (send hyp-distn :param-values) 1) (/ new-value (sqrt n)))
    (setf (select (send true-distn :param-values) 1) (/ new-value (sqrt n)))
    (send hyp-distn :check-params)
    (send true-distn :check-params)
    (setf (select (send w :params) (send self :paramnum)) (* (select (send true-distn :param-values) 1) (sqrt n)))
    (call-next-method params)))

(defproto n-sym-proto '() () power-sym-proto)

(defmeth n-sym-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 2))

(defmeth n-sym-proto :update (params)
  (let* ((w (send self :wind))
         (true-distn (send w :true-distn))
         (hyp-distn (send w :hyp-distn))
         (new-value (with-input-from-string (s (select params 0)) (read s)))
         (n new-value)
         (hyp-s (select (send hyp-distn :param-values) 1))
         (true-s (select (send true-distn :param-values) 1))
         (s (select (send w :params) 1))
         (n (if (<= n 0) 1 (ceiling n))))
  (setf (select (send hyp-distn :param-values) 1) (/ s (sqrt n)))
  (setf (select (send true-distn :param-values) 1) (/ s (sqrt n)))
  (send hyp-distn :check-params)
  (send true-distn :check-params)
  (setf (select (send w :params) (send self :paramnum)) (round (^ (/ s (select (send true-distn :param-values) 1)) 2)))
  (call-next-method params)))

(defproto alpha-sym-proto '() () power-sym-proto)

(defmeth alpha-sym-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 3))

(defmeth alpha-sym-proto :update (params)
  (let* ((w (send self :wind))
        (true-distn (send w :true-distn))
        (hyp-distn (send w :hyp-distn))
        (new-value (with-input-from-string (s (select params 0)) (read s)))
        (nv (if (< new-value 0.0000001) .0000001 new-value))
        (nv (if (> nv .999999) .999999 nv)))
  (setf (select (send w :params) (send self :paramnum)) nv)
  (send true-distn :x1 (funcall (send hyp-distn :quant) (- 1 nv)))
  (call-next-method params)))
  

;;

(defproto param0-sym-proto '() () param-sym-proto)

(defmeth param0-sym-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 0))

(defproto ci-param0-sym-proto '() () ci-param-sym-proto)

(defmeth ci-param0-sym-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 0))

;;

(defproto param1-sym-proto '() () param-sym-proto)

(defmeth param1-sym-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 1))

(defproto ci-param1-sym-proto '() () ci-param-sym-proto)

(defmeth ci-param1-sym-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 1))

;; change the quantile

(defproto lft-quant-proto '() () lft-arr-proto)

(defmeth lft-quant-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 2))

(defproto rt-quant-proto '() () rt-arr-proto)

(defmeth rt-quant-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 2))

(defproto quant-sym-proto '() () param-sym-proto)

(defmeth quant-sym-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 2))

;; change the sample size

(defproto lft-samp-proto '() () lft-arr-proto)

(defmeth lft-samp-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 3))

(defproto rt-samp-proto '() () rt-arr-proto)

(defmeth rt-samp-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 3))

(defproto ci-lft-samp-proto '() () ci-lft-arr-proto)

(defmeth ci-lft-samp-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 3))

(defproto ci-rt-samp-proto '() () ci-rt-arr-proto)

(defmeth ci-rt-samp-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 3))

;; sample size symbol

(defproto sample-sym-proto '() () param-sym-proto)

(defmeth sample-sym-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 3))

(defproto ci-sample-sym-proto '() () ci-param-sym-proto)

(defmeth ci-sample-sym-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 3))

;;
;; alpha arrows
;;

(defproto ci-lft-alpha-proto '() () ci-lft-arr-proto)

(defmeth ci-lft-alpha-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 4))

(defproto ci-rt-alpha-proto '() () ci-rt-arr-proto)

(defmeth ci-rt-alpha-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 4))

(defproto ci-alpha-sym-proto '() () ci-param-sym-proto)

(defmeth ci-alpha-sym-proto :isnew (lft tp wind distn)
  (call-next-method lft tp wind distn 4))
 











