(provide ":StatIns:radbtn.lsp")
(require ":StatIns:cntls.lsp")


;;
;; Radio button proto
;;

(defproto radio-btn-proto '(item mgr) () cntl-proto)

(defmeth radio-btn-proto :isnew (lft tp wth ht title wind mgr item)
  (send self :item item)
  (send self :mgr mgr)
  (call-next-method lft tp wth ht title 1 wind))

(defmeth radio-btn-proto :item (&optional item)
  (if item (setf (slot-value 'item) item))
  (slot-value 'item))

(defmeth radio-btn-proto :mgr (&optional mgr)
  (if mgr (setf (slot-value 'mgr) mgr))
  (slot-value 'mgr))

(defmeth radio-btn-proto :draw-circle ()
  (let* ((l (send self :lft))
         (tp (send self :tp))
         (w (send self :wind)))
    (send w :frame-oval l tp 10 10)))

(defmeth radio-btn-proto :draw-title ()
  (let* ((l (send self :lft))
         (tp (send self :tp))
         (w (send self :wind))
         (title (send self :title)))
    (send w :draw-string title (+ l 12) (+ tp 9))))

(defmeth radio-btn-proto :fill-circle ()
  (let* ((l (send self :lft))
         (tp (send self :tp))
         (w (send self :wind))
         (old-color (send w :draw-color)))
    (send w :draw-color 'BLUE)
    (send w :paint-oval (+ l 2) (+ tp 2) 6 6)
    (send w :draw-color old-color)))

(defmeth radio-btn-proto :erase-circle ()
  (let* ((l (send self :lft))
         (tp (send self :tp))
         (w (send self :wind)))
    (send w :erase-oval (+ l 2) (+ tp 2) 6 6)))
  
(defmeth radio-btn-proto :redraw ()
  (send self :draw-title)
  (send self :draw-circle)
  (call-next-method))

(defmeth radio-btn-proto :draw-cntl-hilited ()
  (send self :fill-circle))
  
(defmeth radio-btn-proto :draw-cntl-unhilited ()
  (send self :erase-circle))

(defmeth radio-btn-proto :no-mod-click (x y m1 m2)
 (send (send self :mgr) :change-item (send self :item)))

(defmeth radio-btn-proto :mod1-click (x y m1 m2)
 (send (send self :mgr) :change-item (send self :item)))

(defmeth radio-btn-proto :mod2-click (x y m1 m2)
 (send (send self :mgr) :change-item (send self :item)))

(defmeth radio-btn-proto :mod12-click (x y m1 m2)
 (send (send self :mgr) :change-item (send self :item)))

;;
;; Stat window PDF radio button
;;

(defproto pdf-radio-btn-proto () () radio-btn-proto)

(defmeth pdf-radio-btn-proto :no-mod-click (x y m1 m2)
  (let* ((w (send self :wind))
         (distn (send w :distn))
         (name (send distn :name))
         (pdf (send w :pdf)))
    (send self :redraw)
    (send w :clear-lines :draw nil)
    (send w :pdf 1)
    (send w :add-pdf 't)
    (send w :title (format nil "~a f(x)" name))
    (send w :redraw-content)
    (call-next-method x y m1 m2)))


;;
;; Stat window CDF radio button
;;

(defproto cdf-radio-btn-proto () () radio-btn-proto)

(defmeth cdf-radio-btn-proto :no-mod-click (x y m1 m2)
  (let* ((w (send self :wind))
         (distn (send w :distn))
         (name (send distn :name)))
    (send self :redraw)
    (send w :clear-lines :draw nil)
    (send w :pdf 0)
    (send w :add-cdf 't)
    (send w :title (format nil "~a F(x)" name))
    (send w :redraw-content)
    (call-next-method x y m1 m2)))


;;
;; Radio button manager
;;

(defproto radio-mgr-proto '(btns item) () nil)

(defmeth radio-mgr-proto :btns (&optional btns)
  (if btns (setf (slot-value 'btns) btns))
  (slot-value 'btns))

(defmeth radio-mgr-proto :item (&optional item)
  (if item (setf (slot-value 'item) item))
  (slot-value 'item))

(defmeth radio-mgr-proto :change-item (item)
  (send (select (send self :btns) (send self :item)) :hilite 0)
  (send (select (send self :btns) (send self :item)) :redraw)
  (send (select (send self :btns) item) :hilite 1)
  (send (select (send self :btns) item) :redraw)
  (send self :item item))

(defun make-distn-radio-buttons (lft tp wind)
  (let* ((mgr (send radio-mgr-proto :new))
         (pdf-btn (send pdf-radio-btn-proto :new lft tp (+ 12 (send wind :text-width "f(x)")) 12 "f(x)" wind mgr 0))
         (cdf-btn (send cdf-radio-btn-proto :new lft (+ tp 15) (+ 12 (send wind :text-width "F(x)")) 12 "F(x)" wind mgr 1)))
    (send mgr :btns (list pdf-btn cdf-btn))
    (send mgr :item 0)
    (send pdf-btn :hilite 1)
    (send wind :add-overlay pdf-btn)
    (send wind :add-overlay cdf-btn)))

         




