(require ":StatIns:cntls.lsp")
(provide ":StatIns:textbtn.lsp")

;;
;; Button for entering text
;;

(defproto param-btn-proto '(param-num) () cntl-proto)

(defmeth param-btn-proto :param-num (&optional param-num)
  (if param-num (setf (slot-value 'param-num) param-num))
  (slot-value 'param-num))

(defmeth param-btn-proto :isnew (lft tp ht wth title parts wind value)
  (call-next-method lft tp ht wth title parts wind))

(defmeth param-btn-proto :draw-value ()
  (let* ((l (send self :lft))
         (tp (send self :tp))
         (w (send self :wind))
         (val (format nil "~,4f" (send self :get-value))))
    (send w :draw-string val (+ l 9 2) (+ tp 9))))

(defmeth param-btn-proto :frame-cntl ()
  (let* ((l (send self :lft))
         (wth (send self :wth))
         (r (+ l wth))
         (tp (send self :tp))
         (ht (send self :ht))
         (b (+ tp ht))
         (w (send self :wind)))
    (send w :frame-rect (+ l 9) tp (- r (+ l 7)) (- b tp))))

(defmeth param-btn-proto :draw-symbol ()
  (let* ((l (send self :lft))
         (wth (send self :wth))
         (r (+ l wth))
         (tp (send self :tp))
         (ht (send self :ht))
         (b (+ tp ht))
         (w (send self :wind))
         (bm (send self :get-symbol)))
    (if bm (send w :draw-bitmap bm l (+ tp 2)))))

#|(defmeth param-btn-proto :draw-cntl-hilited ()
  (let* ((l (send self :lft))
         (wth (send self :wth))
         (r (+ l wth))
         (tp (send self :tp))
         (ht (send self :ht))
         (b (+ tp ht))
         (w (send self :wind)))
    (send w :draw-color 'black)
    (send self :draw-symbol)
    (send self :frame-cntl)
    (send w :paint-rect (+ l 9 1) (+ tp 1) (- r (+ l 9)) (- (- b tp) 2))
    (send w :draw-color 'white)
    (send self :draw-value)
    (send w :draw-color 'black)))#|

(defmeth param-btn-proto :draw-cntl-hilited ()
  nil)


  
|#(defmeth param-btn-proto :draw-cntl-unhilited ()
  (let* ((l (send self :l))
         (r (send self :r))
         (tp (send self :tp))
         (b (send self :b))
         (w (send self :wind)))
    (send self :draw-symbol)
    (send w :draw-color 'black)
    (send self :frame-cntl)
    (send w :draw-color 'white)
    (send w :paint-rect (+ l 9 1) (+ tp 1) (- r (+ l 9)) (- (- b tp) 2))
    (send w :draw-color 'black)
    (send self :draw-value)))|#

(defmeth param-btn-proto :draw-cntl-unhilited ()
  (call-next-method))

(defmeth param-btn-proto :do-click (x y m1 m2)
  (call-next-method x y m1 m2)
  (send self :hilite 0)
  (send self :redraw))

(defmeth param-btn-proto :redraw ()
  (call-next-method))

(defmeth param-btn-proto :get-value ()
  (let* ((w (send self :wind))
         (distn (send w :distn))
         (param-num (send self :param-num))
         (value (select (send distn :param-values) param-num)))
    value))

(defmeth param-btn-proto :get-symbol ()
  (let* ((w (send self :wind))
         (distn (send w :distn))
         (param-num (send self :param-num))
         (symbol (select (send distn :param-symbols) param-num)))
    symbol))

(defmeth param-btn-proto :no-mod-click (x y m1 m2)
  (let* ((w (send self :wind)))
    (send self :show-dialog)))

(defmeth param-btn-proto :show-dialog ()
  (let* ((w (send self :wind))
         (distn (send w :distn))
         (param-num (send self :param-num))
         (param-value (format nil "~,5f" (select (send distn :param-values) param-num)))
         (param-item (send edit-text-item-proto :new param-value :text-length 20))
         (param-title (send text-item-proto :new (select (send distn :param-names) param-num)))
         (equal-item (send text-item-proto :new " ="))
         (equal2-item (send text-item-proto :new "="))
         (incr (format nil "~,5f" (select (send distn :param-incrs) param-num)))
         (incr-item (send edit-text-item-proto :new incr :text-length 20))
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
         (pdf-btn (select (send w :cntls) 0))
         (pdf (send pdf-btn :hilite)))
   (setf params (send dlg :modal-dialog))
   (cond (params
         (setf (select (send distn :param-values) param-num) (with-input-from-string (s (select params 0)) (read s)))
         (setf (select (send distn :param-incrs) param-num) (with-input-from-string (s (select params 1)) (read s)))
         (send w :clear-lines :draw nil)
         (if (= pdf 1) (send w :add-pdf nil) (send w :add-cdf nil))
         (send w :redraw)))))



;;
;; Proto for changing param 0 values
;;

(defproto param0-btn-proto '() () param-btn-proto)

(defmeth param0-btn-proto :isnew (l r tp b title parts wind value)
  (send self :param-num 0)
  (call-next-method l r tp b title parts wind value))

;;
;; Proto for changing param 0 values
;;

(defproto param1-btn-proto '() () param-btn-proto)

(defmeth param1-btn-proto :isnew (l r tp b title parts wind value)
  (send self :param-num 1)
  (call-next-method l r tp b title parts wind value))

(defmeth param1-btn-proto :redraw ()
  (let* ((w (send self :wind))
         (distn (send w :distn))
         (l (send self :l))
         (r (send self :r))
         (b (send self :b))
         (tp (send self :tp))
         (param-value1 (select (send distn :param-values) 1)))
    (if param-value1 (call-next-method) (send w :erase-rect l tp (- r l) (- b tp)))))

;;
;; Button for changing changing probability bounds
;;

(defproto prob-bounds-btn-proto '() () param-btn-proto)

(defmeth prob-bounds-btn-proto :show-dialog ()
  (let* ((w (send self :wind))
         (distn (send w :distn))
         (pdf (send w :pdf))
         (ineq (send distn :ineq))
         (x1 (format nil "~,5f" (send distn :x1)))
         (x1-item (send edit-text-item-proto :new x1 :text-length 4))
         (x2 (format nil "~,5f" (send distn :x2)))
         (x2-item (send edit-text-item-proto :new x2 :text-length 4))
         (ineq-t1 nil) 
         (ineq-t2 nil)
         (ineq-t3 nil)
         (prob-item-list nil)
         (prob-item (cond 
                      ((= ineq 0)
                       (setf ineq-t1 (send text-item-proto :new "P( x ="))
                       (setf ineq-t2 (send text-item-proto :new " )"))
                       (setf prob-item-list (list ineq-t1 x1-item ineq-t2)))
                      ((= ineq 1)
                       (setf ineq-t1 (send text-item-proto :new "P( x �"))
                       (setf ineq-t2 (send text-item-proto :new " )"))
                       (setf prob-item-list (list ineq-t1 x1-item ineq-t2)))
                      ((= ineq 2)
                       (setf ineq-t1 (send text-item-proto :new "P( x <"))
                       (setf ineq-t2 (send text-item-proto :new " )"))
                       (setf prob-item-list (list ineq-t1 x1-item ineq-t2)))
                      ((= ineq 3)
                       (setf ineq-t1 (send text-item-proto :new "P( x �"))
                       (setf ineq-t2 (send text-item-proto :new " )"))
                       (setf prob-item-list (list ineq-t1 x1-item ineq-t2)))
                      ((= ineq 4)
                       (setf ineq-t1 (send text-item-proto :new "( x >"))
                       (setf ineq-t2 (send text-item-proto :new " )"))
                       (setf prob-item-list (list ineq-t1 x1-item ineq-t2)))
                      ((= ineq 5)
                       (setf ineq-t1 (send text-item-proto :new "P("))
                       (setf ineq-t2 (send text-item-proto :new "� x �"))
                       (setf ineq-t3 (send text-item-proto :new ")"))
                       (setf prob-item-list (list ineq-t1 x1-item ineq-t2 x2-item ineq-t3)))
                      ((= ineq 6)
                       (setf ineq-t1 (send text-item-proto :new "P("))
                       (setf ineq-t2 (send text-item-proto :new "< x �"))
                       (setf ineq-t3 (send text-item-proto :new ")"))
                       (setf prob-item-list (list ineq-t1 x1-item ineq-t2 x2-item ineq-t3)))
                      ((= ineq 7)
                       (setf ineq-t1 (send text-item-proto :new "P("))
                       (setf ineq-t2 (send text-item-proto :new "� x <"))
                       (setf ineq-t3 (send text-item-proto :new ")"))
                       (setf prob-item-list (list ineq-t1 x1-item ineq-t2 x2-item ineq-t3)))
                      ((= ineq 8)
                       (setf ineq-t1 (send text-item-proto :new "P("))
                       (setf ineq-t2 (send text-item-proto :new "< x <"))
                       (setf ineq-t3 (send text-item-proto :new ")"))
                       (setf prob-item-list (list ineq-t1 x1-item ineq-t2 x2-item ineq-t3)))
                      ((= ineq 9)
                       (setf ineq-t1 (send text-item-proto :new "P( x �"))
                       (setf ineq-t2 (send text-item-proto :new "or"))
                       (setf ineq-t3 (send text-item-proto :new "� x )"))
                       (setf prob-item-list (list ineq-t1 x1-item ineq-t2 x2-item ineq-t3)))
                      ((= ineq 10)
                       (setf ineq-t1 (send text-item-proto :new "P( x <"))
                       (setf ineq-t2 (send text-item-proto :new "or"))
                       (setf ineq-t3 (send text-item-proto :new "� x )"))
                       (setf prob-item-list (list ineq-t1 x1-item ineq-t2 x2-item ineq-t3)))
                      ((= ineq 11)
                       (setf ineq-t1 (send text-item-proto :new "P( x �"))
                       (setf ineq-t2 (send text-item-proto :new "or"))
                       (setf ineq-t3 (send text-item-proto :new "< x )"))
                       (setf prob-item-list (list ineq-t1 x1-item ineq-t2 x2-item ineq-t3)))
                      ((= ineq 12)
                       (setf ineq-t1 (send text-item-proto :new "P( x <"))
                       (setf ineq-t2 (send text-item-proto :new "or"))
                       (setf ineq-t3 (send text-item-proto :new "< x )"))
                       (setf prob-item-list (list ineq-t1 x1-item ineq-t2 x2-item ineq-t3)))))
         (ok (send modal-button-proto :new "OK" :action
                   #'(lambda ()
                       (let* ((x1-txt (send x1-item :text))
                              (x2-txt (if (> ineq 4) 
                                          (send x2-item :text)
                                          (format nil "~,5f" (send distn :x2)))))
                         (list x1-txt x2-txt)))))
                            
         (cancel (send button-item-proto :new "Cancel" :action
                       #'(lambda ()
                           (let ((dialog (send cancel :dialog)))
                             (send dialog :modal-dialog-return nil)))))
         (dlg (send modal-dialog-proto :new (list prob-item-list
                                                  (list ok cancel)) :default-button ok)))
    (setf xs (send dlg :modal-dialog))
    (cond (xs
           (send distn :x1 (with-input-from-string (s (select xs 0)) (read s)))
           (send distn :x2 (with-input-from-string (s (select xs 1)) (read s)))
           (if (= pdf 1) 
               (send w :add-pdf 't) 
               (send w :add-cdf 't))
           (send w :redraw)))))

(defmeth prob-bounds-btn-proto :draw-value ()
  nil)

(defmeth prob-bounds-btn-proto :frame-cntl ()
  nil)

#|(defmeth prob-bounds-btn-proto :redraw ()
  (let* ((l (send self :lft))
         (wth (send self :wth))
         (r (+ l wth))
         (tp (send self :tp))
         (ht (send self :ht))
         (b (+ tp ht))
         (w (send self :wind))
         (distn (send w :distn))
         (ineq (send distn :ineq))
         (x1 (send distn :x1))
         (x2 (send distn :x2))
         (prob (send distn :calc-prob))
         (prob-text (cond 
                      ((= ineq 0)
                       (format nil "(x = ~,2f) = ~,4f" x1 prob)) 
                      ((= ineq 1)
                       (format nil "(x � ~,2f) = ~,4f" x1 prob)) 
                      ((= ineq 2)
                       (format nil "(x < ~,2f) = ~,4f" x1 prob)) 
                      ((= ineq 3)
                       (format nil "(x � ~,2f) = ~,4f" x1 prob)) 
                      ((= ineq 4)
                       (format nil "(x > ~,2f) = ~,4f" x1 prob))
                      ((= ineq 5)
                       (format nil "(~,2f � x � ~,2f) = ~,4f" x1 x2 prob))
                      ((= ineq 6)
                       (format nil "(~,2f < x � ~,2f) = ~,4f" x1 x2 prob))
                      ((= ineq 7)
                       (format nil "(~,2f � x < ~,2f) = ~,4f" x1 x2 prob))
                      ((= ineq 8)
                       (format nil "(~,2f < x < ~,2f) = ~,4f" x1 x2 prob))
                      ((= ineq 9)
                       (format nil "(x � ~,2f or ~,2f � x) = ~,4f" x1 x2 prob))
                      ((= ineq 10)
                       (format nil "(x < ~,2f or ~,2f � x) = ~,4f" x1 x2 prob))
                      ((= ineq 11)
                       (format nil "(x � ~,2f or ~,2f < x) = ~,4f" x1 x2 prob))
                      ((= ineq 12)
                       (format nil "(x < ~,2f or ~,2f < x) = ~,4f" x1 x2 prob)))))
    (send w :draw-color 'RED)
    (send w :draw-string prob-text l (+ tp 9))
    (send w :draw-color 'BLACK)))|#

(defmeth prob-bounds-btn-proto :redraw ()
  (let* ((l (send self :lft))
         (wth (send self :wth))
         (r (+ l wth))
         (tp (send self :tp))
         (ht (send self :ht))
         (b (+ tp ht))
         (w (send self :wind))
         (distn (send w :distn))
         (ineq (send distn :ineq))
         (x1 (send distn :x1))
         (x2 (send distn :x2))
         (prob (send distn :calc-prob))
         (old-color (send w :draw-color))
         (prob-value (format nil "~,4f" prob))
         (x1-txt (format nil "~,2f" x1))
         (x2-txt (format nil "~,2f" x2))
         (tt (+ tp 9))
         (tw l))
    (send w :erase-rect l tp 200 11)
    (cond 
      ((= ineq 0)
       (send w :draw-string "(x = " l tt)
       (setf tw (+ tw (send w :text-width "(x = ")))
       (send w :draw-color 'RED)
       (send w :draw-string x1-txt tw tt)
       (setf tw (+ tw (send w :text-width x1-txt))))
      ((= ineq 1)
       (send w :draw-string "(x � " l tt)
       (setf tw (+ tw (send w :text-width "(x = ")))
       (send w :draw-color 'RED)
       (send w :draw-string x1-txt tw tt)
       (setf tw (+ tw (send w :text-width x1-txt))))
      ((= ineq 2)
       (send w :draw-string "(x < " l tt)
       (setf tw (+ tw (send w :text-width "(x = ")))
       (send w :draw-color 'RED)
       (send w :draw-string x1-txt tw tt)
       (setf tw (+ tw (send w :text-width x1-txt))))
      ((= ineq 3)
       (send w :draw-string "(x � " l tt)
       (setf tw (+ tw (send w :text-width "(x = ")))
       (send w :draw-color 'RED)
       (send w :draw-string x1-txt tw tt)
       (setf tw (+ tw (send w :text-width x1-txt))))
      ((= ineq 4)
       (send w :draw-string "(x > " l tt)
       (setf tw (+ tw (send w :text-width "(x = ")))
       (send w :draw-color 'RED)
       (send w :draw-string x1-txt tw tt)
       (setf tw (+ tw (send w :text-width x1-txt))))
      ((= ineq 5)
       (send w :draw-string "(" tw tt)
       (setf tw (+ tw (send w :text-width "(")))
       (send w :draw-color 'RED)
       (send w :draw-string x1-txt tw tt)
       (setf tw (+ tw (send w :text-width x1-txt)))
       (send w :draw-color old-color)
       (send w :draw-string " � x � " tw tt)
       (send w :draw-color 'RED)
       (setf tw (+ tw (send w :text-width " � x � ")))
       (send w :draw-string x2-txt tw tt)
       (setf tw (+ tw (send w :text-width x2-txt)))) 
      ((= ineq 6)
       (send w :draw-string "(" tw tt)
       (setf tw (+ tw (send w :text-width "(")))
       (send w :draw-color 'RED)
       (send w :draw-string x1-txt tw tt)
       (setf tw (+ tw (send w :text-width x1-txt)))
       (send w :draw-color old-color)
       (send w :draw-string " < x � " tw tt)
       (send w :draw-color 'RED)
       (setf tw (+ tw (send w :text-width " < x � ")))
       (send w :draw-string x2-txt tw tt)
       (setf tw (+ tw (send w :text-width x2-txt))))
      ((= ineq 7)
       (send w :draw-string "(" tw tt)
       (setf tw (+ tw (send w :text-width "(")))
       (send w :draw-color 'RED)
       (send w :draw-string x1-txt tw tt)
       (setf tw (+ tw (send w :text-width x1-txt)))
       (send w :draw-color old-color)
       (send w :draw-string " � x < " tw tt)
       (send w :draw-color 'RED)
       (setf tw (+ tw (send w :text-width " � x < ")))
       (send w :draw-string x2-txt tw tt)
       (setf tw (+ tw (send w :text-width x2-txt))))
      ((= ineq 8)
       (send w :draw-string "(" tw tt)
       (setf tw (+ tw (send w :text-width "(")))
       (send w :draw-color 'RED)
       (send w :draw-string x1-txt tw tt)
       (setf tw (+ tw (send w :text-width x1-txt)))
       (send w :draw-color old-color)
       (send w :draw-string " < x < " tw tt)
       (send w :draw-color 'RED)
       (setf tw (+ tw (send w :text-width " < x < ")))
       (send w :draw-string x2-txt tw tt)
       (setf tw (+ tw (send w :text-width x2-txt))))
      ((= ineq 9)
       (send w :draw-string "(x � " tw tt)
       (setf tw (+ tw (send w :text-width "(x � ")))
       (send w :draw-color 'RED)
       (send w :draw-string x1-txt tw tt)
       (setf tw (+ tw (send w :text-width x1-txt)))
       (send w :draw-color old-color)
       (send w :draw-string " or x � " tw tt)
       (send w :draw-color 'RED)
       (setf tw (+ tw (send w :text-width " or x � ")))
       (send w :draw-string x2-txt tw tt)
       (setf tw (+ tw (send w :text-width x2-txt))))
      ((= ineq 10)
       (send w :draw-string "(x < " tw tt)
       (setf tw (+ tw (send w :text-width "(x < ")))
       (send w :draw-color 'RED)
       (send w :draw-string x1-txt tw tt)
       (setf tw (+ tw (send w :text-width x1-txt)))
       (send w :draw-color old-color)
       (send w :draw-string " or x � " tw tt)
       (send w :draw-color 'RED)
       (setf tw (+ tw (send w :text-width " or x � ")))
       (send w :draw-string x2-txt tw tt)
       (setf tw (+ tw (send w :text-width x2-txt))))
      ((= ineq 11)
       (send w :draw-string "(x � " tw tt)
       (setf tw (+ tw (send w :text-width "(x � ")))
       (send w :draw-color 'RED)
       (send w :draw-string x1-txt tw tt)
       (setf tw (+ tw (send w :text-width x1-txt)))
       (send w :draw-color old-color)
       (send w :draw-string " or x > " tw tt)
       (send w :draw-color 'RED)
       (setf tw (+ tw (send w :text-width " or x > ")))
       (send w :draw-string x2-txt tw tt)
       (setf tw (+ tw (send w :text-width x2-txt))))
      ((= ineq 12)
       (send w :draw-string "(x < " tw tt)
       (setf tw (+ tw (send w :text-width "(x < ")))
       (send w :draw-color 'RED)
       (send w :draw-string x1-txt tw tt)
       (setf tw (+ tw (send w :text-width x1-txt)))
       (send w :draw-color old-color)
       (send w :draw-string " or x > " tw tt)
       (send w :draw-color 'RED)
       (setf tw (+ tw (send w :text-width " or x > ")))
       (send w :draw-string x2-txt tw tt)
       (setf tw (+ tw (send w :text-width x2-txt)))))
    (send w :draw-color old-color)
    (send w :draw-string ") = " tw (+ tp 9))
    (setf tw (+ tw (send w :text-width ") = ")))
    (send w :draw-color 'RED)
    (send w :draw-string prob-value tw (+ tp 9))
    (send w :draw-color old-color)       
    (call-next-method)))
         
                  
            