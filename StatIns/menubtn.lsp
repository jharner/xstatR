(provide ":StatIns:menubtn.lsp")
(require ":StatIns:cntls.lsp")

;;
;; Menu button proto
;;

(defproto menu-btn-proto '(menu arrow checked-item) () cntl-proto)

(defmeth menu-btn-proto :menu (&optional menu)
  (if menu (setf (slot-value 'menu) menu))
  (slot-value 'menu))

(defmeth menu-btn-proto :checked-item (&optional checked-item)
  (if checked-item (setf (slot-value 'checked-item) checked-item))
  (slot-value 'checked-item))

(defmeth menu-btn-proto :arrow (&optional arrow)
  (if arrow (setf (slot-value 'arrow) arrow))
  (slot-value 'arrow))

(defmeth menu-btn-proto :isnew (lft tp wth ht title part wind menu checked-item)
  (send self :menu menu)
  (send self :arrow dwnarr)
  (send self :checked-item checked-item)
  (call-next-method lft tp wth ht title part wind))

(defmeth menu-btn-proto :draw-title ()
  (let* ((lft (send self :lft))
         (tp (send self :tp))
         (w (send self :wind))
         (title (send self :title)))
    (send w :draw-string title (+ lft 12) (+ tp 9))))

(defmeth menu-btn-proto :draw-arrow ()
  (let* ((lft (send self :lft))
         (tp (send self :tp))
         (w (send self :wind))
         (arrow (send self :arrow)))
    (send w :draw-bitmap arrow (+ lft 2) (+ tp 4))))

(defmeth menu-btn-proto :draw-shadow ()
  (let* ((lft (send self :lft))
         (tp (send self :tp))
         (ht (send self :ht))
         (wth (send self :wth))
         (w (send self :wind))
         (arrow (send self :arrow)))
    (send w :draw-line (+ lft 1) (+ tp ht) (+ lft wth) (+ tp ht)) 
    (send w :draw-line (+ lft wth) (+ tp 1)  (+ lft wth) (+ tp ht))))

(defmeth menu-btn-proto :redraw ()
  (let* ((w (send self :wind))
         (lft (send self :lft))
         (wth (send self :wth))
         (tp (send self :tp))
         (ht (send self :ht)))
    (send w :erase-rect lft tp wth ht)
    (send self :draw-arrow)
    (send self :draw-title)
    (send self :frame-cntl)
    (send self :draw-shadow)
    (call-next-method)))

(defmeth menu-btn-proto :do-click (x y m1 m2)
  (let* ((lft (send self :lft))
         (tp (send self :tp))
         (w (send self :wind))
         (handle nil))
    (cond ((= (send self :in-cntl x y) 1) 
           (send (send self :menu) :popup lft tp w)
           (setf handle 't)))
    handle))

;;
;; Distribution menu button proto
;;

(defproto distn-menu-btn-proto () () menu-btn-proto)

(defmeth distn-menu-btn-proto :isnew (lft tp wth ht title part wind checked-item)
  (let* ((distn-list (send wind :distn-list))
         (item nil)
         (vari nil)
         (i 0))
    (send self :menu (send menu-proto :new "Menu"))
    (dolist (distn distn-list item)
            (let* ((name (send distn :name))
                   (mdistn distn)
                   (tmenu self))
              (setf i (+ i 1))
              (send (send self :menu) :append-items (send menu-item-proto :new name 
                                                          :action #'(lambda () 
                                                                      (let* ((pdf (send wind :pdf))
                                                                             (s-pdf (send wind :s-pdf))
                                                                             (name (send mdistn :name)))
                                                                         
                                                                        (send wind :distn mdistn)
                                                                        (send wind :clear-lines)
                                                                        (cond 
                                                                          ((and (= s-pdf 0) (= pdf 1)) 
                                                                           (send wind :add-pdf 't)
                                                                           (send wind :title (format nil "~a f(x)" name)))
                                                                          ((and (= s-pdf 0) (= pdf 0))
                                                                            (send wind :add-cdf 't)
                                                                            (send wind :title (format nil "~a F(x)" name))))
                                                                        (cond
                                                                          ((= s-pdf 1)
                                                                           (send wind :add-sampling-pdf 't)
                                                                           (send wind :title (format nil "~a f(x)" name))))
                                                                        (send self :redraw)
                                                                        (send wind :redraw-content)))))))
    ;;(send wind :menu (send self :menu))
    ;;(setf a-menu (send self :menu))
    ;;(setf a-items (send a-menu :items))
    ;;(send (select a-items checked-item) :mark t)
    (call-next-method lft tp wth ht "Oops!" part wind (send self :menu) checked-item)))


(defmeth distn-menu-btn-proto :draw-title ()
  (let* ((w (send self :wind))
         (distn (send w :distn))
         (title (send distn :name)))
    (send self :title title)
    (call-next-method)))

;;
;; Menu button for CLT
;;

(defproto samp-menu-btn-proto () () distn-menu-btn-proto)

(defmeth samp-menu-btn-proto :isnew (lft tp wth ht title part wind checked-item)
  (let* ((dash (send dash-item-proto :new))
         (sample (send menu-item-proto :new "Sampling Distn"))
         (parent (send menu-item-proto :new "Parent"))
         (hist (send menu-item-proto :new "Histogram"))
         (kern (send menu-item-proto :new "Smoother")))
    (call-next-method lft tp wth ht title part wind checked-item)
    (send sample :action #'(lambda()
                             (if (send sample :mark) (send sample :mark nil) (send sample :mark 't))
                             (if (send sample :mark) (send wind :samp 1) (send wind :samp 0))
                             (send wind :add-sampling-pdf 't)))     
    (send parent :action #'(lambda()
                             (if (send parent :mark) (send parent :mark nil) (send parent :mark 't))
                             (if (send parent :mark) (send wind :pdf 1) (send wind :pdf 0))
                             (send wind :add-sampling-pdf 't)))
    (send hist :action #'(lambda()
                             (if (send hist :mark) (send hist :mark nil) (send hist :mark 't))
                             (if (send hist :mark) (send wind :hist 1) (send wind :hist 0))
                             (send wind :add-sampling-pdf 't)))
    (send kern :action #'(lambda()
                             (if (send kern :mark) (send kern :mark nil) (send kern :mark 't))
                             (if (send kern :mark) (send wind :kern 1) (send wind :kern 0))
                             (send wind :add-sampling-pdf 't)))
    (send sample :mark 't)
    (send (send self :menu) :append-items dash sample parent hist kern)))
                       
;;
;; Menu button for Power
;;

(defproto power-menu-btn-proto () () menu-btn-proto)

(defmeth power-menu-btn-proto :isnew (lft tp wth ht title part wind checked-item)
  (let* ((up (send menu-item-proto :new "Upper"))
         (low (send menu-item-proto :new "Lower"))
         (two (send menu-item-proto :new "Two")))
    (send self :menu (send menu-proto :new "Menu"))
    (send up :action #'(lambda()
                         (send up :mark 't)
                         (send low :mark nil)
                         (send two :mark nil)
                         (send wind :title "Power - Upper Tailed")
                         (send wind :tail 0)
                         (send (send wind :true-distn) :ineq 3)
                         (send (select (send wind :param-txt) 4) :redraw)
                         (send (select (send wind :param-txt) 5) :redraw)
                         (send wind :add-distns nil)
                         (send wind :redraw-content)))
    (send low :action #'(lambda()
                         (send up :mark nil)
                         (send low :mark 't)
                         (send two :mark nil)
                         (send wind :title "Power - Lower Tailed")
                         (send wind :tail 1)
                         (send (send wind :true-distn) :ineq 1)
                         (send (select (send wind :param-txt) 4) :redraw)
                         (send (select (send wind :param-txt) 5) :redraw)
                         (send wind :add-distns nil)
                         (send wind :redraw-content)))
    (send two :action #'(lambda()
                          (send up :mark nil)
                          (send low :mark nil)
                          (send two :mark 't)
                          (send wind :title "Power - Two Tailed")
                          (send wind :tail 2)
                          (send (send wind :true-distn) :ineq 9)
                          (send (select (send wind :param-txt) 4) :redraw)
                          (send (select (send wind :param-txt) 5) :redraw)
                          (send wind :add-distns nil)
                          (send wind :redraw-content)))
    (send up :mark 't)
    (send (send self :menu) :append-items up low two)
    (call-next-method lft tp wth ht "Upper Tailed" 1 wind (send self :menu) 1)))

(defmeth power-menu-btn-proto :draw-title ()
  (let* ((w (send self :wind))
         (tail (send w :tail)))
    (cond ((= tail 0)
           (send self :title "Upper"))
      ((= tail 1)
       (send self :title "Lower"))
      ((= tail 2)
       (send self :title "Two")))
    (call-next-method)))





;;
;; Probability Inequality menu button proto
;;

(defproto ineq-menu-btn-proto () () menu-btn-proto)

(defmeth ineq-menu-btn-proto :isnew (lft tp wth ht title wind)
  (let* ((ineq-list (list "x = a" "x Ç= a" "x Ç a" "x È= a" "x È a" 
                          "a Ç= x Ç= b" "a Ç x Ç= b" "a Ç= x Ç b" "a Ç x Ç b"
                         "x Ç= a  or  x È= b" "x Ç a  or  x È= b" "x Ç= a  or  x È b" "x Ç a  or  x È b"))
         (item nil)
         (i 0))
    (send self :menu (send menu-proto :new "Menu"))
    (dolist (ineq ineq-list item)
            (let* ((j i))
              (send (send self :menu) :append-items (send menu-item-proto :new ineq 
                                                        :action #'(lambda ()
                                                                    (let* ((pdf (send wind :pdf)))
                                                                    (send (send wind :distn) :ineq j)
                                                                    (if (= pdf 1) 
                                                                        (send wind :add-pdf 't) 
                                                                        (send wind :add-cdf 't))
                                                                    (send wind :redraw-content)))))
              (setf i (+ i 1))))
    ;;(send wind :menu (send self :menu))
    (call-next-method lft tp wth ht "P" 1 wind (send self :menu) 1)))