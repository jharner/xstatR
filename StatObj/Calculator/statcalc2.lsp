;;
;; Sub-View for the expression
;;


(defproto stat-calc-expression-text '(text hilite exp) () stat-calc-sub-view "Stat Calc Sub View")

(defmeth stat-calc-expression-text :text (&optional text)
  (if text (setf (slot-value 'text) text))
  (slot-value 'text))

(defmeth stat-calc-expression-text :hilite (&optional hilite)
  (if hilite (setf (slot-value 'hilite) hilite))
  (slot-value 'hilite))

(defmeth stat-calc-expression-text :exp (&optional exp)
  (if exp (setf (slot-value 'exp) exp))
  (slot-value 'exp))

(defmeth stat-calc-expression :return-width ()
nil)

(defmeth stat-calc-expression-text :draw (left)
  (let* ((top (send self :top))
         (window (send self :window))
         (text (send self :text))
         (hilite (send self :hilite))
         (width (send self :return-width))
         (bottom (+ (send window :text-ascent) (send window :text-descent))))
    (send window :erase-rect left top width bottom)
    (send window :draw-string text left (+ top 9))
    (cond ((= hilite 1)
          (send window :draw-mode 'xor)
          (send window :paint-rect left top (send window :text-width text) bottom)
          (send window :draw-mode 'normal))) 
    (+ left (send window :text-width text))))

(defmeth stat-calc-expression-text :do-click (x y m1 m2)
(let* ((left (send self :left))
       (right (send self :right))
       (top (send self :top))
       (bottom (send self :bottom))
       (exp (send self :exp))
       (etop (+ (send exp :top) 1))
       (eleft (+ (send exp :left) 1))
       (ebottom (- (send exp :bottom) 1))
       (eright (- (send exp :right) 1))
       (fullexp (send exp :full-exp))
       (fullleft (send fullexp :left))
       (sub-views (send self :sub-views))
       (w (send self :window))
       (handled nil))
  (cond ((and (>= x left) (<= x right) (>= y top) (<= y bottom))
         (send (send self :exp) :cur-exp self)
         (send fullexp :hilite-sub-views 0)
         (send self :hilite-sub-views 1)
         (send exp :draw)
         (setf handled 't)))
  (cond ((not (and (>= x left) (<= x right) (>= y top) (<= y bottom)))
         (dolist (view sub-views 't)
                 (if view (setf handled (send view :do-click x y m1 m2))))))
  handled))

(defmeth stat-calc-expression-text :hilite-sub-views (hilite)
  (let* ((sub-views (send self :sub-views))
         (left (send self :left)))
    (send self :hilite hilite)
    (dolist (view sub-views 't)
            (cond (view
                   (send view :hilite-sub-views hilite))))))

(defproto stat-calc-unary-text '() () stat-calc-expression-text "Stat Calc Sub View")

(defmeth stat-calc-unary-text :draw (left)
  (let* ((top (send self :top))
         (bottom (send self :bottom))
         (window (send self :window))
         (text (send self :text))
         (sub-views (send self :sub-views)))
    (send self :left left)
    (setf left (call-next-method left))
    (send self :right left)
    (send window :draw-string "(" (+ left 2) (+ top 9))
    (setf left (+ left (send window :text-width "(")))
    (setf left (send (select sub-views 0) :draw left))
    (send window :draw-string ")" (+ left 2) (+ top 9))
    (setf left (+ left (send window :text-width ")" )))))

(defmeth stat-calc-unary-text :return-width ()
(let* ((text (send self :text))
       (w (send self :window))
       (sub-views (send self :sub-views)))
  (+ 2 (send w :text-width text) (send w :text-width ")") (send w :text-width "(") (send (select sub-views 0) :return-width))))

(defproto stat-calc-dist-text '() () stat-calc-expression-text "Stat Calc Sub View")

(defmeth stat-calc-dist-text :draw (left)
  (let* ((top (send self :top))
         (bottom (send self :bottom))
         (window (send self :window))
         (text (send self :text))
         (sub-views (send self :sub-views)))
    (send self :left left)
    (setf left (call-next-method left))
    (send self :right left)
    (send window :draw-string "(" (+ left 2) (+ top 9))
    (setf left (+ left (send window :text-width "(")))
    (setf left (send (select sub-views 0) :draw left))
    (send window :draw-string "," (+ left 2) (+ top 9))
    (setf left (+ left (send window :text-width ",") 2))
    (setf left (send (select sub-views 1) :draw left))
    (send window :draw-string ")" (+ left 2) (+ top 9))
    (setf left (+ left (send window :text-width ")" )))))

(defmeth stat-calc-dist-text :return-width ()
(let* ((text (send self :text))
       (w (send self :window))
       (sub-views (send self :sub-views)))
  (+ 2 (send w :text-width text) (send w :text-width ",") (send w :text-width ")") (send w :text-width "(") (send (select sub-views 0) :return-width) (send (select sub-views 1) :return-width))))




(defproto stat-calc-binary-text '() () stat-calc-expression-text "Stat Calc Sub View")

(defmeth stat-calc-binary-text :return-width ()
(let* ((text (send self :text))
       (w (send self :window))
       (sub-views (send self :sub-views)))
  (+ 4 (send w :text-width text) (send w :text-width "(") (send w :text-width ")") 
     (send (select sub-views 0) :return-width) (send (select sub-views 1) :return-width))))

(defmeth stat-calc-binary-text :draw (left)
  (let* ((top (send self :top))
         (bottom (send self :bottom))
         (window (send self :window))
         (text (send self :text))
         (sub-views (send self :sub-views)))
    (send window :draw-string "(" (+ left 2) (+ top 9))
    (setf left (+ left (send window :text-width "(")))
    (setf left (send (select sub-views 0) :draw left))
    (send self :left left)
    (setf left (call-next-method (+ left 2)))
    (send self :right left)
    (setf left (send (select sub-views 1) :draw left))
    (send window :draw-string ")" (+ left 2) (+ top 9))
    (setf left (+ left (send window :text-width ")")))))

(defproto stat-calc-number-text '() () stat-calc-expression-text "Stat Calc Sub View")

(defmeth stat-calc-number-text :return-width ()
(let* ((text (send self :text))
       (w (send self :window))
       (sub-views (send self :sub-views)))
  (+ 2 (send w :text-width text))))

(defmeth stat-calc-number-text :draw (left)
  (let* ((top (send self :top))
         (bottom (send self :bottom))
         (window (send self :window))
         (text (send self :text))
         (sub-views (send self :sub-views)))
    (send self :left left)
    (setf left (call-next-method (+ left 2)))
    (send self :right left)))

;;
;; Sub-view for the variable list
;;
;; Last modified 3/8/96
;;

(defproto stat-calc-var-list '(first-var last-var vis-vars var-list expression) () stat-calc-sub-view "Stat Calc Sub View")

(defmeth stat-calc-var-list :expression (&optional expression)
  (if expression (setf (slot-value 'expression) expression))
  (slot-value 'expression))

(defmeth stat-calc-var-list :first-var (&optional first-var)
  (if first-var (setf (slot-value 'first-var) first-var))
  (slot-value 'first-var))

(defmeth stat-calc-var-list :last-var (&optional last-var)
  (if last-var (setf (slot-value 'last-var) last-var))
  (slot-value 'last-var))

(defmeth stat-calc-var-list :vis-vars (&optional vis-vars)
  (if vis-vars (setf (slot-value 'vis-vars) vis-vars))
  (slot-value 'vis-vars))

(defmeth stat-calc-var-list :var-list (&optional var-list)
  (if var-list (setf (slot-value 'var-list) var-list))
  (slot-value 'var-list))

(defmeth stat-calc-var-list :scroll-down-one ()
  (let* ((left (send self :left))
         (right (send self :right))
         (top (send self :top))
         (bottom (send self :bottom))
         (vars (send self :var-list))
         (num-vars (length vars))
         (first-v (send self :first-var))
         (last-v (send self :last-var))
         (first (if (not (= (- num-vars 1) last-v)) (+ first-v 1) first-v))
         (last (if (not (= (- num-vars 1) last-v)) (+ last-v 1) last-v))
         (w (send self :window))
         (vis-vars (send self :create-var-sub-views (select vars (iseq first last)) left top (- right 12) w)))
    (cond ((not (= (- num-vars 1) last-v)) 
           (send self :first-var first)
           (send self :last-var last)
           (send self :sub-views (combine (select (send self :sub-views) 0) vis-vars))
           (send w :erase-rect (+ left 1) (+ top 1) (- (- right left) 13) (- (- bottom top) 2))
           (send self :draw)))))

(defmeth stat-calc-var-list :scroll-up-one ()
(let* ((left (send self :left))
         (right (send self :right))
         (top (send self :top))
         (bottom (send self :bottom))
         (vars (send self :var-list))
         (num-vars (length vars))
         (first-v (send self :first-var))
         (last-v (send self :last-var))
         (first (if (not (= 0 first-v)) (- first-v 1) first-v))
         (last (if (not (= 0 first-v)) (- last-v 1) last-v))
         (w (send self :window))
         (vis-vars (send self :create-var-sub-views (select vars (iseq first last)) left top (- right 12) w)))
    (cond ((not (= 0 first-v))
           (send self :first-var first)
           (send self :last-var last)
           (send self :sub-views (combine (select (send self :sub-views) 0) vis-vars))
           (send w :erase-rect (+ left 1) (+ top 1) (- (- right left) 13) (- (- bottom top) 2))
           (send self :draw)))))

(defmeth stat-calc-var-list :isnew (window left top right bottom variables)
  (let* ((scroll-bar (send stat-calc-var-v-scroll :new window (- sc-vlist-right 11) 
                           sc-vlist-top sc-vlist-right sc-vlist-bottom self))
         (vars (send self :create-var-sub-views variables left top (- right 12) window)))
    (if (> (length variables) 7) (send self :last-var 6) (send self :last-var (- (length variables) 1)))
    (send self :first-var 0)
    (setf vars (select vars (iseq (send self :first-var) (send self :last-var))))
    (send self :vis-vars vars)
    (send self :var-list variables)
    (send self :sub-views (combine scroll-bar (send self :vis-vars)))
    (call-next-method window left top right bottom)))

(defmeth stat-calc-var-list :create-var-sub-views (variables left top right w)
  (let* ((j 0)
         (i 1)
         (subb nil))
    (dolist (var variables subb)
            (setf subb (combine subb 
                                (send stat-calc-var-name :new var w self 
                                      left 
                                      (+ top (* i 3) (* j 9)) 
                                      right (+ top (* i 3) (* j 9) 12))))
            (setf i (+ i 1))
            (setf j (+ j 1)))
    (remove nil subb)))
            
            
            


(defmeth stat-calc-var-list :draw ()
    (call-next-method))

(defmeth stat-calc-var-list :do-click (x y m1 m2)
(let* ((left (send self :left))
       (right (send self :right))
       (bottom (send self :bottom))
       (top (send self :top))
       (handled nil))
       (cond ((and (>= x left) (<= x right) (>= y top) (<= y bottom))
              (call-next-method x y m1 m2)))))


;;
;; Sub-view for scroll bar up-arrow
;;
;; Last modified 3/8/96
;;

(defproto stat-calc-up-arrow '(bar bitmap) () stat-calc-sub-view "Stat Calc Sub View")

(defmeth stat-calc-up-arrow :bar (&optional bar)
  (if bar (setf (slot-value 'bar) bar))
  (slot-value 'bar))

(defmeth stat-calc-up-arrow :bitmap (&optional bitmap)
  (if bitmap (setf (slot-value 'bitmap) bitmap))
  (slot-value 'bitmap))

(defmeth stat-calc-up-arrow :isnew (window bar bitmap left top right bottom)
  (send self :bar bar)
  (send self :window window)
  (send self :top top)
  (send self :left left)
  (send self :right right)
  (send self :bitmap bitmap)
  (send self :bottom (+ top 10))
  (call-next-method window left top right (+ top 10)))

(defmeth stat-calc-up-arrow :draw ()
  (let* ((bar (send self :bar))
         (top (send bar :top))
         (bottom (send bar :bottom))
         (left (send bar :left))
         (right (send bar :right))
         (width (- right left))
         (height (- bottom top))
         (bitmap (send self :bitmap))
         (w (send self :window)))
    (send w :draw-bitmap bitmap (+ left 1) (+ top 2))))

(defmeth stat-calc-up-arrow :while-mouse-button-down ()
(let* ((left (send self :left))
         (right (send self :right))
         (top (send self :top))
         (bottom (send self :bottom))
         (w (send self :window))
         (bar (send self :bar))
         (scroll-field (send bar :scroll-field))
         (in 't)
         (black 't))
  (send w :while-button-down 
        #'(lambda (x y)
            (cond ((and (>= x left) (<= x right) (>= y top) (<= y bottom))
                   (send self :bitmap up-arrow-s)
                   (send self :draw)
                   (send scroll-field :scroll-up-one)
                   (pause 5)
                   (setf in 't)))
            (cond ((not (and (>= x left) (<= x right) (>= y top) (<= y bottom)))
                   (send self :bitmap up-arrow)
                   (send self :draw)
                   (setf in nil)))) nil)
  in))

(defmeth stat-calc-up-arrow :do-click (x y m1 m2)
(let* ((scroll-field (send (send self :bar) :scroll-field))
       (left (send self :left))
       (right (send self :right))
       (bottom (send self :bottom))
       (top (send self :top))
       (w (send self :window))
       (handled nil))
       (cond ((and (>= x left) (<= x right) (>= y top) (<= y bottom))
              (send self :bitmap up-arrow-s)
              (send self :draw)
              (send scroll-field :scroll-up-one)
              (pause 5)
              (cond ((send self :while-mouse-button-down)
                     (send self :bitmap up-arrow)
                     (send self :draw)
                     (setf handled 't)))
              handled))))

;;
;; Sub-view for scroll bar down-arrow
;;
;; Last modified 3/8/96
;;

(defproto stat-calc-down-arrow '(bar bitmap) () stat-calc-sub-view "Stat Calc Sub View")

(defmeth stat-calc-down-arrow :bitmap (&optional bitmap)
  (if bitmap (setf (slot-value 'bitmap) bitmap))
  (slot-value 'bitmap))

(defmeth stat-calc-down-arrow :bar (&optional bar)
  (if bar (setf (slot-value 'bar) bar))
  (slot-value 'bar))

(defmeth stat-calc-down-arrow :isnew (window bar bitmap left top right bottom)
  (send self :bar bar)
  (send self :window window)
  (send self :left left)
  (send self :right right)
  (send self :bitmap bitmap)
  (send self :bottom bottom)
  (send self :top (- bottom 9))
  (call-next-method window left (- bottom 9) right bottom))

(defmeth stat-calc-down-arrow :draw ()
  (let* ((bar (send self :bar))
         (top (send bar :top))
         (bottom (send bar :bottom))
         (left (send bar :left))
         (right (send bar :right))
         (bitmap (send self :bitmap))
         (w (send self :window)))
    (send w :draw-bitmap bitmap (+ left 1) (- bottom 7))
    (send self :bottom bottom)
    (send self :top (- bottom 9))
    (send self :right right)
    (send self :left left)))

(defmeth stat-calc-down-arrow :while-mouse-button-down ()
(let* ((left (send self :left))
         (right (send self :right))
         (top (send self :top))
         (bottom (send self :bottom))
         (w (send self :window))
         (bar (send self :bar))
         (scroll-field (send bar :scroll-field))
         (in 't)
         (black 't))
  (send w :while-button-down 
        #'(lambda (x y)
            (cond ((and (>= x left) (<= x right) (>= y top) (<= y bottom))
                   (send self :bitmap down-arrow-s)
                   (send self :draw)
                   (send scroll-field :scroll-down-one)
                   (pause 5)
                   (setf in 't)))
            (cond ((not (and (>= x left) (<= x right) (>= y top) (<= y bottom)))
                   (send self :bitmap down-arrow)
                   (send self :draw)
                   (setf in nil)))) nil)
  in))

(defmeth stat-calc-down-arrow :do-click (x y m1 m2)
(let* ((scroll-field (send (send self :bar) :scroll-field))
       (left (send self :left))
       (right (send self :right))
       (bottom (send self :bottom))
       (top (send self :top))
       (w (send self :window))
       (handled nil))
       (cond ((and (>= x left) (<= x right) (>= y top) (<= y bottom))
              (send self :bitmap down-arrow-s)
              (send self :draw)
              (send scroll-field :scroll-down-one)
              (pause 5)
              (cond ((send self :while-mouse-button-down)
                     (send self :bitmap down-arrow)
                     (send self :draw)
                     (setf handled 't)))
              handled))))

;;
;; Sub-view for scroll bar thumb drag
;;
;; Last modified 3/8/96
;;

(defproto stat-calc-thumb '(bar) () stat-calc-sub-view "Stat Calc Sub View")

(defmeth stat-calc-thumb :bar (&optional bar)
  (if bar (setf (slot-value 'bar) bar))
  (slot-value 'bar))

(defmeth stat-calc-thumb :isnew (window bar)
  (send self :bar bar)
  (send self :window window)
  (send self :top 0)
  (send self :left 0)
  (send self :right 0)
  (send self :bottom 0))

(defmeth stat-calc-thumb :draw ()
  (let* ((bar (send self :bar))
         (top (send bar :top))
         (bottom (send bar :bottom))
         (left (send bar :left))
         (right (send bar :right))
         (width (- right left))
         (height (- bottom top))
         (w (send self :window)))
    (send w :frame-rect (+ left 2) (+ top 12) (- width 4) (- height 23))))


(defproto stat-calc-v-thumb '(bar) () stat-calc-sub-view "Stat Calc Sub View")

(defmeth stat-calc-v-thumb :bar (&optional bar)
  (if bar (setf (slot-value 'bar) bar))
  (slot-value 'bar))

(defmeth stat-calc-v-thumb :isnew (window bar)
  (send self :bar bar)
  (send self :window window)
  (send self :top 0)
  (send self :left 0)
  (send self :right 0)
  (send self :bottom 0))



(defmeth stat-calc-v-thumb :draw ()
  (let* ((bar (send self :bar))
         (top (send bar :top))
         (bottom (send bar :bottom))
         (left (send bar :left))
         (right (send bar :right))
         (width (- right left))
         (height (- bottom top))
         (w (send self :window))
         (num-vars (length (send (send bar :scroll-field) :var-list)))
         (scale (if (<= num-vars 7) 1 (/ 7 num-vars)))
         (height (ceiling (* scale (- height 23))))
         (first (send (send bar :scroll-field) :first-var))
         (scroll-dist (floor (* (/ height (- num-vars 7)) first))))
   (send w :erase-rect (+ left 2) (+ top 12) (- width 4) (- (- bottom top) 23)) 
   (send w :frame-rect (+ left 2) (+ top 12 scroll-dist) (- width 4) height)))     
    

;;
;; Sub-view for the variable list scroll bar
;;
;; Last modified 3/8/96
;;

(defproto stat-calc-var-scroll '(scroll-field) () stat-calc-sub-view "Stat Calc Sub View")

(defmeth stat-calc-var-scroll :isnew (window left top right bottom scroll-field)
  (let* ((up (send stat-calc-up-arrow :new window self up-arrow left top right bottom))
         (down (send stat-calc-down-arrow :new window self down-arrow left top right bottom))
         (thumb (send stat-calc-thumb :new window self)))
    (send self :scroll-field scroll-field)
    (send self :sub-views (list up down thumb))
    (call-next-method window left top right bottom)))

(defmeth stat-calc-var-scroll :scroll-field (&optional scroll-field)
  (if scroll-field (setf (slot-value 'scroll-field) scroll-field))
  (slot-value 'scroll-field))

(defmeth stat-calc-var-scroll :draw ()
    (call-next-method))

(defproto stat-calc-var-v-scroll '(scroll-field) () stat-calc-sub-view "Stat Calc Sub View")

(defmeth stat-calc-var-v-scroll :isnew (window left top right bottom scroll-field)
  (let* ((up (send stat-calc-up-arrow :new window self up-arrow left top right bottom))
         (down (send stat-calc-down-arrow :new window self down-arrow left top right bottom))
         (thumb (send stat-calc-v-thumb :new window self)))
    (send self :scroll-field scroll-field)
    (send self :sub-views (list up down thumb))
    (call-next-method window left top right bottom)))

(defmeth stat-calc-var-v-scroll :scroll-field (&optional scroll-field)
  (if scroll-field (setf (slot-value 'scroll-field) scroll-field))
  (slot-value 'scroll-field))

(defmeth stat-calc-var-v-scroll :draw ()
    (call-next-method))

;;
;; Sub-View of a variable in the variable list
;;

(defproto stat-calc-var-name '(variable var-list) () stat-calc-sub-view "Stat Calc Sub View")

(defmeth stat-calc-var-name :var-list (&optional var-list)
  (if var-list (setf (slot-value 'var-list) var-list))
  (slot-value 'var-list))

(defmeth stat-calc-var-name :variable (&optional variable)
  (if variable (setf (slot-value 'variable) variable))
  (slot-value 'variable))

(defmeth stat-calc-var-name :isnew (variable window var-list left top right bottom)
 (send self :variable variable)
 (send self :var-list var-list)
 (call-next-method window left top right bottom))

(defmeth stat-calc-var-name :draw ()
  (let* ((left (send self :left))
         (top (send self :top))
         (w (send self :window))
         (name (send (eval (send self :variable)) :name)))
    (send w :draw-string (format nil "~a" name) (+ left 3) (+ top 9))))

(defmeth stat-calc-var-name :while-mouse-button-down ()
(let* ((left (send self :left))
         (right (send self :right))
         (top (send self :top))
         (bottom (send self :bottom))
         (w (send self :window))
         (in 't)
         (black 't))
  (send w :while-button-down 
        #'(lambda (x y)
            (cond ((and (>= x (+ left 1)) (<= x right) (>= y top) (<= y bottom) (not in))
                   (send w :paint-rect (+ left 1) top (- right left) (- bottom top))
                   (setf in 't)))
            (cond ((and (not (and (>= x (+ left 1)) (<= x right) (>= y top) (<= y bottom))) in)
                   (send w :paint-rect (+ left 1) top (- right left) (- bottom top))
                   (setf in nil)))))
  in))

(defmeth stat-calc-var-name :do-click (x y m1 m2)
  (let* ((left (send self :left))
         (right (send self :right))
         (top (send self :top))
         (bottom (send self :bottom))
         (w (send self :window))
         (fullexp (send (send (send self :var-list) :expression) :full-exp))
         (handled nil))
    (cond ((and (>= x left) (<= x right) (>= y top) (<= y bottom))
           (send w :draw-mode 'xor)
           (send w :paint-rect (+ left 1) top (- right left) (- bottom top))
           (cond ((send self :while-mouse-button-down)
                  (send w :paint-rect (+ left 1) top (- right left) (- bottom top))
                  (setf handled 't)))
           (cond (handled
                  (send self :edit-current-exp nil (list fullexp))))
           (send w :draw-mode 'normal)
           handled))))

(defmeth stat-calc-var-name :edit-current-exp (plus-exp sub-exp)
  (let* ((expr (send (send self :var-list) :expression))
         (etop (+ (send expr :top) 1))
         (ebottom (- (send expr :bottom) 1))
         (eright (- (send expr :right) 1))
         (eleft (+ (send expr :left) 1))
         (curexp (send expr :cur-exp))
         (fullexp (send expr :full-exp))
         (top (send fullexp :top))
         (left (send fullexp :left))
         (w (send expr :window))
         (name (format nil "~a" (send (eval (send self :variable)) :name)))
         (height (+ (send w :text-ascent) (send w :text-descent)))
         (width (send fullexp :return-width))
         (i 0))
   (cond ((not plus-exp) 
          (setf plus-exp (send stat-calc-number-text :new w (send fullexp :left) (send fullexp :top) (send fullexp :right) (send fullexp :bottom)))
          (send plus-exp :text name)
          (send plus-exp :exp expr)
          (send plus-exp :hilite-sub-views 1)))
   (dolist (view sub-exp 't)
           (cond ((eq curexp view)
                  (send w :erase-rect left top width height) 
                  (setf (select sub-exp i) plus-exp)
                  (cond ((eq view fullexp) 
                         (send expr :full-exp plus-exp)
                         (setf (select (send expr :sub-views) 1) plus-exp)))
                  (send expr :cur-exp plus-exp)
                  (send w :erase-rect eleft etop (- eright eleft) (- ebottom etop))
                  (send expr :draw)))
           (cond ((not (eq curexp view))
                  (send self :edit-current-exp plus-exp (send view :sub-views))))
           (setf i (+ i 1)))))