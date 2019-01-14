(provide "statcalc")

;;
;; This file contains stat-calc-proto along with
;; the protos that are used within stat-calc-proto
;;

;;
;; Definition of stat-calc-proto
;;

(defproto stat-calc-proto '(sub-views cur-selection cur-palette) 
  () graph-window-proto "Stat Calc")

;;
;; Initialize stat-calc-proto
;;

(defmeth stat-calc-proto :isnew (dataset)
  (let* ((var-name (send stat-calc-name
                         :new "Under Construction" self sc-margin sc-margin
                         sc-vlist-right 21))
         (expression (send stat-calc-expression
                           :new self sc-exp-left sc-exp-top sc-exp-right
                           sc-exp-bottom))
         (variable-list (send stat-calc-var-list
                              :new self sc-vlist-left sc-vlist-top 
                              sc-vlist-right sc-vlist-bottom
                              (send dataset :variables)))
         (palette (send stat-calc-palette
                        :new self expression sc-palette-left sc-palette-top
                        sc-palette-right sc-palette-bottom))
         (palette-selector (send stat-calc-pal-selector
                                 :new self palette sc-ps-left sc-ps-top
                                 sc-ps-right sc-ps-bottom)))
    (send variable-list :expression expression)
    (send self :sub-views
          (list variable-list palette-selector palette expression var-name))
    (send self :location sc-loc-left sc-loc-top)
    (send self :size sc-min-width sc-min-height)
    (call-next-method)))

;;;
;;; Constucts calculator
;;;

(defun make-calculator (dataset)
  (send stat-calc-proto :new dataset))

;;
;; Accessors and Mutators
;;

(defmeth stat-calc-proto :sub-views (&optional sub-views)
  (if sub-views (setf (slot-value 'sub-views) sub-views))
  (slot-value 'sub-views))

(defmeth stat-calc-proto :cur-selection (&optional cur-selection)
  (if cur-selection (setf (slot-value 'cur-selection) cur-selection))
  (slot-value 'cur-selection))

(defmeth stat-calc-proto :cur-palette (&optional cur-palette)
  (if cur-palette (setf (slot-value 'cur-palette) cur-palette))
  (slot-value 'cur-palette))

;;
;; Redraw the calculator
;;

(defmeth stat-calc-proto :redraw ()
  (let* ((sub-views (send self :sub-views)))
    (dolist (view sub-views 't)
            (send view :draw))
    (call-next-method)))

(defmeth stat-calc-proto :resize ()
  (if (< (first (send self :size)) (+ sc-min-width 15))
      (send self :size (+ sc-min-width 15) (second (send self :size))))
  (if (< (second (send self :size)) sc-min-height)
      (send self :size (first (send self :size)) sc-min-height))
  (call-next-method))

;;
;; Handle mouse clicks
;;

(defmeth stat-calc-proto :do-click (x y m1 m2)
  (let* ((sub-views (send self :sub-views))
         (handled nil))
    (dolist (view sub-views handled)
            (if (send view :do-click x y m1 m2) 
                (setf handled 't)))))

;;
;; Handle key strokes
;;

(defmeth stat-calc-proto :do-key (key m1 m2))

;;
;; Expression tree methods
;;

(defmeth stat-calc-proto :add-to-tree ()
  nil)

(defmeth stat-calc-proto :delete-from-tree ()
  nil)

(defmeth stat-calc-proto :modify-tree ()
  nil)


;;
;; Constants to prevent retyping
;;

(setf sc-min-height 310)
(setf sc-min-width 410)
(setf sc-loc-left 50)
(setf sc-loc-top 50)

(setf sc-margin 5)
(setf sc-scroll-width 11)
(setf sc-ps-button-height 15)
(setf sc-ps-button-width 60)

(setf sc-vlist-width 150)
(setf sc-vlist-left sc-margin)
(setf sc-vlist-right (+ sc-vlist-left sc-vlist-width))

(setf sc-ps-width 245)
(setf sc-ps-height 45)
(setf sc-ps-left (+ sc-vlist-right sc-margin))
(setf sc-ps-top sc-margin)
(setf sc-ps-right (+ sc-ps-left sc-ps-width))
(setf sc-ps-bottom (+ sc-ps-top sc-ps-height)) 

(setf sc-palette-width sc-ps-width)
(setf sc-palette-height 61)
(setf sc-palette-left sc-ps-left)
(setf sc-palette-top (+ sc-ps-bottom sc-margin))
(setf sc-palette-right (+ sc-palette-left sc-palette-width))
(setf sc-palette-bottom (+ sc-palette-top sc-palette-height -1))


(setf sc-vlist-height (+ sc-ps-height sc-palette-height -17))
(setf sc-vlist-top 26)
(setf sc-vlist-bottom (+ sc-vlist-top sc-vlist-height))

(setf sc-exp-width (+ sc-vlist-width sc-ps-width sc-margin))
(setf sc-exp-height (- sc-min-height (+ (* 3 sc-margin) sc-vlist-height)))
(setf sc-exp-left sc-margin)
(setf sc-exp-top (+ sc-vlist-bottom sc-margin))
(setf sc-exp-right (+ sc-exp-left sc-exp-width))
(setf sc-exp-bottom (+ sc-exp-top sc-exp-height))

;;
;; Proto definition for calc's sub-views
;;

(defproto stat-calc-sub-view '(window sub-views left top right bottom) () nil
  "Stat Calc Sub View")

;;
;; Slot accessors and mutators for stat-calc-sub-view
;;

(defmeth stat-calc-sub-view :window (&optional window)
  (if window (setf (slot-value 'window) window))
  (slot-value 'window))

(defmeth stat-calc-sub-view :sub-views (&optional sub-views)
  (if sub-views (setf (slot-value 'sub-views) sub-views))
  (slot-value 'sub-views))

(defmeth stat-calc-sub-view :left (&optional left)
  (if left (setf (slot-value 'left) left))
  (slot-value 'left))

(defmeth stat-calc-sub-view :top (&optional top)
  (if top (setf (slot-value 'top) top))
  (slot-value 'top))

(defmeth stat-calc-sub-view :right (&optional right)
  (if right (setf (slot-value 'right) right))
  (slot-value 'right))

(defmeth stat-calc-sub-view :bottom (&optional bottom)
  (if bottom (setf (slot-value 'bottom) bottom))
  (slot-value 'bottom))

;;
;; Is new method for stat-calc-proto
;;

(defmeth stat-calc-sub-view :isnew (window left top right bottom)
  (send self :window window)
  (send self :left left)
  (send self :right right)
  (send self :top top)
  (send self :bottom bottom)
  (send self :window window))

;;
;; Method to draw the sub-views
;;

(defmeth stat-calc-sub-view :draw ()
  (let* ((sub-views (send self :sub-views))
         (left (send self :left))
         (right (send self :right))
         (top (send self :top))
         (bottom (send self :bottom))
         (window (send self :window)))
    (send window :frame-rect left top (- right left) (- bottom top))
    (dolist (view sub-views 't)
            (send view :draw))))

;;
;; Method to handle clicks
;;

(defmeth stat-calc-sub-view :do-click (x y m1 m2)
  (let* ((sub-views (send self :sub-views))
         (handled nil))
    (dolist (view sub-views handled)
            (if (send view :do-click x y m1 m2) 
                (setf handled 't)))))


;;
;; Method to handle key strokes
;;

(defmeth stat-calc-sub-view :do-key (key m1 m2))

;;
;; Sub-View for the new variables name
;;

(defproto stat-calc-name '(name) () stat-calc-sub-view "Stat Calc Sub View")

(defmeth stat-calc-name :name (&optional name)
  (if name (setf (slot-value 'name) name))
  (slot-value 'name))

(defmeth stat-calc-name :isnew (name window left top right bottom)
  (send self :name name)
  (call-next-method window left top right bottom))

(defmeth stat-calc-name :draw ()
  (let* ((left (send self :left))
         (right (send self :right))
         (top (send self :top))
         (bottom (send self :bottom))
         (w (send self :window)))
    (send w :draw-string (send self :name) (+ left 3) (+ top 11))
    (call-next-method)))


;;
;; Sub-View for the selecting the palettes
;;

(defproto stat-calc-ps-button '(ps title palette) () stat-calc-sub-view "Stat Calc Sub View")

(defmeth stat-calc-ps-button :ps (&optional ps)
  (if ps (setf (slot-value 'ps) ps))
  (slot-value 'ps))

(defmeth stat-calc-ps-button :title (&optional title)
  (if title (setf (slot-value 'title) title))
  (slot-value 'title))

(defmeth stat-calc-ps-button :palette (&optional palette)
  (if palette (setf (slot-value 'palette) palette))
  (slot-value 'palette))

(defmeth stat-calc-ps-button :isnew (title ps window pale left top right bottom)
  (let* ((rightt (+ left sc-ps-button-width))
         (bottomm (+ top sc-ps-button-height)))
    (send self :title title)
    (send self :ps ps)
    (send self :palette pale)
    (call-next-method window left top rightt bottomm)))

(defmeth stat-calc-ps-button :draw ()
  (let* ((left (send self :left))
         (right (send self :right))
         (top (send self :top))
         (bottom (send self :bottom))
         (title (send self :title))
         (w (send self :window))
         (v-text (floor (* .5 (- (- bottom top) 9))))
         (h-text (floor (* .5 (- (- right left) (send w :text-width title))))))
    (cond 
      ((eq self (send w :cur-palette))
       (send w :paint-rect left top (- right left) (- bottom top))
       (send w :draw-mode 'xor))
      (t
       (send w :erase-rect left top (- right left) (- bottom top))))
    (send w :draw-string title (+ left h-text) (+ top v-text 8))
    (send w :draw-mode 'normal)
    (call-next-method)))

(defmeth stat-calc-ps-button :do-click (x y m1 m2)
  (let* ((left (send self :left))
         (right (send self :right))
         (top (send self :top))
         (bottom (send self :bottom))
         (w (send self :window))
         (c-pal (send w :cur-palette))
         (palette (select (send w :sub-views) 2))
         (p-left (send palette :left))
         (p-right (send palette :right))
         (p-bottom (send palette :bottom))
         (p-top (send palette :top))
         (handled nil))
    (cond ((and (>= x left) (<= x right) (>= y top) (<= y bottom))
           (send w :cur-palette self)
           (send palette :sub-views (send self :palette))
           (send w :erase-rect (+ p-left 1) (+ p-top 1) (- (- p-right p-left) 2) (- (- p-bottom p-top) 2))
           (send palette :draw) 
           (send c-pal :draw)
           (send self :draw)
           (setf handled 't)
           handled))))

;;
;; Functions for creating the palettes
;;

(defun create-algebraic-palette (palette window)
  (let* ((p-left (send palette :left))
         (p-top (send palette :top))
         (add (send stat-calc-plus-button :new "add" palette window alg-add 
                     (+ p-left 3) (+ p-top 3) (+ p-left 19) (+ p-top 19)))
         (left (send add :right))
         (top (send add :top))
         (sub (send stat-calc-minus-button :new "sub" palette window alg-sub 
                     (+ left 3) top (+ left 19) (+ top 16)))
         (left (send sub :right))
         (mult (send stat-calc-multiply-button :new "mult" palette window alg-mult 
                     (+ left 3) top (+ left 19) (+ top 16)))
         (left (send mult :right))
         (div (send stat-calc-divide-button :new "div" palette window alg-div 
                     (+ left 3) top (+ left 19) (+ top 16)))
         (left (send add :left))
         (top  (+ (send add :bottom) 3))
         (power (send stat-calc-p-button :new "power" palette window alg-power 
                     left top (+ left 16) (+ top 16)))
         (left (send power :right))
         (sqrt (send stat-calc-p-button :new "sqrt" palette window alg-sqrt 
                     (+ left 3) top (+ left 19) (+ top 16)))
         (left (send sqrt :right))
         (fact (send stat-calc-p-button :new "fact" palette window alg-fact 
                     (+ left 3) top (+ left 19) (+ top 16)))
         (left (send fact :right))
         (sign (send stat-calc-p-button :new "sign" palette window alg-sign 
                     (+ left 3) top (+ left 19) (+ top 16))))
   (list add sub mult div power sqrt fact sign)))

(defun create-modeling-palette (palette window)
 (let* ((p-left (send palette :left))
         (p-top (send palette :top))
         (add (send stat-calc-p-button :new "add" palette window mod-add 
                     (+ p-left 3) (+ p-top 3) (+ p-left 19) (+ p-top 19)))
         (left (send add :right))
         (top (send add :top))
         (sub (send stat-calc-p-button :new "sub" palette window mod-sub 
                     (+ left 3) top (+ left 19) (+ top 16)))
         (left (send sub :right))
         (mult (send stat-calc-p-button :new "mult" palette window mod-mult 
                     (+ left 3) top (+ left 19) (+ top 16)))
         (left (send add :left))
         (top  (+ (send add :bottom) 3))
         (div (send stat-calc-p-button :new "div" palette window mod-div 
                     left top (+ left 16) (+ top 16)))
         (left (send div :right))
         (col (send stat-calc-p-button :new "col" palette window mod-col 
                     (+ left 3) top (+ left 19) (+ top 16)))
         (left (send col :right))
         (power (send stat-calc-p-button :new "power" palette window mod-power 
                     (+ left 3) top (+ left 19) (+ top 16))))
   (list add sub mult div col power)))

(defun create-parameter-palette (palette window)
 (let* ((p-left (send palette :left))
         (p-top (send palette :top))
         (pbeta (send stat-calc-p-button :new "beta" palette window betap 
                     (+ p-left 3) (+ p-top 3) (+ p-left 19) (+ p-top 19)))
         (left (send pbeta :right))
         (top (send pbeta :top))
         (pmu (send stat-calc-p-button :new "mu" palette window mup 
                     (+ left 3) top (+ left 19) (+ top 16)))
         (left (send pmu :right))
         (psigma (send stat-calc-p-button :new "sigma" palette window sigmap 
                     (+ left 3) top (+ left 19) (+ top 16)))
         (left (send pbeta :left))
         (top  (+ (send pbeta :bottom) 3))
         (ptheta (send stat-calc-p-button :new "theta" palette window thetap 
                     left top (+ left 16) (+ top 16)))
         (left (send ptheta :right))
         (plambda (send stat-calc-p-button :new "lambda" palette window lambdap 
                     (+ left 3) top (+ left 19) (+ top 16)))
         (left (send plambda :right))
         (pphi (send stat-calc-p-button :new "phi" palette window phip 
                     (+ left 3) top (+ left 19) (+ top 16))))
   (list pbeta pmu psigma ptheta plambda pphi)))

(defun create-transcend-palette (palette window)
 (let* ((p-left (send palette :left))
         (p-top (send palette :top))
         (exponent (send stat-calc-p-button :new "exp" palette window exponent 
                     (+ p-left 3) (+ p-top 3) (+ p-left 19) (+ p-top 19)))
         (left (send exponent :right))
         (top (send exponent :top))
         (natln (send stat-calc-ln-button :new "ln" palette window natlog 
                     (+ left 3) top (+ left 19) (+ top 16)))
         (left (send natln :right))
         (absv (send stat-calc-p-button :new "abs" palette window absval 
                     (+ left 3) top (+ left 19) (+ top 16)))
         (left (send exponent :left))
         (top  (+ (send exponent :bottom) 3))
         (gama (send stat-calc-p-button :new "gamma" palette window gammaf 
                     left top (+ left 16) (+ top 16)))
         (left (send gama :right))
         (lngama (send stat-calc-p-button :new "lngamma" palette window lngammaf 
                     (+ left 3) top (+ left 19) (+ top 16))))
   (list exponent natln absv gama lngama)))

(defun create-distribution-palette (palette window)
  (let* ((p-left (send palette :left))
         (p-top (send palette :top))
         (nd (send stat-calc-norm-button :new "normal" palette window normald 
                     (+ p-left 3) (+ p-top 3) (+ p-left 19) (+ p-top 19)))
         (left (send nd :right))
         (top (send nd :top))
         (bino (send stat-calc-p-button :new "binomial" palette window binomiald 
                     (+ left 3) top (+ left 19) (+ top 16)))
         (left (send bino :right))
         (fd (send stat-calc-p-button :new "F" palette window fd 
                     (+ left 3) top (+ left 19) (+ top 16)))
         (left (send fd :right))
         (csq (send stat-calc-p-button :new "chisq" palette window chisqd 
                     (+ left 3) top (+ left 19) (+ top 16)))
         (left (send csq :right))
         (tdist (send stat-calc-p-button :new "t" palette window td 
                     (+ left 3) top (+ left 19) (+ top 16)))
         (left (send nd :left))
         (top  (+ (send nd :bottom) 3))
         (unif (send stat-calc-p-button :new "uniform" palette window uniformd 
                     left top (+ left 16) (+ top 16)))
         (left (send unif :right))
         (pois (send stat-calc-p-button :new "poisson" palette window poissond 
                     (+ left 3) top (+ left 19) (+ top 16)))
         (left (send pois :right))
         (gam (send stat-calc-p-button :new "gamma" palette window gammad 
                     (+ left 3) top (+ left 19) (+ top 16)))
         (left (send gam :right))
         (bet (send stat-calc-p-button :new "beta" palette window betad 
                     (+ left 3) top (+ left 19) (+ top 16)))
         (left (send bet :right))
         (cach (send stat-calc-p-button :new "cauchy" palette window cauchyd 
                     (+ left 3) top (+ left 19) (+ top 16))))
   (list nd bino fd csq tdist unif pois gam bet cach)))
;;
;; Sub-View for the selecting the palettes
;;

(defproto stat-calc-pal-selector '() () stat-calc-sub-view "Stat Calc Sub View")

(defmeth stat-calc-pal-selector :isnew (window palette left top right bottom)
  (let* ((algebraic (send stat-calc-ps-button :new "Algebraic" self window (create-algebraic-palette palette window) (+ left 5) (+ top 5) 0 0))
         (transcen (send stat-calc-ps-button :new "Transcend" self window (create-transcend-palette palette window) (+ left 10 sc-ps-button-width)
                         (+ top 5) 0 0))
         (parameters (send stat-calc-ps-button :new "Distribution" self window (create-distribution-palette palette window) (+ left 15 (* 2 sc-ps-button-width))
                           (+ top 5) 0 0))
         (model (send stat-calc-ps-button :new "Model" self window (create-modeling-palette palette window) (+ left 5) (+ top 10 sc-ps-button-height) 0 0))
         (dist (send stat-calc-ps-button :new "Parameters" self window (create-parameter-palette palette window) (+ left 10 sc-ps-button-width)
                           (+ top 10 sc-ps-button-height) 0 0)))
    (send self :sub-views (list algebraic transcen parameters model dist))
    (send window :cur-palette algebraic) 
    (call-next-method window left top right bottom)))

(defmeth stat-calc-pal-selector :draw ()
  (call-next-method))

;;
;; Sub-view for the palette items
;;

(defproto stat-calc-palette '(expression) () stat-calc-sub-view "Stat Calc Sub View")

(defmeth stat-calc-palette :expression (&optional expression)
  (if expression (setf (slot-value 'expression) expression))
  (slot-value 'expression))

(defmeth stat-calc-palette :isnew (window expression left top right bottom)
  (call-next-method window left top right bottom)
  (send self :expression expression)
  (send self :sub-views (create-algebraic-palette self window)))

(defmeth stat-calc-palette :draw ()
  (call-next-method))

;;
;; Sub-view for the expression
;;

(defproto stat-calc-expression '(cur-exp full-exp) () stat-calc-sub-view "Stat Calc Sub View")

(defmeth stat-calc-expression :cur-exp (&optional cur-exp)
  (if cur-exp (setf (slot-value 'cur-exp) cur-exp))
  (slot-value 'cur-exp))

(defmeth stat-calc-expression :full-exp (&optional full-exp)
  (if full-exp (setf (slot-value 'full-exp) full-exp))
  (slot-value 'full-exp))

(defmeth stat-calc-expression :isnew (window left top right bottom)
  (let* ((scroll-bar (send stat-calc-var-scroll :new window (- sc-exp-right sc-scroll-width) 
                           top right bottom self))
         (empty-exp (send stat-calc-number-text :new window (+ left 3) (+ top (ceiling (* .5 (- bottom top)))) right bottom)))
    (send empty-exp :text "?")
    (send empty-exp :hilite-sub-views 1)
    (send empty-exp :exp self)
    (send self :sub-views (list scroll-bar empty-exp))
    (send self :cur-exp empty-exp)
    (send self :full-exp empty-exp)
    (call-next-method window left top right bottom)))

(defmeth stat-calc-expression :draw ()
  (let* ((w (send self :window))
         (top (send self :top))
         (left (send self :left))
         (bottom (- (second (send w :size)) sc-margin))
         (right (- (first (send w :size)) (+ sc-margin 15)))
         (sub-views (send self :sub-views))
         (exp-top (+ top (ceiling (* .5 (- bottom top)))))
         (width (send (select sub-views 1) :return-width))
         (height (+ (send w :text-ascent) (send w :text-descent)))
         (exp-left (ceiling (+ left (* .5 (- (- right left) width)))))
         (scroll (first (send self :sub-views))))
    (send scroll :bottom bottom)
    (send scroll :right right)
    (send scroll :left (- right sc-scroll-width))
    (send self :bottom bottom)
    (send self :right right)
    (send (select (send self :sub-views) 0) :draw)
    (send w :erase-rect (+ left 1) (+ top 1) (- (- right left) 1) (- (- bottom top)))
    (send (select (send self :sub-views) 1) :draw exp-left)
    (send w :frame-rect left top (- right left) (- bottom top))))


