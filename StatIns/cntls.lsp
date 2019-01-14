(provide ":StatIns:cntls.lsp")

;;
;; General control proto
;;

(defproto cntl-proto '(lft tp wth ht hilite title part wind) () graph-overlay-proto)

(defmeth cntl-proto :lft (&optional lft)
  (if lft (setf (slot-value 'lft) lft))
  (slot-value 'lft))

(defmeth cntl-proto :tp (&optional tp)
  (if tp (setf (slot-value 'tp) tp))
  (slot-value 'tp))

(defmeth cntl-proto :wth (&optional wth)
  (if wth (setf (slot-value 'wth) wth))
  (slot-value 'wth))

(defmeth cntl-proto :ht (&optional ht)
  (if ht (setf (slot-value 'ht) ht))
  (slot-value 'ht))

(defmeth cntl-proto :hilite (&optional hilite)
  (if hilite (setf (slot-value 'hilite) hilite))
  (slot-value 'hilite))

(defmeth cntl-proto :title (&optional title)
  (if title (setf (slot-value 'title) title))
  (slot-value 'title))

(defmeth cntl-proto :part (&optional part)
  (if part (setf (slot-value 'part) part))
  (slot-value 'part))

(defmeth cntl-proto :wind (&optional wind)
  (if wind (setf (slot-value 'wind) wind))
  (slot-value 'wind))

(defmeth cntl-proto :isnew (lft tp wth ht title part wind)
  (send self :lft lft)
  (send self :wth wth)
  (send self :tp tp)
  (send self :ht ht)
  (send self :title title)
  (send self :part part)
  (send self :hilite 0)
  (send self :part 1)
  (send self :wind wind))

(defmeth cntl-proto :in-cntl (x y)
  (let* ((left (send self :lft))
         (wth (send self :wth))
         (right (+ left wth))
         (tp (send self :tp))
         (ht (send self :ht))
         (bottom (+ tp ht)))
    (if (and (>= x left) (<= x right) (>= y tp) (<= y bottom)) 1 0)))

(defmeth cntl-proto :frame-cntl ()
  (let* ((lft (send self :lft))
         (wth (send self :wth))
         (tp (send self :tp))
         (ht (send self :ht))
         (w (send self :wind)))
    (send w :frame-rect lft tp wth ht)))

(defmeth cntl-proto :redraw ()
  (let* ((draw nil)
         (hilite (send self :hilite)))
    (if (= hilite 1) (send self :draw-cntl-hilited) (send self :draw-cntl-unhilited))
    (call-next-method)))

(defmeth cntl-proto :do-click (x y m1 m2)
  (let* ((lft (send self :lft))
         (wth (send self :wth))
         (tp (send self :tp))
         (ht (send self :ht))
         (in-cntl (send self :in-cntl x y))
         (part (send self :part))
         (handled nil))
    (cond ((= in-cntl 1)
          (setf handled 't)
          (send self :handle-click x y m1 m2)))
   handled))

(defmeth cntl-proto :handle-click (x y m1 m2)
 (send self :which-part x y)
 (send self :hilite 1)
 (send self :redraw)
 (setf in (send self :while-mouse-button-down x y m1 m2))
 (if in (send self :released-in x y m1 m2) (send self :released-out x y m1 m2)))



(defmeth cntl-proto :while-mouse-button-down (x y m1 m2)
(let* ((left (send self :lft))
       (wth (send self :wth))
       (right (+ left wth))
       (top (send self :tp))
       (ht (send self :ht))
       (bottom (+ top ht))
       (w (send self :wind))
       (cntl self)
       (in 't))
  (send w :while-button-down 
        #'(lambda (x y)
            (if in (send self :press-action))
            (cond ((and (>= x left) (<= x right) (>= y top) (<= y bottom) (not in))
                   (send self :hilite 1)
                   (send self :redraw)
                   (setf in 't)))
            (cond ((and (not (and (>= x left) (<= x right) (>= y top) (<= y bottom))) in)
                   (send self :hilite 0)
                   (send self :redraw)
                   (setf in nil)))) nil)
  in))


(defmeth cntl-proto :released-in (x y m1 m2)
  (cond 
    ((and (not m1) (not m2))
     (send self :no-mod-click x y m1 m2))
    ((and m1 (not m2))
     (send self :mod1-click x y m1 m2))
    ((and (not m1) m2)
     (send self :mod2-click x y m1 m2))
    ((and m1 m2)
     (send self :mod12-click x y m1 m2))))


;; Methods to override


(defmeth cntl-proto :draw-cntl-hilited ()
  nil)

(defmeth cntl-proto :draw-cntl-unhilited ()
  nil)

(defmeth cntl-proto :released-out (x y m1 m2)
nil)
    
 (defmeth cntl-proto :no-mod-click (x y m1 m2)
 nil)

(defmeth cntl-proto :mod1-click (x y m1 m2)
 nil)

(defmeth cntl-proto :mod2-click (x y m1 m2)
 nil)

(defmeth cntl-proto :mod12-click (x y m1 m2)
 nil)

(defmeth cntl-proto :press-action ()
nil)

(defmeth cntl-proto :which-part (x y)
nil)
    
      
  




















