;;;;
;;;; Variable overlay Prototype
;;;;

(defproto variable-overlay-proto '(pos var state label
                                       x-center y-center x y)
  nil graph-overlay-proto)

(defmeth variable-overlay-proto :isnew (pos var state)
  (setf (slot-value 'pos) pos)
  (setf (slot-value 'x-center) (+ (* (floor (/ pos 5)) 50) 24))
  (setf (slot-value 'y-center) (+ 52 (* (- pos (* (floor (/ pos 5)) 5)) 64)))
  (setf (slot-value 'var) (eval var))
  (setf (slot-value 'state) state)
  (let ((label (send (eval var) :name)))
    (setf (slot-value 'label) label)))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth variable-overlay-proto :pos ()
  (slot-value 'pos))

(defmeth variable-overlay-proto :var ()
  (slot-value 'var))

(defmeth variable-overlay-proto :state ()
  (slot-value 'state))

;;;
;;; Other Methods
;;;

(defmeth variable-overlay-proto :redraw ()
  (let* ((plot (send self :graph))
         (xydata (send plot :dataset))
         (at (send xydata :at))
         (bt (send xydata :bt))
         (pos (slot-value 'pos))
         (x-center (slot-value 'x-center))
         (x-start (- x-center 25))
         (y-center (slot-value 'y-center))
         (y-start (- y-center 27)))
    (send plot :erase-rect x-start y-start 51 65)
    (send plot :frame-rect x-start y-start 51 65)
    (send plot :draw-string (format nil "~s" (slot-value 'state))
          (+ x-start 3) (+ y-start 10))
    (send plot :draw-string (format nil "~s" (slot-value 'label))
          (+ x-start 3) (+ y-start 63))
    (send plot :draw-line x-center (+ y-center 2)
          (floor (+ x-center (* (select at pos) 24)))
          (floor (- (+ y-center 2) (* (select bt pos) 24))))
    (send plot :draw-point x-center (+ y-center 2))
    (send plot :frame-oval (+ x-start 1) (+ y-start 5) 49 49)))

(defmeth variable-overlay-proto :in-var (pos x y)
(let* ((x-start (- (slot-value 'x-center) 25))
         (y-start (- (slot-value 'y-center) 28)))
    (and (<= x-start x (+ x-start 50)) (<= y-start y (+ y-start 65)))))

(defmeth variable-overlay-proto :in-what (pos x y)
  (let* ((plot (send self :graph))
         (xydata (send plot :dataset))
         (var-states (send xydata :states))
         (old-state (slot-value 'state))
         (x-center (slot-value 'x-center))
         (y-center (slot-value 'y-center))
         (x-dev (- x x-center))
         (y-dev (- y y-center))
         (k (/ 1 (sqrt 2)))
         (hyp (sqrt (+ (^ x-dev 2) (^ y-dev 2))))
         (ratio (/ x-dev hyp)))
    (cond
      ((and (>= ratio k) (< 5 hyp 25)) (setf state 'X))
      ((and (< (abs ratio) k) (< y-dev 0) (< 5 hyp 25)) (setf state 'Y))
      ((< 5 hyp 25) (setf state 'A))
      ((<= hyp 5) (setf state 'O)))
    (if (not (eql old-state state))
        (progn
         (setf (slot-value 'state) state)
         (setf (select var-states pos) state)
         (send xydata :states var-states)))))

(defmeth variable-overlay-proto :do-click (x y m1 m2)
  (let* ((plot (send self :graph))
         (pos (slot-value 'pos))
         (in-var (send self :in-var pos x y)))
    (if in-var
        (progn
         (send self :in-what pos x y)
         (send self :redraw)))
    in-var))

;;;;
;;;;
;;;; Variable line overlay Prototype
;;;;
;;;;

(defproto variable-line-overlay-proto '(pos x-center y-center)
  nil graph-overlay-proto)

(defmeth variable-line-overlay-proto :isnew (pos)
  (setf (slot-value 'pos) pos)
  (setf (slot-value 'x-center) (+ (* (floor (/ pos 5)) 50) 24))
  (setf (slot-value 'y-center) (+ 52 (* (- pos (* (floor (/ pos 5)) 5)) 64))))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth variable-line-overlay-proto :pos ()
  (slot-value 'pos))

;;;
;;; Other Methods
;;;

(defmeth variable-line-overlay-proto :redraw ()
  (let* ((plot (send self :graph))
         (xydata (send plot :dataset))
         (at (send xydata :at))
         (bt (send xydata :bt))
         (pos (slot-value 'pos))
         (x-center (slot-value 'x-center))
         (x-start (- x-center 25))
         (y-center (slot-value 'y-center))
         (y-start (- y-center 27)))
    (send plot :erase-oval (+ x-start 2) (+ y-start 6) 47 47)
    (send plot :draw-line x-center (+ y-center 2)
          (floor (+ x-center (* (select at pos) 24)))
          (floor (- (+ y-center 2) (* (select bt pos) 24))))
    (send plot :draw-point x-center (+ y-center 2))))
