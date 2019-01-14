;;;;
;;;;
;;;; Plot Overlays
;;;;
;;;;

;;;;
;;;; Mask overlay Prototype
;;;;

(defproto mask-overlay-proto '(x-start x-stop y-start y-stop)
                                       nil graph-overlay-proto )

(defmeth mask-overlay-proto :isnew (x-start x-stop y-start y-stop)
  (setf (slot-value 'x-start) x-start)
  (setf (slot-value 'x-stop) x-stop)
  (setf (slot-value 'y-start) y-start)
  (setf (slot-value 'y-stop) y-stop)
  (call-next-method))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth mask-overlay-proto :x-start (&optional x-start)
  (if x-start (setf (slot-value 'x-start) x-start))
  (slot-value 'x-start))

(defmeth mask-overlay-proto :x-stop (&optional x-stop)
  (if x-stop (setf (slot-value 'x-stop) x-stop))
  (slot-value 'x-stop))

(defmeth mask-overlay-proto :y-start (&optional y-start)
  (if y-start (setf (slot-value 'y-start) y-start))
  (slot-value 'y-start))

(defmeth mask-overlay-proto :y-stop (&optional y-stop)
  (if y-stop (setf (slot-value 'y-stop) y-stop))
  (slot-value 'y-stop))

;;;
;;; Other Methods
;;;

(defmeth mask-overlay-proto :in-region (x y)
  (and (<= (slot-value 'x-start) x (slot-value 'x-stop))
       (<= (slot-value 'y-start) y (slot-value 'y-stop))))

(defmeth mask-overlay-proto :do-click (x y m1 m2)
  (let ((in-region (send self :in-region x y)))
    (if in-region t)))

;;
;; Draws the line separating the overlays and the graph
;;

(defmeth mask-overlay-proto :redraw ()
  (let* ((plot (send self :graph))
         (x1 (send self :x-start))
         (x2 (send self :x-stop (send plot :canvas-width)))
         (y2 (- (send self :y-stop) 2)))
    (send plot :draw-line x1 y2 x2 y2)))

;;;; Border Overlay Prototype

(defproto border-overlay-proto '(offset)
  nil graph-overlay-proto)

;;; Slot Accessors and Mutators

(defmeth border-overlay-proto :offset (&optional offset)
  (if offset (setf (slot-value 'offset) offset))
  (slot-value 'offset))

;;; Other Methods

(defmeth border-overlay-proto :do-click (x y m1 m2)
  (let* ((plot (send self :graph))
         (offset (slot-value 'offset))
         (width (send plot :canvas-width)))
    (and (<= 0 x width) (<= 0 y offset))))

;;;;
;;;; Status overlay Prototype
;;;;

(defproto  status-overlay-proto '(x-start info) nil graph-overlay-proto)

(defmeth status-overlay-proto :isnew (x-start info)
  (setf (slot-value 'x-start) x-start)
  (setf (slot-value 'info) info)
  (call-next-method))

;;;
;;; Other Methods
;;;

(defmeth status-overlay-proto :x-start (&optional set)
  (if set (setf (slot-value 'x-start) set))
  (slot-value 'x-start))

(defmeth status-overlay-proto :info (&optional set)
  (if set (setf (slot-value 'info ) set ))
  (slot-value 'info ))

(defmeth status-overlay-proto :redraw ()
  (let* ((plot (send self :graph))
         (width (send plot :canvas-width))
         (status (slot-value 'info))
         (x-start (slot-value 'x-start)))
    (send plot :erase-rect  x-start 26 width 40)
    (send plot :draw-string status
          (+ x-start (round (/ (- (- width x-start)
                                  (send plot :text-width status)) 2))) 38)
    t))

;;;;
;;;;
;;;; Menu overlay Prototype
;;;;
;;;;

(defproto menu-overlay-proto '(position type string) nil graph-overlay-proto)

(defmeth menu-overlay-proto :isnew (position type string)
  (setf (slot-value 'position) position)
  (setf (slot-value 'type) type)
  (setf (slot-value 'string) string))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth menu-overlay-proto :position (&optional position)
  (if position (setf (slot-value 'position) position))
  (slot-value 'position))

(defmeth menu-overlay-proto :type ()
  (slot-value 'type))

(defmeth menu-overlay-proto :string (&optional string)
  (if string (setf (slot-value 'string) string))
  (slot-value 'string))

;;;
;;; Other Methods
;;;

(defmeth menu-overlay-proto :redraw ()
  (let* ((plot (send self :graph))
         (position (slot-value 'position))
         (start (* position 14))
         (string (slot-value 'string)))
    (send plot :frame-rect (+ start 5) 5
          (+ (* 5 *line-height*) 5) *line-height*)
    (send plot :draw-line (+ start 8) (+ *line-height* 5)
          (+ (* 5 *line-height*) 9) (+ *line-height* 5))
    (send plot :draw-line (+ (* 5 *line-height*) 10) 8
          (+ (* 5 *line-height*) 10) (+ *line-height* 5))
    (send plot :draw-string string (+ start 10) 16)
    (send plot :paint-poly (list (list (+ (* 4 *line-height*) 5) 9)
                                 (list 14 0) (list -7 7) (list -7 -7)) nil)))
                                 

(defmeth menu-overlay-proto :in-menu (x y)
  (let ((start (* (slot-value 'position) 14)))
    (and (< start x (+ start 80)) (< 5 y 20))))

(defmeth menu-overlay-proto :do-click (x y m1 m2)
  (let ((in-menu (send self :in-menu x y))
        (start (* (slot-value 'position) 14))
        (plot (send self :graph)))
    (if in-menu (send (send plot :plot-menu) :popup (+ start 5) 5 plot))))

;;;;
;;;; Button Overlay Prototype
;;;;

(defproto button-overlay-proto '(position bitmap inv-bitmap)
  nil graph-overlay-proto)

;;; Slot Accessors and Mutators

(defmeth button-overlay-proto :position (&optional position)
  (if position (setf (slot-value 'position) position))
  (slot-value 'position))

(defmeth button-overlay-proto :bitmap (&optional bitmap)
  (if bitmap (setf (slot-value 'bitmap) bitmap))
  (slot-value 'bitmap))

(defmeth button-overlay-proto :inv-bitmap (&optional inv-bitmap)
  (if inv-bitmap (setf (slot-value 'inv-bitmap) inv-bitmap))
  (slot-value 'inv-bitmap))

;;; Other Methods

(defmeth button-overlay-proto :in-box (position x y)
  (let ((start (* position 14)))
    (and (< start x (+ start 15)) (< 5 y (+ 20)))))

(defmeth button-overlay-proto :redraw (plot position bitmap)
  (let ((start (* position 14)))
    (send plot :frame-rect start 5 15 15)
    (send plot :draw-bitmap bitmap (+ start 1) 6)))

;;;;
;;;;
;;;; Select Button Overlay Proto
;;;;
;;;;

(defproto select-button-overlay-proto nil nil button-overlay-proto)

(defmeth select-button-overlay-proto :isnew (position)
  (send self :position position)
  (send self :bitmap
        '#2a((0 0 0 0 0 0 0 0 0 0 0 0)
             (0 1 0 0 0 0 0 0 0 0 0 0)
             (0 1 1 0 0 0 0 0 0 0 0 0)
             (0 1 1 1 1 0 0 0 0 0 0 0)
             (0 1 1 1 1 1 0 0 0 0 0 0)
             (0 1 1 1 0 0 0 0 0 0 0 0)
             (0 1 0 0 0 0 0 0 0 0 0 0)
             (0 0 0 0 1 0 0 0 0 0 0 0)
             (0 0 0 0 0 0 0 0 0 0 0 0)
             (0 0 0 0 0 1 0 0 0 0 0 0)
             (0 0 0 0 0 0 0 0 0 0 0 0)
             (0 0 0 0 0 0 1 0 0 0 0 0))))

;;;
;;; Other Methods
;;;

(defmeth select-button-overlay-proto :redraw ()
  (let ((plot (send self :graph))
        (position (slot-value 'position))
        (bitmap (slot-value 'bitmap)))
    (call-next-method plot position bitmap)))

(defmeth select-button-overlay-proto :do-click (x y m1 m2)
  (let* ((position (slot-value 'position))
         (plot (send self :graph))
         (in-box (send self :in-box position x y)))
    (if in-box (send plot :mouse-mode 'selecting))))


;;;;
;;;;
;;;; Brush Button Overlay Proto
;;;;
;;;;

(defproto brush-button-overlay-proto nil nil button-overlay-proto)

(defmeth brush-button-overlay-proto :isnew (position)
  (send self :position position)
  (send self :bitmap
        '#2a((0 0 0 0 0 0 0 0 0 0 0 0)
             (0 1 1 1 1 1 1 1 1 1 0 0)
             (0 0 1 0 1 0 1 0 1 0 1 0)
             (0 0 1 0 1 0 1 0 1 0 1 0)
             (0 0 1 0 0 0 0 0 0 0 1 0)
             (0 0 1 0 0 0 0 0 0 0 1 0)
             (0 0 1 1 1 1 1 1 1 1 1 0)
             (0 0 1 0 0 0 0 0 0 0 1 0)
             (0 0 1 1 1 1 1 1 1 1 1 0)
             (0 0 0 0 0 1 0 1 0 0 0 0)
             (0 0 0 0 0 1 0 1 0 0 0 0)
             (0 0 0 0 0 0 1 0 0 0 0 0))))
;;;
;;; Other Methods
;;;

(defmeth brush-button-overlay-proto :redraw ()
  (let ((plot (send self :graph))
        (position (slot-value 'position))
        (bitmap (slot-value 'bitmap)))
    (call-next-method plot position bitmap)))

(defmeth brush-button-overlay-proto :do-click (x y m1 m2)
  (let* ((position (slot-value 'position))
         (plot (send self :graph))
         (in-box (send self :in-box position x y)))
    (if in-box (send plot :mouse-mode 'brushing))))

;;;;
;;;;
;;;; Label Button Overlay Proto
;;;;
;;;;

(defproto label-button-overlay-proto '(labels) nil button-overlay-proto)

(defmeth label-button-overlay-proto :isnew (position)
  (send self :position position)
  (setf (slot-value 'labels) nil)
  (send self :bitmap
        '#2a((0 0 0 0 0 0 0 0 0 0 0 0)
             (0 1 1 0 0 0 0 0 0 0 0 0)
             (0 1 1 0 0 0 0 0 0 0 0 0)
             (0 1 1 0 0 0 0 0 0 0 0 0)
             (0 1 1 0 0 0 0 0 0 0 0 0)
             (0 1 1 0 0 0 0 0 0 0 0 0)
             (0 1 1 0 0 0 0 0 0 0 0 0)
             (0 1 1 0 0 0 0 0 0 0 0 0)
             (0 1 1 0 0 0 0 0 0 0 0 0)
             (0 1 1 1 1 1 1 1 1 1 0 0)
             (0 1 1 1 1 1 1 1 1 1 0 0)
             (0 0 0 0 0 0 0 0 0 0 0 0)))
  (send self :inv-bitmap
        '#2a((1 1 1 1 1 1 1 1 1 1 1 1)
             (1 0 0 1 1 1 1 1 1 1 1 1)
             (1 0 0 1 1 1 1 1 1 1 1 1)
             (1 0 0 1 1 1 1 1 1 1 1 1)
             (1 0 0 1 1 1 1 1 1 1 1 1)
             (1 0 0 1 1 1 1 1 1 1 1 1)
             (1 0 0 1 1 1 1 1 1 1 1 1)
             (1 0 0 1 1 1 1 1 1 1 1 1)
             (1 0 0 1 1 1 1 1 1 1 1 1)
             (1 0 0 0 0 0 0 0 0 0 1 1)
             (1 0 0 0 0 0 0 0 0 0 1 1)
             (1 1 1 1 1 1 1 1 1 1 1 1))))

;;;
;;; Other Methods
;;;

(defmeth label-button-overlay-proto :redraw ()
  (let ((position (slot-value 'position))
        (plot (send self :graph))
        (labels (slot-value 'labels))
        (bitmap (slot-value 'bitmap))
        (inv-bitmap (slot-value 'inv-bitmap)))
    (if labels
        (call-next-method plot position inv-bitmap)
        (call-next-method plot position bitmap))))

(defmeth label-button-overlay-proto :do-click (x y m1 m2)
  (let* ((position (slot-value 'position))
         (plot (send self :graph))
         (labels (slot-value 'labels))
         (in-box (send self :in-box position x y)))
    (when in-box
          (send plot :showing-labels (not labels))
          (setf (slot-value 'labels) (not labels))
          (send self :redraw))))

;;;; Animate Button Overlay Proto

(defproto animate-button-overlay-proto nil nil button-overlay-proto)

(defmeth animate-button-overlay-proto :isnew (position)
  (send self :position position)
  (send self :bitmap
        '#2a((0 0 0 0 0 0 0 0 0 0 0 0)
             (0 0 0 0 0 0 0 0 0 0 0 0)
             (0 0 0 0 0 0 0 0 0 0 0 0)
             (0 0 0 1 0 0 0 0 0 1 0 0)
             (0 0 1 1 0 0 0 0 0 1 1 0)
             (0 1 1 1 1 1 0 1 1 1 1 1)
             (0 1 1 1 1 1 0 1 1 1 1 1)
             (0 0 1 1 0 0 0 0 0 1 1 0)
             (0 0 0 1 0 0 0 0 0 1 0 0)
             (0 0 0 0 0 0 0 0 0 0 0 0)
             (0 0 0 0 0 0 0 0 0 0 0 0)
             (0 0 0 0 0 0 0 0 0 0 0 0))))

;;; Other Methods

(defmeth animate-button-overlay-proto :redraw ()
  (let ((plot (send self :graph))
        (position (slot-value 'position))
        (bitmap (slot-value 'bitmap)))
    (call-next-method plot position bitmap)))

(defmeth animate-button-overlay-proto :do-click (x y m1 m2)
  (let* ((position (slot-value 'position))
         (plot (send self :graph)))
    (when (send self :in-box position x y)
          (send plot :while-button-down
                #'(lambda (x y)
                    (send plot :animate (> x (+ 7 (* position 14)))))
                nil))))

;;;;
;;;;
;;;;  Stop-Button-Overlay-Proto  
;;;;
;;;;

(defproto stop-button-overlay-proto nil nil button-overlay-proto)

(defmeth stop-button-overlay-proto :isnew (position)
   (send self :position position)
   (send self :bitmap
         '#2a((0 0 0 0 0 0 0 0 0 0 0 0)
              (0 0 0 1 1 1 1 1 1 0 0 0)
              (0 0 1 1 1 0 0 1 1 1 0 0)
              (0 0 1 1 0 0 0 0 0 0 0 0)
              (0 0 1 1 1 0 0 0 0 0 0 0)
              (0 0 0 1 1 1 1 0 0 0 0 0)
              (0 0 0 0 0 1 1 1 1 0 0 0)
              (0 0 0 0 0 0 0 1 1 1 0 0)
              (0 0 0 0 0 0 0 0 1 1 0 0)
              (0 0 1 1 1 0 0 1 1 1 0 0)
              (0 0 0 1 1 1 1 1 1 0 0 0)
              (0 0 0 0 0 0 0 0 0 0 0 0))))

;;;;
;;;; Other Methods 
;;;;

(defmeth stop-button-overlay-proto :redraw ()
  (let ((plot (send self :graph))
        (position (slot-value 'position))
        (bitmap (slot-value 'bitmap)))
    (call-next-method plot position bitmap)))

(defmeth stop-button-overlay-proto :do-click (x y m1 m2)
  (let* ((position (slot-value 'position))
         (in-box (send self :in-box position x y)))
    (when in-box
          (let* ((plot (send self :graph))
                 (xydata (send plot :dataset)))
          (when (= (send plot :move-it) 4)
                (do (( i 0 (+ i 1))
                     (history (send xydata :history) (rest history))
                     (depth (send plot :depth) depth))
                    ((= depth i) (send xydata :history history))))
          (send plot :move-it 0)
          (send plot :idle-on nil)
          (send (send plot :status-overlay)
                :info "Waiting for the next command")
          (send (send plot :status-overlay) :redraw)))))


;;;;
;;;;
;;;; Symbol Button Overlay Proto
;;;;
;;;;

(defproto symbol-button-overlay-proto '(position) nil graph-overlay-proto)

(defmeth symbol-button-overlay-proto :isnew (position)
  (setf (slot-value 'position) position))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth symbol-button-overlay-proto :position (&optional position)
  (if position (setf (slot-value 'position) position))
  (slot-value 'position))

;;;
;;; Other Methods
;;;

(defmeth symbol-button-overlay-proto :which-symbol (position x y)
  (let* ((start (* position 14))
         (symbol
          (cond
            ((and (< start x (+ start 7)) (< 5 y 12)) 'dot)
            ((and (< (+ start 8) x (+ start 14)) (< 5 y 12)) 'dot1)
            ((and (< (+ start 15) x (+ start 22)) (< 5 y 12)) 'dot2)
            ((and (< (+ start 23) x (+ start 29)) (< 5 y 12)) 'dot3)
            ((and (< (+ start 30) x (+ start 37)) (< 5 y 12)) 'dot4)
            ((and (< (+ start 38) x (+ start 44)) (< 5 y 12)) 'disk)
            ((and (< start x (+ start 7)) (< 13 y 20)) 'diamond)
            ((and (< (+ start 8) x (+ start 14)) (< 13 y 20)) 'cross)
            ((and (< (+ start 15) x (+ start 22)) (< 13 y 20))  'square)
            ((and (< (+ start 23) x (+ start 29)) (< 13 y 20)) 'wedge1)
            ((and (< (+ start 30) x (+ start 37)) (< 13 y 20)) 'wedge2)
            ((and (< (+ start 38) x (+ start 44)) (< 13 y 20)) 'x))))
    symbol))
      
(defmeth symbol-button-overlay-proto :redraw ()
  (let* ((position (slot-value 'position))
         (plot (send self :graph))
         (start (* position 14)))
    (send plot :frame-rect start 5 45 15)
    (send plot :draw-symbol 'dot nil (+ start 3) 8)
    (send plot :draw-symbol 'dot1 nil (+ start 10) 8)
    (send plot :draw-symbol 'dot2 nil (+ start 17) 8)
    (send plot :draw-symbol 'dot3 nil (+ start 24) 8)
    (send plot :draw-symbol 'dot4 nil (+ start 31) 8)
    (send plot :draw-symbol 'disk nil (+ start 40) 9)
    (send plot :draw-symbol 'diamond nil (+ start 5) 16)
    (send plot :draw-symbol 'cross nil (+ start 12) 16)
    (send plot :draw-symbol 'square nil (+ start 18) 16)
    (send plot :draw-symbol 'wedge1 nil (+ start 26) 16)
    (send plot :draw-symbol 'wedge2 nil (+ start 33) 16)
    (send plot :draw-symbol 'x nil (+ start 40) 16)))

(defmeth symbol-button-overlay-proto :do-click (x y m1 m2)
  (let* ((position (slot-value 'position))
         (plot (send self :graph))
         (symbol (send self :which-symbol position x y)))
    (when symbol
          (send plot :adjust-points :symbol symbol))))

;;;;
;;;;
;;;; Color Button Overlay Proto
;;;;
;;;;

(defproto color-button-overlay-proto '(position) nil graph-overlay-proto)

(defmeth color-button-overlay-proto :isnew (position)
  (send self :position position))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth color-button-overlay-proto :position (&optional position)
  (if position (setf (slot-value 'position) position))
  (slot-value 'position))

;;;
;;; Other Methods
;;;

(defmeth color-button-overlay-proto :which-color (position x y)
  (let ((start (* position 14)))
    (cond
      ((and (< start x (+ start 7)) (< 5 y 12)) 'black)
      ((and (< (+ start 8) x (+ start 14)) (< 5 y 12)) 'white)
      ((and (< (+ start 15) x (+ start 22)) (< 5 y 12)) 'red)
      ((and (< (+ start 23) x (+ start 29)) (< 5 y 12)) 'green)
      ((and (< start x (+ start 7)) (< 13 y 20)) 'blue)
      ((and (< (+ start 8) x (+ start 14)) (< 13 y 20)) 'cyan)
      ((and (< (+ start 15) x (+ start 22)) (< 13 y 20)) 'magenta)
      ((and (< (+ start 23) x (+ start 29)) (< 13 y 20)) 'yellow))))

(defmeth color-button-overlay-proto :redraw ()
  (let* ((position (slot-value 'position))
         (plot (send self :graph))
         (color (send plot :draw-color))
         (start (* position 14)))
    (send plot :draw-color 'black)
    (send plot :paint-rect start 5 7 7)
    (send plot :draw-color 'white)
    (send plot :paint-rect (+ start 8) 5 7 7)
    (send plot :draw-color 'red)
    (send plot :paint-rect (+ start 15) 5 7 7)
    (send plot :draw-color 'green)
    (send plot :paint-rect (+ start 23) 5 7 7)
    (send plot :draw-color 'blue)
    (send plot :paint-rect start 13 7 7)
    (send plot :draw-color 'cyan)
    (send plot :paint-rect (+ start 8) 13 7 7)
    (send plot :draw-color 'magenta)
    (send plot :paint-rect (+ start 15) 13 7 7)
    (send plot :draw-color 'yellow)
    (send plot :paint-rect (+ start 23) 13 7 7)
    (send plot :draw-color color)))

(defmeth color-button-overlay-proto :do-click (x y m1 m2)
  (let* ((position (slot-value 'position))
         (plot (send self :graph))
         (color (send self :which-color position x y)))
    (when color
          (send plot :adjust-points :color color))))

;;;
;;; Name-List Overlay Prototype
;;;

(defproto name-list-overlay-proto '(width) nil graph-overlay-proto)

(defmeth name-list-overlay-proto :isnew (width)
  (setf (slot-value 'width) width))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth name-list-overlay-proto :width (&optional width)
  (if width (setf (slot-value 'width) width))
  (slot-value 'width))

;;;
;;; Other Methods
;;;

(defmeth name-list-overlay-proto :redraw ()
  (let* ((plot (send self :graph))
         (position (- (slot-value 'width) 28)))
    (send plot :draw-line (+ position 2) 5 (+ position 2) 19)
    (send plot :draw-line (+ position 5) 5 (+ position 5) 19)))

(defmeth name-list-overlay-proto :in-name (x y)
  (let ((start (- (slot-value 'width) 26)))
    (and (< start x (+ start 4)) (< 5 y 20))))

(defmeth name-list-overlay-proto :do-click (x y m1 m2)
  (let ((in-name (send self :in-name x y))
        (plot (send self :graph)))
    (when in-name
          (if (not (send plot :name-list))
              (display-data-list plot)))))

;;;
;;; Link Overlay Prototype
;;;

(defproto link-overlay-proto '(linked width) nil graph-overlay-proto)

(defmeth link-overlay-proto :isnew (width)
  (setf (slot-value 'linked) t)
  (setf (slot-value 'width) width))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth link-overlay-proto :linked (&optional linked)
  (if linked (setf (slot-value 'linked) linked))
  (slot-value 'linked))

(defmeth link-overlay-proto :width (&optional width)
  (if width (setf (slot-value 'width) width))
  (slot-value 'width))

;;; Other Methods

(defmeth link-overlay-proto :redraw ()
  (let* ((plot (send self :graph))
         (position (- (slot-value 'width) 14))
         (linked (send self :linked)))
    (if linked
        (send plot :paint-rect position 5 8 15)
        (send plot :frame-rect position 5 8 15))))

(defmeth link-overlay-proto :in-link (x y)
  (let ((start (- (slot-value 'width) 14)))
    (and (< start x (+ start 8)) (< 5 y 20))))

(defmeth link-overlay-proto :do-click (x y m1 m2)
  (let* ((in-link (send self :in-link x y))
         (plot (send self :graph))
         (linked (send self :linked)))
    (if in-link
        (send self :linked (not linked)))
    (send plot :redraw-overlays)))

