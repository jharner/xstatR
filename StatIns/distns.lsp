(provide ":StatIns:distns.lsp")
(require ":StatIns:symbols.lsp")

;;
;; General distribution proto
;;

(defproto distn-proto '(pdf sampling-pdf cdf quant lb ub wind name 
                            param-values param-names param-incrs param-buttons param-symbols
                            ineq x1 x2 q1) () )

(defmeth distn-proto :name (&optional name)
  (if name (setf (slot-value 'name) name))
  (slot-value 'name))

(defmeth distn-proto :q1 (&optional q1)
  (if q1 (setf (slot-value 'q1) q1))
  (slot-value 'q1))

(defmeth distn-proto :sample ()
  nil)

(defmeth distn-proto :check-params ()
  (let* ((q (select (send self :param-values) 2))
         (q-incr (select (send self :param-incrs) 2))
         (s-size (select (send self :param-values) 3))
         (s-incr (select (send self :param-incrs) 3)))
    (cond ((< q 0.000001)
           (setf (select (send self :param-values) 2) 0.000001)))
    (cond ((> q .999999)
           (setf (select (send self :param-values) 2) .999999)))
    (cond ((< s-size 1)
           (setf (select (send self :param-values) 3) 1)))
    (setf (select (send self :param-incrs) 3) (floor (select (send self :param-incrs) 3)))))
 
    

(defmeth distn-proto :shade-prob ()
  nil)

(defmeth distn-proto :shade-quant ()
  (let* ((dumb (send self :check-params))
         (w (send self :wind))
         (quant (send self :quant))
         (co (send w :content-origin))
         (q1 (select (send self :param-values) 2))
         (x1 (funcall quant q1))
         (pt (send w :canvas-to-real (select co 0) (select co 1)))
         (xo (select pt 0)))
    (send w :add-lines (list (list x1 x1 xo) (list 0 q1 q1)) :color 'GREEN)))

(defmeth distn-proto :pdf (&optional pdf)
  (if pdf (setf (slot-value 'pdf) pdf))
  (slot-value 'pdf))

(defmeth distn-proto :cdf (&optional cdf)
  (if cdf (setf (slot-value 'cdf) cdf))
  (slot-value 'cdf))

(defmeth distn-proto :sampling-pdf (&optional sampling-pdf)
  (if sampling-pdf (setf (slot-value 'sampling-pdf) sampling-pdf))
  (slot-value 'sampling-pdf))

(defmeth distn-proto :param-names (&optional param-names)
  (if param-names (setf (slot-value 'param-names) param-names))
  (slot-value 'param-names))

(defmeth distn-proto :param-values (&optional param-values)
  (if param-values (setf (slot-value 'param-values) param-values))
  (slot-value 'param-values))

(defmeth distn-proto :param-incrs (&optional param-incrs)
  (if param-incrs (setf (slot-value 'param-incrs) param-incrs))
  (slot-value 'param-incrs))

(defmeth distn-proto :param-buttons (&optional param-buttons)
  (if param-buttons (setf (slot-value 'param-buttons) param-buttons))
  (slot-value 'param-buttons))

(defmeth distn-proto :param-symbols (&optional param-symbols)
  (if param-symbols (setf (slot-value 'param-symbols) param-symbols))
  (slot-value 'param-symbols))

(defmeth distn-proto :quant (&optional quant)
  (if quant (setf (slot-value 'quant) quant))
  (slot-value 'quant))

(defmeth distn-proto :lb (&optional lb)
  (if lb (setf (slot-value 'lb) lb))
  (slot-value 'lb))

(defmeth distn-proto :ub (&optional ub)
  (if ub (setf (slot-value 'ub) ub))
  (slot-value 'ub))

(defmeth distn-proto :wind (&optional wind)
  (if wind (setf (slot-value 'wind) wind))
  (slot-value 'wind))

(defmeth distn-proto :ineq (&optional ineq)
  (if ineq (setf (slot-value 'ineq) ineq))
  (slot-value 'ineq))

(defmeth distn-proto :x1 (&optional x1)
  (if x1 (setf (slot-value 'x1) x1))
  (slot-value 'x1))

(defmeth distn-proto :x2 (&optional x2)
  (if x2 (setf (slot-value 'x2) x2))
  (slot-value 'x2))

(defmeth distn-proto :isnew (pdf sampling-pdf cdf quant wind)
  (send self :pdf pdf)
  (send self :sampling-pdf sampling-pdf)
  (send self :cdf cdf)
  (send self :quant quant)
  (send self :wind wind)
  (send self :ineq 0)
  (send self :q1 .5)
  (send self :x1 (funcall (send self :quant) .025))
  (send self :x2 (funcall (send self :quant) .975)))

(defmeth distn-proto :mean ()
  nil)

(defmeth distn-proto :std-dev ()
  nil)

(defmeth distn-proto :quantile (q)
  (funcall (send self :quant) q))

(defmeth distn-proto :leq-prob (x)
  nil)

(defmeth distn-proto :l-prob (x)
  nil)

(defmeth distn-proto :geq-prob (x)
  nil)

(defmeth distn-proto :g-prob (x)
  nil)

(defmeth distn-proto :eq-prob (x)
  nil)

;;
;; Methods for  Prob( a ² x ² b)
;;

(defmeth distn-proto :btwn-nn (x1 x2)
  (let* ((prob (- (send self :leq-prob x2) (send self :l-prob x1))))
   (if (< prob 0) 0 prob)))

(defmeth distn-proto :btwn-sn (x1 x2)
  (let* ((prob (- (send self :leq-prob x2) (send self :leq-prob x1))))
   (if (< prob 0) 0 prob)))

(defmeth distn-proto :btwn-ns (x1 x2)
  (let* ((prob (- (send self :l-prob x2) (send self :l-prob x1))))
   (if (< prob 0) 0 prob))) 

(defmeth distn-proto :btwn-ss (x1 x2)
  (let* ((prob (- (send self :l-prob x2) (send self :leq-prob x1))))
   (if (< prob 0) 0 prob)))

;;
;; Methods for  Prob( x ² a  or  x ³ b)
;;

(defmeth distn-proto :out-nn (x1 x2)
  (let* ((prob (+ (send self :leq-prob x1) (send self :geq-prob x2))))
   (if (> prob 1) 1 prob)))

(defmeth distn-proto :out-sn (x1 x2)
  (let* ((prob (+ (send self :l-prob x1) (send self :geq-prob x2))))
   (if (> prob 1) 1 prob)))

(defmeth distn-proto :out-ns (x1 x2)
  (let* ((prob (+ (send self :leq-prob x1) (send self :g-prob x2))))
   (if (> prob 1) 1 prob))) 

(defmeth distn-proto :out-ss (x1 x2)
  (let* ((prob (+ (send self :l-prob x1) (send self :g-prob x2))))
   (cond 
     ((> prob 1)
      1)
     ((= x1 x2)
      0)
     ('T
      prob))))
     

(defmeth distn-proto :calc-prob ()
  (let* ((ineq (send self :ineq))
         (x1 (send self :x1))
         (x2 (send self :x2))
         (prob (cond 
                      ((= ineq 0)
                       (send self :eq-prob x1))
                      ((= ineq 1)
                       (send self :leq-prob x1))
                      ((= ineq 2)
                       (send self :l-prob x1))
                      ((= ineq 3)
                       (send self :geq-prob x1)) 
                      ((= ineq 4)
                       (send self :g-prob x1)) 
                      ((= ineq 5)
                       (send self :btwn-nn x1 x2))
                      ((= ineq 6)
                       (send self :btwn-ns x1 x2))
                      ((= ineq 7)
                       (send self :btwn-sn x1 x2))
                      ((= ineq 8)
                       (send self :btwn-ss x1 x2))
                      ((= ineq 9)
                       (send self :out-nn x1 x2))
                      ((= ineq 10)
                       (send self :out-ns x1 x2))
                      ((= ineq 11)
                       (send self :out-sn x1 x2))
                      ((= ineq 12)
                       (send self :out-ss x1 x2)))))
    prob))