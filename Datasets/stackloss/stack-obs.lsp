(def loss '(42 37 37 28 18 18 19 20 15 14 14 13 11 12 8 7 8 8 9 15 15))
(def air '(80 80 75 62 62 62 62 62 58 58 58 58 58 58 50 50 50 50 50 56 70)) 
(def temp '(27 27 25 24 22 23 24 24 23 18 18 17 18 19 18 18 19 19 20 20 20))
(def conc '(89 88 90 87 87 87 93 93 87 80 89 88 82 93 89 86 72 79 80 82 91))

(defproto stack-obs '(air temp conc loss) () obs-proto)

(defmeth stack-obs :air () (slot-value 'air))
(defmeth stack-obs :temp () (slot-value 'temp))
(defmeth stack-obs :conc () (slot-value 'conc))
(defmeth stack-obs :loss () (slot-value 'loss))

(setf stack-data (read-data-var "StatR/stackloss.txt"))

(setf stack-var-list (first stack-data))
(setf stack-data (second stack-data))

(setf stack-obs-list
      (mapcar #'(lambda (air temp conc loss label)
		  (send stack-obs :new
			:air air
			:temp temp
			:conc conc
			:loss loss
			:label (format nil "~d" label)))
			(select stack-data 0) (select stack-data 1) (select stack-data 2)
			(select stack-data 3) (iseq 1 21)))
		
(setf stack-loss (make-data-set 'stack-loss stack-obs-list stack-var-list))