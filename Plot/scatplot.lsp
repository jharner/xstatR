;;
;; Scatterplot proto
;;

(defproto scatplot-proto () () (list plot-mixin scatterplot-proto))

; Constructor function

(defun make-scatplot (x-var y-var dataset)
  (let* ((obs (send dataset :observations))
	 		(var-names (send dataset :variable-names))
	 		(x-pos (position x-var var-names))
	 		(y-pos (position y-var var-names))
	 		(plots (send dataset :plots))
	 		(graph (send scatplot-proto
		      		:new (select var-names (list x-pos y-pos)) dataset)))
	(send graph :use-color t)
    (send graph :new-menu)
    (send graph :add-observations obs)
    (send graph :adjust-to-data)
    (setf plots (cons graph plots))
    (send dataset :slot-value 'plots plots)
    graph))
