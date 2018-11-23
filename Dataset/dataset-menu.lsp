;;
;; Data Browser Menus and Items
;;

;;
;; Stat Menu and Items
;;

(defproto stat-menu-proto () () menu-proto "Stat Menu")

(defmeth stat-menu-proto :isnew (browser)
  (call-next-method "Stat")
  (let ((dataset (send browser :dataset)))
   	(send self :append-items
          (send menu-item-proto :new "Plot")
          (send dash-item-proto :new)
          (send menu-item-proto :new "yPlot"
                :action #'(lambda () (make-yPlot dataset)))
          (send menu-item-proto :new "xyPlot"
                :action #'(lambda () (make-xyPlot dataset)))
          (send dash-item-proto :new)
          (send menu-item-proto :new "y~x...Model"
                :action #'(lambda () (send model-win-proto :new dataset)))
        )
  )
)

;;
;; Var Menu and Items
;;

(defproto var-menu-proto () () menu-proto "Variable Menu")

(defmeth var-menu-proto :isnew (browser)
  (call-next-method "Variable")
  (let ((dataset (send browser :dataset)))
    (send self :append-items
          (send menu-item-proto :new "Var")
          (send dash-item-proto :new)
          (send menu-item-proto :new "New Variable..."
                :action #'(lambda () nil))
          (send dash-item-proto :new)
          (send menu-item-proto :new "Clear Roles"
                :action #'(lambda () nil))
          (send menu-item-proto :new "Clear X Roles"
                :action #'(lambda () nil))
          (send menu-item-proto :new "Clear Y Roles"
                :action #'(lambda () nil)))))
                
;;
;; VarType Menu and Items
;;

(defproto type-menu-proto '() () menu-proto "Type Menu")

(defmeth type-menu-proto :isnew (var-view)
  (call-next-method "Type")
  (let ((var (send var-view :variable)))
  	(send self :append-items
     	  (send menu-item-proto :new "Type")
      	  (send dash-item-proto :new)
       	  (send menu-item-proto :new "Numeric"
         		:action #'(lambda () (send var :type 'N)))
       	  (send menu-item-proto :new "Categoric"
         		:action #'(lambda () (send var :type 'C)))
       	  (send dash-item-proto :new)
       	  (send menu-item-proto :new "Label"
         		:action #'(lambda ()
        			(let ((owner (send var :owner)))
          				(send owner :label var)))))))

