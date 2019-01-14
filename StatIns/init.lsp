(require ":StatIns:distwind.lsp")
(setf nc (make-color 'GREY .80 .80 .80))

;;
;; Make a distribution window
;;
;; 0 = Normal
;; 1 = t
;; 2 = F
;; 3 = Chi-Square
;; 4 = Binomial
;; 5 = Poisson

;;(setf dw (make-distn-window 0))
;;(send dw :back-color 'BLACK)
;;(send dw :draw-color 'WILL)

;;
;; Make a sampling window
;;
;; 0 = Normal
;; 1 = Chi-Square

;;(setf sw (make-samp-window 1))
;;(send sw :back-color 'BLACK)
;;(send sw :draw-color 'GREY)

;;(setf cw (make-ci-window))
(load ":IntroStat:distwind.lsp")
(setf pw (make-power-window))


