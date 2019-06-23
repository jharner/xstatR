(load "Rsupport.lsp")
(setf R (send Rengine-proto :new))

;(if (not (find "StatObj" *modules* :test #'equal))
;    (load "StatObj/statobj.lsp"))
