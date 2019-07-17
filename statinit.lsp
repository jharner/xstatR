(load "Dataset/dataset.lsp")
(load "Dataset/dataset-mixin.lsp")
(load "Dataset/dataset-menu.lsp")
(load "Dataset/dataset-browser.lsp")
(load "Plot/plot.lsp")
;(load "Plot/scatplot.lsp")
(load "Model/model.lsp")

(load "Rsupport.lsp")
(setf R (send Rengine-proto :new))

(load-dataset "stackloss" :rEngine R)
;(load "Datasets/stackloss/stackloss.lsp")

;(if (not (find "StatObj" *modules* :test #'equal))
;    (load "statobj.lsp"))
