(load "Dataset/dataset.lsp")
(load "Dataset/dataset-mixin.lsp")
;(load "Dataset/dataset-menu.lsp")
(load "Dataset/dataset-browser.lsp")
(load "Plot/plot.lsp")
;(load "Plot/scatplot.lsp")
(load "Rsupport.lsp")
(setf R (send Rengine-proto :new))
(load "Model/model.lsp")
(load-dataset "stackloss" :rEngine R)
;(load "Datasets/stackloss/stackloss.lsp")

