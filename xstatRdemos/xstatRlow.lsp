(setf y-list (callR "rnorm(50)"))
(setf y (first y-list))
(histogram y)
(saveToR "y" y-list)