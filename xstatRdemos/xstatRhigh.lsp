(setf iris (send R :data "iris"))
(send R :save-dataset "iris.data" iris)
(send R :call "ls()")
(setf x (send R :call "iris.data"))