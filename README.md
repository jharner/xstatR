## xstatR Development Repository

This repo is used to build a docker image for R and xlisp-stat and communications bridge between them.

Assuming you are in your xstatR local repo, execute:  

./start.sh build  

to build the image.

Prior to running xstatR, start an X11 terminal session and execute:

xhost +


Once clients are able to connect, execute:

./start.sh run  

to run the image.



