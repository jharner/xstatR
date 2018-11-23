## xstatR Development Repository

This repo is used to build a docker image for R and xlisp-stat and communications bridge between them.

Assuming you are in your xstatR local repo, execute:  

./start.sh build  

to build the image. After the initial build, you only needs to run:  

./start.sh  

unless you have modified the Dockerfile or removed the image.

To login run:

docker exec -it xstat-rsh bash 

The container runs in the background and is automatically stopped and removed once you exited bash.



