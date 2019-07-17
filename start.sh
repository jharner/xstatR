#!/bin/bash

while [[ $# -gt 0 ]]
do
key=$1

case $key in
	-b|--build|build)
	docker build -t xstat-r:0.5.0 .
	docker tag xstat-r:0.5.0 xstat-r:latest
	;;
	-r|--run|run)
	echo "start xstatR"
        xhost + 127.0.0.1
        docker run -it --rm --env="DISPLAY=host.docker.internal:0" xstat-r
	;;
    *)
        echo "unknown option"
        exit 1
        ;;
esac
shift
done 
