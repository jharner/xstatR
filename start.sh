#!/bin/bash

while [[ $# -gt 0 ]]
do
key=$1

case $key in
	-b|--build|build)
	docker build -t xstat-r:0.5.0 .
	docker tag xstat-r:0.5.0 xstat-r:latest
	;;
	*)
	echo "unknown option $key"
	exit 1
	;;
esac
shift
done 

docker run -d --rm --name xstat-rsh xstat-r


