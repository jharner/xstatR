FROM ubuntu:18.04
MAINTAINER Jim Harner ejharner@gmail.com

# update apt
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && apt-get install -y r-base r-base-dev gcc make libc6-dev libx11-dev

RUN mkdir -p /xstatR/build
COPY ./*.lsp /xstatR/
COPY ./xlispstatR /xstatR/xlispstatR
COPY ./Dataset /xstatR/Dataset
COPY ./Datasets /xstatR/Datasets
COPY ./Model /xstatR/Model
COPY ./Plot /xstatR/Plot
COPY ./StatObj /xstatR/StatObj
WORKDIR /xstatR/xlispstatR
RUN ./configure --prefix=/xstatR/build && make && make install

WORKDIR /xstatR
CMD ./build/bin/xlispstat

#CMD exec /bin/bash -c "trap : TERM INT; sleep infinity & wait"
