FROM library/ubuntu:bionic
MAINTAINER Jim Harner ejharner@gmail.com

# update apt
RUN apt-get update

CMD exec /bin/bash -c "trap : TERM INT; sleep infinity & wait"
