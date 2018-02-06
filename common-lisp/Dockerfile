FROM debian:stable

RUN export DEBIAN_FRONTEND=noninteractive && \
	apt-get update && \
	apt-get install -y apt-utils && \
	apt-get upgrade -y && \
	apt-get install -y apt-file sudo git sbcl cclive ecl emacs25-nox slime && \
	apt-file update && \
	useradd -ms /bin/bash -G sudo kdridi && \
	passwd -d kdridi

USER kdridi
WORKDIR /home/kdridi
ENV HOME /home/kdridi