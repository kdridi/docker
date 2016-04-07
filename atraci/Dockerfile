FROM kdridi/base:ubuntu-14.04-r3
MAINTAINER Karim DRIDI <karim.dridi@gmail.com>

USER root
ENV USER root
ENV HOME /root
ENV DEBIAN_FRONTEND noninteractive

COPY 01proxy /etc/apt/apt.conf.d/01proxy

RUN apt-get update -y
RUN apt-get upgrade -y

WORKDIR $HOME
# SYSTEM #######################################################################
RUN apt-get install -y libgtk2.0-0 libnss3 libgconf-2-4 libasound2 libxtst6 libnotify4
RUN cp /lib/x86_64-linux-gnu/libudev.so.1 /lib/x86_64-linux-gnu/libudev.so.0
################################################################################

RUN apt-get autoremove -y
RUN apt-get autoclean -y
RUN apt-get clean -y

RUN rm -rf /etc/apt/apt.conf.d/01proxy

USER kdridi
ENV USER kdridi
ENV HOME /home/$USER

WORKDIR $HOME
# USER #########################################################################
RUN wget -c https://github.com/Atraci/Atraci/releases/download/0.7.0/Atraci-linux64.tar.gz && \
  	tar -xzf Atraci-linux64.tar.gz && \
  	rm -rf Atraci-linux64.tar.gz
################################################################################

WORKDIR $HOME
CMD [ "./linux64/Atraci" ]
