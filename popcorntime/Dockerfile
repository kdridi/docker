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
RUN apt-get install -y libxrender1 libatk1.0-0 libpangocairo-1.0-0 libxi6 libxcomposite1 libnss3 libxfixes3 libxdamage1 libgconf-2-4 libgdk-pixbuf2.0-0 libgtk2.0-0 libudev1
RUN wget http://popcorn-time.to/Popcorn-Time-linux64.tar.gz
RUN tar -xf Popcorn-Time-linux64.tar.gz -C /usr/local/bin/
RUN rm Popcorn-Time-linux64.tar.gz
RUN cp /lib/x86_64-linux-gnu/libudev.so.1 /lib/x86_64-linux-gnu/libudev.so.0
RUN apt-add-repository -y multiverse
RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get install -y ttf-mscorefonts-installer
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
################################################################################

WORKDIR $HOME
CMD [ "Popcorn-Time" ]
