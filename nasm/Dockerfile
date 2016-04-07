FROM kdridi/base:ubuntu-14.04-r3
MAINTAINER Karim DRIDI <karim.dridi@gmail.com>

USER root
ENV USER root
ENV HOME /root
ENV DEBIAN_FRONTEND noninteractive

COPY 01proxy /etc/apt/apt.conf.d/01proxy

RUN apt-get update -y
RUN apt-get upgrade -y

# SYSTEM #######################################################################
RUN apt-get install -y nasm
RUN git clone https://github.com/vertis/objconv.git objconv.git
RUN g++ -o objconv -O2 objconv.git/src/*.cpp
RUN mv objconv /usr/bin/objconv
RUN rm -rf objconv.git
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

WORKDIR /app
CMD [ "zsh" ]