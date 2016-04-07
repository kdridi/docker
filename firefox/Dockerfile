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
RUN apt-get install -y firefox cmake gcc g++ git pkg-config ragel libasound2-dev libssl-dev libglib2.0-dev libconfig-dev libpango1.0-dev libgl1-mesa-dev libevent-dev libgtk2.0-dev libxrandr-dev libxrender-dev libxcursor-dev libv4l-dev libgles2-mesa-dev libavcodec-dev libva-dev libvdpau-dev libdrm-dev libpulse-dev
RUN git clone https://github.com/i-rinat/freshplayerplugin.git && \
    cd freshplayerplugin && \
    mkdir build && \
    cd build && \
    cmake .. && \
    make -j8 && \
    make install
RUN apt-add-repository -y multiverse
RUN apt-get autoremove -y
RUN apt-get autoclean -y
RUN apt-get clean -y
RUN apt-get update -y
RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get install -y pepperflashplugin-nonfree
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
RUN mkdir -p .mozilla/plugins && \
    cp /usr/local/lib/mozilla/plugins/libfreshwrapper-flashplayer.so .mozilla/plugins/
################################################################################

CMD [ "firefox", "https://www.google.fr/" ]
