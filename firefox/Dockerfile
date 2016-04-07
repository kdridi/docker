FROM ubuntu:14.04

USER root
ENV USER root
ENV HOME /root
ENV DEBIAN_FRONTEND noninteractive

COPY 01proxy /etc/apt/apt.conf.d/01proxy

RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get install -y sudo pulseaudio vim wget ca-certificates build-essential git command-not-found zsh gdb software-properties-common

RUN export uid=1000 gid=1000 && login=kdridi && home=/home/${login} && \
    git clone git://github.com/robbyrussell/oh-my-zsh.git ${home}/.oh-my-zsh && \
    echo "${login}:x:${uid}:${gid}:${login},,,:${home}:/bin/bash" >> /etc/passwd && \
    echo "${login}:x:${uid}:" >> /etc/group && \
    echo "${login} ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/${login} && \
    chmod 0440 /etc/sudoers.d/${login} && \
    chown ${uid}:${gid} -R ${home}

USER kdridi
ENV USER kdridi
ENV HOME /home/$USER

WORKDIR $HOME
COPY zshrc.conf .zshrc

USER root
ENV USER root
ENV HOME /$USER
ENV DEBIAN_FRONTEND noninteractive

WORKDIR $HOME
RUN apt-get install -y firefox
RUN apt-get install -y cmake gcc g++ git pkg-config ragel libasound2-dev libssl-dev libglib2.0-dev libconfig-dev libpango1.0-dev libgl1-mesa-dev libevent-dev libgtk2.0-dev libxrandr-dev libxrender-dev libxcursor-dev libv4l-dev libgles2-mesa-dev libavcodec-dev libva-dev libvdpau-dev libdrm-dev libpulse-dev
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

RUN apt-get autoremove -y
RUN apt-get autoclean -y
RUN apt-get clean -y

RUN export uid=1000 gid=1000 && login=kdridi && home=/home/${login} && \
    chown ${uid}:${gid} -R ${home}

USER kdridi
ENV USER kdridi
ENV HOME /home/$USER

WORKDIR $HOME
RUN mkdir -p .mozilla/plugins && \
    cp /usr/local/lib/mozilla/plugins/libfreshwrapper-flashplayer.so .mozilla/plugins/

CMD firefox
