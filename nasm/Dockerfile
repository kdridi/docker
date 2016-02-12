FROM ubuntu:14.04

USER root
ENV USER root
ENV HOME /root
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get install -y sudo pulseaudio vim wget ca-certificates build-essential git command-not-found zsh gdb

RUN export uid=1000 gid=1000 && \
    mkdir -p /home/kdridi && \
    echo "kdridi:x:${uid}:${gid}:kdridi,,,:/home/kdridi:/bin/bash" >> /etc/passwd && \
    echo "kdridi:x:${uid}:" >> /etc/group && \
    echo "kdridi ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/kdridi && \
    chmod 0440 /etc/sudoers.d/kdridi && \
    chown ${uid}:${gid} -R /home/kdridi

RUN apt-get install -y nasm

USER kdridi
ENV USER kdridi
ENV HOME /home/$USER
ENV DISPLAY :0.0
ENV PULSE_SERVER tcp:172.17.0.1
ENV LC_ALL C.UTF-8

WORKDIR $HOME
RUN git clone git://github.com/robbyrussell/oh-my-zsh.git .oh-my-zsh
COPY zshrc.conf .zshrc

WORKDIR /app

CMD /bin/zsh
