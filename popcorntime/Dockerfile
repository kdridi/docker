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
RUN apt-get install -y libxrender1 libatk1.0-0 libpangocairo-1.0-0 libxi6 libxcomposite1 libnss3 libxfixes3 libxdamage1 libgconf-2-4 libgdk-pixbuf2.0-0 libgtk2.0-0 libudev1
RUN wget http://popcorn-time.to/Popcorn-Time-linux64.tar.gz
RUN tar -xf Popcorn-Time-linux64.tar.gz -C /usr/local/bin/
RUN rm Popcorn-Time-linux64.tar.gz
RUN cp /lib/x86_64-linux-gnu/libudev.so.1 /lib/x86_64-linux-gnu/libudev.so.0

RUN wget "ftp://ftp.free.fr/mirrors/ftp.ubuntu.com/ubuntu/ls-lR.gz"
RUN apt-add-repository -y multiverse
RUN apt-get autoremove -y
RUN apt-get autoclean -y
RUN apt-get clean -y
RUN apt-get update -y
RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get install -y ttf-mscorefonts-installer

RUN apt-get autoremove -y
RUN apt-get autoclean -y
RUN apt-get clean -y

RUN export uid=1000 gid=1000 && login=kdridi && home=/home/${login} && \
    chown ${uid}:${gid} -R ${home}

USER kdridi
ENV USER kdridi
ENV HOME /home/$USER

WORKDIR $HOME
CMD Popcorn-Time
